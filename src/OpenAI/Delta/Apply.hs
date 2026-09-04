{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}

module OpenAI.Delta.Apply (apply, ApplyResult(..)) where

import qualified Data.ByteArray as Ba
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Bl
import Data.Int (Int32, Int64)
import Data.List (foldl', sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Mp
import Data.Maybe (catMaybes, mapMaybe)
import Data.Scientific (Scientific, toRealFloat)
import Data.Set (Set)
import qualified Data.Set as St
import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Aeson as Ae
import Data.Aeson (Value)

import qualified Crypto.Hash as Ch

import Hasql.Statement (Statement)
import qualified Hasql.TH as TH
import qualified Hasql.Transaction as Htx

import qualified OpenAI.Delta.Snap as Ds
import qualified OpenAI.Delta.Types as Dt
import qualified OpenAI.Conversation.Json.Schema as Jd
import qualified OpenAI.Conversation.Json.V1.Schema as Jv1
import qualified OpenAI.Conversation.Json.V1.Convert as Jcv
import qualified OpenAI.Conversation.Serialize.Content as Cw
import qualified OpenAI.Conversation.Serialize.ConversationStmt as Cs


data ApplyResult = ApplyResult {
    uidConv :: Int64
    , nodeAddedCnt :: Int
    , nodeMovedCnt :: Int
    , nodeRewriteCnt :: Int
    , msgAddedCnt :: Int
    , msgRewriteCnt :: Int
    , metaUpdated :: Bool
    , ingestRecorded :: Bool
  }
  deriving stock (Eq, Show)


data ApplySt = ApplySt {
    uidsNodeAS :: Map Text Int64
    , nodesAddedAS :: Int
    , msgsAddedAS :: Int
  }


apply :: Text -> Jv1.Conversation -> Ds.ConvSnap -> Dt.Delta -> Htx.Transaction (Either [Dt.Conflict] ApplyResult)
apply sourceKey conversation jsonSnap delta =
  case preflight conversation jsonSnap delta of
    conflicts@(_ : _) -> pure $ Left conflicts
    [] -> do
      metadataChanged <- applyMeta delta.uidConv delta.metaAct
      let
        uidByEid = initialNodeUids delta.nodeActs
        nodeSnapByEid = snapNodeMap jsonSnap
        addActs = sortAddActs delta.nodeActs
        rewriteActs = rewriteNodeActs delta.nodeActs
        addedEids = St.fromList $ map eidAddNA addActs

      moveRez <- applyMoves uidByEid nodeSnapByEid delta.nodeActs
      case moveRez of
        Left conflicts -> abort conflicts
        Right movedCnt -> do
          addRez <- applyAddedNodes delta.uidConv conversation uidByEid addActs
          case addRez of
            Left conflicts -> abort conflicts
            Right addState -> do
              msgAddRez <- applyMessageAdds conversation addedEids addState.uidsNodeAS rewriteActs
              case msgAddRez of
                Left conflicts -> abort conflicts
                Right explicitMsgCnt -> do
                  msgRewriteRez <- applyMessageRewrites conversation rewriteActs
                  case msgRewriteRez of
                    Left conflicts -> abort conflicts
                    Right rewrittenMsgCnt ->
                      -- TODO: handle the V1 vs V2 correctly.
                      let
                        v2Conv = Jcv.v1ToCurrent conversation
                      in do
                      Htx.statement (delta.uidConv, nonEmpty sourceKey, Just $ sha256 $ Ae.encode v2Conv
                        , "applied") Cs.insertConversationIngest

                      pure $ Right ApplyResult {
                          uidConv = delta.uidConv
                          , nodeAddedCnt = addState.nodesAddedAS
                          , nodeMovedCnt = movedCnt
                          , nodeRewriteCnt = countNodeRewrites rewriteActs
                          , msgAddedCnt = addState.msgsAddedAS + explicitMsgCnt
                          , msgRewriteCnt = rewrittenMsgCnt
                          , metaUpdated = metadataChanged
                          , ingestRecorded = True
                        }


abort :: [Dt.Conflict] -> Htx.Transaction (Either [Dt.Conflict] a)
abort conflicts = do
  Htx.condemn
  pure $ Left conflicts


applyMeta :: Int64 -> Dt.MetaAct -> Htx.Transaction Bool
applyMeta conversationUid metaAct =
  case metaAct of
    Dt.KeepMeta -> pure False
    Dt.UpdateMeta titleOld titleNew timeOld timeNew -> do
      Htx.statement (conversationUid, toRealFloat timeOld, titleOld) Cs.insertConversationPrevious
      Htx.statement (titleNew, toRealFloat timeNew, conversationUid) Cs.updateConversation
      pure True
    Dt.RejectOlderMeta _ _ -> pure False


applyMoves :: Map Text Int64 -> Map Text Ds.NodeSnap -> [Dt.NodeAct]
      -> Htx.Transaction (Either [Dt.Conflict] Int)
applyMoves uidByEid nodeSnapByEid actions =
  scanNode 0 $ mapMaybe moveFromAct actions
  where
  scanNode movedCnt [] = pure $ Right movedCnt
  scanNode movedCnt ((refNode, eidParentNew, seqChildNew, seqPreNew) : rest) =
    case (uidRefNode refNode, Mp.lookup (eidRefNode refNode) nodeSnapByEid, parentUid uidByEid eidParentNew) of
      (Nothing, _, _) -> abort [Dt.MissingDbNodeC $ eidRefNode refNode]
      (_, Nothing, _) -> abort [Dt.MissingJsonNodeC $ eidRefNode refNode]
      (_, _, Left conflict) -> abort [conflict]
      (Just nodeUid, Just nodeSnap, Right parentFk) -> do
        Htx.statement (parentFk, nodeSnap.seqNode, seqChildNew, seqPreNew, nodeUid) Cs.updateNodeOrder
        scanNode (movedCnt + 1) rest


applyAddedNodes :: Int64 -> Jv1.Conversation -> Map Text Int64 -> [Dt.NodeAct]
      -> Htx.Transaction (Either [Dt.Conflict] ApplySt)
applyAddedNodes conversationUid conversation uidByEid =
  foldNodes ApplySt {
      uidsNodeAS = uidByEid
      , nodesAddedAS = 0
      , msgsAddedAS = 0
    }
  where
  foldNodes state [] = pure $ Right state
  foldNodes state (action : rest) =
    case action of
      Dt.AddNA eidNode eidParent seqNode seqChild seqPre ->
        case (Mp.lookup eidNode conversation.mappingCv, parentUid state.uidsNodeAS eidParent) of
          (Nothing, _) -> abort [Dt.MissingJsonNodeC eidNode]
          (_, Left conflict) -> abort [conflict]
          (Just node, Right parentFk) -> do
            nodeUid <- Htx.statement (conversationUid, eidNode, parentFk, seqNode, seqChild, seqPre) Cs.insertNode
            let
              stateWithNode = state {
                  uidsNodeAS = Mp.insert eidNode nodeUid state.uidsNodeAS
                  , nodesAddedAS = state.nodesAddedAS + 1
                }

            case node.messageNd of
              Nothing -> foldNodes stateWithNode rest
              Just message -> do
                messageRez <- Cw.insertMsgTree nodeUid message
                case messageRez of
                  Left err ->
                    abort [Dt.BrokenShapeC $ "message insert failed for node " <> eidNode <> ": " <> err]
                  Right _ ->
                    foldNodes stateWithNode {
                        msgsAddedAS = stateWithNode.msgsAddedAS + 1
                      } rest
      _ -> foldNodes state rest


applyMessageAdds :: Jv1.Conversation -> Set Text -> Map Text Int64 -> [Dt.NodeAct]
      -> Htx.Transaction (Either [Dt.Conflict] Int)
applyMessageAdds conversation addedEids uidByEid actions =
  foldMsg 0 $ mapMaybe addMsgFromNodeAct actions
  where
  foldMsg addedCnt [] = pure $ Right addedCnt
  foldMsg addedCnt ((eidNode, eidMsg) : rest)
    | St.member eidNode addedEids = foldMsg addedCnt rest
    | otherwise =
        case (Mp.lookup eidNode uidByEid, messageAtNode conversation eidNode eidMsg) of
          (Nothing, _) -> abort [Dt.MissingDbNodeC eidNode]
          (_, Nothing) -> abort [Dt.MissingJsonNodeC $ "message " <> eidMsg <> " at node " <> eidNode]
          (Just nodeUid, Just message) -> do
            messageRez <- Cw.insertMsgTree nodeUid message
            case messageRez of
              Left err ->
                abort [Dt.BrokenShapeC $ "message insert failed for " <> eidMsg <> ": " <> err]
              Right _ ->
                foldMsg (addedCnt + 1) rest


applyMessageRewrites :: Jv1.Conversation -> [Dt.NodeAct]
      -> Htx.Transaction (Either [Dt.Conflict] Int)
applyMessageRewrites conversation actions =
  iterMsg 0 $ mapMaybe rewriteMsgFromNodeAct actions
  where
  messageByEid = jsonMessageMap conversation

  iterMsg rewrittenCnt [] = pure $ Right rewrittenCnt
  iterMsg rewrittenCnt ((refMsg, hashOld) : rest) =
    case (uidRefMsg refMsg, Mp.lookup (eidRefMsg refMsg) messageByEid) of
      (Nothing, _) ->
        abort [Dt.BrokenShapeC $ "message rewrite has no DB uid: " <> eidRefMsg refMsg]
      (_, Nothing) ->
        abort [Dt.MissingJsonNodeC $ "message " <> eidRefMsg refMsg]
      (Just messageUid, Just message) -> do
        rewriteRez <- rewriteMessage messageUid hashOld message
        case rewriteRez of
          Left conflicts -> abort conflicts
          Right () -> iterMsg (rewrittenCnt + 1) rest


rewriteMessage :: Int64 -> Maybe Dt.Hash -> Jd.Message
      -> Htx.Transaction (Either [Dt.Conflict] ())
rewriteMessage messageUid oldHash message = do
  previousTimes <- Htx.statement messageUid selectMessageTimes
  case previousTimes of
    Nothing ->
      pure $ Left [Dt.BrokenShapeC $ "message row disappeared before rewrite: " <> T.pack (show messageUid)]
    Just (timeCreateOld, timeUpdateOld) -> do
      payloadOld <- Htx.statement messageUid Cs.selectMessagePayload
      let
        hashOldBytes =
          case oldHash of
            Just hashValue -> bytesHash hashValue
            Nothing -> sha256 $ Ae.encode payloadOld

      Htx.statement
        (messageUid, timeCreateOld, timeUpdateOld, hashOldBytes, payloadOld)
        Cs.insertMessagePrevious

      rewriteRez <- Cw.rewriteMsgTree messageUid message
      case rewriteRez of
        Left err ->
          pure $ Left [Dt.BrokenShapeC $ "message rewrite failed for " <> message.idMsg <> ": " <> err]
        Right () ->
          pure $ Right ()


preflight :: Jv1.Conversation -> Ds.ConvSnap -> Dt.Delta -> [Dt.Conflict]
preflight conversation jsonSnap delta =
  identityConflicts conversation jsonSnap delta
    <> actionConflicts delta
    <> duplicateConflicts conversation delta
    <> validateNodeRefs delta.nodeActs
    <> validateAddActs conversation jsonSnap initialUids addActs
    <> validateMoveActs jsonSnap initialUids delta.nodeActs
    <> validateMessageActs conversation delta.nodeActs
  where
  initialUids = initialNodeUids delta.nodeActs
  addActs = sortAddActs delta.nodeActs


identityConflicts :: Jv1.Conversation -> Ds.ConvSnap -> Dt.Delta -> [Dt.Conflict]
identityConflicts conversation jsonSnap delta =
  catMaybes [
      mismatch "JSON conversation and delta" conversation.convIdCv delta.eidConv
      , mismatch "JSON conversation and snapshot" conversation.convIdCv jsonSnap.eidConv
    ]
  where
  mismatch label expected actual
    | expected == actual = Nothing
    | otherwise =
        Just $ Dt.BrokenShapeC $ label <> " eid mismatch: expected " <> expected <> ", received " <> actual


actionConflicts :: Dt.Delta -> [Dt.Conflict]
actionConflicts delta =
  metaConflicts delta.metaAct <> concatMap nodeConflicts delta.nodeActs
  where
  metaConflicts metaAct =
    case metaAct of
      Dt.RejectOlderMeta _ _ -> [Dt.OlderJsonC]
      _ -> []

  nodeConflicts nodeAct =
    case nodeAct of
      Dt.ConflictNA _ conflict -> [conflict]
      Dt.RewriteNA _ (Just messageAct) -> messageConflicts messageAct
      _ -> []

  messageConflicts messageAct =
    case messageAct of
      Dt.ConflictMA _ conflict -> [conflict]
      _ -> []


duplicateConflicts :: Jv1.Conversation -> Dt.Delta -> [Dt.Conflict]
duplicateConflicts conversation delta =
  map Dt.DuplicateEidC duplicateNodeAdds <> map Dt.DuplicateEidC duplicateMessageEids
  where
  duplicateNodeAdds = duplicates $ map eidAddNA $ sortAddActs delta.nodeActs
  duplicateMessageEids = duplicates $ map fst $ messageEntries conversation


validateNodeRefs :: [Dt.NodeAct] -> [Dt.Conflict]
validateNodeRefs =
  concatMap validate
  where
  validate action =
    case action of
      Dt.KeepNA refNode -> requireNodeUid refNode
      Dt.MoveNA refNode _ _ _ _ _ _ -> requireNodeUid refNode
      Dt.RewriteNA refNode messageAct -> requireNodeUid refNode <> maybe [] validateMessageRef messageAct
      Dt.ConflictNA refNode _ -> requireNodeUid refNode
      Dt.AddNA {} -> []

  validateMessageRef messageAct =
    case messageAct of
      Dt.KeepMA refMsg -> requireMsgUid refMsg
      Dt.RewriteMA refMsg _ _ _ -> requireMsgUid refMsg
      Dt.ConflictMA refMsg _ -> requireMsgUid refMsg
      Dt.AddMA _ _ -> []

  requireNodeUid refNode =
    case uidRefNode refNode of
      Nothing -> [Dt.MissingDbNodeC $ eidRefNode refNode]
      Just _ -> []

  requireMsgUid refMsg =
    case uidRefMsg refMsg of
      Nothing -> [Dt.BrokenShapeC $ "message action has no DB uid: " <> eidRefMsg refMsg]
      Just _ -> []


validateAddActs :: Jv1.Conversation -> Ds.ConvSnap -> Map Text Int64 -> [Dt.NodeAct] -> [Dt.Conflict]
validateAddActs conversation jsonSnap initialUids actions =
  snd $ foldl' foldValidation (Mp.keysSet initialUids, []) actions
  where
  nodeSnapByEid = snapNodeMap jsonSnap

  foldValidation (knownEids, conflicts) action =
    case action of
      Dt.AddNA eidNode eidParent seqNode seqChild seqPre ->
        let
          nodeConflicts =
            case Mp.lookup eidNode conversation.mappingCv of
              Nothing -> [Dt.MissingJsonNodeC eidNode]
              Just node
                | node.parentNd /= eidParent -> [Dt.ParentMismatchC eidNode]
                | otherwise -> []

          snapConflicts =
            case Mp.lookup eidNode nodeSnapByEid of
              Nothing -> [Dt.MissingJsonNodeC eidNode]
              Just nodeSnap
                | nodeSnap.eidParent /= eidParent -> [Dt.ParentMismatchC eidNode]
                | nodeSnap.seqNode /= seqNode || nodeSnap.seqChild /= seqChild || nodeSnap.seqPre /= seqPre ->
                    [Dt.BrokenShapeC $ "node order differs between snapshot and delta: " <> eidNode]
                | otherwise -> []

          parentConflicts =
            case eidParent of
              Nothing -> []
              Just parentEid
                | parentEid == eidNode -> [Dt.BrokenShapeC $ "node cannot be its own parent: " <> eidNode]
                | St.member parentEid knownEids -> []
                | otherwise -> [Dt.MissingDbNodeC parentEid]

          duplicateConflict
            | St.member eidNode knownEids = [Dt.DuplicateEidC eidNode]
            | otherwise = []
        in
        ( St.insert eidNode knownEids
        , conflicts <> duplicateConflict <> nodeConflicts <> snapConflicts <> parentConflicts
        )
      _ -> (knownEids, conflicts)


validateMoveActs :: Ds.ConvSnap -> Map Text Int64 -> [Dt.NodeAct] -> [Dt.Conflict]
validateMoveActs jsonSnap initialUids =
  concatMap validate
  where
  nodeSnapByEid = snapNodeMap jsonSnap

  validate action =
    case action of
      Dt.MoveNA refNode _ eidParentNew _ seqChildNew _ seqPreNew ->
        let
          eidNode = eidRefNode refNode

          refConflicts =
            case uidRefNode refNode of
              Nothing -> [Dt.MissingDbNodeC eidNode]
              Just _ -> []

          parentConflicts =
            case eidParentNew of
              Nothing -> []
              Just parentEid
                | parentEid == eidNode -> [Dt.BrokenShapeC $ "node cannot be moved under itself: " <> eidNode]
                | Mp.member parentEid initialUids -> []
                | otherwise -> [Dt.MissingDbNodeC parentEid]

          snapConflicts =
            case Mp.lookup eidNode nodeSnapByEid of
              Nothing -> [Dt.MissingJsonNodeC eidNode]
              Just nodeSnap
                | nodeSnap.eidParent /= eidParentNew -> [Dt.ParentMismatchC eidNode]
                | nodeSnap.seqChild /= seqChildNew || nodeSnap.seqPre /= seqPreNew ->
                    [Dt.BrokenShapeC $ "move order differs between snapshot and delta: " <> eidNode]
                | otherwise -> []
        in
        refConflicts <> parentConflicts <> snapConflicts
      _ -> []


validateMessageActs :: Jv1.Conversation -> [Dt.NodeAct] -> [Dt.Conflict]
validateMessageActs conversation =
  concatMap validateNode
  where
  messageByEid = jsonMessageMap conversation

  validateNode action =
    case action of
      Dt.RewriteNA _ (Just messageAct) -> validateMessage messageAct
      _ -> []

  validateMessage messageAct =
    case messageAct of
      Dt.KeepMA _ -> []
      Dt.AddMA eidNode eidMsg ->
        case messageAtNode conversation eidNode eidMsg of
          Nothing -> [Dt.MissingJsonNodeC $ "message " <> eidMsg <> " at node " <> eidNode]
          Just _ -> []
      Dt.RewriteMA refMsg _ _ _ ->
        case Mp.lookup (eidRefMsg refMsg) messageByEid of
          Nothing -> [Dt.MissingJsonNodeC $ "message " <> eidRefMsg refMsg]
          Just _ -> []
      Dt.ConflictMA _ conflict -> [conflict]


initialNodeUids :: [Dt.NodeAct] -> Map Text Int64
initialNodeUids =
  foldl' addRef Mp.empty
  where
  addRef uidByEid action =
    case nodeRefFromAct action of
      Nothing -> uidByEid
      Just refNode ->
        case uidRefNode refNode of
          Nothing -> uidByEid
          Just nodeUid -> Mp.insert (eidRefNode refNode) nodeUid uidByEid


nodeRefFromAct :: Dt.NodeAct -> Maybe Dt.RefNode
nodeRefFromAct action =
  case action of
    Dt.KeepNA refNode -> Just refNode
    Dt.MoveNA refNode _ _ _ _ _ _ -> Just refNode
    Dt.RewriteNA refNode _ -> Just refNode
    Dt.ConflictNA refNode _ -> Just refNode
    Dt.AddNA {} -> Nothing


sortAddActs :: [Dt.NodeAct] -> [Dt.NodeAct]
sortAddActs = sortOn seqPreAdd . filter isAddNA


rewriteNodeActs :: [Dt.NodeAct] -> [Dt.NodeAct]
rewriteNodeActs = filter isRewriteNA


isAddNA :: Dt.NodeAct -> Bool
isAddNA action =
  case action of
    Dt.AddNA {} -> True
    _ -> False


isRewriteNA :: Dt.NodeAct -> Bool
isRewriteNA action =
  case action of
    Dt.RewriteNA {} -> True
    _ -> False


eidAddNA :: Dt.NodeAct -> Text
eidAddNA action =
  case action of
    Dt.AddNA eidNode _ _ _ _ -> eidNode
    _ -> ""


seqPreAdd :: Dt.NodeAct -> Int32
seqPreAdd action =
  case action of
    Dt.AddNA _ _ _ _ seqPre -> seqPre
    _ -> maxBound


moveFromAct :: Dt.NodeAct -> Maybe (Dt.RefNode, Maybe Text, Int32, Int32)
moveFromAct action =
  case action of
    Dt.MoveNA refNode _ eidParentNew _ seqChildNew _ seqPreNew ->
      Just (refNode, eidParentNew, seqChildNew, seqPreNew)
    _ -> Nothing


addMsgFromNodeAct :: Dt.NodeAct -> Maybe (Text, Text)
addMsgFromNodeAct action =
  case action of
    Dt.RewriteNA _ (Just messageAct) ->
      case messageAct of
        Dt.AddMA eidNode eidMsg -> Just (eidNode, eidMsg)
        _ -> Nothing
    _ -> Nothing


rewriteMsgFromNodeAct :: Dt.NodeAct -> Maybe (Dt.RefMsg, Maybe Dt.Hash)
rewriteMsgFromNodeAct action =
  case action of
    Dt.RewriteNA _ (Just messageAct) ->
      case messageAct of
        Dt.RewriteMA refMsg hashOld _ _ -> Just (refMsg, hashOld)
        _ -> Nothing
    _ -> Nothing


countNodeRewrites :: [Dt.NodeAct] -> Int
countNodeRewrites =
  length . filter writesNode
  where
  writesNode action =
    case action of
      Dt.RewriteNA _ (Just messageAct) ->
        case messageAct of
          Dt.AddMA {} -> True
          Dt.RewriteMA {} -> True
          _ -> False
      _ -> False


snapNodeMap :: Ds.ConvSnap -> Map Text Ds.NodeSnap
snapNodeMap snapshot =
  Mp.fromList $ map (\node -> (node.eidNode, node)) snapshot.nodes


jsonMessageMap :: Jv1.Conversation -> Map Text Jd.Message
jsonMessageMap conversation =
  Mp.fromList $ messageEntries conversation


messageEntries :: Jv1.Conversation -> [(Text, Jd.Message)]
messageEntries conversation =
  mapMaybe fromNode $ Mp.elems conversation.mappingCv
  where
  fromNode :: Jv1.Node -> Maybe (Text, Jd.Message)
  fromNode node =
    case node.messageNd of
      Nothing -> Nothing
      Just message -> Just (message.idMsg, message)


messageAtNode :: Jv1.Conversation -> Text -> Text -> Maybe Jd.Message
messageAtNode conversation eidNode eidMsg = do
  node <- Mp.lookup eidNode conversation.mappingCv
  message <- node.messageNd
  if message.idMsg == eidMsg then Just message else Nothing


parentUid :: Map Text Int64 -> Maybe Text -> Either Dt.Conflict (Maybe Int64)
parentUid _ Nothing =
  Right Nothing

parentUid uidByEid (Just eidParent) =
  case Mp.lookup eidParent uidByEid of
    Nothing -> Left $ Dt.MissingDbNodeC eidParent
    Just parentUidValue -> Right $ Just parentUidValue


eidRefNode :: Dt.RefNode -> Text
eidRefNode (Dt.RefNode eidNode _) =
  eidNode


uidRefNode :: Dt.RefNode -> Maybe Int64
uidRefNode (Dt.RefNode _ nodeUid) =
  nodeUid


eidRefMsg :: Dt.RefMsg -> Text
eidRefMsg (Dt.RefMsg eidMsg _) =
  eidMsg


uidRefMsg :: Dt.RefMsg -> Maybe Int64
uidRefMsg (Dt.RefMsg _ messageUid) =
  messageUid


bytesHash :: Dt.Hash -> ByteString
bytesHash (Dt.Hash bytes) =
  bytes


duplicates :: Ord a => [a] -> [a]
duplicates values =
  St.toList duplicateSet
  where
  (_, duplicateSet) =
    foldl' setMerge (St.empty, St.empty) values

  setMerge (seen, found) value
    | St.member value seen = (seen, St.insert value found)
    | otherwise = (St.insert value seen, found)


nonEmpty :: Text -> Maybe Text
nonEmpty value
  | T.null $ T.strip value = Nothing
  | otherwise = Just value


sha256 :: Bl.ByteString -> ByteString
sha256 bytes =
  Ba.convert (Ch.hashlazy bytes :: Ch.Digest Ch.SHA256)


selectMessageTimes :: Statement Int64 (Maybe (Maybe Double, Maybe Double))
selectMessageTimes =
  [TH.maybeStatement|
    select
      m.create_time :: float8?,
      m.update_time :: float8?
    from oai.messages m
    where m.uid = $1 :: int8
  |]