module OpenAI.Delta.Snap.Db (load) where

import Data.Int (Int32, Int64)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Mp
import Data.Set (Set)
import qualified Data.Set as St
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import qualified Hasql.Transaction as Htx

import qualified OpenAI.Content.Codec as Cc
import qualified OpenAI.Content.Read as Cr
import qualified OpenAI.Content.Types as Ct
import qualified OpenAI.Delta.Hash as Dh
import qualified OpenAI.Delta.Snap as Snap
import OpenAI.Delta.Snap (ConvSnap)
import OpenAI.Delta.Types (Conflict(..))
import qualified OpenAI.Deserialize.ConversationStmt as Dst


data RowsDb = RowsDb {
    nodesRD :: Vector Dst.NodeRow
    , messagesRD :: Vector Dst.MessageRow
  }


load :: Int64 -> Htx.Transaction (Either [Conflict] ConvSnap)
load uidConv = do
  rowConvMb <- Htx.statement uidConv Dst.selectConversationByUid
  case rowConvMb of
    Nothing -> pure $ Left [BrokenShapeC $ "conversation row not found for uid " <> showT uidConv]
    Just rowConv -> do
      rows <- loadRows uidConv
      let
        conflicts = validateRows rows
      if null conflicts then
        buildConversation rowConv rows
      else
        pure $ Left conflicts


loadRows :: Int64 -> Htx.Transaction RowsDb
loadRows uidConv = do
  nodes <- Htx.statement uidConv Dst.selectNodes
  messages <- Htx.statement uidConv Dst.selectMessagesWithAuthor
  pure RowsDb {
      nodesRD = nodes
      , messagesRD = messages
    }


validateRows :: RowsDb -> [Conflict]
validateRows rows =
  duplicateNodeConflicts rows
    <> duplicateMessageConflicts rows
    <> duplicateNodeMessageConflicts rows
    <> duplicateNodeSeqConflicts rows
    <> missingParentConflicts rows
    <> missingMessageNodeConflicts rows
    <> rootConflicts rows


duplicateNodeConflicts :: RowsDb -> [Conflict]
duplicateNodeConflicts rows =
  map (DuplicateEidC . ("node:" <>)) $ duplicates $ V.map nodeEidRow rows.nodesRD


duplicateMessageConflicts :: RowsDb -> [Conflict]
duplicateMessageConflicts rows =
  map (DuplicateEidC . ("message:" <>)) $ duplicates $ V.map messageEidRow rows.messagesRD


duplicateNodeMessageConflicts :: RowsDb -> [Conflict]
duplicateNodeMessageConflicts rows =
  map duplicateNodeMessage $ duplicates $ V.map messageNodeFkRow rows.messagesRD
  where
  duplicateNodeMessage uidNode =
    BrokenShapeC $ "multiple messages reference node uid " <> showT uidNode


duplicateNodeSeqConflicts :: RowsDb -> [Conflict]
duplicateNodeSeqConflicts rows =
  map duplicateSeq $ duplicates $ V.map nodeSeqRow rows.nodesRD
  where
  duplicateSeq seqNode =
    BrokenShapeC $ "multiple nodes use sequence " <> showT seqNode


missingParentConflicts :: RowsDb -> [Conflict]
missingParentConflicts rows =
  let
    nodeUids = St.fromList $ V.toList $ V.map nodeUidRow rows.nodesRD
  in
  concatMap (checkParent nodeUids) $ V.toList rows.nodesRD


missingMessageNodeConflicts :: RowsDb -> [Conflict]
missingMessageNodeConflicts rows =
  let
    nodeUids = St.fromList $ V.toList $ V.map nodeUidRow rows.nodesRD
  in
  concatMap (checkMessageNode nodeUids) $ V.toList rows.messagesRD


rootConflicts :: RowsDb -> [Conflict]
rootConflicts rows =
  case filter nodeIsRoot $ V.toList rows.nodesRD of
    [] -> [BrokenShapeC "conversation has no root node"]
    [_] -> []
    roots ->
      [BrokenShapeC $ "conversation has multiple root nodes: " <> T.intercalate ", " (map nodeEidRow roots)]


checkParent :: Set Int64 -> Dst.NodeRow -> [Conflict]
checkParent nodeUids (_, eidNode, parentFk, _, _, _) =
  case parentFk of
    Nothing -> []
    Just uidParent
      | St.member uidParent nodeUids -> []
      | otherwise ->
          [BrokenShapeC $ "node " <> eidNode <> " references missing parent uid " <> showT uidParent]


checkMessageNode :: Set Int64 -> Dst.MessageRow -> [Conflict]
checkMessageNode nodeUids row
  | St.member rowNodeUid nodeUids = []
  | otherwise =
      [BrokenShapeC $ "message " <> messageEidRow row <> " references missing node uid " <> showT rowNodeUid]
  where
  rowNodeUid = messageNodeFkRow row


buildConversation :: Dst.ConversationRow -> RowsDb -> Htx.Transaction (Either [Conflict] ConvSnap)
buildConversation (uidConv, eidConv, titleConv, timeCreate, timeUpdate) rows = do
  messagesE <- buildMessagesByNode rows.messagesRD
  case messagesE of
    Left conflicts -> pure $ Left conflicts
    Right messagesByNode ->
      let
        nodes = buildNodes messagesByNode rows.nodesRD
      in
      pure $ Right Snap.ConvSnap {
          eidConv = eidConv
          , uidConv = Just uidConv
          , titleConv = titleConv
          , timeCreateCv = timeCreate
          , timeUpdateCv = timeUpdate
          , nodes = nodes
        }


buildMessagesByNode :: Vector Dst.MessageRow -> Htx.Transaction (Either [Conflict] (Map Int64 Snap.MsgSnap))
buildMessagesByNode rows = do
  results <- mapM buildMessage $ V.toList rows
  let
    conflicts = concat [issues | Left issues <- results]
    messages = [entry | Right entry <- results]
  pure $ if null conflicts then
    Right $ Mp.fromList messages
  else
    Left conflicts


buildMessage :: Dst.MessageRow -> Htx.Transaction (Either [Conflict] (Int64, Snap.MsgSnap))
buildMessage row@(uidMsg, uidNode, eidMsg, timeCreate, timeUpdate, status, endTurn, weight, metadata,
    recipient, channel, _, _, _, _) = do
  contentsE <- Cr.loadByMsg uidMsg
  pure $ case contentsE of
    Left issues ->
      Left $ map (contentConflict uidMsg eidMsg) issues
    Right contents ->
      let
        contentSnaps = map contentFromPayload $ sortOn fst contents
        msgSnap = Snap.MsgSnap {
            eidMsg = eidMsg
            , uidMsg = Just uidMsg
            , timeCreate = timeCreate
            , timeUpdate = timeUpdate
            , status = status
            , endTurn = endTurn
            , weight = weight
            , metadata = metadata
            , recipient = recipient
            , channel = channel
            , contents = contentSnaps
            , hashMsg = Dh.hashMsgDb msgSnap
          }
      in
      Right (uidNode, msgSnap)


contentFromPayload :: (Int32, Ct.Payload) -> Snap.ContentSnap
contentFromPayload (seqContent, payload) =
  let
    contentSnap = Snap.ContentSnap {
        uidContent = Nothing
        , seqContent = seqContent
        , typeContent = payloadKind payload
        , payload = Cc.valuePayload payload
        , hashContent = Dh.hashContentDb contentSnap
      }
  in
  contentSnap


payloadKind :: Ct.Payload -> Text
payloadKind payload =
  case payload of
    Ct.CodePL {} -> "code"
    Ct.ExecOutPL {} -> "execution_output"
    Ct.ModelCtxPL {} -> "model_editable_context"
    Ct.MultiPL {} -> "multimodal_text"
    Ct.ReasoningPL {} -> "reasoning_recap"
    Ct.SystemErrPL {} -> "system_error"
    Ct.TetherBrowsePL {} -> "tether_browsing_display"
    Ct.TetherQuotePL {} -> "tether_quote"
    Ct.TextPL {} -> "text"
    Ct.ThoughtsPL {} -> "thoughts"
    Ct.OtherPL kind _ -> kind


contentConflict :: Int64 -> Text -> Ct.IssueC -> Conflict
contentConflict uidMsg eidMsg issue =
  case issueToConflict issue of
    BrokenShapeC detail ->
      BrokenShapeC $ "message " <> eidMsg <> " uid " <> showT uidMsg <> ": " <> detail
    conflict -> conflict


issueToConflict :: Ct.IssueC -> Conflict
issueToConflict issue =
  case issue of
    Ct.MissingRowIC detail ->
      BrokenShapeC $ "missing content row: " <> detail
    Ct.BadPayloadIC detail ->
      BrokenShapeC $ "invalid content payload: " <> detail
    Ct.PartialMultiIC detail ->
      BrokenShapeC $ "partial multimodal content: " <> detail
    Ct.UnknownKindIC kind ->
      BrokenShapeC $ "unknown content kind without opaque fallback: " <> kind
    Ct.UnsupportedIC detail ->
      BrokenShapeC $ "unsupported content without opaque fallback: " <> detail


buildNodes :: Map Int64 Snap.MsgSnap -> Vector Dst.NodeRow -> [Snap.NodeSnap]
buildNodes messagesByNode rows =
  let
    eidsByUid = Mp.fromList [(nodeUidRow row, nodeEidRow row) | row <- V.toList rows]
    nodes = map (buildNode eidsByUid messagesByNode) $ V.toList rows
  in
  sortOn nodeOrderKey nodes


buildNode :: Map Int64 Text -> Map Int64 Snap.MsgSnap -> Dst.NodeRow -> Snap.NodeSnap
buildNode eidsByUid messagesByNode (uidNode, eidNode, parentFk, seqNode, seqChild, seqPre) =
  let
    nodeSnap = Snap.NodeSnap {
        eidNode = eidNode
        , uidNode = Just uidNode
        , eidParent = parentFk >>= (`Mp.lookup` eidsByUid)
        , uidParent = parentFk
        , seqNode = seqNode
        , seqChild = seqChild
        , seqPre = seqPre
        , msg = Mp.lookup uidNode messagesByNode
        , hashNode = Dh.hashNodeDb nodeSnap
      }
  in
  nodeSnap


nodeOrderKey :: Snap.NodeSnap -> (Int32, Int32, Int32, Text)
nodeOrderKey node = (node.seqPre, node.seqNode, node.seqChild, node.eidNode)


duplicates :: Ord value => Vector value -> [value]
duplicates values =
  St.toList repeated
  where
  (_, repeated) = V.foldl' addValue (St.empty, St.empty) values
  addValue (seen, found) value
    | St.member value seen = (seen, St.insert value found)
    | otherwise = (St.insert value seen, found)


nodeUidRow :: Dst.NodeRow -> Int64
nodeUidRow (uidNode, _, _, _, _, _) = uidNode


nodeEidRow :: Dst.NodeRow -> Text
nodeEidRow (_, eidNode, _, _, _, _) = eidNode


nodeSeqRow :: Dst.NodeRow -> Int32
nodeSeqRow (_, _, _, seqNode, _, _) = seqNode


nodeIsRoot :: Dst.NodeRow -> Bool
nodeIsRoot (_, _, parentFk, _, _, _) =
  case parentFk of
    Nothing -> True
    Just _ -> False


messageNodeFkRow :: Dst.MessageRow -> Int64
messageNodeFkRow (_, uidNode, _, _, _, _, _, _, _, _, _, _, _, _, _) = uidNode


messageEidRow :: Dst.MessageRow -> Text
messageEidRow (_, _, eidMsg, _, _, _, _, _, _, _, _, _, _, _, _) = eidMsg


showT :: Show value => value -> Text
showT = T.pack . show