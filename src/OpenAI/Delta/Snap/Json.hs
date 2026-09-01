{-# LANGUAGE TupleSections #-}

module OpenAI.Delta.Snap.Json (build) where

import qualified Data.Aeson as Ae
import qualified Data.ByteString as Bs
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Mp
import Data.Maybe (maybeToList)
import qualified Data.Set as St
import Data.Text (Text)
import qualified Data.Text as T

import qualified OpenAI.Content.Codec as Codec
import qualified OpenAI.Content.Kind as Ck
import qualified OpenAI.Content.Types as Ct
import qualified OpenAI.Delta.Hash as Dh
import OpenAI.Delta.Snap (ContentSnap(..), ConvSnap(..), MsgSnap(..), NodeSnap(..))
import OpenAI.Delta.Types (Conflict(..), Hash(..))
import qualified OpenAI.Id as Oid
import qualified OpenAI.Json.Reader as Jd
import qualified OpenAI.Order as Oor


build :: Jd.Conversation -> Either [Conflict] ConvSnap
build conversation =
  let
    mapping = conversation.mappingCv
    orderResult = Oor.buildNodeOrd mapping
    validationIssues = validateConversation conversation
    orderIssues =
      case orderResult of
        Left issues -> map orderIssueConflict issues
        Right orders -> validateOrderCoverage mapping orders
  in
  case (validationIssues <> orderIssues, orderResult) of
    (issues, _) | not $ null issues -> Left issues
    (_, Left issues) -> Left $ map orderIssueConflict issues
    (_, Right orders) -> do
      nodes <- traverse (nodeFromOrder mapping) orders
      pure ConvSnap {
          eidConv = conversation.convIdCv
          , uidConv = Nothing
          , titleConv = conversation.titleCv
          , timeCreateCv = conversation.createTimeCv
          , timeUpdateCv = conversation.updateTimeCv
          , nodes = nodes
        }


validateConversation :: Jd.Conversation -> [Conflict]
validateConversation conversation =
  validateConvEid conversation.convIdCv
    <> validateTime "conversation create_time" conversation.createTimeCv
    <> validateTime "conversation update_time" conversation.updateTimeCv
    <> validateMapping conversation.mappingCv
    <> duplicateNodeIssues conversation.mappingCv
    <> duplicateMessageIssues conversation.mappingCv


validateConvEid :: Text -> [Conflict]
validateConvEid eid =
  case Oid.eidConvFromText eid of
    Left issue -> [BrokenShapeC $ "invalid conversation eid: " <> issue]
    Right _ -> []


validateMapping :: Map Text Jd.Node -> [Conflict]
validateMapping mapping =
  concatMap validateEntry $ Mp.toAscList mapping
  where
  validateEntry (eidKey, node) =
    validateNodeKey eidKey
      <> validateNodeEid eidKey node
      <> maybe [] (validateMessage eidKey) node.messageNd


validateNodeKey :: Text -> [Conflict]
validateNodeKey eid =
  case Oid.eidNodeFromText eid of
    Left issue -> [BrokenShapeC $ "invalid node mapping key: " <> issue]
    Right _ -> []


validateNodeEid :: Text -> Jd.Node -> [Conflict]
validateNodeEid eidKey node =
  let
    eidIssues =
      case Oid.eidNodeFromText node.idNd of
        Left issue -> [BrokenShapeC $ "invalid node eid at mapping key " <> quote eidKey <> ": " <> issue]
        Right _ -> []

    mismatchIssues =
      if eidKey == node.idNd then
        []
      else
        [BrokenShapeC $ "node mapping key " <> quote eidKey <> " differs from node eid " <> quote node.idNd]
  in
  eidIssues <> mismatchIssues


validateMessage :: Text -> Jd.Message -> [Conflict]
validateMessage eidNode message =
  let
    eidIssues =
      case Oid.eidMsgFromText message.idMsg of
        Left issue -> [BrokenShapeC $ "invalid message eid at node " <> quote eidNode <> ": " <> issue]
        Right _ -> []

    createIssues =
      maybe [] (validateTime $ "message " <> quote message.idMsg <> " create_time") message.createTimeMsg

    updateIssues =
      maybe [] (validateTime $ "message " <> quote message.idMsg <> " update_time") message.updateTimeMsg

    contentIssues = validateContent message.idMsg message.contentMsg
  in
  eidIssues <> createIssues <> updateIssues <> contentIssues


validateContent :: Text -> Jd.Content -> [Conflict]
validateContent eidMsg content =
  case Codec.fromJson content of
    Left issue -> [codecIssueConflict eidMsg content issue]
    Right _ -> []


validateTime :: Text -> Double -> [Conflict]
validateTime label value =
  if finite value then
    []
  else
    [BrokenShapeC $ label <> " is not finite"]


finite :: Double -> Bool
finite value = not (isNaN value || isInfinite value)


duplicateNodeIssues :: Map Text Jd.Node -> [Conflict]
duplicateNodeIssues mapping =
  map (DuplicateEidC . ("node:" <>)) $ duplicateTexts $ map (.idNd) $ Mp.elems mapping


duplicateMessageIssues :: Map Text Jd.Node -> [Conflict]
duplicateMessageIssues mapping =
  let
    messageEids = [message.idMsg | node <- Mp.elems mapping, message <- maybeToList node.messageNd]
  in
  map (DuplicateEidC . ("message:" <>)) $ duplicateTexts messageEids


duplicateTexts :: [Text] -> [Text]
duplicateTexts values =
  Mp.keys $ Mp.filter (> (1 :: Int)) counts
  where
  counts = Mp.fromListWith (+) $ map (, 1 :: Int) values


validateOrderCoverage :: Map Text Jd.Node -> [Oor.NodeOrd] -> [Conflict]
validateOrderCoverage mapping orders =
  let
    mappedEids = Mp.keysSet mapping
    orderedEids = St.fromList $ map (.eidNode) orders
    missingEids = St.toAscList $ mappedEids `St.difference` orderedEids
    unknownEids = St.toAscList $ orderedEids `St.difference` mappedEids
    duplicateEids = duplicateTexts $ map (.eidNode) orders
  in
  map (BrokenShapeC . ("node omitted from traversal: " <>)) missingEids
    <> map (MissingJsonNodeC . ("ordered node missing from mapping: " <>)) unknownEids
    <> map (DuplicateEidC . ("ordered-node:" <>)) duplicateEids


orderIssueConflict :: Oor.OrdIssue -> Conflict
orderIssueConflict issue = BrokenShapeC $ Oor.renderOrdIssue issue


nodeFromOrder :: Map Text Jd.Node -> Oor.NodeOrd -> Either [Conflict] NodeSnap
nodeFromOrder mapping order =
  case Mp.lookup order.eidNode mapping of
    Nothing -> Left [MissingJsonNodeC order.eidNode]
    Just node -> do
      message <- traverse messageFromJson node.messageNd
      let
        snap0 = NodeSnap {
            eidNode = order.eidNode
            , uidNode = Nothing
            , eidParent = order.eidParent
            , uidParent = Nothing
            , seqNode = order.seqNode
            , seqChild = order.seqChild
            , seqPre = order.seqPre
            , msg = message
            , hashNode = emptyHash
          }
      pure snap0 {hashNode = Dh.hashNodeDb snap0}


messageFromJson :: Jd.Message -> Either [Conflict] MsgSnap
messageFromJson message = do
  content <- contentFromJson message.idMsg message.contentMsg
  let
    snap0 = MsgSnap {
        eidMsg = message.idMsg
        , uidMsg = Nothing
        , timeCreate = message.createTimeMsg
        , timeUpdate = message.updateTimeMsg
        , status = message.statusMsg
        , endTurn = message.endTurnMsg
        , weight = message.weightMsg
        , metadata = Ae.toJSON message.metadataMsg
        , recipient = message.recipientMsg
        , channel = message.channelMsg
        , contents = [content]
        , hashMsg = emptyHash
      }
  pure snap0 {hashMsg = Dh.hashMsgDb snap0}


contentFromJson :: Text -> Jd.Content -> Either [Conflict] ContentSnap
contentFromJson eidMsg content =
  case Codec.fromJson content of
    Left issue -> Left [codecIssueConflict eidMsg content issue]
    Right payload ->
      let
        payloadValue = Codec.valuePayload payload
        kind = Ck.kindFromJson content
        snap0 = ContentSnap {
            uidContent = Nothing
            , seqContent = 0
            , typeContent = Ck.textKC kind
            , payload = payloadValue
            , hashContent = emptyHash
          }
      in
      Right snap0 {hashContent = Dh.hashContentDb snap0}


codecIssueConflict :: Text -> Jd.Content -> Ct.IssueC -> Conflict
codecIssueConflict eidMsg content issue =
  BrokenShapeC $
    "invalid " <> Ck.textKC (Ck.kindFromJson content)
      <> " content at message " <> quote eidMsg <> ": "
      <> T.pack (show issue)


emptyHash :: Hash
emptyHash = Hash Bs.empty


quote :: Text -> Text
quote value = "\"" <> T.replace "\"" "\\\"" value <> "\""