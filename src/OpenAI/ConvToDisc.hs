{-# LANGUAGE LambdaCase #-}

-- | Convert a DB-deserialised OpenAI conversation ('ConversationDb') into the
-- legacy / JSON-derived flat 'Context' representation.
--
-- JSON child order is persisted in @oai.nodes.child_seq@ / @preorder_seq@.
-- Traversal uses @child_seq@ within each parent, with @preorder_seq@ and node
-- UID as deterministic tie-breakers.
module OpenAI.ConvToDisc (analyzeConversation, buildChildMap) where

import Data.Aeson (Value)
import qualified Data.Aeson as Ae
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.Map.Strict as Mp
import Data.Map.Strict (Map)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

import qualified OpenAI.Conversation as Cv
import qualified OpenAI.Discussion.Types as Dt


data Context = Context {
  messages :: [Dt.MessageFsm]
  , currentMsg :: Maybe Dt.MessageFsm
  , inList :: [Cv.MessageDb]
  , issues :: [Text]
} deriving (Show)


initContext :: Context
initContext = Context {
  messages = []
  , currentMsg = Nothing
  , inList = []
  , issues = []
}

-- | Entry point equivalent to 'OpenAI.Parse.analyzeDiscussion'.
--
-- Returns:
--   * Right Context on success, with non-fatal issues recorded in ctx.issues
--   * Left <err> only when the conversation is structurally impossible to traverse
--
-- NOTE: Like the JSON-derived implementation, messages are accumulated by
--       prepending to the list. Callers that want chronological order should
--       use @reverse context.messages@.
analyzeConversation :: Cv.ConversationDb -> Either Text Dt.Discussion
analyzeConversation conv =
  let
    mapping = fixEndTurn conv.nodesCv
    (childMap, chIssues) = buildChildMap mapping
    mbRoot = findRootNode mapping
  in
  case mbRoot of
    Nothing -> Left $ "@[analyzeConversation] no root node found for conversation: "
          <> conv.titleCv <> ", eid: " <> conv.eidCv
    Just rootNode ->
      let
        rawCtxt = runMsgEnc initContext mapping childMap rootNode.eidNd-- runFSM initContext mapping childMap rootNode.eidNd
        finalCtxt = finaliseContext rawCtxt
      in
      Right $ Dt.Discussion { title = conv.titleCv,eid = conv.eidCv, messages = finalCtxt.messages, issues = chIssues <> finalCtxt.issues }


fixEndTurn :: Map Text Cv.NodeDb -> Map Text Cv.NodeDb
fixEndTurn mapping =
  let
    updList = Mp.foldr endTurnDetector [] mapping
  in
  Mp.fromList [ (node.eidNd, node) | node <- updList ]
  where
  endTurnDetector :: Cv.NodeDb -> [Cv.NodeDb] -> [Cv.NodeDb]
  endTurnDetector node nodeList =
    case node.messageNd of
      Nothing -> node : nodeList
      Just msg ->
        case msg.authorMsg.roleAu of
          "user" -> case nodeList of
            -- In case 2 user nodes show up at the start of a conversation:
            [] -> [node]
            hN : restN ->
              case hN.messageNd of
                Nothing -> node : nodeList
                Just headMsg ->
                  let
                    updMsg = headMsg { Cv.endTurnMsg = Just True }
                    updNode = hN { Cv.messageNd = Just updMsg }
                  in
                  [node, updNode] <> restN
          _ -> node : nodeList

-- -----------------------------
-- Root / children discovery
-- -----------------------------

-- TODO: deduplicate from Json.Node.Order.selectRoot (but this is for NodeDb).
findRootNode :: Map Text Cv.NodeDb -> Maybe Cv.NodeDb
findRootNode mapping =
  case Mp.lookup "client-created-root" mapping of
    Just node -> Just node
    Nothing -> L.find (\node -> isNothing node.parentFkNd) (Mp.elems mapping)


-- | Build parent->children adjacency lists.
--
-- Returns @(childMap, issuesAboutOrphans)@.
--
-- Child lists are ordered by:
--
--   1. @child_seq@
--   2. @preorder_seq@
--   3. node UID
--
-- This preserves the JSON child ordering persisted in the DB.
buildChildMap :: Map Text Cv.NodeDb -> (Map Text [Text], [Text])
buildChildMap mapping =
  let
    nodesOrd = L.sortOn orderKeyNd (Mp.elems mapping)
    eidByUid = Mp.fromList [ (node.uidNd, node.eidNd) | node <- nodesOrd ]

    scanNode :: (Map Text [Text], [Text]) -> Cv.NodeDb -> (Map Text [Text], [Text])
    scanNode (childMap, issuesRev) node =
      case node.parentFkNd of
        Nothing -> (childMap, issuesRev)
        Just parentUid -> case Mp.lookup parentUid eidByUid of
          Nothing -> (
              childMap, ("@[buildChildMap] node has missing parent_fk: node="
                <> node.eidNd <> ", parent_uid=" <> (T.pack . show) parentUid) : issuesRev
            )
          Just parentEid -> (Mp.insertWith (flip (++)) parentEid [node.eidNd] childMap, issuesRev)

    (childMap, issuesRev) = L.foldl' scanNode (Mp.empty, []) nodesOrd
  in
  (childMap, reverse issuesRev)


orderKeyNd :: Cv.NodeDb -> (Maybe Int, Int, Int, Int)
orderKeyNd node =
  ( fromIntegral <$> node.parentFkNd
  , fromIntegral node.seqChildNd
  , fromIntegral node.seqPreNd
  , fromIntegral node.uidNd
  )


-- -----------------------------
-- FSM traversal
-- -----------------------------

runFSM :: Context -> Map Text Cv.NodeDb -> Map Text [Text] -> Text -> Context
runFSM context mapping childMap nodeEid =
  case Mp.lookup nodeEid mapping of
    Nothing -> context { issues = ("node not found: " <> nodeEid) : context.issues }
    Just node ->
      let
        updCtxt = case node.messageNd of
          Nothing -> context
          Just msg ->
            case T.toLower msg.authorMsg.roleAu of
              "user" -> handleUserMsg context msg
              "assistant" -> handleAssistantMsg context msg
              "system" -> handleSystemMsg context msg
              "tool" -> handleToolMsg context msg
              other -> context { issues = ("unknown role: " <> other) : context.issues }
        children = Mp.findWithDefault [] nodeEid childMap
      in
      -- foldl' walks child1, child2, child3 in persisted JSON order.
      L.foldl' (\accum childId -> runFSM accum mapping childMap childId) updCtxt children


runMsgEnc :: Context -> Map Text Cv.NodeDb -> Map Text [Text] -> Text -> Context
runMsgEnc context mapping childMap nodeEid =
  case Mp.lookup nodeEid mapping of
    Nothing -> context { issues = ("missing node: " <> nodeEid) : context.issues }
    Just node ->
      let
        updCtxt = case node.messageNd of
          Nothing -> context { issues = ("node without message: " <> nodeEid) : context.issues }
          Just aMsg ->
            case T.toLower aMsg.authorMsg.roleAu of
              "user" -> userMsgAction context aMsg
              "assistant" -> assistantMsgAction context aMsg
              "system" -> systemMsgAction context aMsg
              "tool" -> toolMsgAction context aMsg
              unknown -> context { issues = ("unknown role: " <> unknown) : context.issues }
        children = Mp.findWithDefault [] nodeEid childMap
      in
      L.foldl' (\accum childId -> runMsgEnc accum mapping childMap childId) updCtxt children


finaliseContext :: Context -> Context
finaliseContext context =
  case context.inList of
    [] -> context { messages = context.messages, issues = reverse context.issues }
    _ ->
      let
        (asstFsm, newIssues) = parseAsstMsgs context.inList
      in
      context { messages = asstFsm : context.messages, issues = reverse $ newIssues <> context.issues }


userMsgAction :: Context -> Cv.MessageDb -> Context
userMsgAction context msg =
  let
    timing = Dt.Timing { Dt.createTime = msg.createTimeMsg, Dt.updateTime = msg.updateTimeMsg }
    userFsm = Dt.UserMF timing $ Dt.UserMessage { Dt.textUM = contentTextV msg.contentsMsg, attachmentsUM = [] }
  in
  if null context.inList then
    context { messages = userFsm : context.messages }
  else
    let
      (asstFsm, newIssues) = parseAsstMsgs context.inList
    in
    context { messages = [userFsm, asstFsm] <> context.messages, inList = [], issues = newIssues <> context.issues }


parseAsstMsgs :: [Cv.MessageDb] -> (Dt.MessageFsm, [Text])
parseAsstMsgs msgs =
  let
    (mbLastMsg, otherMsgs, issues) = extractLastMsg msgs
  in
  case mbLastMsg of
    Just targetMsg ->
      let
        subActs = concatMap (V.toList . subActFromContent) $ reverse otherMsgs
        timing = Dt.Timing { createTime = targetMsg.createTimeMsg, updateTime = targetMsg.updateTimeMsg }
        response = Just (Dt.ResponseAst { textRA = contentTextV targetMsg.contentsMsg })
        assistantMsg = Dt.AssistantMessage { response = response, attachmentsAM = [], subActions = subActs }
      in
      (Dt.AssistantMF timing assistantMsg, issues)
  where
  extractLastMsg :: [Cv.MessageDb] -> (Maybe Cv.MessageDb, [Cv.MessageDb], [Text])
  extractLastMsg =
    foldr (\msg (mbLastMsg, otherMsgs, issues) ->
        if isContentText msg.contentsMsg then
          let
            updList = case mbLastMsg of
              Just lastMsg -> lastMsg : otherMsgs
              Nothing -> otherMsgs
            in
            (Just msg, updList, issues)
        else
          (mbLastMsg, msg : otherMsgs, issues)
      ) (Nothing, [], [])


contentTextV :: V.Vector Cv.ContentDb -> Text
contentTextV contentsV =
  T.intercalate " :|: " (V.toList $ V.map contentText contentsV)

contentText :: Cv.ContentDb -> Text
contentText content =
  case content of
    Cv.CodeCT_Db lang mbRespFmt text -> "```" <> lang <> "```\n" <> maybe "" (<> "\n") mbRespFmt <> text
    Cv.ExecutionOutputCT_Db text -> text
    Cv.MultimodalTextCT_Db partsV -> T.intercalate " :|: " (V.toList $ V.map multiModalText partsV)
    Cv.ModelEditableContextCT_Db modelSetContext repo repoSummary structured -> modelSetContext <> "\n" <> maybe "(no repo)" jsonValueToText repo <> "\n" <> maybe "(no repo summary)" jsonValueToText repoSummary <> "\n" <> maybe "(no structured)" jsonValueToText structured
    Cv.ReasoningRecapCT_Db partsV -> partsV
    Cv.SystemErrorCT_Db name text -> name <> " " <> text
    Cv.TetherBrowsingDisplayCT_Db result summary assets tetherId -> result <> "\n" <> maybe "no summary" jsonValueToText summary <> "\n" <> maybe "no assets" jsonValueToText assets <> "\n" <> maybe "no tether ID" id tetherId
    Cv.TetherQuoteCT_Db url domain text title tetherId -> url <> " " <> domain <> " " <> text <> " " <> title <> " " <> maybe "no tether ID" id tetherId
    Cv.TextCT_Db partsV -> T.intercalate " :|: " (V.toList partsV)
    Cv.ThoughtsCT_Db sourceAnalysisMsgId thoughts -> showThoughts thoughts <> "\n source ID: " <> sourceAnalysisMsgId
    Cv.UnknownCT_Db contentType raw -> contentType <> " " <> jsonValueToText raw
    _ -> "" -- Should be redundant to indicate exhaustive pattern match.


multiModalText :: Cv.MultiModalPartDb -> Text
multiModalText part =
  case part of
    Cv.TextPT_Db text -> text
    Cv.AudioTranscriptionPT_Db audio -> "audio: " <> audio.textAtp
    Cv.AudioAssetPointerPT_Db audioPtr -> "audio: " <> audioPtr.assetPointerAap
          <> " metadata: " <> maybe "no metadata" (T.pack . show) audioPtr.metadataAap
    Cv.ImageAssetPointerPT_Db image -> "image: " <> image.assetPointerIap
          <> " width: " <> (T.pack . show) image.widthIap <> " height: " <> (T.pack . show) image.heightIap
          <> " fovea: " <> maybe "no fovea" jsonValueToText image.foveaIap
          <> " metadata: " <> maybe "no metadata" (T.pack . show) image.metadataIap
    Cv.RealTimeUserAVPT_Db realTimeUserAV -> "real time user AV: " <> maybe "no video container" jsonValueToText realTimeUserAV.videoContainerAssetPointerRtuav
          <> " audio start timestamp: " <> maybe "no audio start timestamp" (T.pack . show) realTimeUserAV.audioStartTimestampRtuav


isContentText :: V.Vector Cv.ContentDb -> Bool
isContentText =
  V.any (\case 
    Cv.TextCT_Db _ -> True
    _ -> False
  )


subActFromContent :: Cv.MessageDb -> V.Vector Dt.SubAction
subActFromContent msg =
  V.concatMap anActFromContent msg.contentsMsg
  where
  anActFromContent :: Cv.ContentDb -> V.Vector Dt.SubAction
  anActFromContent content =
    case content of
      Cv.CodeCT_Db language responseFormatName text -> V.singleton . Dt.CodeSA $ Dt.Code { Dt.languageCC = language, Dt.responseFormatNameCC = responseFormatName, Dt.textCC = text }
      Cv.ExecutionOutputCT_Db text -> V.singleton . Dt.IntermediateSA $ "Exec Output: " <> text
      Cv.MultimodalTextCT_Db partsV -> V.singleton . Dt.IntermediateSA $ "MultimodalText: " <> T.intercalate " :|: " (V.toList $ V.map multiModalText partsV)
      Cv.ModelEditableContextCT_Db modelSetContext repo repoSummary structured ->
        V.singleton . Dt.IntermediateSA $ "ModelEditableContext: " <> modelSetContext <> "\n" <> maybe "(no repo)" jsonValueToText repo <> "\n"
            <> maybe "(no repo summary)" jsonValueToText repoSummary <> "\n" <> maybe "(no structured)" jsonValueToText structured
      Cv.ReasoningRecapCT_Db text -> V.singleton . Dt.IntermediateSA $ "Recap:" <> text
      Cv.SystemErrorCT_Db name text -> V.singleton . Dt.IntermediateSA $ "SystemError: " <> name <> " " <> text
      Cv.TetherBrowsingDisplayCT_Db result summary assets tetherId -> V.singleton . Dt.IntermediateSA $ "TetherBrowsingDisplay: " <> result <> "\n" <> maybe "no summary" jsonValueToText summary <> "\n" <> maybe "no assets" jsonValueToText assets <> "\n" <> maybe "no tether ID" id tetherId
      Cv.TetherQuoteCT_Db url domain text title tetherId -> V.singleton . Dt.IntermediateSA $ "TetherQuote: " <> url <> " " <> domain <> " " <> text <> " " <> title <> " " <> maybe "no tether ID" id tetherId
      Cv.TextCT_Db partsV -> V.singleton . Dt.IntermediateSA $ T.intercalate " |<part>| " (V.toList partsV)
      Cv.ThoughtsCT_Db sourceAnalysisMsgId thoughts -> thoughtsToSubAct thoughts
      Cv.UnknownCT_Db contentType raw -> V.singleton . Dt.IntermediateSA $ contentType <> " " <> jsonValueToText raw
      _ -> V.empty


thoughtsToSubAct :: V.Vector Cv.ThoughtDb -> V.Vector Dt.SubAction
thoughtsToSubAct =
  V.map aThoughToSubAct
  where
  aThoughToSubAct :: Cv.ThoughtDb -> Dt.SubAction
  aThoughToSubAct thought =
    Dt.ReflectionSA $ Dt.Reflection { 
      Dt.summaryRF = thought.summaryTh, Dt.contentRF = thought.contentTh
      , Dt.chunksRF = decodeChunks thought.chunksTh, Dt.finishedRF = Just thought.finishedTh
    }


assistantMsgAction :: Context -> Cv.MessageDb -> Context
assistantMsgAction context msg =
  context { inList = msg : context.inList }


systemMsgAction :: Context -> Cv.MessageDb -> Context
systemMsgAction context msg =
  let
    timing = Dt.Timing { createTime = msg.createTimeMsg, updateTime = msg.updateTimeMsg }
    newMsg = Dt.SystemMF timing $ Dt.SystemMessage { textSM = contentTextV msg.contentsMsg }
  in
  context { messages = newMsg : context.messages }

toolMsgAction :: Context -> Cv.MessageDb -> Context
toolMsgAction context msg =
  let
    timing = Dt.Timing { createTime = msg.createTimeMsg, updateTime = msg.updateTimeMsg }
    newMsg = Dt.ToolMF timing $ Dt.ToolMessage { textTM = contentTextV msg.contentsMsg }
  in
  context { messages = newMsg : context.messages }


-- -----------------------------
-- Role handlers
-- -----------------------------

handleUserMsg :: Context -> Cv.MessageDb -> Context
handleUserMsg context msg =
  case firstTextContent msg.contentsMsg of
    Just partsV ->
      let
        userMsg = Dt.UserMessage { textUM = T.intercalate " |<part>| " (V.toList partsV), attachmentsUM = [] }
        timing = Dt.Timing { createTime = msg.createTimeMsg, updateTime = msg.updateTimeMsg }
      in
        context { messages = Dt.UserMF timing userMsg : context.messages }
    Nothing -> context { issues = ( "user msg eid: " <> msg.eidMsg
            <> " missing TextCT content; contents=" <> summarizeContents msg.contentsMsg
          ) : context.issues
        }


handleAssistantMsg :: Context -> Cv.MessageDb -> Context
handleAssistantMsg context msg =
  let
    timing = Dt.Timing { createTime = msg.createTimeMsg, updateTime = msg.updateTimeMsg }
    contents = V.toList msg.contentsMsg
  in
  {- Until the end-turn flag is reliable, we won't use the following code: -}
  if msg.endTurnMsg == Just True then
    case contents of
      [] -> finalizeAssistant context msg Nothing
      _ ->
        let
          prefix = init contents
          lastC = last contents
          ctx1 = L.foldl' (\accum contentDb -> applyAssistantContent accum timing msg contentDb) context prefix
        in
        finalizeAssistant ctx1 msg (Just lastC)
  else
  L.foldl' (\accum contentDb -> applyAssistantContent accum timing msg contentDb) context contents
  --}
  {-- Unconditional version:
  case contents of
    [] -> finalizeAssistant context msg Nothing
    _ ->
      let
        prefix = init contents
        lastC = last contents
        ctx1 = L.foldl' (\accum contentDb -> applyAssistantContent accum timing msg contentDb) context prefix
      in
      finalizeAssistant ctx1 msg (Just lastC)
  --}


finalizeAssistant :: Context -> Cv.MessageDb -> Maybe Cv.ContentDb -> Context
finalizeAssistant context msg mbFinalContent =
  let
    resp = maybe (Dt.ResponseAst {textRA = "No response"}) responseFromContent mbFinalContent
  in
  case context.currentMsg of
    Nothing ->
      let
        assistantMsg = Dt.AssistantMessage {
              response = Just resp
            , attachmentsAM = []
            , subActions = []
            }
        timing = Dt.Timing { Dt.createTime = msg.createTimeMsg, Dt.updateTime = msg.updateTimeMsg }
      in
      context { messages = Dt.AssistantMF timing assistantMsg : context.messages }
    Just astMsg ->
      let
        updMsg = case astMsg of
            Dt.AssistantMF timing prevMsg ->
              Dt.AssistantMF timing prevMsg { Dt.subActions = reverse prevMsg.subActions, Dt.response = Just resp }
            _ -> astMsg
      in
      context { messages = updMsg : context.messages, currentMsg = Nothing }


applyAssistantContent :: Context -> Dt.Timing -> Cv.MessageDb -> Cv.ContentDb -> Context
applyAssistantContent context timing msg = \case
  Cv.CodeCT_Db lang mFmt txt -> codeP context timing msg lang mFmt txt
  Cv.TextCT_Db partsV -> textP context timing msg (V.toList partsV)
  Cv.ThoughtsCT_Db srcAnalysis thoughtsV ->
    thoughtsP context timing msg srcAnalysis (V.toList thoughtsV)
  -- Mirror Parse: other assistant content types are currently ignored in the FSM.
  _ -> context


codeP :: Context -> Dt.Timing -> Cv.MessageDb -> Text -> Maybe Text -> Text -> Context
codeP context timing msg language responseFormatName text =
  let
    subAction = Dt.CodeSA Dt.Code {
          Dt.languageCC = language, Dt.responseFormatNameCC = responseFormatName, Dt.textCC = text
        }
    eiNewMsg = case context.currentMsg of
        Just prevMsg -> case prevMsg of
          Dt.AssistantMF t assistantMsg -> Right $ Dt.AssistantMF t assistantMsg { Dt.subActions = subAction : assistantMsg.subActions }
          _ -> Left $ "assistant msg eid: " <> msg.eidMsg <> " is not an assistant message: " <> T.pack (show prevMsg)
        Nothing -> Right $ Dt.AssistantMF timing (Dt.AssistantMessage {
            Dt.response = Just (responseFromContent (Cv.CodeCT_Db language responseFormatName text))
          , Dt.attachmentsAM = []
          , Dt.subActions = [subAction]
          })
  in
  case eiNewMsg of
    Left errMsg -> context { issues = errMsg : context.issues }
    Right newMsg -> context { currentMsg = Just newMsg }


textP :: Context -> Dt.Timing -> Cv.MessageDb -> [Text] -> Context
textP context timing msg parts =
  let
    subAction = Dt.IntermediateSA (T.intercalate " |<part>| " parts)
    eiNewMsg =
      case context.currentMsg of
        Just prevMsg -> case prevMsg of
          Dt.AssistantMF t assistantMsg -> Right $ Dt.AssistantMF t assistantMsg { Dt.subActions = subAction : assistantMsg.subActions }
          _ -> Left $ "assistant msg eid: " <> msg.eidMsg <> " is not an assistant message: " <> T.pack (show prevMsg)
        Nothing -> Right $ Dt.AssistantMF timing (Dt.AssistantMessage {
                response = Just (responseFromContent (Cv.TextCT_Db (V.fromList parts)))
              , attachmentsAM = []
              , subActions = [subAction]
              }
            )
  in
  case eiNewMsg of
    Left errMsg -> context { issues = errMsg : context.issues }
    Right newMsg -> context { currentMsg = Just newMsg }


thoughtsP :: Context -> Dt.Timing -> Cv.MessageDb -> Text -> [Cv.ThoughtDb] -> Context
thoughtsP context timing msg _sourceAnalysisMsgId thoughts =
  let
    -- Keep the JSON path behaviour: create sub-actions in reverse order because
    -- the final assistant message reverses the whole subAction list.
    subActions = map (\aThought -> Dt.ReflectionSA Dt.Reflection {
              Dt.summaryRF = aThought.summaryTh, Dt.contentRF = aThought.contentTh
            , Dt.chunksRF = decodeChunks aThought.chunksTh, Dt.finishedRF = Just aThought.finishedTh
            }
        ) (reverse thoughts)
    eiNewMsg = case context.currentMsg of
        Just prevMsg ->
          case prevMsg of
            Dt.AssistantMF t assistantMsg -> Right $ Dt.AssistantMF t assistantMsg { Dt.subActions = subActions <> assistantMsg.subActions }
            _ -> Left $ "assistant msg eid: " <> msg.eidMsg <> " is not an assistant message: " <> T.pack (show prevMsg)
        Nothing -> Right $ Dt.AssistantMF timing (Dt.AssistantMessage {
                  Dt.response = Just (responseFromContent (Cv.ThoughtsCT_Db "" (V.fromList thoughts)))
                , Dt.attachmentsAM = []
                , Dt.subActions = subActions
                })
  in
  case eiNewMsg of
    Left errMsg -> context { issues = errMsg : context.issues }
    Right newMsg -> context { currentMsg = Just newMsg }


handleSystemMsg :: Context -> Cv.MessageDb -> Context
handleSystemMsg context msg =
  let
    systemMsg = Dt.SystemMessage { Dt.textSM = summarizeContents msg.contentsMsg }
    timing = Dt.Timing { Dt.createTime = msg.createTimeMsg, Dt.updateTime = msg.updateTimeMsg }
  in
  context { messages = Dt.SystemMF timing systemMsg : context.messages }


handleToolMsg :: Context -> Cv.MessageDb -> Context
handleToolMsg context msg =
  let
    toolMsg = Dt.ToolMessage { Dt.textTM = summarizeContents msg.contentsMsg }
    timing = Dt.Timing { Dt.createTime = msg.createTimeMsg, Dt.updateTime = msg.updateTimeMsg }
  in
  context { messages = Dt.ToolMF timing toolMsg : context.messages }


-- -----------------------------
-- Response mapping (content -> ResponseAst)
-- -----------------------------

responseFromContent :: Cv.ContentDb -> Dt.ResponseAst
responseFromContent = \case
  Cv.CodeCT_Db _language _responseFormatName text -> Dt.ResponseAst { Dt.textRA = "CodeContent: " <> text }
  Cv.ExecutionOutputCT_Db text -> Dt.ResponseAst { Dt.textRA = "ExecutionOutputContent: " <> text }
  Cv.ModelEditableContextCT_Db modelSetContext _repo _repoSummary _structured ->
    Dt.ResponseAst { Dt.textRA = "ModelEditableContent: " <> modelSetContext }
  Cv.MultimodalTextCT_Db parts -> Dt.ResponseAst { Dt.textRA = "MultimodalTextContent: " <> T.pack (show parts) }
  Cv.ReasoningRecapCT_Db content -> Dt.ResponseAst { Dt.textRA = "ReasoningRecapContent: " <> content }
  Cv.SystemErrorCT_Db name text -> Dt.ResponseAst { Dt.textRA = "SystemErrorContent: " <> name <> " " <> text }
  Cv.TetherBrowsingDisplayCT_Db result summary assets tetherId -> Dt.ResponseAst {
        textRA = "TetherBrowsingDisplayContent: " <> result <> " " <> T.pack (show summary) <> " "
            <> T.pack (show assets) <> " " <> fromMaybeText "No tetherID" tetherId
      }
  Cv.TetherQuoteCT_Db url domain text title tetherId -> Dt.ResponseAst {
        textRA = "TetherQuoteContent: " <> url <> " " <> domain <> " " <> text <> " "
            <> title <> " " <> fromMaybeText "No tetherID" tetherId
      }
  Cv.TextCT_Db partsV -> Dt.ResponseAst { Dt.textRA = T.intercalate " |<part>| " (V.toList partsV) }
  Cv.ThoughtsCT_Db sourceAnalysisMsgId thoughts ->
    Dt.ResponseAst { Dt.textRA = showThoughts thoughts } -- "ThoughtsContent: " <> sourceAnalysisMsgId <> " " <>
  Cv.UnknownCT_Db contentType raw -> Dt.ResponseAst { Dt.textRA = "OtherContent: " <> contentType <> " " <> jsonValueToText raw }


showThoughts :: V.Vector Cv.ThoughtDb -> Text
showThoughts thoughtsV =
  let
    thoughtsText = map (\thought -> "Thought: " <> thought.summaryTh <> " " <> thought.contentTh) (V.toList thoughtsV)
  in
  T.intercalate "\n" thoughtsText

-- -----------------------------
-- Small helpers
-- -----------------------------

firstTextContent :: V.Vector Cv.ContentDb -> Maybe (V.Vector Text)
firstTextContent contents =
  case L.find isTextCT (V.toList contents) of
    Just (Cv.TextCT_Db partsV) -> Just partsV
    _ -> Nothing
  where
  isTextCT = \case
    Cv.TextCT_Db {} -> True
    _ -> False

summarizeContents :: V.Vector Cv.ContentDb -> Text
summarizeContents contents =
  let
    tags = map contentTag (V.toList contents)
  in
  "contents=" <> T.intercalate "," tags


contentTag :: Cv.ContentDb -> Text
contentTag = \case
  Cv.CodeCT_Db {} -> "code"
  Cv.ExecutionOutputCT_Db {} -> "execution_output"
  Cv.MultimodalTextCT_Db {} -> "multimodal_text"
  Cv.ModelEditableContextCT_Db {} -> "model_editable_context"
  Cv.ReasoningRecapCT_Db {} -> "reasoning_recap"
  Cv.SystemErrorCT_Db {} -> "system_error"
  Cv.TetherBrowsingDisplayCT_Db {} -> "tether_browsing_display"
  Cv.TetherQuoteCT_Db {} -> "tether_quote"
  Cv.TextCT_Db {} -> "text"
  Cv.ThoughtsCT_Db {} -> "thoughts"
  Cv.UnknownCT_Db ct _ -> "unknown:" <> ct

fromMaybeText :: Text -> Maybe Text -> Text
fromMaybeText def mbTxt = maybe def id mbTxt

jsonValueToText :: Value -> Text
jsonValueToText = TE.decodeUtf8 . BL.toStrict . Ae.encode

decodeChunks :: Value -> [Text]
decodeChunks v =
  case Ae.fromJSON v :: Ae.Result [Value] of
    Ae.Success xs -> map (T.pack . show) xs
    Ae.Error _ -> [jsonValueToText v]

--}