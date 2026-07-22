{-# LANGUAGE LambdaCase #-}

-- | Convert a DB-deserialised OpenAI conversation ('ConversationDb') into the
-- legacy / JSON-derived flat 'Context' representation.
--
-- This mirrors the FSM logic in 'OpenAI.Parse' as closely as possible.
--
-- JSON child order is persisted in @oai.nodes.child_seq@ / @preorder_seq@.
-- Traversal uses @child_seq@ within each parent, with @preorder_seq@ and node
-- UID as deterministic tie-breakers.
module OpenAI.ConvToDisc
  ( analyzeConversation
  ) where

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
import OpenAI.Types


-- | Entry point equivalent to 'OpenAI.Parse.analyzeDiscussion'.
--
-- Returns:
--   * Right Context on success, with non-fatal issues recorded in ctx.issues
--   * Left <err> only when the conversation is structurally impossible to traverse
--
-- NOTE: Like the JSON-derived implementation, messages are accumulated by
--       prepending to the list. Callers that want chronological order should
--       use @reverse context.messages@.
analyzeConversation :: Cv.ConversationDb -> Either Text Context
analyzeConversation conv =
  let
    mapping = conv.nodesCv
    (childMap, orderIssues) = buildChildMap mapping
    mbRoot = findRootNode mapping
    initCtxt =
      Context
        { messages = []
        , currentMsg = Nothing
        , issues = orderIssues
        }
  in
    case mbRoot of
      Nothing ->
        Left $
          "@[analyzeConversation] no root node found for conversation: "
            <> conv.titleCv <> ", eid: " <> conv.eidCv
      Just rootNode ->
        Right $ runFSM initCtxt mapping childMap rootNode.eidNd


-- -----------------------------
-- Root / children discovery
-- -----------------------------

findRootNode :: Map Text Cv.NodeDb -> Maybe Cv.NodeDb
findRootNode mapping =
  case Mp.lookup "client-created-root" mapping of
    Just node -> Just node
    Nothing ->
      L.find (\node -> isNothing node.parentFkNd) (Mp.elems mapping)

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

    step :: (Map Text [Text], [Text]) -> Cv.NodeDb -> (Map Text [Text], [Text])
    step (childMap, issuesRev) node =
      case node.parentFkNd of
        Nothing ->
          (childMap, issuesRev)

        Just parentUid ->
          case Mp.lookup parentUid eidByUid of
            Nothing ->
              ( childMap
              , ("@[buildChildMap] node has missing parent_fk: node=" <> node.eidNd <> ", parent_uid=" <> showText parentUid)
                  : issuesRev
              )

            Just parentEid ->
              ( Mp.insertWith (flip (++)) parentEid [node.eidNd] childMap
              , issuesRev
              )

    (childMap, issuesRev) = L.foldl' step (Mp.empty, []) nodesOrd
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
    Nothing ->
      context { issues = ("node not found: " <> nodeEid) : context.issues }

    Just node ->
      let
        updCtxt =
          case node.messageNd of
            Nothing -> context
            Just msg ->
              case T.toLower msg.authorMsg.roleAu of
                "user" -> handleUserMsg context msg
                "assistant" -> handleAssistantMsg context msg
                "system" -> handleSystemMsg context msg
                "tool" -> handleToolMsg context msg
                other ->
                  context { issues = ("unknown role: " <> other) : context.issues }

        children = Mp.findWithDefault [] nodeEid childMap
      in
        -- foldl' walks child1, child2, child3 in persisted JSON order.
        L.foldl' (\acc childId -> runFSM acc mapping childMap childId) updCtxt children


-- -----------------------------
-- Role handlers
-- -----------------------------

handleUserMsg :: Context -> Cv.MessageDb -> Context
handleUserMsg context msg =
  case firstTextContent msg.contentsMsg of
    Just partsV ->
      let
        userMsg =
          UserMessage
            { textUM = T.intercalate " |<part>| " (V.toList partsV)
            , attachmentsUM = []
            }

        timing =
          Timing
            { createTime = msg.createTimeMsg
            , updateTime = msg.updateTimeMsg
            }
      in
        context { messages = UserMF timing userMsg : context.messages }

    Nothing ->
      context
        { issues =
            ( "user msg eid: " <> msg.eidMsg
                <> " missing TextCT content; contents=" <> summarizeContents msg.contentsMsg
            )
              : context.issues
        }


handleAssistantMsg :: Context -> Cv.MessageDb -> Context
handleAssistantMsg context msg =
  let
    timing =
      Timing
        { createTime = msg.createTimeMsg
        , updateTime = msg.updateTimeMsg
        }

    contents = V.toList msg.contentsMsg
  in
    if msg.endTurnMsg == Just True
      then
        case contents of
          [] ->
            finalizeAssistant context timing msg Nothing

          _ ->
            let
              prefix = init contents
              lastC = last contents
              ctx1 = L.foldl' (\acc contentDb -> applyAssistantContent acc timing msg contentDb) context prefix
            in
              finalizeAssistant ctx1 timing msg (Just lastC)
      else
        L.foldl' (\acc contentDb -> applyAssistantContent acc timing msg contentDb) context contents


finalizeAssistant :: Context -> Timing -> Cv.MessageDb -> Maybe Cv.ContentDb -> Context
finalizeAssistant context _timing msg mbFinalContent =
  let
    resp =
      case mbFinalContent of
        Nothing -> ResponseAst { textRA = "No response" }
        Just contentDb -> buildAssistantResponse contentDb
  in
    case context.currentMsg of
      Nothing ->
        let
          assistantMsg =
            AssistantMessage
              { response = Just resp
              , attachmentsAM = []
              , subActions = []
              }

          timing =
            Timing
              { createTime = msg.createTimeMsg
              , updateTime = msg.updateTimeMsg
              }
        in
          context { messages = AssistantMF timing assistantMsg : context.messages }

      Just astMsg ->
        let
          updMsg =
            case astMsg of
              AssistantMF timing prevMsg ->
                AssistantMF timing prevMsg
                  { subActions = reverse prevMsg.subActions
                  , response = Just resp
                  }

              _ ->
                astMsg
        in
          context { messages = updMsg : context.messages, currentMsg = Nothing }


applyAssistantContent :: Context -> Timing -> Cv.MessageDb -> Cv.ContentDb -> Context
applyAssistantContent context timing msg = \case
  Cv.CodeCT_Db lang mFmt txt ->
    codeP context timing msg lang mFmt txt

  Cv.TextCT_Db partsV ->
    textP context timing msg (V.toList partsV)

  Cv.ThoughtsCT_Db srcAnalysis thoughtsV ->
    thoughtsP context timing msg srcAnalysis (V.toList thoughtsV)

  -- Mirror Parse: other assistant content types are currently ignored in the FSM.
  _ ->
    context


codeP :: Context -> Timing -> Cv.MessageDb -> Text -> Maybe Text -> Text -> Context
codeP context timing msg language responseFormatName text =
  let
    subAction =
      CodeSA Code
        { languageCC = language
        , responseFormatNameCC = responseFormatName
        , textCC = text
        }

    eiNewMsg =
      case context.currentMsg of
        Just prevMsg ->
          case prevMsg of
            AssistantMF t assistantMsg ->
              Right $ AssistantMF t assistantMsg { subActions = subAction : assistantMsg.subActions }

            _ ->
              Left $
                "assistant msg eid: " <> msg.eidMsg
                  <> " is not an assistant message: " <> T.pack (show prevMsg)

        Nothing ->
          Right $
            AssistantMF timing
              (AssistantMessage
                { response = Just (buildAssistantResponse (Cv.CodeCT_Db language responseFormatName text))
                , attachmentsAM = []
                , subActions = [subAction]
                }
              )
  in
    case eiNewMsg of
      Left errMsg -> context { issues = errMsg : context.issues }
      Right newMsg -> context { currentMsg = Just newMsg }


textP :: Context -> Timing -> Cv.MessageDb -> [Text] -> Context
textP context timing msg parts =
  let
    subAction = IntermediateSA (T.intercalate " |<part>| " parts)

    eiNewMsg =
      case context.currentMsg of
        Just prevMsg ->
          case prevMsg of
            AssistantMF t assistantMsg ->
              Right $ AssistantMF t assistantMsg { subActions = subAction : assistantMsg.subActions }

            _ ->
              Left $
                "assistant msg eid: " <> msg.eidMsg
                  <> " is not an assistant message: " <> T.pack (show prevMsg)

        Nothing ->
          Right $
            AssistantMF timing
              (AssistantMessage
                { response = Just (buildAssistantResponse (Cv.TextCT_Db (V.fromList parts)))
                , attachmentsAM = []
                , subActions = [subAction]
                }
              )
  in
    case eiNewMsg of
      Left errMsg -> context { issues = errMsg : context.issues }
      Right newMsg -> context { currentMsg = Just newMsg }


thoughtsP :: Context -> Timing -> Cv.MessageDb -> Text -> [Cv.ThoughtDb] -> Context
thoughtsP context timing msg _sourceAnalysisMsgId thoughts =
  let
    -- Keep the JSON path behaviour: create sub-actions in reverse order because
    -- the final assistant message reverses the whole subAction list.
    subActions =
      map
        (\aThought ->
          ReflectionSA Reflection
            { summaryRF = aThought.summaryTh
            , contentRF = aThought.contentTh
            , chunksRF = decodeChunks aThought.chunksTh
            , finishedRF = Just aThought.finishedTh
            }
        )
        (reverse thoughts)

    eiNewMsg =
      case context.currentMsg of
        Just prevMsg ->
          case prevMsg of
            AssistantMF t assistantMsg ->
              Right $ AssistantMF t assistantMsg { subActions = subActions <> assistantMsg.subActions }

            _ ->
              Left $
                "assistant msg eid: " <> msg.eidMsg
                  <> " is not an assistant message: " <> T.pack (show prevMsg)

        Nothing ->
          Right $
            AssistantMF timing
              (AssistantMessage
                { response = Just (buildAssistantResponse (Cv.ThoughtsCT_Db "" (V.fromList thoughts)))
                , attachmentsAM = []
                , subActions = subActions
                }
              )
  in
    case eiNewMsg of
      Left errMsg -> context { issues = errMsg : context.issues }
      Right newMsg -> context { currentMsg = Just newMsg }


handleSystemMsg :: Context -> Cv.MessageDb -> Context
handleSystemMsg context msg =
  let
    systemMsg =
      SystemMessage
        { textSM = summarizeContents msg.contentsMsg
        }

    timing =
      Timing
        { createTime = msg.createTimeMsg
        , updateTime = msg.updateTimeMsg
        }
  in
    context { messages = SystemMF timing systemMsg : context.messages }


handleToolMsg :: Context -> Cv.MessageDb -> Context
handleToolMsg context msg =
  let
    toolMsg =
      ToolMessage
        { textTM = summarizeContents msg.contentsMsg
        }

    timing =
      Timing
        { createTime = msg.createTimeMsg
        , updateTime = msg.updateTimeMsg
        }
  in
    context { messages = ToolMF timing toolMsg : context.messages }


-- -----------------------------
-- Response mapping (content -> ResponseAst)
-- -----------------------------

buildAssistantResponse :: Cv.ContentDb -> ResponseAst
buildAssistantResponse = respFromContent

respFromContent :: Cv.ContentDb -> ResponseAst
respFromContent = \case
  Cv.CodeCT_Db _language _responseFormatName text ->
    ResponseAst { textRA = "CodeContent: " <> text }

  Cv.ExecutionOutputCT_Db text ->
    ResponseAst { textRA = "ExecutionOutputContent: " <> text }

  Cv.ModelEditableContextCT_Db modelSetContext _repo _repoSummary _structured ->
    ResponseAst { textRA = "ModelEditableContent: " <> modelSetContext }

  Cv.MultimodalTextCT_Db parts ->
    ResponseAst { textRA = "MultimodalTextContent: " <> T.pack (show parts) }

  Cv.ReasoningRecapCT_Db content ->
    ResponseAst { textRA = "ReasoningRecapContent: " <> content }

  Cv.SystemErrorCT_Db name text ->
    ResponseAst { textRA = "SystemErrorContent: " <> name <> " " <> text }

  Cv.TetherBrowsingDisplayCT_Db result summary assets tetherId ->
    ResponseAst
      { textRA =
          "TetherBrowsingDisplayContent: "
            <> result <> " "
            <> T.pack (show summary) <> " "
            <> T.pack (show assets) <> " "
            <> fromMaybeText "No tetherID" tetherId
      }

  Cv.TetherQuoteCT_Db url domain text title tetherId ->
    ResponseAst
      { textRA =
          "TetherQuoteContent: "
            <> url <> " "
            <> domain <> " "
            <> text <> " "
            <> title <> " "
            <> fromMaybeText "No tetherID" tetherId
      }

  Cv.TextCT_Db partsV ->
    ResponseAst { textRA = T.intercalate " |<part>| " (V.toList partsV) }

  Cv.ThoughtsCT_Db sourceAnalysisMsgId _thoughts ->
    ResponseAst { textRA = "ThoughtsContent: " <> sourceAnalysisMsgId }

  Cv.UnknownCT_Db contentType raw ->
    ResponseAst { textRA = "OtherContent: " <> contentType <> " " <> jsonValueToText raw }


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
      Cv.TextCT_Db{} -> True
      _ -> False

summarizeContents :: V.Vector Cv.ContentDb -> Text
summarizeContents contents =
  let
    tags = map contentTag (V.toList contents)
  in
    "contents=" <> T.intercalate "," tags

contentTag :: Cv.ContentDb -> Text
contentTag = \case
  Cv.CodeCT_Db{} -> "code"
  Cv.ExecutionOutputCT_Db{} -> "execution_output"
  Cv.MultimodalTextCT_Db{} -> "multimodal_text"
  Cv.ModelEditableContextCT_Db{} -> "model_editable_context"
  Cv.ReasoningRecapCT_Db{} -> "reasoning_recap"
  Cv.SystemErrorCT_Db{} -> "system_error"
  Cv.TetherBrowsingDisplayCT_Db{} -> "tether_browsing_display"
  Cv.TetherQuoteCT_Db{} -> "tether_quote"
  Cv.TextCT_Db{} -> "text"
  Cv.ThoughtsCT_Db{} -> "thoughts"
  Cv.UnknownCT_Db ct _ -> "unknown:" <> ct

fromMaybeText :: Text -> Maybe Text -> Text
fromMaybeText def mbTxt =
  maybe def id mbTxt

jsonValueToText :: Value -> Text
jsonValueToText =
  TE.decodeUtf8 . BL.toStrict . Ae.encode

decodeChunks :: Value -> [Text]
decodeChunks v =
  case Ae.fromJSON v :: Ae.Result [Value] of
    Ae.Success xs -> map (T.pack . show) xs
    Ae.Error _ -> [jsonValueToText v]

showText :: Show a => a -> Text
showText =
  T.pack . show

