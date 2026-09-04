module OpenAI.Conversation.Unify where

import qualified Data.Map.Strict as Mp
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Aeson as Ae

import qualified OpenAI.Conversation.Json.Schema as Jd
import OpenAI.Types


runFSM :: Context -> [Jd.Message] -> Context
runFSM context messages =
  foldr switchFsm context messages
  where
  switchFsm :: Jd.Message -> Context -> Context
  switchFsm message context =
    case message.authorMsg.roleAu of
      "user" -> handleUserMsg context message
      "assistant" -> handleAssistantMsg context message
      "system" -> handleSystemMsg context message
      "tool" -> handleToolMsg context message
      _ -> context { issues = "unknown role: " <> message.authorMsg.roleAu : context.issues }


handleUserMsg :: Context -> Jd.Message -> Context
handleUserMsg context message =
  case message.contentMsg of
    Jd.TextCT parts ->
      let
        userMsg = UserMessage {
          textUM = T.intercalate " |<part>| " parts.partsTP
          , attachmentsUM = []
        }
        timing = Timing {
          createTime = Just message.createTimeMsg
          , updateTime = message.updateTimeMsg
        }
        updCtxt = context { messages = UserMF timing userMsg : context.messages }
      in
      updCtxt
    _ -> context { issues = "user msg id: " <> message.idMsg <> " unknown content type: " <> T.pack (show message.contentMsg) : context.issues }


handleAssistantMsg :: Context -> Jd.Message -> Context
handleAssistantMsg context message =
  let
    timing = Timing {
      createTime = Just message.createTimeMsg
      , updateTime = message.updateTimeMsg
    }
  in
  if message.endTurnMsg == Just True then
    case context.currentMsg of
      Nothing ->
        let
          assistantMsg = AssistantMessage {
            response = Just $ buildAssistantResponse Nothing message
            , attachmentsAM = []
            , subActions = []
          }
        in
        context { messages = AssistantMF timing assistantMsg : context.messages }
      Just astMsg ->
        let
          updMsg = case astMsg of
            AssistantMF timing prevMsg ->
              AssistantMF timing prevMsg {
                  subActions = reverse prevMsg.subActions
                  , response = Just $ buildAssistantResponse (Just prevMsg) message
                }
            -- TODO: handle a current message being non-assistant:
            _ -> astMsg
        in
        context { messages = updMsg : context.messages, currentMsg = Nothing }
  else  -- Not end-turn situation:
    case message.contentMsg of
      Jd.CodeCT codePl -> codeP timing codePl.languageCP codePl.responseFormatNameCP codePl.textCP
      Jd.ExecutionOutputCT execOutput ->
        -- TODO.
        context
      Jd.MultimodalTextCT mmText ->
        -- TODO.
        context
      Jd.ModelEditableContextCT modelCtx ->
        -- TODO.
        context
      Jd.ReasoningRecapCT content ->
        -- TODO.
        context
      Jd.SystemErrorCT sysError ->
        -- TODO.
        context
      Jd.TetherBrowsingDisplayCT tBrowsing ->
        -- TODO.
        context
      Jd.TetherQuoteCT tQuote ->
        -- TODO.
        context
      Jd.TextCT parts -> textP timing parts.partsTP
      Jd.ThoughtsCT tContent -> thoughtsP timing tContent.thoughtsTP tContent.sourceAnalysisMsgIdTP
      Jd.OtherCT valueMap ->
        context
      _ -> context { issues = "assistant msg id: " <> message.idMsg <> " unknown content type: " <> T.pack (show message.contentMsg) : context.issues }
  where
  codeP :: Timing -> Text -> Maybe Text -> Text -> Context
  codeP timing language responseFormatName text =
    let
      subAction = CodeSA Code {
        languageCC = language
        , responseFormatNameCC = responseFormatName
        , textCC = text
      }
      ieNewMsg = case context.currentMsg of
        Just prevMsg ->
          case prevMsg of
            AssistantMF timing assistantMsg ->
              Right $ AssistantMF timing assistantMsg { subActions = subAction : assistantMsg.subActions }
            _ -> Left $ "assistant msg id: " <> message.idMsg <> " is not an assistant message: " <> T.pack (show prevMsg)
        Nothing -> Right $ AssistantMF timing (AssistantMessage {
              response = Just $ buildAssistantResponse Nothing message
              , attachmentsAM = []
              , subActions = [ subAction ]
            })
    in
    case ieNewMsg of
      Left errMsg -> context { issues = errMsg : context.issues }
      Right newMsg -> context { currentMsg = Just newMsg }
  textP :: Timing -> [Text] -> Context
  textP timing parts =
    let
      subAction = IntermediateSA (T.intercalate " |<part>| " parts)
      ieNewMsg = case context.currentMsg of
        Just prevMsg ->
          case prevMsg of
            AssistantMF timing assistantMsg ->
              Right $ AssistantMF timing assistantMsg { subActions = subAction : assistantMsg.subActions }
            _ -> Left $ "assistant msg id: " <> message.idMsg <> " is not an assistant message: " <> T.pack (show prevMsg)
        Nothing ->
          let
            assistantMsg = AssistantMessage {
              response = Just $ buildAssistantResponse Nothing message
              , attachmentsAM = []
              , subActions = [ subAction ]
            }
          in
          Right $ AssistantMF timing assistantMsg
    in
    case ieNewMsg of
      Left errMsg -> context { issues = errMsg : context.issues }
      Right newMsg -> context { currentMsg = Just newMsg }
  thoughtsP :: Timing -> [Jd.ThoughtContent] -> Text -> Context
  thoughtsP timing thoughts sourceAnalysisMsgId =
    let
      -- They need to be backward as we reverse the list later.
      subActions = map (\aThought ->
        ReflectionSA Reflection {
            summaryRF =  aThought.summaryTC
          , contentRF = aThought.contentTC
          , chunksRF = aThought.chunksTC
          , finishedRF = Just aThought.finishedTC
          }
        ) $ reverse thoughts
      ieNewMsg = case context.currentMsg of
        Just prevMsg ->
          case prevMsg of
            AssistantMF timing assistantMsg ->
              Right $ AssistantMF timing assistantMsg { subActions = subActions <> assistantMsg.subActions }
            _ -> Left $ "assistant msg id: " <> message.idMsg <> " is not an assistant message: " <> T.pack (show prevMsg)
        Nothing -> Right $ AssistantMF timing (AssistantMessage {
              response = Just $ buildAssistantResponse Nothing message
              , attachmentsAM = []
              , subActions = subActions
            })
    in
    case ieNewMsg of
      Left errMsg -> context { issues = errMsg : context.issues }
      Right newMsg -> context { currentMsg = Just newMsg }


buildAssistantResponse :: Maybe AssistantMessage -> Jd.Message -> ResponseAst
buildAssistantResponse mbAstMsg jsonMsg =
  case mbAstMsg of
    Just assistantMsg ->
      respFromContent jsonMsg.contentMsg
    Nothing -> respFromContent jsonMsg.contentMsg


respFromContent :: Jd.Content -> ResponseAst
respFromContent content =
  case content of
    Jd.CodeCT codePl -> ResponseAst {
      textRA = "CodeContent: " <> codePl.textCP
    }
    Jd.ExecutionOutputCT execOutput -> ResponseAst {
      textRA = "ExecutionOutputContent: " <> execOutput.textEO
    }
    Jd.ModelEditableContextCT modelCtx -> ResponseAst {
      textRA = "ModelEditableContent: " <> modelCtx.modelSetMEC
    }
    Jd.MultimodalTextCT mmText -> ResponseAst {
      textRA = "MultimodalTextContent: " <> T.intercalate " |<part>| " (map (T.pack . show) mmText.partsMmt)
    }
    Jd.ReasoningRecapCT reasoningRecap -> ResponseAst {
      textRA = "ReasoningRecapContent: " <> reasoningRecap.contentRR
    }
    Jd.SystemErrorCT sysError -> ResponseAst {
      textRA = "SystemErrorContent: " <> sysError.nameSER <> " " <> sysError.textSER
    }
    Jd.TetherBrowsingDisplayCT tBrowsing -> ResponseAst {
      textRA = "TetherBrowsingDisplayContent: " <> tBrowsing.resultTbd <> " " <> (T.pack . show) tBrowsing.summaryTbd <> " " <> (T.pack . show) tBrowsing.assetsTbd <> " " <> fromMaybe "No tetherID" tBrowsing.tetherIDTbd
    }
    Jd.TetherQuoteCT tQuote -> ResponseAst {
      textRA = "TetherQuoteContent: " <> tQuote.urlTq <> " " <> tQuote.domainTq <> " " <> tQuote.textTq <> " " <> tQuote.titleTq <> " " <> fromMaybe "No tetherID" tQuote.tetherIDTq
    }
    Jd.TextCT parts -> ResponseAst {
      textRA = T.intercalate " |<part>| " parts.partsTP
    }
    Jd.ThoughtsCT tContent -> ResponseAst {
      textRA = "ThoughtsContent: " <> tContent.sourceAnalysisMsgIdTP
    }
    Jd.OtherCT valueMap -> ResponseAst {
      textRA = "OtherContent: " <> valueMap.contentTypeOpl <> " " <> (T.pack . show) valueMap.rawOpl
    }
    _ -> ResponseAst {
      textRA = "UnknownContent: " <> (T.pack . show) content
    }

handleSystemMsg :: Context -> Jd.Message -> Context
handleSystemMsg context message =
  let
    systemMsg = SystemMessage {
      textSM = (T.pack . show) message.contentMsg
    }
    timing = Timing {
      createTime = Just message.createTimeMsg
      , updateTime = message.updateTimeMsg
    }
  in
  context { messages = SystemMF timing systemMsg : context.messages }

handleToolMsg :: Context -> Jd.Message -> Context
handleToolMsg context message =
  let
    toolMsg = ToolMessage {
      textTM = (T.pack . show) message.contentMsg
    }
    timing = Timing {
      createTime = Just message.createTimeMsg
      , updateTime = message.updateTimeMsg
    }
  in
  context { messages = ToolMF timing toolMsg : context.messages }
