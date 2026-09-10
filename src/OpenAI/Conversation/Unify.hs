module OpenAI.Conversation.Unify where

import qualified Data.Map.Strict as Mp
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Aeson as Ae

import qualified OpenAI.Conversation.Json.Schema as Jd
import qualified OpenAI.Discussion.Types as Dt
import qualified OpenAI.Conversation.Context as Ct


runFSM :: Ct.Context -> [Jd.Message] -> Ct.Context
runFSM context messages =
  foldr switchFsm context messages
  where
  switchFsm :: Jd.Message -> Ct.Context -> Ct.Context
  switchFsm message context =
    case message.authorMsg.roleAu of
      "user" -> handleUserMsg context message
      "assistant" -> handleAssistantMsg context message
      "system" -> handleSystemMsg context message
      "tool" -> handleToolMsg context message
      _ -> context { Ct.issues = "unknown role: " <> message.authorMsg.roleAu : context.issues }


handleUserMsg :: Ct.Context -> Jd.Message -> Ct.Context
handleUserMsg context message =
  case message.contentMsg of
    Jd.TextCT parts ->
      let
        userMsg = Dt.UserMessage {
          textUM = T.intercalate " |<part>| " parts.partsTP
          , attachmentsUM = []
        }
        timing = Dt.Timing {
          createTime = Just message.createTimeMsg
          , updateTime = message.updateTimeMsg
        }
        updCtxt = context { Ct.messages = Dt.UserMF timing userMsg : context.messages }
      in
      updCtxt
    _ -> context { Ct.issues = "user msg id: " <> message.idMsg <> " unknown content type: " <> T.pack (show message.contentMsg) : context.issues }


handleAssistantMsg :: Ct.Context -> Jd.Message -> Ct.Context
handleAssistantMsg context message =
  let
    timing = Dt.Timing {
      createTime = Just message.createTimeMsg
      , updateTime = message.updateTimeMsg
    }
  in
  if message.endTurnMsg == Just True then
    case context.currentMsg of
      Nothing ->
        let
          assistantMsg = Dt.AssistantMessage {
            response = Just $ buildAssistantResponse Nothing message
            , attachmentsAM = []
            , subActions = []
          }
        in
        context { Ct.messages = Dt.AssistantMF timing assistantMsg : context.messages }
      Just astMsg ->
        let
          updMsg = case astMsg of
            Dt.AssistantMF timing prevMsg ->
              Dt.AssistantMF timing prevMsg {
                  Dt.subActions = reverse prevMsg.subActions
                  , Dt.response = Just $ buildAssistantResponse (Just prevMsg) message
                }
            -- TODO: handle a current message being non-assistant:
            _ -> astMsg
        in
        context { Ct.messages = updMsg : context.messages, Ct.currentMsg = Nothing }
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
      _ -> context { Ct.issues = "assistant msg id: " <> message.idMsg <> " unknown content type: " <> T.pack (show message.contentMsg) : context.issues }
  where
  codeP :: Dt.Timing -> Text -> Maybe Text -> Text -> Ct.Context
  codeP timing language responseFormatName text =
    let
      subAction = Dt.CodeSA Dt.Code {
        Dt.languageCC = language
        , Dt.responseFormatNameCC = responseFormatName
        , Dt.textCC = text
      }
      ieNewMsg = case context.currentMsg of
        Just prevMsg ->
          case prevMsg of
            Dt.AssistantMF timing assistantMsg ->
              Right $ Dt.AssistantMF timing assistantMsg { Dt.subActions = subAction : assistantMsg.subActions }
            _ -> Left $ "assistant msg id: " <> message.idMsg <> " is not an assistant message: " <> T.pack (show prevMsg)
        Nothing -> Right $ Dt.AssistantMF timing (Dt.AssistantMessage {
              response = Just $ buildAssistantResponse Nothing message
              , attachmentsAM = []
              , subActions = [ subAction ]
            })
    in
    case ieNewMsg of
      Left errMsg -> context { Ct.issues = errMsg : context.issues }
      Right newMsg -> context { Ct.currentMsg = Just newMsg }
  textP :: Dt.Timing -> [Text] -> Ct.Context
  textP timing parts =
    let
      subAction = Dt.IntermediateSA (T.intercalate " |<part>| " parts)
      ieNewMsg = case context.currentMsg of
        Just prevMsg ->
          case prevMsg of
            Dt.AssistantMF timing assistantMsg ->
              Right $ Dt.AssistantMF timing assistantMsg { Dt.subActions = subAction : assistantMsg.subActions }
            _ -> Left $ "assistant msg id: " <> message.idMsg <> " is not an assistant message: " <> T.pack (show prevMsg)
        Nothing ->
          let
            assistantMsg = Dt.AssistantMessage {
              response = Just $ buildAssistantResponse Nothing message
              , attachmentsAM = []
              , subActions = [ subAction ]
            }
          in
          Right $ Dt.AssistantMF timing assistantMsg
    in
    case ieNewMsg of
      Left errMsg -> context { Ct.issues = errMsg : context.issues }
      Right newMsg -> context { Ct.currentMsg = Just newMsg }
  thoughtsP :: Dt.Timing -> [Jd.ThoughtContent] -> Text -> Ct.Context
  thoughtsP timing thoughts sourceAnalysisMsgId =
    let
      -- They need to be backward as we reverse the list later.
      subActions = map (\aThought ->
        Dt.ReflectionSA Dt.Reflection {
            Dt.summaryRF =  aThought.summaryTC
          , Dt.contentRF = aThought.contentTC
          , Dt.chunksRF = aThought.chunksTC
          , Dt.finishedRF = Just aThought.finishedTC
          }
        ) $ reverse thoughts
      ieNewMsg = case context.currentMsg of
        Just prevMsg ->
          case prevMsg of
            Dt.AssistantMF timing assistantMsg ->
              Right $ Dt.AssistantMF timing assistantMsg { Dt.subActions = subActions <> assistantMsg.subActions }
            _ -> Left $ "assistant msg id: " <> message.idMsg <> " is not an assistant message: " <> T.pack (show prevMsg)
        Nothing -> Right $ Dt.AssistantMF timing (Dt.AssistantMessage {
              response = Just $ buildAssistantResponse Nothing message
              , attachmentsAM = []
              , subActions = subActions
            })
    in
    case ieNewMsg of
      Left errMsg -> context { Ct.issues = errMsg : context.issues }
      Right newMsg -> context { Ct.currentMsg = Just newMsg }


buildAssistantResponse :: Maybe Dt.AssistantMessage -> Jd.Message -> Dt.ResponseAst
buildAssistantResponse mbAstMsg jsonMsg =
  case mbAstMsg of
    Just assistantMsg ->
      respFromContent jsonMsg.contentMsg
    Nothing -> respFromContent jsonMsg.contentMsg


respFromContent :: Jd.Content -> Dt.ResponseAst
respFromContent content =
  case content of
    Jd.CodeCT codePl -> Dt.ResponseAst {
      textRA = "CodeContent: " <> codePl.textCP
    }
    Jd.ExecutionOutputCT execOutput -> Dt.ResponseAst {
      textRA = "ExecutionOutputContent: " <> execOutput.textEO
    }
    Jd.ModelEditableContextCT modelCtx -> Dt.ResponseAst {
      textRA = "ModelEditableContent: " <> modelCtx.modelSetMEC
    }
    Jd.MultimodalTextCT mmText -> Dt.ResponseAst {
      textRA = "MultimodalTextContent: " <> T.intercalate " |<part>| " (map (T.pack . show) mmText.partsMmt)
    }
    Jd.ReasoningRecapCT reasoningRecap -> Dt.ResponseAst {
      textRA = "ReasoningRecapContent: " <> reasoningRecap.contentRR
    }
    Jd.SystemErrorCT sysError -> Dt.ResponseAst {
      textRA = "SystemErrorContent: " <> sysError.nameSER <> " " <> sysError.textSER
    }
    Jd.TetherBrowsingDisplayCT tBrowsing -> Dt.ResponseAst {
      textRA = "TetherBrowsingDisplayContent: " <> tBrowsing.resultTbd <> " " <> (T.pack . show) tBrowsing.summaryTbd <> " " <> (T.pack . show) tBrowsing.assetsTbd <> " " <> fromMaybe "No tetherID" tBrowsing.tetherIDTbd
    }
    Jd.TetherQuoteCT tQuote -> Dt.ResponseAst {
      textRA = "TetherQuoteContent: " <> tQuote.urlTq <> " " <> tQuote.domainTq <> " " <> tQuote.textTq <> " " <> tQuote.titleTq <> " " <> fromMaybe "No tetherID" tQuote.tetherIDTq
    }
    Jd.TextCT parts -> Dt.ResponseAst {
      textRA = T.intercalate " |<part>| " parts.partsTP
    }
    Jd.ThoughtsCT tContent -> Dt.ResponseAst {
      textRA = "ThoughtsContent: " <> tContent.sourceAnalysisMsgIdTP
    }
    Jd.OtherCT valueMap -> Dt.ResponseAst {
      textRA = "OtherContent: " <> valueMap.contentTypeOpl <> " " <> (T.pack . show) valueMap.rawOpl
    }
    _ -> Dt.ResponseAst {
      textRA = "UnknownContent: " <> (T.pack . show) content
    }

handleSystemMsg :: Ct.Context -> Jd.Message -> Ct.Context
handleSystemMsg context message =
  let
    systemMsg = Dt.SystemMessage {
      textSM = (T.pack . show) message.contentMsg
    }
    timing = Dt.Timing {
      createTime = Just message.createTimeMsg
      , updateTime = message.updateTimeMsg
    }
  in
  context { Ct.messages = Dt.SystemMF timing systemMsg : context.messages }

handleToolMsg :: Ct.Context -> Jd.Message -> Ct.Context
handleToolMsg context message =
  let
    toolMsg = Dt.ToolMessage {
      textTM = (T.pack . show) message.contentMsg
    }
    timing = Dt.Timing {
      createTime = Just message.createTimeMsg
      , updateTime = message.updateTimeMsg
    }
  in
  context { Ct.messages = Dt.ToolMF timing toolMsg : context.messages }
