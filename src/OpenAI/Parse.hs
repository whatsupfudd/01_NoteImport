{-# LANGUAGE DeriveGeneric #-}

module OpenAI.Parse where

import qualified Data.ByteString.Lazy as Bl
import qualified Data.List as L
import qualified Data.Map.Strict as Mp
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import GHC.Generics (Generic)

import qualified Data.Aeson as Ae

import qualified Options.Runtime as Rto
import qualified OpenAI.Json.Reader as Jd
import OpenAI.Types


showDiscussion :: Jd.Conversation -> (Text, [Text])
showDiscussion discussion =
  case analyzeDiscussion discussion of
    Left err -> ("", [err])
    Right fsmResult ->
      let
        messages = reverse fsmResult.messages
      in
      {- case rootChild of
        Just node -> mapM_ (analyseChild discussion.mappingCv 0) node.childrenNd
        Nothing -> putStrLn "No root child found"
      -}
      (T.intercalate "\n" (map showMessage messages), fsmResult.issues)

discussionToElm :: Jd.Conversation -> Either Text Text
discussionToElm discussion =
  case analyzeDiscussion discussion of
    Left err -> Left err
    Right fsmResult ->
      let
        messages = reverse fsmResult.messages
        elmStructs = zipWith messageToElm messages [1..]
      in
      Right $ "[\n  " <> T.intercalate "\n  , " (filter (not . T.null) elmStructs) <> "\n  ]"


analyzeDiscussion :: Jd.Conversation -> Either Text Context
analyzeDiscussion discussion =
  let
    -- rootChild = Mp.lookup "client-created-root" discussion.mappingCv
    mbRootNode = findRootNode discussion.mappingCv
  in
  case mbRootNode of
    Just rootNode ->
      Right $ runFSM initContext discussion.mappingCv rootNode.idNd
    Nothing -> Left $ "@[analyzeDiscussion] no root node found for discussion: " <> discussion.titleCv <> ", id: " <> discussion.convIdCv


showMessage :: MessageFsm -> Text
showMessage message =
  let
    basicText =
      case message of
        UserMF timing userMsg -> "---- User ---\n" <> userMsg.textUM
        AssistantMF timing assistantMsg -> "---- Assistant ---\n"
          <> showSubActions assistantMsg.subActions
          <> maybe "\nNo response" (\rep -> "\n>>> RESP >>>\n" <> rep.textRA) assistantMsg.response
        SystemMF timing systemMsg -> "---- System ---\n" <> systemMsg.textSM
        ToolMF timing toolMsg -> "---- Tool ---\n" <> toolMsg.textTM
        UnknownMF timing unknownMsg -> "---- Unknown ---\n" <> unknownMsg.textUM
  in
  basicText <> "\n"


messageToElm :: MessageFsm -> Int -> Text
messageToElm message index =
  let
    msgID = "msg_" <> T.pack (show index)
  in
  case message of
    UserMF timing userMsg -> "{ id = \""
        <> msgID
        <> "\", kind = T.Question, title = \""
        <> msgID
        <> "\", body = [ T.Basic \"\"\"" <> sanitizeText userMsg.textUM <> "\"\"\"] }"
    AssistantMF timing assistantMsg ->
      let
        content = map subActionToElm assistantMsg.subActions
            <> [ "T.LineSep"
                , "T.Basic \"\"\"" <> maybe "No response" (\rep -> sanitizeText rep.textRA) assistantMsg.response <> "\"\"\"" 
               ]
      in
      "{ id = \"" <> msgID
        <> "\", kind = T.Answer, title = \""
        <> msgID
        <> "\", body = [" <> T.intercalate "\n  , " content <> "\n    ] }"
    _ -> ""


subActionToElm :: SubAction -> Text
subActionToElm subAction =
  case subAction of
    IntermediateSA text ->
      let
        strContent = if text == "" then
            "\"\""
          else
            "\"\"\"" <> sanitizeText text <> "\"\"\""
      in
      "T.Intermediate " <> strContent
    ReflectionSA reflection -> "T.Reflect \"" <> sanitizeText reflection.summaryRF <> "\""
        <> "\"\"\"" <> sanitizeText reflection.contentRF <> "\"\"\""
    CodeSA code -> case code.languageCC of
      "json" -> case Ae.eitherDecode (Bl.fromStrict $ TE.encodeUtf8 code.textCC) :: Either String OaiCodeJson of
        Left err -> "T.Error \"CodeSA: " <> code.languageCC <> " err: " <> sanitizeText (T.pack err) <> "\""
        Right oaiCodeJson -> case oaiCodeJson.typeOJ of
          "document" -> "T.Document \"\"\"" <> sanitizeText oaiCodeJson.contentOJ <> "\"\"\""
          _ -> "T.Error \"\"\"CodeSA:" <> code.languageCC <> " unknown type: "
              <> oaiCodeJson.typeOJ <> "\n" <> sanitizeText oaiCodeJson.contentOJ <> "\"\"\""
      _ -> "T.Error \"\"\"CodeSA: " <> code.languageCC <> "\n"
            <> fromMaybe "No response format name" code.responseFormatNameCC <> "\n" <> sanitizeText code.textCC <> "\"\"\""
    ToolCallSA toolCall -> "T.ToolCall \"\"\"" <> toolCall.toolNameTC <> "\n" <> sanitizeText toolCall.toolInputTC <> "\"\"\""
    _ -> "T.Error \"UnknownSubAction: " <> T.pack (show subAction) <> "\""


sanitizeText :: Text -> Text
sanitizeText =
  T.replace "\"" "\\\"" . T.replace "\\" "\\\\"


showSubActions :: [SubAction] -> Text
showSubActions subActions =
  case subActions of
    [] -> ""
    _ -> "---- SubActions ---\n" <> T.intercalate "\n" (map showSubAction subActions)


showSubAction :: SubAction -> Text
showSubAction subAction =
  case subAction of
    IntermediateSA text -> "---- Intermediate ---\n" <> text
    ReflectionSA reflection -> "---- Reflection ---\n" <> reflection.summaryRF <> "\n" <> reflection.contentRF <> "\n" <> T.pack (show reflection.chunksRF) <> "\n" <> T.pack (show reflection.finishedRF)
    CodeSA code -> "---- Code ---\n" <> code.languageCC <> "\n" <> maybe "No response format name" id code.responseFormatNameCC <> "\n" <> code.textCC
    ToolCallSA toolCall -> "---- ToolCall ---\n" <> toolCall.toolNameTC <> "\n" <> toolCall.toolInputTC
    _ -> "---- Unknown ---\n" <> T.pack (show subAction)




runFSM :: Context -> Mp.Map Text Jd.Node -> Text -> Context
runFSM context mapping nodeID =
  case Mp.lookup nodeID mapping of
    Nothing -> context { issues = "node not found: " <> nodeID : context.issues }
    Just node ->
      let
        updCtxt = case node.messageNd of
          Just message ->
            case message.authorMsg.roleAu of
              "user" -> handleUserMsg context message
              "assistant" -> handleAssistantMsg context message
              "system" -> handleSystemMsg context message
              "tool" -> handleToolMsg context message
              _ -> context { issues = "unknown role: " <> message.authorMsg.roleAu : context.issues }
          Nothing -> context
            -- context { issues = "no message found: " <> nodeID : context.issues }
      in
      foldr (\nodeID accum -> runFSM accum mapping nodeID) updCtxt node.childrenNd


handleUserMsg :: Context -> Jd.Message -> Context
handleUserMsg context message =
  case message.contentMsg of
    Jd.TextCT parts ->
      let
        userMsg = UserMessage {
          textUM = T.intercalate " |<part>| " parts
          , attachmentsUM = []
        }
        timing = Timing {
          createTime = message.createTimeMsg
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
      createTime = message.createTimeMsg
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
      Jd.CodeCT language responseFormatName text -> codeP timing language responseFormatName text
      Jd.ExecutionOutputCT text ->
        -- TODO.
        context
      Jd.MultimodalTextCT {} ->
        -- TODO.
        context
      Jd.ModelEditableContextCT modelSetContext repository repoSummary structuredContext ->
        -- TODO.
        context
      Jd.ReasoningRecapCT content ->
        -- TODO.
        context
      Jd.SystemErrorCT name text ->
        -- TODO.
        context
      Jd.TetherBrowsingDisplayCT result summary assets tetherID ->
        -- TODO.
        context
      Jd.TetherQuoteCT url domain text title tetherID ->
        -- TODO.
        context
      Jd.TextCT parts -> textP timing parts
      Jd.ThoughtsCT thoughts sourceAnalysisMsgId -> thoughtsP timing thoughts sourceAnalysisMsgId
      Jd.OtherCT contentType raw ->
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
  thoughtsP :: Timing -> [Jd.Thought] -> Text -> Context
  thoughtsP timing thoughts sourceAnalysisMsgId =
    let
      -- They need to be backward as we reverse the list later.
      subActions = map (\aThought -> ReflectionSA Reflection {
            summaryRF =  aThought.summaryTh
          , contentRF = aThought.contentTh
          , chunksRF = maybe [] (map (T.pack . show)) aThought.chunksTh
          , finishedRF = aThought.finishedTh
          }) $ reverse thoughts
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
    Jd.CodeCT language responseFormatName text -> ResponseAst {
      textRA = "CodeContent: " <> text
    }
    Jd.ExecutionOutputCT text -> ResponseAst {
      textRA = "ExecutionOutputContent: " <> text
    }
    Jd.ModelEditableContextCT modelSetContext repository repoSummary structuredContext -> ResponseAst {
      textRA = "ModelEditableContent: " <> modelSetContext
    }
    Jd.MultimodalTextCT parts -> ResponseAst {
      textRA = "MultimodalTextContent: " <> (T.pack . show) parts
    }
    Jd.ReasoningRecapCT content -> ResponseAst {
      textRA = "ReasoningRecapContent: " <> content
    }
    Jd.SystemErrorCT name text -> ResponseAst {
      textRA = "SystemErrorContent: " <> name <> " " <> text
    }
    Jd.TetherBrowsingDisplayCT result summary assets tetherID -> ResponseAst {
      textRA = "TetherBrowsingDisplayContent: " <> result <> " " <> (T.pack . show) summary <> " " <> (T.pack . show) assets <> " " <> fromMaybe "No tetherID" tetherID
    }
    Jd.TetherQuoteCT url domain text title tetherID -> ResponseAst {
      textRA = "TetherQuoteContent: " <> url <> " " <> domain <> " " <> text <> " " <> title <> " " <> fromMaybe "No tetherID" tetherID
    }
    Jd.TextCT parts -> ResponseAst {
      textRA = T.intercalate " |<part>| " parts
    }
    Jd.ThoughtsCT thoughts sourceAnalysisMsgId -> ResponseAst {
      textRA = "ThoughtsContent: " <> sourceAnalysisMsgId
    }
    Jd.OtherCT contentType raw -> ResponseAst {
      textRA = "OtherContent: " <> contentType <> " " <> (T.pack . show) raw
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
      createTime = message.createTimeMsg
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
      createTime = message.createTimeMsg
      , updateTime = message.updateTimeMsg
    }
  in
  context { messages = ToolMF timing toolMsg : context.messages }

findRootNode :: Mp.Map Text Jd.Node -> Maybe Jd.Node
findRootNode mapping =
  case Mp.lookup "client-created-root" mapping of
    Just node -> Just node
    Nothing ->
      L.find (\node -> isNothing node.parentNd) $ Mp.elems mapping