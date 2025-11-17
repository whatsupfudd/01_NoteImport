module OpenAI.Parse where

import qualified Data.List as L
import qualified Data.Map.Strict as Mp
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T


import qualified Options.Runtime as Rto
import qualified OpenAI.Json.Reader as Jd


analyzeDiscussion :: Jd.Discussion -> IO ()
analyzeDiscussion discussion =
  let
    -- rootChild = Mp.lookup "client-created-root" discussion.mappingCv
    fsmResult = runFSM initContext discussion.mappingCv "client-created-root"
    messages = reverse fsmResult.messages
  in
  {- case rootChild of
    Just node -> mapM_ (analyseChild discussion.mappingCv 0) node.childrenNd
    Nothing -> putStrLn "No root child found"
  -}
  mapM_ (putStrLn . T.unpack . showMessage) messages


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
    CodeSA code -> "---- Code ---\n" <> code.languageCC <> "\n" <> maybe "No response format name" (\name -> name) code.responseFormatNameCC <> "\n" <> code.textCC
    ToolCallSA toolCall -> "---- ToolCall ---\n" <> toolCall.toolNameTC <> "\n" <> toolCall.toolInputTC
    _ -> "---- Unknown ---\n" <> T.pack (show subAction)



data Context = Context {
  messages :: [ MessageFsm ]
  , currentMsg :: Maybe MessageFsm
  , issues :: [ Text ]
} deriving (Show)


initContext :: Context
initContext = Context {
  messages = []
  , currentMsg = Nothing
  , issues = []
}


data MessageFsm =
  UserMF Timing UserMessage
  | AssistantMF Timing AssistantMessage
  | SystemMF Timing SystemMessage
  | ToolMF Timing ToolMessage
  | UnknownMF Timing UnknownMessage
  deriving (Show)

data Timing = Timing {
  createTime :: Maybe Double
  , updateTime :: Maybe Double
} deriving (Show)


data UserMessage = UserMessage {
  textUM :: Text
  , attachmentsUM :: [ Text ]
} deriving (Show)


data AssistantMessage = AssistantMessage {
  response :: Maybe ResponseAst
  , attachmentsAM :: [ Text ]
  , subActions :: [ SubAction ]
} deriving (Show)

newtype ResponseAst = ResponseAst {
  textRA :: Text
} deriving (Show)


data SubAction = SubAction
  | ReflectionSA Reflection
  | CodeSA Code
  | ToolCallSA ToolCall
  | IntermediateSA Text
  deriving (Show)


data Reflection = Reflection {
  summaryRF :: Text
  , contentRF :: Text
  , chunksRF :: [ Text ]
  , finishedRF :: Maybe Bool
} deriving (Show)


data Code = Code {
  languageCC :: Text
  , responseFormatNameCC :: Maybe Text
  , textCC :: Text
} deriving (Show)


data ToolCall = ToolCall {
  toolNameTC :: Text
  , toolInputTC :: Text
} deriving (Show)


newtype SystemMessage = SystemMessage {
  textSM :: Text
} deriving (Show)


newtype ToolMessage = ToolMessage {
  textTM :: Text
} deriving (Show)

newtype UnknownMessage = UnknownMessage {
  textUM :: Text
} deriving (Show)


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
    Jd.TextContent parts ->
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
  if message.endTurnMsg == Just True then
    case context.currentMsg of
      Nothing ->
        let
          assistantMsg = AssistantMessage {
            response = Just $ buildAssistantResponse Nothing message
            , attachmentsAM = []
            , subActions = []
          }
          timing = Timing {
            createTime = message.createTimeMsg
            , updateTime = message.updateTimeMsg
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
    let
      timing = Timing {
        createTime = message.createTimeMsg
        , updateTime = message.updateTimeMsg
      }
    in
    case message.contentMsg of
      Jd.TextContent parts ->
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
      Jd.ThoughtsContent thoughts sourceAnalysisMsgId ->
        let
          subActions = map (\aThought -> ReflectionSA Reflection {
                summaryRF =  aThought.summaryTh
              , contentRF = aThought.contentTh
              , chunksRF = maybe [] (map (T.pack . show)) aThought.chunksTh
              , finishedRF = aThought.finishedTh
              }) thoughts
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
      Jd.CodeContent language responseFormatName text ->
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
      Jd.ModelEditableContext modelSetContext repository repoSummary structuredContext ->
        context
      Jd.OtherContent contentType raw ->
        context


buildAssistantResponse :: Maybe AssistantMessage -> Jd.Message -> ResponseAst
buildAssistantResponse mbAstMsg jsonMsg =
  case mbAstMsg of
    Just assistantMsg ->
      respFromContent jsonMsg.contentMsg
    Nothing -> respFromContent jsonMsg.contentMsg


respFromContent :: Jd.Content -> ResponseAst
respFromContent content =
  case content of
    Jd.TextContent parts -> ResponseAst {
      textRA = T.intercalate " |<part>| " parts
    }
    Jd.ModelEditableContext modelSetContext repository repoSummary structuredContext -> ResponseAst {
      textRA = "ModelEditableContext: " <> modelSetContext
    }
    Jd.ThoughtsContent thoughts sourceAnalysisMsgId -> ResponseAst {
      textRA = "ThoughtsContent: " <> sourceAnalysisMsgId
    }
    Jd.CodeContent language responseFormatName text -> ResponseAst {
      textRA = "CodeContent: " <> text
    }
    Jd.OtherContent contentType raw -> ResponseAst {
      textRA = "OtherContent: " <> contentType <> " " <> (T.pack . show) raw
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