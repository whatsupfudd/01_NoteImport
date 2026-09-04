module OpenAI.Conversation.Process where

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
import qualified OpenAI.Conversation.Json.Schema as Jd
import OpenAI.Conversation.Unify (runFSM)
import OpenAI.Types


toText :: Jd.Conversation -> (Text, [Text])
toText conversation =
  let
    context = analyze conversation
    messages = reverse context.messages
  in
  (T.intercalate "\n" (map showMessage messages), context.issues)


toElm :: Jd.Conversation -> Either Text Text
toElm conversation =
  let
    context = analyze conversation
  in
  case context.issues of
    [] ->
      let
        messages = reverse context.messages
        elmStructs = zipWith messageToElm messages [1..]
      in
      Right $ "[\n  " <> T.intercalate "\n  , " (filter (not . T.null) elmStructs) <> "\n  ]"
    _ -> Left $ "@[toElm] issues: " <> T.intercalate ", " context.issues


analyze :: Jd.Conversation -> Context
analyze conversation =
  -- Always succeed since the errors are accumulated in the context.
  runFSM initContext conversation.messagesCv


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


sanitizeText :: Text -> Text
sanitizeText =
  T.replace "\"" "\\\"" . T.replace "\\" "\\\\"

