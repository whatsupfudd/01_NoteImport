{-# LANGUAGE DerivingStrategies #-}

module OpenAI.Conversation.Json.Reader (
  VersionJson(..)
  , parse, parseFromValue, parseFollow, mergeBrowseJson
  , parsePages
  , module Js
)
where

import qualified Data.ByteString.Lazy as Bsl
import Data.Bifunctor (first)
import Data.Text (Text)

import qualified Data.Aeson as Ae
import qualified Data.Aeson.KeyMap as Km
import qualified Data.Aeson.Types as Aet

import qualified OpenAI.Conversation.Json.V1.Schema as Jv1
import qualified OpenAI.Conversation.Json.V1.Convert as Jc
import qualified OpenAI.Conversation.Json.Schema as Js
import OpenAI.Conversation.Json.Types
import qualified OpenAI.Conversation.Json.Node.Build as Nb


data MixedConversation =
  V1Cv Jv1.Conversation
  | V2Cv Js.Conversation


parseMixed :: Ae.Value -> Aet.Parser MixedConversation
parseMixed =
  Ae.withObject "OpenAI conversation" (\obj ->
    case ( Km.member "mapping" obj, Km.member "messages" obj) of
      (True, False)  -> V1Cv <$> Ae.parseJSON (Ae.Object obj)
      (False, True)  -> V2Cv <$> Ae.parseJSON (Ae.Object obj)
      (False, False) -> fail "@[parseMixed] No mapping or messages found in OpenAI conversation."
      (True, True)   -> fail "@[parseMixed] Ambiguous OpenAI JSON format."
    )


parse :: Bsl.ByteString -> Either String Js.Conversation
parse jsonContent = do
  parseRaw jsonContent >>= finalize


parseRaw :: Bsl.ByteString -> Either String Js.Conversation
parseRaw jsonContent =
  Ae.eitherDecode jsonContent >>= parseFromValueRaw


parseFromValue :: Ae.Value -> Either String Js.Conversation
parseFromValue rawJson = parseFromValueRaw rawJson >>= finalize


parseFromValueRaw :: Ae.Value -> Either String Js.Conversation
parseFromValueRaw rawJson =
  case Aet.parseEither parseMixed rawJson :: Either String MixedConversation of
    Left err -> Left err
    Right mxConv -> case mxConv of
      V1Cv conv -> Right $ Jc.v1ToCurrent conv
      V2Cv conv -> Right conv


finalize :: Js.Conversation -> Either String Js.Conversation
finalize = first Nb.renderIssuesNB . Nb.buildNodeMapCv


parseFollow :: Bsl.ByteString -> Either String Js.FollowConv
parseFollow jsonContent = do
  Ae.eitherDecode jsonContent :: Either String Js.FollowConv

mergeBrowseJson :: Js.Conversation -> [Js.FollowConv] -> Either String Js.Conversation
mergeBrowseJson conv followConvs = do
  Right $ foldl mergeFollow conv followConvs
  where
  mergeFollow :: Js.Conversation -> Js.FollowConv -> Js.Conversation
  mergeFollow accum aFollow =
    accum { Js.messagesCv = accum.messagesCv <> aFollow.messagesCv 
        , Js.safeUrlsCv = accum.safeUrlsCv <> aFollow.safeUrlsCv
        , Js.blockedUrlsCv = accum.blockedUrlsCv <> aFollow.blockedUrlsCv      
      }


-- Page management:
data PageJson = 
    ConversationPg Js.Conversation
  | FollowPg Js.FollowConv


parsePages :: [Bsl.ByteString] -> Either String Js.Conversation
parsePages jsonContents = do
  pages <- traverse parsePageRaw jsonContents
  let
    conversations = [conv | ConversationPg conv <- pages]
    follows = [page | FollowPg page <- pages]

  case conversations of
    [conv] -> case conv.versionJsonCv of
      V2vj -> mergeBrowseJson conv follows
      V1vj -> Left "@[parsePages] expected a V2 conversation envelope"
    [] -> Left "@[parsePages] no full conversation envelope found"
    _ -> Left "@[parsePages] multiple full conversation envelopes found"


parsePageRaw :: Bsl.ByteString -> Either String PageJson
parsePageRaw jsonContent = do
  value <- Ae.eitherDecode jsonContent
  case value of
    Ae.Object object | Km.member "conversation_id" object ->
      ConversationPg <$> parseFromValueRaw value
    _ -> FollowPg <$> Aet.parseEither Ae.parseJSON value