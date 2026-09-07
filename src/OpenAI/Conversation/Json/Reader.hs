{-# LANGUAGE DerivingStrategies #-}

module OpenAI.Conversation.Json.Reader (
  VersionJson(..)
  , parse, parseFromValue, parseFollow, mergeBrowseJson
  , module OpenAI.Conversation.Json.Schema
)
where

import qualified Data.ByteString.Lazy as Bs

import qualified Data.Aeson as Ae
import qualified Data.Aeson.KeyMap as Km
import qualified Data.Aeson.Types as Aet

import qualified OpenAI.Conversation.Json.V1.Schema as Jv1
import qualified OpenAI.Conversation.Json.V1.Convert as Jc
import OpenAI.Conversation.Json.Schema
import qualified OpenAI.Conversation.Json.Schema as Jd
import OpenAI.Conversation.Json.Types


data MixedConversation =
  V1Cv Jv1.Conversation
  | V2Cv Conversation


parseMixed :: Ae.Value -> Aet.Parser MixedConversation
parseMixed =
  Ae.withObject "OpenAI conversation" (\obj ->
    case ( Km.member "mapping" obj, Km.member "messages" obj) of
      (True, False)  -> V1Cv <$> Ae.parseJSON (Ae.Object obj)
      (False, True)  -> V2Cv <$> Ae.parseJSON (Ae.Object obj)
      (False, False) -> fail "@[parseMixed] No mapping or messages found in OpenAI conversation."
      (True, True)   -> fail "@[parseMixed] Ambiguous OpenAI JSON format."
    )


parse :: Bs.ByteString -> Either String Conversation
parse jsonContent = do
  case Ae.eitherDecode jsonContent :: Either String Ae.Value of
    Left err -> Left err
    Right rawJson ->
      parseFromValue rawJson


parseFromValue :: Ae.Value -> Either String Conversation
parseFromValue rawJson = do
  case Aet.parseEither parseMixed rawJson :: Either String MixedConversation of
    Left err -> Left err
    Right mxConv -> case mxConv of
      V1Cv conv -> Right $ Jc.v1ToCurrent conv
      V2Cv conv -> Right conv


parseFollow :: Bs.ByteString -> Either String FollowConv
parseFollow jsonContent = do
  Ae.eitherDecode jsonContent :: Either String FollowConv

mergeBrowseJson :: Conversation -> [FollowConv] -> Either String Conversation
mergeBrowseJson conv followConvs = do
  Right $ foldl mergeFollow conv followConvs
  where
  mergeFollow :: Conversation -> FollowConv -> Conversation
  mergeFollow accum aFollow =
    accum { messagesCv = accum.messagesCv <> aFollow.messagesCv 
        , safeUrlsCv = accum.safeUrlsCv <> aFollow.safeUrlsCv
        , blockedUrlsCv = accum.blockedUrlsCv <> aFollow.blockedUrlsCv      
      }
