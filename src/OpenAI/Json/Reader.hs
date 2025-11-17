{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OpenAI.Json.Reader where

import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Mp
import Data.Text (Text)

import System.Environment (getArgs)

import Data.Aeson
import Data.Aeson.Types (Value, Object, Parser)
import qualified Data.Aeson.KeyMap as Km

import GHC.Generics (Generic)


-- Helper to convert Object to Map String Value
objectToMap :: Object -> Mp.Map String Value
objectToMap anObject =
  let
    keyMap = Km.toMap anObject
  in
    Mp.mapKeys show keyMap


data Discussion = Discussion {
  titleCv :: String,
  createTimeCv :: Double,
  updateTimeCv :: Double,
  mappingCv :: Mp.Map String Node
} deriving (Show)

instance FromJSON Discussion where
  parseJSON = withObject "Discussion" $ \o ->
    Discussion
      <$> o .: "title"
      <*> o .: "create_time"
      <*> o .: "update_time"
      <*> o .: "mapping"


data Node = Node {
  idNd :: String,
  messageNd :: Maybe Message,
  parentNd :: Maybe String,
  childrenNd :: [String]
} deriving (Show)

instance FromJSON Node where
  parseJSON = withObject "Node" $ \o -> Node
    <$> o .: "id"
    <*> o .:? "message"
    <*> o .:? "parent"
    <*> o .: "children"


data Message = Message {
  idMsg :: String,
  authorMsg :: Author,
  createTimeMsg :: Maybe Double,
  updateTimeMsg :: Maybe Double,
  contentMsg :: Content,
  statusMsg :: String,
  endTurnMsg :: Maybe Bool,
  weightMsg :: Double,
  metadataMsg :: Mp.Map String Value,
  recipientMsg :: String,
  channelMsg :: Maybe String
} deriving (Show)

instance FromJSON Message where
  parseJSON = withObject "Message" $ \o -> Message
    <$> o .: "id"
    <*> o .: "author"
    <*> o .:? "create_time"
    <*> o .:? "update_time"
    <*> o .: "content"
    <*> o .: "status"
    <*> o .:? "end_turn"
    <*> o .: "weight"
    <*> (objectToMap <$> o .: "metadata")
    <*> o .: "recipient"
    <*> o .:? "channel"


data Author = Author {
  roleAu :: String,
  nameAu :: Maybe String,
  metadataAu :: Mp.Map String Value
} deriving (Show)

instance FromJSON Author where
  parseJSON = withObject "Author" $ \o -> Author
    <$> o .: "role"
    <*> o .:? "name"
    <*> (objectToMap <$> o .: "metadata")


data Content =
  TextContent {
      partsCt :: [String]
    }
  | ModelEditableContext {
      modelSetContextMec :: String,
      repositoryMec :: Maybe Value,
      repoSummaryMec :: Maybe Value,
      structuredContextMec :: Maybe Value
    }
  | ThoughtsContent {
      thoughtsTc :: [Thought],
      sourceAnalysisMsgIdTc :: String
    }
  | CodeContent {
      languageCc :: String,
      responseFormatNameCc :: Maybe String,
      textCc :: String
    }
  | OtherContent {
    contentTypeOc :: String,
    rawOc :: Mp.Map String Value
  } deriving (Show)

instance FromJSON Content where
  parseJSON = withObject "Content" $ \o -> do
    ctype :: String <- o .: "content_type"
    case ctype of
      "text" -> TextContent <$> o .: "parts"
      "model_editable_context" -> ModelEditableContext
        <$> o .: "model_set_context"
        <*> o .:? "repository"
        <*> o .:? "repo_summary"
        <*> o .:? "structured_context"
      "thoughts" -> ThoughtsContent
        <$> o .: "thoughts"
        <*> o .: "source_analysis_msg_id"
      "code" -> CodeContent
        <$> o .: "language"
        <*> o .:? "response_format_name"
        <*> o .: "text"
      _ -> OtherContent ctype <$> pure (objectToMap o)


data Thought = Thought {
  summaryTh :: String,
  contentTh :: String,
  chunksTh :: [Value],
  finishedTh :: Bool
} deriving (Show, Generic)

instance FromJSON Thought where
  parseJSON = withObject "Thought" $ \o -> Thought
    <$> o .: "summary"
    <*> o .: "content"
    <*> o .: "chunks"
    <*> o .: "finished"

