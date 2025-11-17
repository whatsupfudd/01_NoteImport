{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OpenAI.Json.Reader where

import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Mp
import Data.Text (Text)
import qualified Data.Text as T

import System.Environment (getArgs)

import Data.Aeson
import Data.Aeson.Types (Value, Object, Parser)
import qualified Data.Aeson.KeyMap as Km

import GHC.Generics (Generic)


-- Helper to convert Object to Map String Value
objectToMap :: Object -> Mp.Map Text Value
objectToMap anObject =
  let
    keyMap = Km.toMap anObject
  in
    Mp.mapKeys (T.pack . show) keyMap


data Discussion = Discussion {
  titleCv :: Text,
  createTimeCv :: Double,
  updateTimeCv :: Double,
  mappingCv :: Mp.Map Text Node,
  convIdCv :: Text
} deriving (Show)

instance FromJSON Discussion where
  parseJSON = withObject "Discussion" $ \o ->
    Discussion
      <$> o .: "title"
      <*> o .: "create_time"
      <*> o .: "update_time"
      <*> o .: "mapping"
      <*> o .: "conversation_id"


data Node = Node {
  idNd :: Text,
  messageNd :: Maybe Message,
  parentNd :: Maybe Text,
  childrenNd :: [Text]
} deriving (Show)

instance FromJSON Node where
  parseJSON = withObject "Node" $ \o -> Node
    <$> o .: "id"
    <*> o .:? "message"
    <*> o .:? "parent"
    <*> o .: "children"


data Message = Message {
  idMsg :: Text,
  authorMsg :: Author,
  createTimeMsg :: Maybe Double,
  updateTimeMsg :: Maybe Double,
  contentMsg :: Content,
  statusMsg :: Text,
  endTurnMsg :: Maybe Bool,
  weightMsg :: Double,
  metadataMsg :: Mp.Map Text Value,
  recipientMsg :: Text,
  channelMsg :: Maybe Text
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
  roleAu :: Text,
  nameAu :: Maybe Text,
  metadataAu :: Mp.Map Text Value
} deriving (Show)

instance FromJSON Author where
  parseJSON = withObject "Author" $ \o -> Author
    <$> o .: "role"
    <*> o .:? "name"
    <*> (objectToMap <$> o .: "metadata")


data Content =
  TextContent {
      partsCt :: [Text]
    }
  | ModelEditableContext {
      modelSetContextMec :: Text,
      repositoryMec :: Maybe Value,
      repoSummaryMec :: Maybe Value,
      structuredContextMec :: Maybe Value
    }
  | ThoughtsContent {
      thoughtsTc :: [Thought],
      sourceAnalysisMsgIdTc :: Text
    }
  | CodeContent {
      languageCc :: Text,
      responseFormatNameCc :: Maybe Text,
      textCc :: Text
    }
  | OtherContent {
    contentTypeOc :: Text,
    rawOc :: Mp.Map Text Value
  } deriving (Show)

instance FromJSON Content where
  parseJSON = withObject "Content" $ \o -> do
    ctype :: Text <- o .: "content_type"
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
  summaryTh :: Text,
  contentTh :: Text,
  chunksTh :: Maybe [Value],
  finishedTh :: Maybe Bool
} deriving (Show, Generic)

instance FromJSON Thought where
  parseJSON = withObject "Thought" $ \o -> Thought
    <$> o .: "summary"
    <*> o .: "content"
    <*> o .:? "chunks"
    <*> o .:? "finished"

