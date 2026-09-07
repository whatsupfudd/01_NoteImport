{-# LANGUAGE DeriveGeneric #-}

module OpenAI.Conversation.Json.V1.Schema where

import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Mp
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T

import Data.Aeson
import Data.Aeson.Types (Value, Object, Parser)
import qualified Data.Aeson.KeyMap as Km

import GHC.Generics (Generic)

import OpenAI.Conversation.Json.Utils (objectToMapV1)
import OpenAI.Conversation.Json.MsgSchema (Message (..))
import OpenAI.Conversation.Json.Node (Node, buildChildrenNd)

data Conversation = Conversation {
  titleCv :: Text,
  createTimeCv :: Scientific,
  updateTimeCv :: Scientific,
  nodeMapCv :: Mp.Map Text Node,
  convIdCv :: Text
} deriving (Show, Generic)

instance FromJSON Conversation where
  parseJSON = withObject "Conversation" $ \o ->
    Conversation
      <$> o .: "title"
      <*> o .: "create_time"
      <*> o .: "update_time"
      <*> (buildChildrenNd <$> o .: "mapping")
      <*> o .: "conversation_id"

