{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module OpenAI.Conversation.Json.Node where

import Control.Applicative ((<|>))

import Data.Text (Text)
import Data.Aeson (FromJSON (..), ToJSON, withObject, (.:), (.:?))

import GHC.Generics (Generic)

import OpenAI.Conversation.Json.MsgSchema (Message)

data Node = Node {
  idNd :: Text,
  messageNd :: Maybe Message,
  parentNd :: Maybe Text,
  childrenNd :: [Text]
} deriving (Show, Generic, ToJSON)

instance FromJSON Node where
  parseJSON = withObject "Node" $ \o -> Node
    <$> o .: "id"
    <*> o .:? "message"
    <*> o .:? "parent"
    <*> (o .: "children" <|> pure [])
