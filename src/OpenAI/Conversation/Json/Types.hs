{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module OpenAI.Conversation.Json.Types where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON (..))


data VersionJson = V1vj | V2vj
  deriving (Show, Generic, ToJSON)
