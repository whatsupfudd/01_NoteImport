{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module OpenAI.Conversation.Json.Schema (
  Conversation(..), Owner(..), PageInfo(..), FollowConv(..),
  module OpenAI.Conversation.Json.MsgSchema
  ) where

import Control.Applicative ((<|>))

import qualified Data.Map.Strict as Mp
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import GHC.Generics (Generic)

import Data.Aeson (FromJSON(..), ToJSON(..), Object, Value(..), withObject, (.:), (.:?))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as Km
import Data.Aeson.Types (Parser)

import OpenAI.Conversation.Json.Utils
import OpenAI.Conversation.Json.Types
import OpenAI.Conversation.Json.MsgSchema
import qualified OpenAI.Conversation.Json.Node as Nd


data Conversation = Conversation {
  versionJsonCv :: VersionJson
  , titleCv :: Text
  , createTimeCv :: Scientific
  , updateTimeCv :: Scientific
  , oaiIdCv :: Text
  , moderationResultsCv :: [Value]
  , pluginIdsCv :: Maybe Value
  , templateIdCv :: Text
  , gizmoIdCv :: Text
  , gizmoTypeCv :: Text
  , isArchivedCv :: Bool
  , isStarredCv :: Maybe Value
  , safeUrlsCv :: [Text]
  , blockedUrlsCv :: [Value]
  , defaultModelSlugCv :: Text
  , atlasModeEnabledCv :: Maybe Value
  , conversationOriginCv :: Maybe Value
  , isReadOnlyCv :: Maybe Value
  , voiceCv :: Maybe Value
  , asyncStatusCv :: Maybe Value
  , disabledToolIdsCv :: [Value]
  , isTemporaryChatCv :: Bool
  , isDoNotRememberCv :: Bool
  , memoryScopeCv :: Text
  , contextScopesCv :: [Value]
  , sugarItemIdCv :: Maybe Value
  , sugarItemVisibleCv :: Bool
  , pinnedTimeCv :: Maybe Value
  , isStudyModeCv :: Bool
  , ownerCv :: Owner
  , messagesCv :: [Message]
  , currentNodeCv :: Text
  , pageInfoCv :: PageInfo
  , contextTruncationContinuationCv :: Maybe Value
  , nodeMapCv :: Mp.Map Text Nd.Node
  } deriving (Show, Generic, ToJSON)

instance FromJSON Conversation where
  parseJSON = withObject "Conversation" $ \o ->
    Conversation V2vj
      <$> o .: "title"
      <*> o .: "create_time"
      <*> o .: "update_time"
      <*> o .: "conversation_id"
      <*> o .: "moderation_results"
      <*> o .: "plugin_ids"
      <*> o .: "conversation_template_id"
      <*> o .: "gizmo_id"
      <*> o .: "gizmo_type"
      <*> o .: "is_archived"
      <*> o .: "is_starred"
      <*> o .: "safe_urls"
      <*> o .: "blocked_urls"
      <*> o .: "default_model_slug"
      <*> o .: "atlas_mode_enabled"
      <*> o .: "conversation_origin"
      <*> o .: "is_read_only"
      <*> o .: "voice"
      <*> o .: "async_status"
      <*> o .: "disabled_tool_ids"
      <*> o .: "is_temporary_chat"
      <*> o .: "is_do_not_remember"
      <*> o .: "memory_scope"
      <*> o .: "context_scopes"
      <*> o .: "sugar_item_id"
      <*> o .: "sugar_item_visible"
      <*> o .: "pinned_time"
      <*> o .: "is_study_mode"
      <*> o .: "owner"
      <*> o .: "messages"
      <*> o .: "current_node"
      <*> o .: "page_info"
      <*> o .: "context_truncation_continuation"
      <*> pure Mp.empty
 


data FollowConv = FollowConv {
    messagesCv :: [Message]
  , pageInfoCv :: PageInfo
  , safeUrlsCv :: [Text]
  , blockedUrlsCv :: [Value]
  }
  deriving (Show, Generic, ToJSON)

instance FromJSON FollowConv where
  parseJSON = withObject "FollowConv" $ \o ->
    FollowConv
      <$> o .: "messages"
      <*> o .: "page_info"
      <*> o .: "safe_urls"
      <*> o .: "blocked_urls"


data Owner = Owner
  { userIdOwner :: Text
  , userEmailOwner :: Text
  , nameOwner :: Text
  , avatarUrlOwner :: Text
  } deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Owner where
  parseJSON = withObject "Owner" $ \o ->
    Owner
      <$> o .: "user_id"
      <*> o .: "user_email"
      <*> o .: "name"
      <*> o .: "avatar_url"



data PageInfo = PageInfo
  { startCursorPI :: Text
  , endCursorPI :: Text
  , hasPreviousPI :: Bool
  , hasNextPagePI :: Bool
  } deriving (Show, Eq, Generic, ToJSON)

instance FromJSON PageInfo where
  parseJSON = withObject "PageInfo" $ \o ->
    PageInfo
      <$> o .: "start_cursor"
      <*> o .: "end_cursor"
      <*> o .: "has_previous_page"
      <*> o .: "has_next_page"
