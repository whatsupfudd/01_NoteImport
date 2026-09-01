module OpenAI.Json.V2 where

import Control.Applicative ((<|>))

import qualified Data.Map.Strict as Mp
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Data.Aeson (FromJSON(..), Object, Value(..), withObject, (.:), (.:?))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as Km
import Data.Aeson.Types (Parser)

import OpenAI.Json.Utils



data Conversation = Conversation {
    titleCv :: Text
  , createTimeCv :: Scientific
  , updateTimeCv :: Scientific
  , moderationResultsCv :: [Value]
  , pluginIdsCv :: Maybe Value
  , conversationIdCv :: Text
  , conversationTemplateIdCv :: Text
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
  } deriving (Show, Eq)


instance FromJSON Conversation where
  parseJSON = withObject "Conversation" $ \o ->
    Conversation
      <$> o .: "title"
      <*> o .: "create_time"
      <*> o .: "update_time"
      <*> o .: "moderation_results"
      <*> o .: "plugin_ids"
      <*> o .: "conversation_id"
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


data Owner = Owner
  { userIdOwner :: Text
  , userEmailOwner :: Text
  , nameOwner :: Text
  , avatarUrlOwner :: Text
  } deriving (Show, Eq)

instance FromJSON Owner where
  parseJSON = withObject "Owner" $ \o ->
    Owner
      <$> o .: "user_id"
      <*> o .: "user_email"
      <*> o .: "name"
      <*> o .: "avatar_url"


data Message = Message
  { idMsg :: Text
  , authorMsg :: AuthorMessage
  , createTimeMsg :: Scientific
  , updateTimeMsg :: Maybe Scientific
  , contentMsg :: Content
  , statusMsg :: Text
  , endTurnMsg :: Maybe Bool
  , weightMsg :: Scientific
  , metadataMsg :: Mp.Map Text Value
  , recipientMsg :: Text
  , channelMsg :: Maybe Text
  } deriving (Show, Eq)

instance FromJSON Message where
  parseJSON = withObject "Message" $ \o ->
    Message
      <$> o .: "id"
      <*> o .: "author"
      <*> o .: "create_time"
      <*> o .: "update_time"
      <*> o .: "content"
      <*> o .: "status"
      <*> o .: "end_turn"
      <*> o .: "weight"
      <*> (objectToMap <$> o .: "metadata")
      <*> o .: "recipient"
      <*> o .: "channel"


data AuthorMessage = AuthorMessage
  { roleAuthorMessage :: Text
  , nameAuthorMessage :: Maybe Text
  , metadataAuthorMessage :: Mp.Map Text Value
  } deriving (Show, Eq)

instance FromJSON AuthorMessage where
  parseJSON = withObject "AuthorMessage" $ \o ->
    AuthorMessage
      <$> o .: "role"
      <*> o .: "name"
      <*> (objectToMap <$> o .: "metadata")


data Content =
    TextCT TextPayload
  | ModelEditableContextCT ModelEditableContextPayload
  | CodeCT CodePayload
  | ThoughtsCT ThoughtsPayload
  | ReasoningRecapCT ReasoningRecapPayload
  | ExecutionOutputCT ExecutionOutputPayload
  deriving (Show, Eq)

instance FromJSON Content where
  parseJSON = withObject "Content" $ \o -> do
    tagValue <- o .: "content_type"
    case (tagValue :: Text) of
      "text" -> TextCT <$> parseJSON (Object o)
      "model_editable_context" -> ModelEditableContextCT <$> parseJSON (Object o)
      "code" -> CodeCT <$> parseJSON (Object o)
      "thoughts" -> ThoughtsCT <$> parseJSON (Object o)
      "reasoning_recap" -> ReasoningRecapCT <$> parseJSON (Object o)
      "execution_output" -> ExecutionOutputCT <$> parseJSON (Object o)
      other ->
        fail ("Unknown content_type for CT: " <> T.unpack other)

data TextPayload = TextPayload
  { partsTextPayload :: [Text]
  } deriving (Show, Eq)

instance FromJSON TextPayload where
  parseJSON = withObject "TextPayload" $ \o ->
    TextPayload
      <$> o .: "parts"

data ModelEditableContextPayload = ModelEditableContextPayload
  { modelSetContextModelEditableContextPayload :: Text
  , repositoryModelEditableContextPayload :: Maybe Value
  , repoSummaryModelEditableContextPayload :: Maybe Value
  , structuredContextModelEditableContextPayload :: Maybe Value
  } deriving (Show, Eq)

instance FromJSON ModelEditableContextPayload where
  parseJSON = withObject "ModelEditableContextPayload" $ \o ->
    ModelEditableContextPayload
      <$> o .: "model_set_context"
      <*> o .: "repository"
      <*> o .: "repo_summary"
      <*> o .: "structured_context"

data CodePayload = CodePayload
  { languageCodePayload :: Text
  , responseFormatNameCodePayload :: Maybe Value
  , textCodePayload :: Text
  } deriving (Show, Eq)

instance FromJSON CodePayload where
  parseJSON = withObject "CodePayload" $ \o ->
    CodePayload
      <$> o .: "language"
      <*> o .: "response_format_name"
      <*> o .: "text"

data ThoughtsPayload = ThoughtsPayload
  { thoughtsThoughtsPayload :: [ThoughtMessageContentThoughts]
  , sourceAnalysisMsgIdThoughtsPayload :: Text
  } deriving (Show, Eq)

instance FromJSON ThoughtsPayload where
  parseJSON = withObject "ThoughtsPayload" $ \o ->
    ThoughtsPayload
      <$> o .: "thoughts"
      <*> o .: "source_analysis_msg_id"

data ThoughtMessageContentThoughts = ThoughtMessageContentThoughts
  { summaryThoughtMessageContentThoughts :: Text
  , contentThoughtMessageContentThoughts :: Text
  , chunksThoughtMessageContentThoughts :: [Value]
  , finishedThoughtMessageContentThoughts :: Bool
  } deriving (Show, Eq)

instance FromJSON ThoughtMessageContentThoughts where
  parseJSON = withObject "ThoughtMessageContentThoughts" $ \o ->
    ThoughtMessageContentThoughts
      <$> o .: "summary"
      <*> o .: "content"
      <*> o .: "chunks"
      <*> o .: "finished"

data ReasoningRecapPayload = ReasoningRecapPayload
  { contentReasoningRecapPayload :: Text
  } deriving (Show, Eq)

instance FromJSON ReasoningRecapPayload where
  parseJSON = withObject "ReasoningRecapPayload" $ \o ->
    ReasoningRecapPayload
      <$> o .: "content"

data ExecutionOutputPayload = ExecutionOutputPayload
  { textExecutionOutputPayload :: Text
  } deriving (Show, Eq)

instance FromJSON ExecutionOutputPayload where
  parseJSON = withObject "ExecutionOutputPayload" $ \o ->
    ExecutionOutputPayload
      <$> o .: "text"

data PageInfo = PageInfo
  { startCursorPageInfo :: Text
  , endCursorPageInfo :: Text
  , hasPreviousPagePageInfo :: Bool
  , hasNextPagePageInfo :: Bool
  } deriving (Show, Eq)

instance FromJSON PageInfo where
  parseJSON = withObject "PageInfo" $ \o ->
    PageInfo
      <$> o .: "start_cursor"
      <*> o .: "end_cursor"
      <*> o .: "has_previous_page"
      <*> o .: "has_next_page"
