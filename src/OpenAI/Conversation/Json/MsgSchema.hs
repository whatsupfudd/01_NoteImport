{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}
module OpenAI.Conversation.Json.MsgSchema where

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

import OpenAI.Conversation.Json.Utils (objectToMap)


data Message = Message {
    idMsg :: Text
  , authorMsg :: Author
  , createTimeMsg :: Scientific
  , updateTimeMsg :: Maybe Scientific
  , contentMsg :: Content
  , statusMsg :: Text
  , endTurnMsg :: Maybe Bool
  , weightMsg :: Scientific
  , metadataMsg :: Mp.Map Text Value
  , recipientMsg :: Text
  , channelMsg :: Maybe Text
  } deriving (Show, Eq, Generic, ToJSON)

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


data Author = Author
  { roleAu :: Text
  , nameAu :: Maybe Text
  , metadataAu :: Mp.Map Text Value
  } deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Author where
  parseJSON = withObject "Author" $ \o ->
    Author
      <$> o .: "role"
      <*> o .: "name"
      <*> (objectToMap <$> o .: "metadata")


data Content =
    CodeCT CodePL
  | ExecutionOutputCT ExecutionOutputPL
  | MultimodalTextCT MultimodalTextPL  -- V1
  | ModelEditableContextCT ModelEditableContextPL
  | ReasoningRecapCT ReasoningRecapPL
  | SystemErrorCT SystemErrorPL  -- V1
  | TetherBrowsingDisplayCT TetherBrowsingDisplayPL
  | TetherQuoteCT TetherQuotePL
  | TextCT TextPL
  | ThoughtsCT ThoughtsPL
  | OtherCT OtherPL
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Content where
  parseJSON = withObject "Content" $ \o -> do
    tagValue <- o .: "content_type"
    case (tagValue :: Text) of
      "code" -> CodeCT <$> parseJSON (Object o)
      "execution_output" -> ExecutionOutputCT <$> parseJSON (Object o)
      "multimodal_text" -> MultimodalTextCT <$> parseJSON (Object o)
      "model_editable_context" -> ModelEditableContextCT <$> parseJSON (Object o)
      "reasoning_recap" -> ReasoningRecapCT <$> parseJSON (Object o)
      "system_error" -> SystemErrorCT <$> parseJSON (Object o)
      "tether_browsing_display" -> TetherBrowsingDisplayCT <$> parseJSON (Object o)
      "tether_quote" -> TetherQuoteCT <$> parseJSON (Object o)
      "text" -> TextCT <$> parseJSON (Object o)
      "thoughts" -> ThoughtsCT <$> parseJSON (Object o)
      other -> pure . OtherCT . OtherPL tagValue $ objectToMap o


data CodePL = CodePL {
    languageCP :: Text
  , responseFormatNameCP :: Maybe Text
  , textCP :: Text
  } deriving (Show, Eq, Generic, ToJSON)

instance FromJSON CodePL where
  parseJSON = withObject "CodePL" $ \o ->
    CodePL
      <$> o .: "language"
      <*> o .: "response_format_name"
      <*> o .: "text"


newtype ExecutionOutputPL = ExecutionOutputPL {
    textEO :: Text
  } deriving (Show, Eq, Generic, ToJSON)

instance FromJSON ExecutionOutputPL where
  parseJSON = withObject "ExecutionOutputPL" $ \o ->
    ExecutionOutputPL <$> o .: "text"


newtype MultimodalTextPL = MultimodalTextPL {
    partsMmt :: [MultiModalPart]
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON MultimodalTextPL where
  parseJSON = withObject "MultimodalTextPL" $ \o ->
    MultimodalTextPL <$> o .: "parts"


data MultiModalPart =
  TextPT Text
  | AudioTranscriptionPT AudioTranscriptionPL
  | AudioAssetPointerPT AudioAssetPointer
  | ImageAssetPointerPT ImageAssetPointerPL
  | RealTimeUserAVPT RealTimeUserAVPL
  deriving (Show, Eq, Generic, ToJSON)


instance FromJSON MultiModalPart where
  parseJSON aValue =
    case aValue of
      String aText -> pure $ TextPT aText
      Object o -> do
        ptype <- o .: "content_type"
        case ptype of
          "audio_transcription" -> AudioTranscriptionPT <$> parseJSON (Object o)
          "audio_asset_pointer" -> AudioAssetPointerPT <$> parseJSON (Object o)
          "image_asset_pointer" -> ImageAssetPointerPT <$> parseJSON (Object o)
          "real_time_user_audio_video_asset_pointer" -> RealTimeUserAVPT <$> parseJSON (Object o)
          _ -> fail $ "Unknown part type: " <> T.unpack ptype
      _ -> fail $ "@[MultiModalPart.parseJSON] unexpected value: " <> show aValue


data AudioTranscriptionPL = AudioTranscriptionPL {
    textAtp :: Text,
    directionAtp :: Text,
    decodingIdAtp :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON AudioTranscriptionPL where
  parseJSON = withObject "AudioTranscriptionPL" $ \o ->
    AudioTranscriptionPL
      <$> o .: "text"
      <*> o .: "direction"
      <*> o .:? "decoding_id"

data AudioAssetPointer = AudioAssetPointer {
  expiryDatetimeAap :: Maybe Scientific,
  assetPointerAap :: Text,
  sizeBytesAap :: Int,
  formatAap :: Text,
  toolAudioDirectionAap :: Maybe Text,
  metadataAap :: Maybe AudioMetadata
} deriving (Show, Eq, Generic, ToJSON)

instance FromJSON AudioAssetPointer where
  parseJSON = withObject "AudioAssetPointer" $ \o -> AudioAssetPointer
    <$> o .:? "expiry_datetime"
    <*> o .: "asset_pointer"
    <*> o .: "size_bytes"
    <*> o .: "format"
    <*> o .:? "tool_audio_direction"
    <*> o .: "metadata"


data AudioMetadata = AudioMetadata {
  startTimestampAm :: Maybe Scientific,
  endTimestampAm :: Maybe Scientific,
  pretokenizedVqAm :: Maybe Value,
  interruptionsAm :: Maybe Value,
  originalAudioSourceAm :: Maybe Value,
  transcriptionAm :: Maybe Value,
  wordTranscriptionAm :: Maybe Value,
  startAm :: Scientific,
  endAm :: Scientific
} deriving (Show, Eq, Generic, ToJSON)


instance FromJSON AudioMetadata where
  parseJSON = withObject "AudioMetadata" $ \o -> AudioMetadata
    <$> o .:? "start_timestamp"
    <*> o .:? "end_timestamp"
    <*> o .:? "pretokenized_vq"
    <*> o .:? "interruptions"
    <*> o .:? "original_audio_source"
    <*> o .:? "transcription"
    <*> o .:? "word_transcription"
    <*> o .: "start"
    <*> o .: "end"

data ImageAssetPointerPL = ImageAssetPointerPL {
    assetPointerPap :: Text,
    sizeBytesPap :: Int,
    widthPap :: Int,
    heightPap :: Int,
    foveaPap :: Maybe Value,
    metadataPap :: Maybe ImageMetadata
  }
  deriving (Show, Eq, Generic, ToJSON)


instance FromJSON ImageAssetPointerPL where
  parseJSON = withObject "ImageAssetPointerPL" $ \o -> ImageAssetPointerPL
    <$> o .: "asset_pointer"
    <*> o .: "size_bytes"
    <*> o .: "width"
    <*> o .: "height"
    <*> o .:? "fovea"
    <*> o .: "metadata"

data ImageMetadata = ImageMetadata {
  dalleMd :: Maybe Dalle,
  gizmoMd :: Maybe Value,
  generationMd :: Maybe Generation,
  containerPixelHeightMd :: Maybe Int,
  containerPixelWidthMd :: Maybe Int,
  emuOmitGlimpseImageMd :: Maybe Value,
  emuPatchesOverrideMd :: Maybe Value,
  lpeKeepPatchIjhwMd :: Maybe Value,
  lpeDeltaEncodingChannelMd :: Maybe Value,
  sanitizedMd :: Bool,
  assetPointerLinkMd :: Maybe Value,
  watermarkedAssetPointerMd :: Maybe Value,
  isNoAuthPlaceholderMd :: Maybe Value
} deriving (Show, Eq, Generic, ToJSON)

instance FromJSON ImageMetadata where
  parseJSON = withObject "Metadata" $ \o -> ImageMetadata
    <$> o .:? "dalle"
    <*> o .:? "gizmo"
    <*> o .:? "generation"
    <*> o .: "container_pixel_height"
    <*> o .: "container_pixel_width"
    <*> o .:? "emu_omit_glimpse_image"
    <*> o .:? "emu_patches_override"
    <*> o .:? "lpe_keep_patch_ijhw"
    <*> o .:? "lpe_delta_encoding_channel"
    <*> o .: "sanitized"
    <*> o .:? "asset_pointer_link"
    <*> o .:? "watermarked_asset_pointer"
    <*> o .:? "is_no_auth_placeholder"


data Dalle = Dalle {
  genIdDa :: Maybe Text,
  promptDa :: Text,
  seedDa :: Maybe Int,
  parentGenIdDa :: Maybe Text,
  editOpDa :: Maybe Text,
  serializationTitleDa :: Text
} deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Dalle where
  parseJSON = withObject "Dalle" $ \o -> Dalle
    <$> o .:? "gen_id"
    <*> o .: "prompt"
    <*> o .:? "seed"
    <*> o .:? "parent_gen_id"
    <*> o .:? "edit_op"
    <*> o .: "serialization_title"

data Generation = Generation {
  genIdGe :: Maybe Text,
  genSizeGe :: Text,
  seedGe :: Maybe Int,
  parentGenIdGe :: Maybe Text,
  heightGe :: Int,
  widthGe :: Int,
  transparentBackgroundGe :: Bool,
  serializationTitleGe :: Text,
  orientationGe :: Maybe Text
} deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Generation where
  parseJSON = withObject "Generation" $ \o -> Generation
    <$> o .:? "gen_id"
    <*> o .: "gen_size"
    <*> o .:? "seed"
    <*> o .:? "parent_gen_id"
    <*> o .: "height"
    <*> o .: "width"
    <*> o .: "transparent_background"
    <*> o .: "serialization_title"
    <*> o .:? "orientation"


data RealTimeUserAVPL = RealTimeUserAVPL {
    expiryDatetimeRtuav :: Maybe Scientific
    , framesApRtuav :: [Value]
    , videoContainerApRtuav :: Maybe Value
    , audioApRtuav :: AudioAssetPointer
    , audioStartTimestampRtuav :: Maybe Scientific
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON RealTimeUserAVPL where
  parseJSON = withObject "RealTimeUserAVPL" $ \o -> RealTimeUserAVPL
    <$> o .:? "expiry_datetime"
    <*> o .: "frames_asset_pointers"
    <*> o .:? "video_container_asset_pointer"
    <*> o .: "audio_asset_pointer"
    <*> o .:? "audio_start_timestamp"


-- Other top-level payloads:

data ModelEditableContextPL = ModelEditableContextPL
  { modelSetMEC :: Text
  , repositoryMEC :: Maybe Value
  , repoSummaryMEC :: Maybe Value
  , structuredMEC :: Maybe Value
  } deriving (Show, Eq, Generic, ToJSON)

instance FromJSON ModelEditableContextPL where
  parseJSON = withObject "ModelEditableContextPL" $ \o ->
    ModelEditableContextPL
      <$> o .: "model_set_context"
      <*> o .: "repository"
      <*> o .: "repo_summary"
      <*> o .: "structured_context"


newtype ReasoningRecapPL = ReasoningRecapPL {
    contentRR :: Text
  } deriving (Show, Eq, Generic, ToJSON)

instance FromJSON ReasoningRecapPL where
  parseJSON = withObject "ReasoningRecapPL" $ \o ->
    ReasoningRecapPL
      <$> o .: "content"

data SystemErrorPL = SystemErrorPL {
    nameSER :: Text
  , textSER :: Text
  } deriving (Show, Eq, Generic, ToJSON)

instance FromJSON SystemErrorPL where
  parseJSON = withObject "SystemErrorPL" $ \o ->
    SystemErrorPL
      <$> o .: "name"
      <*> o .: "text"


data TetherBrowsingDisplayPL = TetherBrowsingDisplayPL {
      resultTbd :: Text
    , summaryTbd :: Maybe Value
    , assetsTbd :: Maybe [Value]
    , tetherIDTbd :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON TetherBrowsingDisplayPL where
  parseJSON = withObject "TetherBrowsingDisplayPL" $ \o ->
    TetherBrowsingDisplayPL
      <$> o .: "result"
      <*> o .: "summary"
      <*> o .: "assets"
      <*> o .: "tether_id"


data TetherQuotePL = TetherQuotePL {
    urlTq :: Text
    , domainTq :: Text
    , textTq :: Text
    , titleTq :: Text
    , tetherIDTq :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON TetherQuotePL where
  parseJSON = withObject "TetherQuotePL" $ \o ->
    TetherQuotePL
      <$> o .: "url"
      <*> o .: "domain"
      <*> o .: "text"
      <*> o .: "title"
      <*> o .: "tether_id"


newtype TextPL = TextPL {
    partsTP :: [Text]
  } deriving (Show, Eq, Generic, ToJSON)

instance FromJSON TextPL where
  parseJSON = withObject "TextPL" $ \o ->
    TextPL
      <$> o .: "parts"

data ThoughtsPL = ThoughtsPL
  {
    thoughtsTP :: [ThoughtContent]
  , sourceAnalysisMsgIdTP :: Text
  } deriving (Show, Eq, Generic, ToJSON)

instance FromJSON ThoughtsPL where
  parseJSON :: Value -> Parser ThoughtsPL
  parseJSON = withObject "ThoughtsPL" $ \o ->
    ThoughtsPL
      <$> o .: "thoughts"
      <*> o .: "source_analysis_msg_id"

data ThoughtContent = ThoughtContent
  { summaryTC :: Text
  , contentTC :: Text
  , chunksTC :: [Text]
  , finishedTC :: Bool
  } deriving (Show, Eq, Generic, ToJSON)

instance FromJSON ThoughtContent where
  parseJSON = withObject "ThoughtContent" $ \o ->
    ThoughtContent
      <$> o .: "summary"
      <*> o .: "content"
      <*> o .: "chunks"
      <*> o .: "finished"

data OtherPL = OtherPL {
  contentTypeOpl :: Text
  , rawOpl :: Mp.Map Text Value
} deriving (Show, Eq, Generic, ToJSON)
