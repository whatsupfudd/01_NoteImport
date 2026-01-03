{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
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


data Conversation = Conversation {
  titleCv :: Text,
  createTimeCv :: Double,
  updateTimeCv :: Double,
  mappingCv :: Mp.Map Text Node,
  convIdCv :: Text
} deriving (Show, Generic, ToJSON)

instance FromJSON Conversation where
  parseJSON = withObject "Conversation" $ \o ->
    Conversation
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
} deriving (Show, Generic, ToJSON)

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
} deriving (Show, Generic, ToJSON)

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
} deriving (Show, Generic, ToJSON)

instance FromJSON Author where
  parseJSON = withObject "Author" $ \o -> Author
    <$> o .: "role"
    <*> o .:? "name"
    <*> (objectToMap <$> o .: "metadata")

{-
content types and frequencies on 577 discussions::
 code                    |  2787
 execution_output        |   726
 multimodal_text         |   747
 reasoning_recap         |  1774
 system_error            |    38
 tether_browsing_display |   159
 tether_quote            |   215
 text                    | 14865
 thoughts                |  3356
-}

data Content =
    CodeCT {
      languageCc :: Text,
      responseFormatNameCc :: Maybe Text,
      textCc :: Text
    }
  | ExecutionOutputCT {
    textEoc :: Text
  }
  | MultimodalTextCT {
    partsMtc :: [MultiModalPart]
  }
  | ModelEditableContextCT {
      modelSetContextMec :: Text,
      repositoryMec :: Maybe Value,
      repoSummaryMec :: Maybe Value,
      structuredContextMec :: Maybe Value
    }
  | ReasoningRecapCT {
    contentRrc :: Text
  }
  | SystemErrorCT {
    nameSes :: Text
    , textSes :: Text
  }
  | TetherBrowsingDisplayCT {
      resultTbd :: Text
    , summaryTbd :: Maybe Text
    , assetsTbd :: Maybe [Value]
    , tetherIDTbd :: Maybe Text
  }
  | TetherQuoteCT {
    urlTq :: Text
    , domainTq :: Text
    , textTq :: Text
    , titleTq :: Text
    , tetherIDTq :: Maybe Text
  }
  | TextCT {
      partsCt :: [Text]
    }
  | ThoughtsCT {
      thoughtsTc :: [Thought],
      sourceAnalysisMsgIdTc :: Text
    }
  | OtherCT {
    contentTypeOc :: Text,
    rawOc :: Mp.Map Text Value
  } deriving (Show, Generic, ToJSON)

instance FromJSON Content where
  parseJSON = withObject "Content" $ \o -> do
    ctype :: Text <- o .: "content_type"
    case ctype of
      "code" -> CodeCT
        <$> o .: "language"
        <*> o .:? "response_format_name"
        <*> o .: "text"
      "execution_output" -> ExecutionOutputCT <$> o .: "text"
      "multimodal_text" -> MultimodalTextCT <$> o .: "parts"
      "model_editable_context" -> ModelEditableContextCT
        <$> o .: "model_set_context"
        <*> o .:? "repository"
        <*> o .:? "repo_summary"
        <*> o .:? "structured_context"
      "reasoning_recap" -> ReasoningRecapCT <$> o .: "content"
      "system_error" -> SystemErrorCT
        <$> o .: "name"
        <*> o .: "text"
      "tether_browsing_display" -> TetherBrowsingDisplayCT
        <$> o .: "result"
        <*> o .:? "summary"
        <*> o .:? "assets"
        <*> o .:? "tether_id"
      "tether_quote" -> TetherQuoteCT
        <$> o .: "url"
        <*> o .: "domain"
        <*> o .: "text"
        <*> o .: "title"
        <*> o .:? "tether_id"
      "text" -> TextCT <$> o .: "parts"
      "thoughts" -> ThoughtsCT
        <$> o .: "thoughts"
        <*> o .: "source_analysis_msg_id"
      _ -> pure $ OtherCT ctype (objectToMap o)

-- Eventually MultiModalPart will be a sum type of all possible part types.
data MultiModalPart =
  TextPT Text
  | AudioTranscriptionPT {
    textAtp :: Text,
    directionAtp :: Text,
    decodingIdAtp :: Maybe Text
  }
  | AudioAssetPointerPT AudioAssetPointer
  | ImageAssetPointerPT {
    assetPointerPap :: Text,
    sizeBytesPap :: Int,
    widthPap :: Int,
    heightPap :: Int,
    foveaPap :: Maybe Value,
    metadataPap :: Maybe ImageMetadata
  }
  | RealTimeUserAVPT {
    expiryDatetimeRtuav :: Maybe Value
    , framesAssetPointersRtuav :: [Value]
    , videoContainerAssetPointer :: Maybe Value
    , audioAssetPointer :: AudioAssetPointer
    , audioStartTimestampRtuav :: Maybe Double
  }
  deriving (Show, Generic, ToJSON)


instance FromJSON MultiModalPart where
  parseJSON aValue =
    case aValue of
      String aText -> pure $ TextPT aText
      Object o -> do
        ptype :: Text <- o .: "content_type"
        case ptype of
          "audio_transcription" -> AudioTranscriptionPT
            <$> o .: "text"
            <*> o .: "direction"
            <*> o .:? "decoding_id"
          "audio_asset_pointer" -> AudioAssetPointerPT <$> parseJSON (Object o)
          "image_asset_pointer" -> ImageAssetPointerPT
            <$> o .: "asset_pointer"
            <*> o .: "size_bytes"
            <*> o .: "width"
            <*> o .: "height"
            <*> o .: "fovea"
            <*> o .:? "metadata"
          "real_time_user_audio_video_asset_pointer" -> RealTimeUserAVPT
            <$> o .:? "expiry_datetime"
            <*> o .: "frames_asset_pointers"
            <*> o .:? "video_container_asset_pointer"
            <*> o .: "audio_asset_pointer"
            <*> o .:? "audio_start_timestamp"
          _ -> fail $ "Unknown part type: " <> T.unpack ptype
      _ -> fail $ "@[MultiModalPart.parseJSON] unexpected value: " <> show aValue



data AudioAssetPointer = AudioAssetPointer {
  expiryDatetimeAap :: Maybe Value,
  assetPointerAap :: Text,
  sizeBytesAap :: Int,
  formatAap :: Text,
  toolAudioDirectionAap :: Maybe Text,
  metadataAap :: Maybe AudioMetadata
} deriving (Show, Generic, ToJSON)

instance FromJSON AudioAssetPointer where
  parseJSON = withObject "AudioAssetPointer" $ \o -> AudioAssetPointer
    <$> o .:? "expiry_datetime"
    <*> o .: "asset_pointer"
    <*> o .: "size_bytes"
    <*> o .: "format"
    <*> o .:? "tool_audio_direction"
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
} deriving (Show, Generic, ToJSON)

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
} deriving (Show, Generic, ToJSON)

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
} deriving (Show, Generic, ToJSON)

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



data AudioMetadata = AudioMetadata {
  startTimestampAm :: Maybe Value,
  endTimestampAm :: Maybe Value,
  pretokenizedVqAm :: Maybe Value,
  interruptionsAm :: Maybe Value,
  originalAudioSourceAm :: Maybe Value,
  transcriptionAm :: Maybe Value,
  wordTranscriptionAm :: Maybe Value,
  startAm :: Double,
  endAm :: Double
} deriving (Show, Generic, ToJSON)


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


data Thought = Thought {
  summaryTh :: Text,
  contentTh :: Text,
  chunksTh :: Maybe [Value],
  finishedTh :: Maybe Bool
} deriving (Show, Generic, ToJSON)

instance FromJSON Thought where
  parseJSON = withObject "Thought" $ \o -> Thought
    <$> o .: "summary"
    <*> o .: "content"
    <*> o .:? "chunks"
    <*> o .:? "finished"

