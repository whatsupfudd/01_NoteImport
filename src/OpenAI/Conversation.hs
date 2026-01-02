{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module OpenAI.Conversation where

import Control.Monad (forM)

import Data.Text (Text)
import Data.Int (Int64, Int32)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Mp
import qualified Data.Vector as V

import qualified Data.Aeson as Ae
import Data.Aeson (Value)


-- | Root representation of a conversation as stored in 'oai.discussions'
data ConversationDb = ConversationDb
  { uidCv         :: Int64
  , titleCv       :: Text
  , eidCv         :: Text            -- external_id (conversation_id in JSON)
  , createTimeCv  :: Double
  , updateTimeCv  :: Double
  , nodesCv       :: Map Text NodeDb -- Keyed by external 'eid' (idNd)
  } deriving (Show)

-- | Represents 'oai.nodes'
data NodeDb = NodeDb
  { uidNd         :: Int64
  , eidNd         :: Text
  , parentFkNd    :: Maybe Int64
  , messageNd     :: Maybe MessageDb
  } deriving (Show)

-- | Represents 'oai.messages'
data MessageDb = MessageDb
  { uidMsg        :: Int64
  , eidMsg        :: Text
  , createTimeMsg :: Maybe Double
  , updateTimeMsg :: Maybe Double
  , statusMsg     :: Text
  , endTurnMsg    :: Maybe Bool
  , weightMsg     :: Double
  , metadataMsg   :: Value
  , recipientMsg  :: Text
  , channelMsg    :: Maybe Text
  , authorMsg     :: AuthorDb
  , contentsMsg   :: V.Vector ContentDb
  } deriving (Show)

-- | Represents 'oai.authors'
data AuthorDb = AuthorDb
  { uidAu         :: Int64
  , roleAu        :: Text
  , nameAu        :: Maybe Text
  , metadataAu    :: Value
  } deriving (Show)

-- | Sum type representing the various joined content tables linked via 'oai.contents'
data ContentDb =
    CodeCT_Db
      { languageCc :: Text
      , responseFormatNameCc :: Maybe Text
      , textCc :: Text
      }
  | ExecutionOutputCT_Db
      { textEoc :: Text
      }
  | MultimodalTextCT_Db
      { partsMtc :: V.Vector MultiModalPartDb
      }
  | ModelEditableContextCT_Db
      { modelSetContextMec :: Text
      , repositoryMec :: Maybe Value
      , repoSummaryMec :: Maybe Value
      , structuredContextMec :: Maybe Value
      }
  | ReasoningRecapCT_Db
      { contentRrc :: Text
      }
  | SystemErrorCT_Db
      { nameSes :: Text
      , textSes :: Text
      }
  | TetherBrowsingDisplayCT_Db
      { resultTbd :: Text
      , summaryTbd :: Maybe Value
      , assetsTbd :: Maybe Value
      , tetherIDTbd :: Maybe Text
      }
  | TetherQuoteCT_Db
      { urlTq :: Text
      , domainTq :: Text
      , textTq :: Text
      , titleTq :: Text
      , tetherIDTq :: Maybe Text
      }
  | TextCT_Db
      { partsCt :: V.Vector Text
      }
  | ThoughtsCT_Db
      { sourceAnalysisMsgIdTc :: Text
      , thoughtsTc :: V.Vector ThoughtDb
      }
  | UnknownCT_Db
      { contentTypeOc :: Text
      , opaqueValueOc :: Value
      }
  deriving (Show)

-- | Represents 'oai.multimodal_parts' and its specialized child tables
data MultiModalPartDb =
    TextPT_Db Text
  | AudioTranscriptionPT_Db AudioTranscriptionDb
  | AudioAssetPointerPT_Db AudioAssetPointerDb
  | ImageAssetPointerPT_Db ImageAssetPointerDb
  | RealTimeUserAVPT_Db RealTimeUserAVDb
  deriving (Show)

data AudioTranscriptionDb = AudioTranscriptionDb
  { textAtp :: Text
  , directionAtp :: Text
  , decodingIdAtp :: Maybe Text
  } deriving (Show)

data ImageAssetPointerDb = ImageAssetPointerDb
  { assetPointerIap :: Text
  , sizeBytesIap    :: Int64
  , widthIap        :: Int32
  , heightIap       :: Int32
  , foveaIap        :: Maybe Value
  , metadataIap     :: Maybe ImageMetadataDb
  } deriving (Show)

data RealTimeUserAVDb = RealTimeUserAVDb
  { expiryDatetimeRtuav     :: Maybe Value
  , framesAssetPointersRtuav :: Value
  , videoContainerAssetPointerRtuav :: Maybe Value
  , audioStartTimestampRtuav :: Maybe Double
  }
 deriving (Show)



-- | Represents 'oai.audio_asset_pointer_mmpart' and 'oai.metadatas_audioasset'
data AudioAssetPointerDb = AudioAssetPointerDb
  { expiryDatetimeAap     :: Maybe Value
  , assetPointerAap       :: Text
  , sizeBytesAap         :: Int64
  , formatAap             :: Text
  , toolAudioDirectionAap :: Maybe Text
  , metadataAap           :: Maybe AudioMetadataDb
  } deriving (Show)

-- | Represents 'oai.metadatas_imgasset' and its relations ('dalles', 'generations')
data ImageMetadataDb = ImageMetadataDb
  { dalleMd                   :: Maybe DalleDb
  , gizmoMd                   :: Maybe Value
  , generationMd              :: Maybe GenerationDb
  , containerPixelHeightMd    :: Maybe Int
  , containerPixelWidthMd     :: Maybe Int
  , emuOmitGlimpseImageMd     :: Maybe Value
  , emuPatchesOverrideMd      :: Maybe Value
  , lpeKeepPatchIjhwMd        :: Maybe Value
  , lpeDeltaEncodingChannelMd :: Maybe Value
  , sanitizedMd               :: Bool
  , assetPointerLinkMd        :: Maybe Value
  , watermarkedAssetPointerMd :: Maybe Value
  , isNoAuthPlaceholderMd     :: Maybe Value
  } deriving (Show)

data DalleDb = DalleDb
  { genIdDa               :: Maybe Text
  , promptDa              :: Text
  , seedDa               :: Maybe Int
  , parentGenIdDa         :: Maybe Text
  , editOpDa              :: Maybe Text
  , serializationTitleDa  :: Text
  } deriving (Show)

data GenerationDb = GenerationDb
  { genIdGe                 :: Maybe Text
  , genSizeGe               :: Text
  , seedGe                  :: Maybe Int
  , parentGenIdGe           :: Maybe Text
  , heightGe                :: Int
  , widthGe                 :: Int
  , transparentBackgroundGe :: Bool
  , serializationTitleGe    :: Text
  , orientationGe           :: Maybe Text
  } deriving (Show)

data AudioMetadataDb = AudioMetadataDb
  { startTimestampAm      :: Maybe Value
  , endTimestampAm        :: Maybe Value
  , pretokenizedVqAm      :: Maybe Value
  , interruptionsAm       :: Maybe Value
  , originalAudioSourceAm :: Maybe Value
  , transcriptionAm       :: Maybe Value
  , wordTranscriptionAm   :: Maybe Value
  , startStampAm          :: Double
  , endStampAm            :: Double
  } deriving (Show)

-- | Represents 'oai.thoughts'
data ThoughtDb = ThoughtDb
  { summaryTh  :: Text
  , contentTh  :: Text
  , chunksTh   :: Value
  , finishedTh :: Bool
  } deriving (Show)
