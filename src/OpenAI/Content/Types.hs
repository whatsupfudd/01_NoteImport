{-# LANGUAGE DerivingStrategies #-}

module OpenAI.Content.Types where

import Data.Int (Int32, Int64)
import Data.Scientific (Scientific)
import Data.Text (Text)

import qualified Data.Aeson as Ae


data KindC
  = CodeKC
  | ExecOutKC
  | ModelCtxKC
  | MultiTextKC
  | ReasoningKC
  | SystemErrKC
  | TetherBrowseKC
  | TetherQuoteKC
  | TextKC
  | ThoughtsKC
  | OtherKC Text
  deriving stock (Eq, Ord, Show)


data Payload
  = CodePL {
      langCode :: Text
      , formatRef :: Maybe Text
      , textCode :: Text
    }
  | ExecOutPL {
      textOut :: Text
    }
  | ModelCtxPL {
      modelSlug :: Text
      , repoJson :: Maybe Ae.Value
      , rsJson :: Maybe Ae.Value
      , scJson :: Maybe Ae.Value
    }
  | MultiPL {
      parts :: [PartPL]
    }
  | ReasoningPL {
      textReasoning :: Text
    }
  | SystemErrPL {
      nameErr :: Text
      , textErr :: Text
    }
  | TetherBrowsePL {
      resultsJson :: Text
      , summaryJson :: Maybe Ae.Value
      , assetsJson :: Maybe Ae.Value
      , tetherId :: Maybe Text
    }
  | TetherQuotePL {
      urlQuote :: Text
      , domainQuote :: Text
      , textQuote :: Text
      , titleQuote :: Text
      , tetherId :: Maybe Text
    }
  | TextPL {
      partsText :: [Text]
    }
  | ThoughtsPL {
      sourceId :: Text
      , thoughts :: [ThoughtRow]
    }
  | OtherPL {
      kindOther :: Text
      , rawOther :: Ae.Value
    }
  deriving stock (Eq, Show)


data PartPL
  = TextPP {
      textPart :: Text
    }
  | AudioTransPP {
      textAudio :: Text
      , direction :: Text
      , decodingId :: Maybe Text
    }
  | AudioAssetPP {
      ptrAudio :: AudioPtr
    }
  | ImageAssetPP {
      ptrImage :: ImagePtr
    }
  | RealtimeAvPP {
      ptrAv :: AvPtr
    }
  deriving stock (Eq, Show)


data AudioPtr = AudioPtr {
    expiryAudio :: Maybe Scientific
    , assetAudio :: Text
    , sizeAudio :: Int64
    , formatAudio :: Text
    , directionAudio :: Maybe Text
    , metadataAudio :: Maybe AudioMeta
  }
  deriving stock (Eq, Show)


data ImagePtr = ImagePtr {
    assetImage :: Text
    , sizeImage :: Int64
    , widthImage :: Int32
    , heightImage :: Int32
    , foveaImage :: Maybe Ae.Value
    , metadataImage :: Maybe ImageMeta
  }
  deriving stock (Eq, Show)


data AvPtr = AvPtr {
    expiryAv :: Maybe Scientific
    , framesAv :: [Ae.Value]
    , videoAv :: Maybe Ae.Value
    , audioAv :: AudioPtr
    , startTimestampAv :: Maybe Scientific
  }
  deriving stock (Eq, Show)


data ImageMeta = ImageMeta {
    dalleImage :: Maybe DalleMeta
    , gizmoImage :: Maybe Ae.Value
    , generationImage :: Maybe GenMeta
    , containerHeightImage :: Maybe Int32
    , containerWidthImage :: Maybe Int32
    , omitGlimpseImage :: Maybe Ae.Value
    , patchesOverrideImage :: Maybe Ae.Value
    , keepPatchIjhwImage :: Maybe Ae.Value
    , deltaEncodingChannelImage :: Maybe Ae.Value
    , sanitizedImage :: Bool
    , assetLinkImage :: Maybe Ae.Value
    , watermarkedImage :: Maybe Ae.Value
    , noAuthPlaceholderImage :: Maybe Ae.Value
  }
  deriving stock (Eq, Show)


data AudioMeta = AudioMeta {
    startTimestampAudio :: Maybe Scientific
    , endTimestampAudio :: Maybe Scientific
    , pretokenizedVqAudio :: Maybe Ae.Value
    , interruptionsAudio :: Maybe Ae.Value
    , originalSourceAudio :: Maybe Ae.Value
    , transcriptionAudio :: Maybe Ae.Value
    , wordTranscriptionAudio :: Maybe Ae.Value
    , startAudio :: Scientific
    , endAudio :: Scientific
  }
  deriving stock (Eq, Show)


data DalleMeta = DalleMeta {
    idDalle :: Maybe Text
    , promptDalle :: Text
    , seedDalle :: Maybe Int64
    , parentIdDalle :: Maybe Text
    , editOpDalle :: Maybe Text
    , titleDalle :: Text
  }
  deriving stock (Eq, Show)


data GenMeta = GenMeta {
    idGen :: Maybe Text
    , sizeGen :: Text
    , seedGen :: Maybe Int64
    , parentIdGen :: Maybe Text
    , heightGen :: Int32
    , widthGen :: Int32
    , transparentGen :: Bool
    , titleGen :: Text
    , orientationGen :: Maybe Text
  }
  deriving stock (Eq, Show)


data ThoughtRow = ThoughtRow {
    summaryThought :: Text
    , contentThought :: Text
    , chunksThought :: Ae.Value
    , finishedThought :: Bool
  }
  deriving stock (Eq, Show)


data IssueC
  = UnknownKindIC Text
  | MissingRowIC Text
  | BadPayloadIC Text
  | PartialMultiIC Text
  | UnsupportedIC Text
  deriving stock (Eq, Show)


data ResultW = ResultW {
    uidMsg :: Int64
    , statW :: StatW
    , notesW :: [Text]
  }
  deriving stock (Eq, Show)


data StatW = StatW {
    msgCnt :: Int
    , authorCnt :: Int
    , contentCnt :: Int
    , partCnt :: Int
    , metaCnt :: Int
    , unknownCnt :: Int
  }
  deriving stock (Eq, Show)


emptyStatW :: StatW
emptyStatW =
  StatW {
      msgCnt = 0
      , authorCnt = 0
      , contentCnt = 0
      , partCnt = 0
      , metaCnt = 0
      , unknownCnt = 0
    }