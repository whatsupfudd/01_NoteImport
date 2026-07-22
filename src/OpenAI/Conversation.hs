{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module OpenAI.Conversation where

import Data.Aeson (Value)
import Data.Int (Int32, Int64)
import qualified Data.List  as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Mp
import Data.Maybe (isJust, isNothing, listToMaybe)
import Data.Text (Text)
import qualified Data.Vector as V

import OpenAI.Order (NodeOrd(..))


-- | Root representation of a raw OpenAI conversation as stored in 'oai.conversations'.
data ConversationDb = ConversationDb {
    uidCv :: Int64
  , titleCv :: Text
  , eidCv :: Text
  , createTimeCv :: Double
  , updateTimeCv :: Double
  , nodesCv :: Map Text NodeDb
  } deriving (Eq, Show)


-- | Represents 'oai.nodes'.
data NodeDb = NodeDb {
    uidNd :: Int64
  , eidNd :: Text
  , parentFkNd :: Maybe Int64
  , seqNodeNd :: Int32
  , seqChildNd :: Int32
  , seqPreNd :: Int32
  , messageNd :: Maybe MessageDb
  } deriving (Eq, Show)


-- | Represents 'oai.messages'.
data MessageDb = MessageDb {
    uidMsg :: Int64
  , eidMsg :: Text
  , createTimeMsg :: Maybe Double
  , updateTimeMsg :: Maybe Double
  , statusMsg :: Text
  , endTurnMsg :: Maybe Bool
  , weightMsg :: Double
  , metadataMsg :: Value
  , recipientMsg :: Text
  , channelMsg :: Maybe Text
  , authorMsg :: AuthorDb
  , contentsMsg :: V.Vector ContentDb
  } deriving (Eq, Show)


-- | Represents 'oai.authors'.
data AuthorDb = AuthorDb {
    uidAu :: Int64
  , roleAu :: Text
  , nameAu :: Maybe Text
  , metadataAu :: Value
  } deriving (Eq, Show)


-- | Sum type representing the various joined content tables linked via 'oai.contents'.
data ContentDb
  = CodeCT_Db
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
  deriving (Eq, Show)


-- | Represents 'oai.multimodal_parts' and its specialized child tables.
data MultiModalPartDb
  = TextPT_Db Text
  | AudioTranscriptionPT_Db AudioTranscriptionDb
  | AudioAssetPointerPT_Db AudioAssetPointerDb
  | ImageAssetPointerPT_Db ImageAssetPointerDb
  | RealTimeUserAVPT_Db RealTimeUserAVDb
  deriving (Eq, Show)


data AudioTranscriptionDb = AudioTranscriptionDb {
    textAtp :: Text
  , directionAtp :: Text
  , decodingIdAtp :: Maybe Text
  } deriving (Eq, Show)


data ImageAssetPointerDb = ImageAssetPointerDb {
    assetPointerIap :: Text
  , sizeBytesIap :: Int64
  , widthIap :: Int32
  , heightIap :: Int32
  , foveaIap :: Maybe Value
  , metadataIap :: Maybe ImageMetadataDb
  } deriving (Eq, Show)


data RealTimeUserAVDb = RealTimeUserAVDb {
    expiryDatetimeRtuav :: Maybe Value
  , framesAssetPointersRtuav :: Value
  , videoContainerAssetPointerRtuav :: Maybe Value
  , audioStartTimestampRtuav :: Maybe Double
  } deriving (Eq, Show)


-- | Represents 'oai.audio_asset_pointer_mmpart' and 'oai.metadatas_audioasset'.
data AudioAssetPointerDb = AudioAssetPointerDb {
    expiryDatetimeAap :: Maybe Value
  , assetPointerAap :: Text
  , sizeBytesAap :: Int64
  , formatAap :: Text
  , toolAudioDirectionAap :: Maybe Text
  , metadataAap :: Maybe AudioMetadataDb
  } deriving (Eq, Show)


-- | Represents 'oai.metadatas_imgasset' and its relations ('dalles', 'generations').
data ImageMetadataDb = ImageMetadataDb {
    dalleMd :: Maybe DalleDb
  , gizmoMd :: Maybe Value
  , generationMd :: Maybe GenerationDb
  , containerPixelHeightMd :: Maybe Int
  , containerPixelWidthMd :: Maybe Int
  , emuOmitGlimpseImageMd :: Maybe Value
  , emuPatchesOverrideMd :: Maybe Value
  , lpeKeepPatchIjhwMd :: Maybe Value
  , lpeDeltaEncodingChannelMd :: Maybe Value
  , sanitizedMd :: Bool
  , assetPointerLinkMd :: Maybe Value
  , watermarkedAssetPointerMd :: Maybe Value
  , isNoAuthPlaceholderMd :: Maybe Value
  } deriving (Eq, Show)


data DalleDb = DalleDb {
    genIdDa :: Maybe Text
  , promptDa :: Text
  , seedDa :: Maybe Int
  , parentGenIdDa :: Maybe Text
  , editOpDa :: Maybe Text
  , serializationTitleDa :: Text
  } deriving (Eq, Show)


data GenerationDb = GenerationDb {
    genIdGe :: Maybe Text
  , genSizeGe :: Text
  , seedGe :: Maybe Int
  , parentGenIdGe :: Maybe Text
  , heightGe :: Int
  , widthGe :: Int
  , transparentBackgroundGe :: Bool
  , serializationTitleGe :: Text
  , orientationGe :: Maybe Text
  } deriving (Eq, Show)


data AudioMetadataDb = AudioMetadataDb {
    startTimestampAm :: Maybe Value
  , endTimestampAm :: Maybe Value
  , pretokenizedVqAm :: Maybe Value
  , interruptionsAm :: Maybe Value
  , originalAudioSourceAm :: Maybe Value
  , transcriptionAm :: Maybe Value
  , wordTranscriptionAm :: Maybe Value
  , startStampAm :: Double
  , endStampAm :: Double
  } deriving (Eq, Show)


-- | Represents 'oai.thoughts'.
data ThoughtDb = ThoughtDb {
    summaryTh :: Text
  , contentTh :: Text
  , chunksTh :: Value
  , finishedTh :: Bool
  } deriving (Eq, Show)


-- | Normalise ordering fields for compatibility with partially backfilled rows.
--   The intended steady-state is that all order columns are present and non-negative.
normaliseNodeDb :: NodeDb -> NodeDb
normaliseNodeDb node =
  let seed = firstSeq [node.seqPreNd, node.seqNodeNd, node.seqChildNd]
      seqNode' = pickSeq seed node.seqNodeNd
      seqChild' = pickSeq seqNode' node.seqChildNd
      seqPre' = pickSeq seqNode' node.seqPreNd
  in node
      { seqNodeNd = seqNode'
      , seqChildNd = seqChild'
      , seqPreNd = seqPre'
      }


rootNode :: ConversationDb -> Maybe NodeDb
rootNode conv =
  let roots = filter (isNothing . parentFkNd) (nodesAsc conv)
      rootSentinel = L.find (\node -> eidNd node == "client-created-root") roots
  in case rootSentinel of
      Just node -> Just node
      Nothing -> listToMaybe roots


childMap :: ConversationDb -> Map Text [Text]
childMap conv =
  let
    nodes = fmap normaliseNodeDb (Mp.elems conv.nodesCv)
    eidByUid = Mp.fromList [(uidNd node, eidNd node) | node <- nodes]
    nodesWithParent =
      L.sortOn
        (\node -> (parentFkNd node, seqChildNd node, seqPreNd node, uidNd node))
        (filter (isJust . parentFkNd) nodes)
    addChild :: Map Text [Text] -> NodeDb -> Map Text [Text]
    addChild acc node =
      case node.parentFkNd >>= (`Mp.lookup` eidByUid) of
        Nothing -> acc
        Just eidParent -> Mp.insertWith (flip (++)) eidParent [node.eidNd] acc
  in
  L.foldl' addChild Mp.empty nodesWithParent


nodeSeqs :: ConversationDb -> V.Vector NodeOrd
nodeSeqs conv =
  let
    nodes = nodesAsc conv
    eidByUid = Mp.fromList [(uidNd node, eidNd node) | node <- nodes]
    toOrd :: NodeDb -> NodeOrd
    toOrd node =
      NodeOrd
        { eidNode = node.eidNd
        , eidParent = node.parentFkNd >>= (`Mp.lookup` eidByUid)
        , seqNode = node.seqNodeNd
        , seqChild = node.seqChildNd
        , seqPre = node.seqPreNd
        }
  in
  V.fromList (map toOrd nodes)


nodesAsc :: ConversationDb -> [NodeDb]
nodesAsc conv =
  L.sortOn (\node -> (node.seqPreNd, node.seqNodeNd, node.uidNd)) $
    fmap normaliseNodeDb (Mp.elems conv.nodesCv)

pickSeq :: Int32 -> Int32 -> Int32
pickSeq fallback seqV
  | seqV >= 0 = seqV
  | otherwise = fallback

firstSeq :: [Int32] -> Int32
firstSeq seqs =
  case L.find (>= 0) seqs of
    Just seqV -> seqV
    Nothing -> 0