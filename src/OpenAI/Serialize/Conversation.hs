{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}
{-# HLINT ignore "Use forM_" #-}
module OpenAI.Serialize.Conversation where

import Control.Monad (when, forM_, forM, void)
import Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString.Lazy as Bs
import Data.Int (Int64, Int32)
import qualified Data.List as L
import qualified Data.Map.Strict as Mp
import qualified Data.HashMap.Strict as HM
import Data.Maybe (isJust, fromJust, fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import qualified Data.Vector as V
import Data.Vector (Vector)

import qualified Data.Aeson as Ae


import qualified Hasql.Pool as Hp
import qualified Hasql.Session as Ses
import qualified OpenAI.Serialize.ConversationStmt as St
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as Tx

import OpenAI.Json.Reader
import OpenAI.Types
import OpenAI.Parse (analyzeDiscussion, findRootNode)
import Data.List (find)

-- | useTx: helper to wrap a statement into a transaction.
useTx :: Hp.Pool -> Tx.Transaction tr -> IO (Either Hp.UsageError tr)
useTx pool stmts = Hp.use pool (Tx.transaction Tx.ReadCommitted Tx.Write stmts)   -- 

addConversation :: Hp.Pool -> Conversation -> IO (Either Hp.UsageError (Either String Int64))
addConversation pool conversation = do
  -- liftIO $ putStrLn $ "@[addConversation] adding conversation: " <> show conversation.titleCv
  useTx pool $ do
    conversationID <- addConversationRoot conversation
    let
      mbRootNode = findRootNode conversation.mappingCv
    case mbRootNode of
      Nothing -> pure . Left $ "@[addConversation] no root node found"
      Just rootNode -> do
        eiTreeRez <- addNodeTreeSession conversationID conversation.mappingCv rootNode.idNd Nothing 0
        case eiTreeRez of
          Left err -> pure . Left $ err <> ": " <> T.unpack ("Title: " <> conversation.titleCv <> ", id: " <> conversation.convIdCv) <> "\n" <> show conversation
          Right () -> pure . Right $ conversationID


addConversationRoot :: Conversation -> Tx.Transaction Int64
addConversationRoot conversation =
  Tx.statement (conversation.titleCv, conversation.convIdCv, conversation.createTimeCv
        , conversation.updateTimeCv) St.insertConversation

addNode :: Int64 -> Maybe Int64 -> Text -> Node -> Int32 -> Tx.Transaction Int64
addNode discussionID mbParentID node_id node seqNbr = do
  nuid <- Tx.statement (discussionID, node_id, mbParentID, seqNbr) St.insertNodeStmt
  when (isJust node.messageNd) $
    addMessage nuid (fromJust node.messageNd) 0
  pure nuid

addMessage :: Int64 -> Message -> Int32 -> Tx.Transaction ()
addMessage node_uid msg seqNbr = do
  muid <- Tx.statement
    (node_uid, msg.idMsg, msg.createTimeMsg, msg.updateTimeMsg, msg.statusMsg
    , msg.endTurnMsg, msg.weightMsg, Ae.toJSON (HM.fromList $ Mp.toList msg.metadataMsg)
    , msg.recipientMsg, msg.channelMsg
    , seqNbr
    ) St.insertMessageStmt
  addAuthor muid msg.authorMsg
  addContent muid msg.contentMsg 0


addAuthor :: Int64 -> Author -> Tx.Transaction ()
addAuthor msg_uid au = Tx.statement (msg_uid, au.roleAu, au.nameAu
          , Ae.toJSON (HM.fromList $ Mp.toList au.metadataAu)) St.insertAuthorStmt


addContent :: Int64 -> Content -> Int32 -> Tx.Transaction ()
addContent msg_uid content seqNbr = do
  cuid <- Tx.statement (msg_uid, contentType content, seqNbr) St.insertContentStmt
  case content of
    CodeCT lang rfn txt -> addCodeContentSession cuid lang rfn txt
    ExecutionOutputCT txt -> addExecutionOutputContentSession cuid txt
    ModelEditableContextCT msc rep rs sc -> addModelEditableContextSession cuid msc rep rs sc
    MultimodalTextCT parts -> forM_ (zip parts [0..]) $ uncurry (addMultiModalPartSession cuid)
    ReasoningRecapCT cnt -> addReasoningRecapContentSession cuid cnt
    SystemErrorCT name txt -> addSystemErrorContentSession cuid name txt
    TetherBrowsingDisplayCT results summary assets tetherID -> addTetherBrowsingDisplayContentSession cuid results summary assets tetherID
    TetherQuoteCT url domain text title tetherID -> addTetherQuoteContentSession cuid url domain text title tetherID
    TextCT parts -> addTextContentSession cuid parts
    ThoughtsCT ths sam -> addThoughtsContentSession cuid ths sam
    OtherCT _ _ -> return ()  -- Skip


contentType :: Content -> Text
contentType (CodeCT {}) = "code"
contentType (ExecutionOutputCT {}) = "execution_output"
contentType (ModelEditableContextCT {}) = "model_editable_context"
contentType (MultimodalTextCT {}) = "multimodal_text"
contentType (ReasoningRecapCT {}) = "reasoning_recap"
contentType (SystemErrorCT {}) = "system_error"
contentType (TetherBrowsingDisplayCT {}) = "tether_browsing_display"
contentType (TetherQuoteCT {}) = "tether_quote"
contentType (TextCT {}) = "text"
contentType (ThoughtsCT {}) = "thoughts"
contentType (OtherCT ct _) = ct


addCodeContentSession :: Int64 -> Text -> Maybe Text -> Text -> Tx.Transaction ()
addCodeContentSession cuid lang rfn txt = Tx.statement (cuid, lang, rfn, txt) St.insertCodeContentStmt

addExecutionOutputContentSession :: Int64 -> Text -> Tx.Transaction ()
addExecutionOutputContentSession cuid txt = Tx.statement (cuid, txt) St.insertExecutionOutputContentStmt


addModelEditableContextSession :: Int64 -> Text -> Maybe Ae.Value -> Maybe Ae.Value -> Maybe Ae.Value -> Tx.Transaction ()
addModelEditableContextSession cuid msc rep rs sc = Tx.statement
    (cuid, msc, fmap Ae.toJSON rep, fmap Ae.toJSON rs, fmap Ae.toJSON sc) St.insertModelEditableContextStmt

addReasoningRecapContentSession :: Int64 -> Text -> Tx.Transaction ()
addReasoningRecapContentSession cuid cnt = Tx.statement (cuid, cnt) St.insertReasoningRecapContentStmt

addSystemErrorContentSession :: Int64 -> Text -> Text -> Tx.Transaction ()
addSystemErrorContentSession cuid name txt = Tx.statement (cuid, name, txt) St.insertSystemErrorContentStmt

addTetherBrowsingDisplayContentSession :: Int64 -> Text -> Maybe Text -> Maybe [Ae.Value] -> Maybe Text -> Tx.Transaction ()
addTetherBrowsingDisplayContentSession cuid results summary assets tetherID = Tx.statement (cuid, results, fmap Ae.toJSON summary, fmap Ae.toJSON assets, tetherID) St.insertTetherBrowsingDisplayContentStmt

addTetherQuoteContentSession :: Int64 -> Text -> Text -> Text -> Text -> Maybe Text -> Tx.Transaction ()
addTetherQuoteContentSession cuid url domain text title tetherID = Tx.statement (cuid, url, domain, text, title, tetherID) St.insertTetherQuoteContentStmt

addTextContentSession :: Int64 -> [Text] -> Tx.Transaction ()
addTextContentSession cuid parts = Tx.statement (cuid, V.fromList parts) St.insertTextContentStmt

addThoughtsContentSession :: Int64 -> [Thought] -> Text -> Tx.Transaction ()
addThoughtsContentSession cuid ths sam = do
  Tx.statement (cuid, sam) St.insertThoughtsContentStmt
  forM_ (zip ths [0..]) $ uncurry (addThought cuid)

addThought :: Int64 -> Thought -> Int32 -> Tx.Transaction ()
addThought tc_uid th seqNbr = Tx.statement
    (tc_uid, th.summaryTh, th.contentTh, Ae.toJSON th.chunksTh, fromMaybe False th.finishedTh, seqNbr) St.insertThoughtStmt


-- Recursive node tree insertion
addNodeTreeSession :: Int64 -> Mp.Map Text Node -> Text -> Maybe Int64 -> Int32 -> Tx.Transaction (Either String ())
addNodeTreeSession discussionID mapping nodeEid mbParentID seqNbr =
  let
    mbNode =  Mp.lookup nodeEid mapping
  in do
  case mbNode of
    Just aNode -> do
      nuid <- addNode discussionID mbParentID nodeEid aNode seqNbr
      forM_ aNode.childrenNd $ \child_id ->
        addNodeTreeSession discussionID mapping child_id (Just nuid) (seqNbr + 1)
      pure $ Right ()
    Nothing -> pure . Left $ "@[addNodeTreeSession] node not found: " <> show nodeEid


-- New stuff for multimodal text content:

{-
assetPointerPap :: String,
    sizeBytesPap :: Int,
    widthPap :: Int,
    heightPap :: Int,
    foveaPap :: Maybe Value,
    metadataPap :: Metadata
-}
addMultiModalPartSession :: Int64 -> MultiModalPart -> Int32 -> Tx.Transaction ()
addMultiModalPartSession contentID part seqNbr = do
  partID <- Tx.statement (contentID, mmPartContentType part, seqNbr) St.insertMultiModalPartStmt
  case part of
    AudioTranscriptionPT text direction decodingId ->
      Tx.statement (partID, text, direction, decodingId) St.insertAudioTranscriptionMMPartStmt
    AudioAssetPointerPT value -> do
      assetPtrID <- Tx.statement (partID, value.expiryDatetimeAap, value.assetPointerAap, fromIntegral value.sizeBytesAap, value.formatAap, value.toolAudioDirectionAap) St.insertAudioAssetPointerMMPartStmt
      case value.metadataAap of
        Just metadata -> addAudioMetadata 1 assetPtrID metadata
        Nothing -> return ()
    TextPT text -> Tx.statement (partID, text) St.insertTextMMPartStmt
    ImageAssetPointerPT assetPointer sizeBytes width height fovea metadata -> do
      imgPartID <- Tx.statement (
            partID, assetPointer, fromIntegral sizeBytes, fromIntegral width, fromIntegral height
            , Ae.toJSON <$> fovea
          ) St.insertImageAssetPointerMMPartStmt
      case metadata of
        Just metadata -> addImageMetadataSession imgPartID metadata
        Nothing -> return ()
    RealTimeUserAVPT expiryDatetime framesAssetPointers videoContainerAssetPointer audioAssetPointer audioStartTimestamp ->
      let
        framePtrValues = if null framesAssetPointers then Nothing else Just (Ae.toJSON framesAssetPointers)
      in do
      assetPtrID <- Tx.statement (
          partID, expiryDatetime, framePtrValues
          , videoContainerAssetPointer
          , audioStartTimestamp
        ) St.insertRealTimeUserAVMMPartStmt
      case audioAssetPointer.metadataAap of
        Just metadata -> addAudioMetadata 2 assetPtrID metadata
        Nothing -> return ()


mmPartContentType :: MultiModalPart -> Text
mmPartContentType (AudioTranscriptionPT {}) = "audio_transcription"
mmPartContentType (AudioAssetPointerPT {}) = "audio_asset_pointer"
mmPartContentType (ImageAssetPointerPT {}) = "image_asset_pointer"
mmPartContentType (RealTimeUserAVPT {}) = "real_time_user_audio_video_asset_pointer"
mmPartContentType (TextPT {}) = "text"


addImageMetadataSession :: Int64 -> ImageMetadata -> Tx.Transaction ()
addImageMetadataSession imagePtrID md = do
  metaID <- Tx.statement (imagePtrID, Ae.toJSON <$> md.gizmoMd, fromIntegral <$> md.containerPixelHeightMd
      , fromIntegral <$> md.containerPixelWidthMd, Ae.toJSON <$> md.emuOmitGlimpseImageMd
      , Ae.toJSON <$> md.emuPatchesOverrideMd, Ae.toJSON <$> md.lpeKeepPatchIjhwMd
      , Ae.toJSON <$> md.lpeDeltaEncodingChannelMd, md.sanitizedMd, Ae.toJSON <$> md.assetPointerLinkMd
      , Ae.toJSON <$> md.watermarkedAssetPointerMd, Ae.toJSON <$> md.isNoAuthPlaceholderMd) St.insertImageMetadataStmt

  case md.dalleMd of
    Just aDalle -> addDalle metaID aDalle
    Nothing -> pure ()
  case md.generationMd of
    Just aGeneration -> addGeneration metaID aGeneration
    Nothing -> pure ()


addDalle :: Int64 -> Dalle -> Tx.Transaction ()
addDalle metaID d =
  Tx.statement (metaID, d.genIdDa, d.promptDa, fromIntegral <$> d.seedDa
      , d.parentGenIdDa, d.editOpDa, d.serializationTitleDa) St.insertDalleStmt


addGeneration :: Int64 -> Generation -> Tx.Transaction ()
addGeneration metaID g =
  Tx.statement (metaID, g.genIdGe, g.genSizeGe, fromIntegral <$> g.seedGe, g.parentGenIdGe
        , fromIntegral g.heightGe, fromIntegral g.widthGe, g.transparentBackgroundGe
        , g.serializationTitleGe, g.orientationGe) St.insertGenerationStmt


addAudioMetadata :: Int32 -> Int64 -> AudioMetadata -> Tx.Transaction ()
addAudioMetadata itemKind itemID md = do
  Tx.statement (
      itemID, itemKind, Ae.toJSON <$> md.startTimestampAm, Ae.toJSON <$> md.endTimestampAm
      , Ae.toJSON <$> md.pretokenizedVqAm, Ae.toJSON <$> md.interruptionsAm
      , Ae.toJSON <$> md.originalAudioSourceAm, Ae.toJSON <$> md.transcriptionAm
      , Ae.toJSON <$> md.wordTranscriptionAm, md.startAm, md.endAm
    ) St.insertAudioMetadataStmt

