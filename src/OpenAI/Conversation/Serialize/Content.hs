{-# LANGUAGE QuasiQuotes #-}

module OpenAI.Conversation.Serialize.Content (
    insertMsgTree, insertContentTree, rewriteMsgTree
  ) where

import Control.Monad (forM_, void)
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific, toRealFloat)
import Data.Text (Text)
import qualified Data.Vector as V

import qualified Data.Aeson as Ae

import Hasql.Statement (Statement)
import qualified Hasql.TH as TH
import qualified Hasql.Transaction as Tx

import qualified OpenAI.Conversation.Json.Schema as Jd
import qualified OpenAI.Conversation.Serialize.ContentStmt as St


insertMsgTree :: Int64 -> Jd.Message -> Tx.Transaction (Either Text Int64)
insertMsgTree uidNode message = do
  uidMsg <- Tx.statement
    ( uidNode
    , message.idMsg
    , Just $ toRealFloat message.createTimeMsg
    , toRealFloat <$> message.updateTimeMsg
    , fromMaybe "<unknown>" message.statusMsg
    , message.endTurnMsg
    , toRealFloat <$> message.weightMsg
    , Ae.toJSON message.metadataMsg
    , fromMaybe "<unknown>" message.recipientMsg
    , message.channelMsg
    , 0
    )
    St.insertMessage

  insertAuthorTree uidMsg message.authorMsg
  contentResult <- insertContentTree uidMsg 0 message.contentMsg

  case contentResult of
    Left issue -> do
      Tx.condemn
      pure $ Left issue
    Right _ ->
      pure $ Right uidMsg


rewriteMsgTree :: Int64 -> Jd.Message -> Tx.Transaction (Either Text ())
rewriteMsgTree uidMsg message = do
  Tx.statement uidMsg deleteAuthorByMsg
  Tx.statement uidMsg deleteContentsByMsg

  Tx.statement
    ( Just $ toRealFloat message.createTimeMsg
    , toRealFloat <$> message.updateTimeMsg
    , fromMaybe "<unknown>" message.statusMsg
    , message.endTurnMsg
    , toRealFloat <$> message.weightMsg
    , Ae.toJSON message.metadataMsg
    , fromMaybe "<unknown>" message.recipientMsg
    , message.channelMsg
    , uidMsg
    )
    updateMessage

  insertAuthorTree uidMsg message.authorMsg
  contentResult <- insertContentTree uidMsg 0 message.contentMsg

  case contentResult of
    Left issue -> do
      Tx.condemn
      pure $ Left issue
    Right _ ->
      pure $ Right ()


insertAuthorTree :: Int64 -> Jd.Author -> Tx.Transaction ()
insertAuthorTree uidMsg author =
  Tx.statement
    ( uidMsg
    , author.roleAu
    , author.nameAu
    , Ae.toJSON author.metadataAu
    )
    St.insertAuthor


insertContentTree :: Int64 -> Int32 -> Jd.Content -> Tx.Transaction (Either Text Int64)
insertContentTree uidMsg seqContent content = do
  uidContent <- Tx.statement
    (uidMsg, contentKind content, seqContent)
    St.insertContent

  case content of
    Jd.CodeCT code ->
      Tx.statement
        (uidContent, code.languageCP, code.responseFormatNameCP, code.textCP)
        St.insertCodeContent

    Jd.ExecutionOutputCT execOut ->
      Tx.statement
        (uidContent, execOut.textEO)
        St.insertExecutionOutputContent

    Jd.MultimodalTextCT mmt ->
      forM_ (zip ([0 ..] :: [Int32]) mmt.partsMmt) $ \(seqPart, part) ->
        insertMultiModalPartTree uidContent seqPart part

    Jd.ModelEditableContextCT modelCtx ->
      Tx.statement
        (uidContent, modelCtx.modelSetMEC, modelCtx.repositoryMEC, modelCtx.repoSummaryMEC, modelCtx.structuredMEC)
        St.insertModelEditableContextContent

    Jd.ReasoningRecapCT recap ->
      Tx.statement
        (uidContent, recap.contentRR)
        St.insertReasoningRecapContent

    Jd.SystemErrorCT sysErr ->
      Tx.statement
        (uidContent, sysErr.nameSER, sysErr.textSER)
        St.insertSystemErrorContent

    Jd.TetherBrowsingDisplayCT tbDisplay ->
      Tx.statement
        (uidContent, tbDisplay.resultTbd, Ae.toJSON <$> tbDisplay.summaryTbd, Ae.toJSON <$> tbDisplay.assetsTbd, tbDisplay.tetherIDTbd)
        St.insertTetherBrowsingDisplayContent

    Jd.TetherQuoteCT tq ->
      Tx.statement
        (uidContent, tq.urlTq, tq.domainTq, tq.textTq, tq.titleTq, tq.tetherIDTq)
        St.insertTetherQuoteContent

    Jd.TextCT text ->
      Tx.statement
        (uidContent, V.fromList text.partsTP)
        St.insertTextContent

    Jd.ThoughtsCT thoughts -> do -- thoughts sourceAnalysisMsgId
      Tx.statement
        (uidContent, thoughts.sourceAnalysisMsgIdTP)
        St.insertThoughtsContent

      forM_ (zip ([0 ..] :: [Int32]) thoughts.thoughtsTP) $ \(seqThought, thought) ->
        insertThoughtTree uidContent seqThought thought

    Jd.OtherCT info ->
      Tx.statement
        (uidContent, Ae.toJSON info.rawOpl)
        St.insertUnknownContent

  pure $ Right uidContent


contentKind :: Jd.Content -> Text
contentKind content =
  case content of
    Jd.CodeCT {} -> "code"
    Jd.ExecutionOutputCT {} -> "execution_output"
    Jd.MultimodalTextCT {} -> "multimodal_text"
    Jd.ModelEditableContextCT {} -> "model_editable_context"
    Jd.ReasoningRecapCT {} -> "reasoning_recap"
    Jd.SystemErrorCT {} -> "system_error"
    Jd.TetherBrowsingDisplayCT {} -> "tether_browsing_display"
    Jd.TetherQuoteCT {} -> "tether_quote"
    Jd.TextCT {} -> "text"
    Jd.ThoughtsCT {} -> "thoughts"
    Jd.OtherCT info -> info.contentTypeOpl


insertThoughtTree :: Int64 -> Int32 -> Jd.ThoughtContent -> Tx.Transaction ()
insertThoughtTree uidContent seqThought thought =
  Tx.statement
    ( uidContent
    , thought.summaryTC
    , thought.contentTC
    , Ae.toJSON thought.chunksTC
    , thought.finishedTC
    , seqThought
    )
    St.insertThought


insertMultiModalPartTree :: Int64 -> Int32 -> Jd.MultiModalPart -> Tx.Transaction ()
insertMultiModalPartTree uidContent seqPart part = do
  uidPart <- Tx.statement
    (uidContent, multiModalPartKind part, seqPart)
    St.insertMultiModalPart

  case part of
    Jd.TextPT text ->
      Tx.statement
        (uidPart, text)
        St.insertTextMMPart

    Jd.AudioTranscriptionPT audioTrans ->
      Tx.statement
        (uidPart, audioTrans.textAtp, audioTrans.directionAtp, audioTrans.decodingIdAtp)
        St.insertAudioTranscriptionMMPart

    Jd.AudioAssetPointerPT audioPtr ->
      void $ insertAudioAssetTree uidPart audioPtr

    Jd.ImageAssetPointerPT imgPtr -> do -- assetPointer sizeBytes width height fovea metadata
      uidPointer <- Tx.statement
        ( uidPart
        , imgPtr.assetPointerPap
        , fromIntegral imgPtr.sizeBytesPap
        , fromIntegral imgPtr.widthPap
        , fromIntegral imgPtr.heightPap
        , imgPtr.foveaPap
        )
        St.insertImageAssetPointerMMPart

      forM_ imgPtr.metadataPap $ insertImageMetadataTree uidPointer

    Jd.RealTimeUserAVPT rtUser -> do -- expiryDatetime framesAssetPointers videoAssetPointer audioPointer audioStartTimestamp
      void $ Tx.statement
        ( uidPart
        , toRealFloat <$> rtUser.expiryDatetimeRtuav
        , Just $ Ae.toJSON rtUser.framesApRtuav
        , rtUser.videoContainerApRtuav
        , toRealFloat <$> rtUser.audioStartTimestampRtuav
        )
        St.insertRealTimeUserAVMMPart

      void $ insertAudioAssetTree uidPart rtUser.audioApRtuav


multiModalPartKind :: Jd.MultiModalPart -> Text
multiModalPartKind part =
  case part of
    Jd.TextPT {} -> "text"
    Jd.AudioTranscriptionPT {} -> "audio_transcription"
    Jd.AudioAssetPointerPT {} -> "audio_asset_pointer"
    Jd.ImageAssetPointerPT {} -> "image_asset_pointer"
    Jd.RealTimeUserAVPT {} -> "real_time_user_av"


insertImageMetadataTree :: Int64 -> Jd.ImageMetadata -> Tx.Transaction ()
insertImageMetadataTree uidPointer metadata = do
  uidMetadata <- Tx.statement
    ( uidPointer
    , metadata.gizmoMd
    , fromIntegral <$> metadata.containerPixelHeightMd
    , fromIntegral <$> metadata.containerPixelWidthMd
    , metadata.emuOmitGlimpseImageMd
    , metadata.emuPatchesOverrideMd
    , metadata.lpeKeepPatchIjhwMd
    , metadata.lpeDeltaEncodingChannelMd
    , metadata.sanitizedMd
    , metadata.assetPointerLinkMd
    , metadata.watermarkedAssetPointerMd
    , metadata.isNoAuthPlaceholderMd
    )
    St.insertImageMetadata

  forM_ metadata.dalleMd $ insertDalleTree uidMetadata
  forM_ metadata.generationMd $ insertGenerationTree uidMetadata


insertDalleTree :: Int64 -> Jd.Dalle -> Tx.Transaction ()
insertDalleTree uidMetadata dalle =
  Tx.statement
    ( uidMetadata
    , dalle.genIdDa
    , dalle.promptDa
    , fromIntegral <$> dalle.seedDa
    , dalle.parentGenIdDa
    , dalle.editOpDa
    , dalle.serializationTitleDa
    )
    St.insertDalle


insertGenerationTree :: Int64 -> Jd.Generation -> Tx.Transaction ()
insertGenerationTree uidMetadata generation =
  Tx.statement
    ( uidMetadata
    , generation.genIdGe
    , generation.genSizeGe
    , fromIntegral <$> generation.seedGe
    , generation.parentGenIdGe
    , fromIntegral generation.heightGe
    , fromIntegral generation.widthGe
    , generation.transparentBackgroundGe
    , generation.serializationTitleGe
    , generation.orientationGe
    )
    St.insertGeneration


insertAudioAssetTree :: Int64 -> Jd.AudioAssetPointer -> Tx.Transaction Int64
insertAudioAssetTree uidPart pointer = do
  uidPointer <- Tx.statement
    ( uidPart
    , toRealFloat <$> pointer.expiryDatetimeAap
    , pointer.assetPointerAap
    , fromIntegral pointer.sizeBytesAap
    , pointer.formatAap
    , pointer.toolAudioDirectionAap
    )
    St.insertAudioAssetPointerMMPart

  forM_ pointer.metadataAap $ insertAudioMetadataTree uidPointer
  pure uidPointer


insertAudioMetadataTree :: Int64 -> Jd.AudioMetadata -> Tx.Transaction ()
insertAudioMetadataTree uidPointer metadata =
  Tx.statement
    ( uidPointer
    , 0
    , toRealFloat <$> metadata.startTimestampAm
    , toRealFloat <$> metadata.endTimestampAm
    , metadata.pretokenizedVqAm
    , metadata.interruptionsAm
    , metadata.originalAudioSourceAm
    , metadata.transcriptionAm
    , metadata.wordTranscriptionAm
    , toRealFloat metadata.startAm
    , toRealFloat metadata.endAm
    )
    St.insertAudioMetadata


updateMessage :: Statement
      ( Maybe Double, Maybe Double, Text, Maybe Bool, Maybe Double, Ae.Value
      , Text, Maybe Text, Int64
      )
      ()
updateMessage =
  [TH.resultlessStatement|
    update oai.messages
    set create_time = $1 :: float8?,
        update_time = $2 :: float8?,
        status = $3 :: text,
        end_turn = $4 :: bool?,
        weight = $5 :: float8?,
        metadata = $6 :: jsonb,
        recipient = $7 :: text,
        channel = $8 :: text?
    where uid = $9 :: int8
  |]


deleteAuthorByMsg :: Statement Int64 ()
deleteAuthorByMsg =
  [TH.resultlessStatement|
    delete from oai.authors
    where message_fk = $1 :: int8
  |]


-- Content subtype tables are expected to reference oai.contents with
-- ON DELETE CASCADE. This keeps rewrite logic independent of every
-- specialised content table.
deleteContentsByMsg :: Statement Int64 ()
deleteContentsByMsg =
  [TH.resultlessStatement|
    delete from oai.contents
    where message_fk = $1 :: int8
  |]