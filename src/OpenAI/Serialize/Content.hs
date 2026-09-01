{-# LANGUAGE QuasiQuotes #-}

module OpenAI.Serialize.Content (
    insertMsgTree, insertContentTree, rewriteMsgTree
  ) where

import Control.Monad (forM_, void)
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Aeson as Ae
import qualified Data.Vector as V
import Hasql.Statement (Statement)
import qualified Hasql.TH as TH
import qualified Hasql.Transaction as Tx

import qualified OpenAI.Json.Reader as Jd
import qualified OpenAI.Serialize.ContentStmt as St


insertMsgTree :: Int64 -> Jd.Message -> Tx.Transaction (Either Text Int64)
insertMsgTree uidNode message = do
  uidMsg <- Tx.statement
    ( uidNode
    , message.idMsg
    , message.createTimeMsg
    , message.updateTimeMsg
    , message.statusMsg
    , message.endTurnMsg
    , message.weightMsg
    , Ae.toJSON message.metadataMsg
    , message.recipientMsg
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
    ( message.createTimeMsg
    , message.updateTimeMsg
    , message.statusMsg
    , message.endTurnMsg
    , message.weightMsg
    , Ae.toJSON message.metadataMsg
    , message.recipientMsg
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
    Jd.CodeCT language formatName codeText ->
      Tx.statement
        (uidContent, language, formatName, codeText)
        St.insertCodeContent

    Jd.ExecutionOutputCT outputText ->
      Tx.statement
        (uidContent, outputText)
        St.insertExecutionOutputContent

    Jd.MultimodalTextCT parts ->
      forM_ (zip ([0 ..] :: [Int32]) parts) $ \(seqPart, part) ->
        insertMultiModalPartTree uidContent seqPart part

    Jd.ModelEditableContextCT modelSetContext repository repoSummary structuredContext ->
      Tx.statement
        (uidContent, modelSetContext, repository, repoSummary, structuredContext)
        St.insertModelEditableContextContent

    Jd.ReasoningRecapCT reasoningText ->
      Tx.statement
        (uidContent, reasoningText)
        St.insertReasoningRecapContent

    Jd.SystemErrorCT errorName errorText ->
      Tx.statement
        (uidContent, errorName, errorText)
        St.insertSystemErrorContent

    Jd.TetherBrowsingDisplayCT result summary assets tetherId ->
      Tx.statement
        (uidContent, result, Ae.toJSON <$> summary, Ae.toJSON <$> assets, tetherId)
        St.insertTetherBrowsingDisplayContent

    Jd.TetherQuoteCT url domain quoteText title tetherId ->
      Tx.statement
        (uidContent, url, domain, quoteText, title, tetherId)
        St.insertTetherQuoteContent

    Jd.TextCT parts ->
      Tx.statement
        (uidContent, V.fromList parts)
        St.insertTextContent

    Jd.ThoughtsCT thoughts sourceAnalysisMsgId -> do
      Tx.statement
        (uidContent, sourceAnalysisMsgId)
        St.insertThoughtsContent

      forM_ (zip ([0 ..] :: [Int32]) thoughts) $ \(seqThought, thought) ->
        insertThoughtTree uidContent seqThought thought

    Jd.OtherCT kind raw ->
      Tx.statement
        (uidContent, Ae.toJSON raw)
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
    Jd.OtherCT kind _ -> kind


insertThoughtTree :: Int64 -> Int32 -> Jd.Thought -> Tx.Transaction ()
insertThoughtTree uidContent seqThought thought =
  Tx.statement
    ( uidContent
    , thought.summaryTh
    , thought.contentTh
    , Ae.toJSON thought.chunksTh
    , fromMaybe False thought.finishedTh
    , seqThought
    )
    St.insertThought


insertMultiModalPartTree :: Int64 -> Int32 -> Jd.MultiModalPart -> Tx.Transaction ()
insertMultiModalPartTree uidContent seqPart part = do
  uidPart <- Tx.statement
    (uidContent, multiModalPartKind part, seqPart)
    St.insertMultiModalPart

  case part of
    Jd.TextPT partText ->
      Tx.statement
        (uidPart, partText)
        St.insertTextMMPart

    Jd.AudioTranscriptionPT partText direction decodingId ->
      Tx.statement
        (uidPart, partText, direction, decodingId)
        St.insertAudioTranscriptionMMPart

    Jd.AudioAssetPointerPT pointer ->
      void $ insertAudioAssetTree uidPart pointer

    Jd.ImageAssetPointerPT assetPointer sizeBytes width height fovea metadata -> do
      uidPointer <- Tx.statement
        ( uidPart
        , assetPointer
        , fromIntegral sizeBytes
        , fromIntegral width
        , fromIntegral height
        , fovea
        )
        St.insertImageAssetPointerMMPart

      forM_ metadata $ insertImageMetadataTree uidPointer

    Jd.RealTimeUserAVPT expiryDatetime framesAssetPointers videoAssetPointer audioPointer audioStartTimestamp -> do
      void $ Tx.statement
        ( uidPart
        , expiryDatetime
        , Just $ Ae.toJSON framesAssetPointers
        , videoAssetPointer
        , audioStartTimestamp
        )
        St.insertRealTimeUserAVMMPart

      void $ insertAudioAssetTree uidPart audioPointer


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
    , pointer.expiryDatetimeAap
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
    , metadata.startTimestampAm
    , metadata.endTimestampAm
    , metadata.pretokenizedVqAm
    , metadata.interruptionsAm
    , metadata.originalAudioSourceAm
    , metadata.transcriptionAm
    , metadata.wordTranscriptionAm
    , metadata.startAm
    , metadata.endAm
    )
    St.insertAudioMetadata


updateMessage :: Statement
      ( Maybe Double, Maybe Double, Text, Maybe Bool, Double, Ae.Value
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
        weight = $5 :: float8,
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