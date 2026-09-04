module OpenAI.Content.Write (
    insertMsg
    , insertAuthor
    , insertPayload
    , insertPart
    , rewriteMsg
  ) where

import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe)
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import qualified Data.Aeson as Ae
import qualified Data.Vector as V
import qualified Hasql.Transaction as Tx

import qualified OpenAI.Content.Codec as Codec
import qualified OpenAI.Content.Kind as Kind
import OpenAI.Content.Types (IssueC(..), PartPL, Payload(..), ResultW(..), StatW(..), emptyStatW)
import qualified OpenAI.Conversation.Json.Schema as Jd
import qualified OpenAI.Conversation.Serialize.ContentStmt as St


insertMsg :: Int64 -> Int32 -> Jd.Message -> Tx.Transaction (Either IssueC ResultW)
insertMsg uidNode seqMsg msg =
  case Codec.fromJson msg.contentMsg of
    Left issue -> pure $ Left issue
    Right payload ->
      case validatePayload payload of
        Left issue -> pure $ Left issue
        Right () -> do
          uidMsg <- Tx.statement
            ( uidNode
            , msg.idMsg
            , Just $ toRealFloat msg.createTimeMsg
            , toRealFloat <$> msg.updateTimeMsg
            , msg.statusMsg
            , msg.endTurnMsg
            , toRealFloat msg.weightMsg
            , Ae.toJSON msg.metadataMsg
            , msg.recipientMsg
            , msg.channelMsg
            , seqMsg
            )
            St.insertMessage

          insertAuthor uidMsg msg.authorMsg
          payloadRez <- insertPayload uidMsg 0 payload

          pure $ case payloadRez of
            Left issue -> Left issue
            Right payloadStat ->
              Right $ ResultW {
                  uidMsg = uidMsg
                  , statW = addStatW statMsgAuthor payloadStat
                  , notesW = []
                }


insertAuthor :: Int64 -> Jd.Author -> Tx.Transaction ()
insertAuthor uidMsg author =
  Tx.statement
    ( uidMsg
    , author.roleAu
    , author.nameAu
    , Ae.toJSON author.metadataAu
    )
    St.insertAuthor


insertPayload :: Int64 -> Int32 -> Payload -> Tx.Transaction (Either IssueC StatW)
insertPayload uidMsg seqContent payload =
  case validatePayload payload of
    Left issue -> pure $ Left issue
    Right () -> do
      let
        content = Codec.toJsonApprox payload
        kindC = Kind.kindFromJson content

      uidContent <- Tx.statement
        (uidMsg, Kind.textKC kindC, seqContent)
        St.insertContent

      bodyRez <-
        case payload of
          MultiPL parts -> insertParts uidContent parts
          _ -> Right <$> insertContentBody uidContent content

      pure $ addStatW statContent <$> bodyRez


insertPart :: Int64 -> Int32 -> PartPL -> Tx.Transaction (Either IssueC StatW)
insertPart uidContent seqPart part =
  case partJson part of
    Left issue -> pure $ Left issue
    Right partJs -> do
      uidPart <- Tx.statement
        (uidContent, Kind.textPP part, seqPart)
        St.insertMultiModalPart

      bodyStat <- insertPartBody uidPart partJs
      pure . Right $ addStatW statPart bodyStat


rewriteMsg :: Int64 -> Jd.Message -> Tx.Transaction (Either IssueC ResultW)
rewriteMsg uidMsg msg =
  case Codec.fromJson msg.contentMsg of
    Left issue -> pure $ Left issue
    Right payload ->
      case validatePayload payload of
        Left issue -> pure $ Left issue
        Right () -> do
          -- Delta.Apply owns previous-state preservation so that the old scalar
          -- values, payload and hash are captured before this replacement.
          Tx.statement uidMsg St.deleteContentTreeByMsg
          Tx.statement uidMsg St.deleteAuthorByMsg

          Tx.statement
            ( Just $ toRealFloat msg.createTimeMsg
            , toRealFloat <$>msg.updateTimeMsg
            , msg.statusMsg
            , msg.endTurnMsg
            , toRealFloat msg.weightMsg
            , Ae.toJSON msg.metadataMsg
            , msg.recipientMsg
            , msg.channelMsg
            , uidMsg
            )
            St.updateMessage

          insertAuthor uidMsg msg.authorMsg
          payloadRez <- insertPayload uidMsg 0 payload

          pure $ case payloadRez of
            Left issue -> Left issue
            Right payloadStat ->
              Right $ ResultW {
                  uidMsg = uidMsg
                  , statW = addStatW statMsgAuthor payloadStat
                  , notesW = []
                }


validatePayload :: Payload -> Either IssueC ()
validatePayload payload =
  case payload of
    MultiPL parts -> mapM_ validatePart parts
    _ -> Right ()


validatePart :: PartPL -> Either IssueC ()
validatePart part =
  case partJson part of
    Left issue -> Left issue
    Right _ -> Right ()


partJson :: PartPL -> Either IssueC Jd.MultiModalPart
partJson part =
  case Codec.toJsonApprox $ MultiPL [part] of
    Jd.MultimodalTextCT mmTxt -> case mmTxt.partsMmt of
      [partJs] -> Right partJs
      _ -> Left $ BadPayloadIC "@[Write.partJson] canonical multimodal part did not produce exactly one JSON part"
    _ -> Left $ BadPayloadIC "@[Write.partJson] canonical multimodal part did not produce multimodal JSON content"


insertParts :: Int64 -> [PartPL] -> Tx.Transaction (Either IssueC StatW)
insertParts uidContent parts =
  go emptyStatW $ zip [0 :: Int32 ..] parts
  where
    go stat [] = pure $ Right stat
    go stat ((seqPart, part) : rest) = do
      partRez <- insertPart uidContent seqPart part
      case partRez of
        Left issue -> pure $ Left issue
        Right partStat -> go (addStatW stat partStat) rest


insertContentBody :: Int64 -> Jd.Content -> Tx.Transaction StatW
insertContentBody uidContent content =
  case content of
    Jd.CodeCT code -> do
      Tx.statement (uidContent, code.languageCP, code.responseFormatNameCP, code.textCP) St.insertCodeContent
      pure emptyStatW

    Jd.ExecutionOutputCT execOut -> do
      Tx.statement (uidContent, execOut.textEO) St.insertExecutionOutputContent
      pure emptyStatW

    Jd.MultimodalTextCT mmt -> do
      rez <- mapM (insertPartBody uidContent) mmt.partsMmt
      case rez of
        [] -> pure emptyStatW
        h : _ -> pure h

    Jd.ModelEditableContextCT modelCtx -> do
      Tx.statement ( uidContent, modelCtx.modelSetMEC, modelCtx.repositoryMEC, modelCtx.repoSummaryMEC
          , modelCtx.structuredMEC
        ) St.insertModelEditableContextContent
      pure emptyStatW

    Jd.ReasoningRecapCT recap -> do
      Tx.statement (uidContent, recap.contentRR) St.insertReasoningRecapContent
      pure emptyStatW

    Jd.SystemErrorCT sysErr -> do
      Tx.statement (uidContent, sysErr.nameSER, sysErr.textSER) St.insertSystemErrorContent
      pure emptyStatW

    Jd.TetherBrowsingDisplayCT tbdr -> do
      Tx.statement ( uidContent, tbdr.resultTbd, Ae.toJSON <$> tbdr.summaryTbd, Ae.toJSON <$> tbdr.assetsTbd
        , tbdr.tetherIDTbd) St.insertTetherBrowsingDisplayContent
      pure emptyStatW

    Jd.TetherQuoteCT tq -> do
      Tx.statement ( uidContent, tq.urlTq, tq.domainTq, tq.textTq, tq.titleTq, tq.tetherIDTq) St.insertTetherQuoteContent
      pure emptyStatW

    Jd.TextCT txt -> do
      Tx.statement
        (uidContent, V.fromList txt.partsTP)
        St.insertTextContent
      pure emptyStatW

    Jd.ThoughtsCT thoughts -> do
      Tx.statement (uidContent, thoughts.sourceAnalysisMsgIdTP) St.insertThoughtsContent
      mapM_ (\(seqThought, thought) -> insertThought uidContent seqThought thought) (zip [0 :: Int32 ..] thoughts.thoughtsTP)
      pure emptyStatW

    Jd.OtherCT info -> do
      Tx.statement (uidContent, Ae.toJSON info.rawOpl) St.insertUnknownContent
      pure statUnknown


insertThought :: Int64 -> Int32 -> Jd.ThoughtContent -> Tx.Transaction ()
insertThought uidContent seqThought thought =
  Tx.statement ( uidContent, thought.summaryTC, thought.contentTC, Ae.toJSON thought.chunksTC
      , thought.finishedTC, seqThought) St.insertThought


insertPartBody :: Int64 -> Jd.MultiModalPart -> Tx.Transaction StatW
insertPartBody uidPart part =
  case part of
    Jd.TextPT text -> do
      Tx.statement
        (uidPart, text)
        St.insertTextMMPart
      pure emptyStatW

    Jd.AudioTranscriptionPT audioTrans -> do
      Tx.statement
        (uidPart, audioTrans.textAtp, audioTrans.directionAtp, audioTrans.decodingIdAtp)
        St.insertAudioTranscriptionMMPart
      pure emptyStatW

    Jd.AudioAssetPointerPT ptr -> insertAudioAsset uidPart ptr

    Jd.ImageAssetPointerPT imgPtr -> do
      uidImage <- Tx.statement
        ( uidPart
        , imgPtr.assetPointerPap
        , fromIntegral imgPtr.sizeBytesPap
        , fromIntegral imgPtr.widthPap
        , fromIntegral imgPtr.heightPap
        , imgPtr.foveaPap
        )
        St.insertImageAssetPointerMMPart

      case imgPtr.metadataPap of
        Nothing -> pure emptyStatW
        Just metadata -> insertImageMetadata uidImage metadata

    Jd.RealTimeUserAVPT rtUser -> do
      uidAv <- Tx.statement
        ( uidPart
        , toRealFloat <$> rtUser.expiryDatetimeRtuav
        , Just $ Ae.Array (V.fromList rtUser.framesApRtuav)
        , rtUser.videoContainerApRtuav
        , toRealFloat <$> rtUser.audioStartTimestampRtuav
        )
        St.insertRealTimeUserAVMMPart

      insertAudioAsset uidAv rtUser.audioApRtuav


insertAudioAsset :: Int64 -> Jd.AudioAssetPointer -> Tx.Transaction StatW
insertAudioAsset uidParent ptr = do
  uidAudio <- Tx.statement
    ( uidParent
    , toRealFloat <$> ptr.expiryDatetimeAap
    , ptr.assetPointerAap
    , fromIntegral ptr.sizeBytesAap
    , ptr.formatAap
    , ptr.toolAudioDirectionAap
    )
    St.insertAudioAssetPointerMMPart

  case ptr.metadataAap of
    Nothing -> pure emptyStatW
    Just metadata -> do
      insertAudioMetadata uidAudio metadata
      pure statMeta


insertAudioMetadata :: Int64 -> Jd.AudioMetadata -> Tx.Transaction ()
insertAudioMetadata uidAudio metadata =
  Tx.statement
    ( uidAudio
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


insertImageMetadata :: Int64 -> Jd.ImageMetadata -> Tx.Transaction StatW
insertImageMetadata uidImage metadata = do
  uidMetadata <- Tx.statement
    ( uidImage
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

  dalleStat <-
    case metadata.dalleMd of
      Nothing -> pure emptyStatW
      Just dalle -> do
        insertDalle uidMetadata dalle
        pure statMeta

  generationStat <-
    case metadata.generationMd of
      Nothing -> pure emptyStatW
      Just generation -> do
        insertGeneration uidMetadata generation
        pure statMeta

  pure $ addStatW statMeta $ addStatW dalleStat generationStat


insertDalle :: Int64 -> Jd.Dalle -> Tx.Transaction ()
insertDalle uidMetadata dalle =
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


insertGeneration :: Int64 -> Jd.Generation -> Tx.Transaction ()
insertGeneration uidMetadata generation =
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


addStatW :: StatW -> StatW -> StatW
addStatW left right =
  StatW {
      msgCnt = left.msgCnt + right.msgCnt
      , authorCnt = left.authorCnt + right.authorCnt
      , contentCnt = left.contentCnt + right.contentCnt
      , partCnt = left.partCnt + right.partCnt
      , metaCnt = left.metaCnt + right.metaCnt
      , unknownCnt = left.unknownCnt + right.unknownCnt
    }


statMsgAuthor :: StatW
statMsgAuthor =
  emptyStatW {
      msgCnt = 1
      , authorCnt = 1
    }


statContent :: StatW
statContent = emptyStatW {contentCnt = 1}


statPart :: StatW
statPart = emptyStatW {partCnt = 1}


statMeta :: StatW
statMeta = emptyStatW {metaCnt = 1}


statUnknown :: StatW
statUnknown = emptyStatW {unknownCnt = 1}