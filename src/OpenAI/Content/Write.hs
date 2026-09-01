module OpenAI.Content.Write (
    insertMsg
    , insertAuthor
    , insertPayload
    , insertPart
    , rewriteMsg
  ) where

import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Aeson as Ae
import qualified Data.Vector as V
import qualified Hasql.Transaction as Tx

import qualified OpenAI.Content.Codec as Codec
import qualified OpenAI.Content.Kind as Kind
import OpenAI.Content.Types (IssueC(..), PartPL, Payload(..), ResultW(..), StatW(..), emptyStatW)
import qualified OpenAI.Json.Reader as Jd
import qualified OpenAI.Serialize.ContentStmt as St


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
            , msg.createTimeMsg
            , msg.updateTimeMsg
            , msg.statusMsg
            , msg.endTurnMsg
            , msg.weightMsg
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
            ( msg.createTimeMsg
            , msg.updateTimeMsg
            , msg.statusMsg
            , msg.endTurnMsg
            , msg.weightMsg
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
    Jd.MultimodalTextCT [partJs] -> Right partJs
    Jd.MultimodalTextCT _ -> Left $ BadPayloadIC "canonical multimodal part did not produce exactly one JSON part"
    _ -> Left $ BadPayloadIC "canonical multimodal part did not produce multimodal JSON content"


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
    Jd.CodeCT{} -> do
      Tx.statement
        (uidContent, content.languageCc, content.responseFormatNameCc, content.textCc)
        St.insertCodeContent
      pure emptyStatW

    Jd.ExecutionOutputCT{} -> do
      Tx.statement
        (uidContent, content.textEoc)
        St.insertExecutionOutputContent
      pure emptyStatW

    Jd.MultimodalTextCT{} ->
      pure emptyStatW

    Jd.ModelEditableContextCT{} -> do
      Tx.statement
        ( uidContent
        , content.modelSetContextMec
        , content.repositoryMec
        , content.repoSummaryMec
        , content.structuredContextMec
        )
        St.insertModelEditableContextContent
      pure emptyStatW

    Jd.ReasoningRecapCT{} -> do
      Tx.statement
        (uidContent, content.contentRrc)
        St.insertReasoningRecapContent
      pure emptyStatW

    Jd.SystemErrorCT{} -> do
      Tx.statement
        (uidContent, content.nameSes, content.textSes)
        St.insertSystemErrorContent
      pure emptyStatW

    Jd.TetherBrowsingDisplayCT{} -> do
      Tx.statement
        ( uidContent
        , content.resultTbd
        , Ae.toJSON <$> content.summaryTbd
        , Ae.toJSON <$> content.assetsTbd
        , content.tetherIDTbd
        )
        St.insertTetherBrowsingDisplayContent
      pure emptyStatW

    Jd.TetherQuoteCT{} -> do
      Tx.statement
        ( uidContent
        , content.urlTq
        , content.domainTq
        , content.textTq
        , content.titleTq
        , content.tetherIDTq
        )
        St.insertTetherQuoteContent
      pure emptyStatW

    Jd.TextCT{} -> do
      Tx.statement
        (uidContent, V.fromList content.partsCt)
        St.insertTextContent
      pure emptyStatW

    Jd.ThoughtsCT{} -> do
      Tx.statement
        (uidContent, content.sourceAnalysisMsgIdTc)
        St.insertThoughtsContent

      mapM_
        (\(seqThought, thought) -> insertThought uidContent seqThought thought)
        (zip [0 :: Int32 ..] content.thoughtsTc)

      pure emptyStatW

    Jd.OtherCT{} -> do
      Tx.statement
        (uidContent, Ae.toJSON content.rawOc)
        St.insertUnknownContent
      pure statUnknown


insertThought :: Int64 -> Int32 -> Jd.Thought -> Tx.Transaction ()
insertThought uidContent seqThought thought =
  Tx.statement
    ( uidContent
    , thought.summaryTh
    , thought.contentTh
    , fromMaybe (Ae.Array mempty) thought.chunksTh
    , fromMaybe False thought.finishedTh
    , seqThought
    )
    St.insertThought


insertPartBody :: Int64 -> Jd.MultiModalPart -> Tx.Transaction StatW
insertPartBody uidPart part =
  case part of
    Jd.TextPT text -> do
      Tx.statement
        (uidPart, text)
        St.insertTextMMPart
      pure emptyStatW

    Jd.AudioTranscriptionPT{} -> do
      Tx.statement
        (uidPart, part.textAtp, part.directionAtp, part.decodingIdAtp)
        St.insertAudioTranscriptionMMPart
      pure emptyStatW

    Jd.AudioAssetPointerPT ptr ->
      insertAudioAsset uidPart ptr

    Jd.ImageAssetPointerPT{} -> do
      uidImage <- Tx.statement
        ( uidPart
        , part.assetPointerPap
        , fromIntegral part.sizeBytesPap
        , fromIntegral part.widthPap
        , fromIntegral part.heightPap
        , part.foveaPap
        )
        St.insertImageAssetPointerMMPart

      case part.metadataPap of
        Nothing -> pure emptyStatW
        Just metadata -> insertImageMetadata uidImage metadata

    Jd.RealTimeUserAVPT{} -> do
      uidAv <- Tx.statement
        ( uidPart
        , part.expiryDatetimeRtuav
        , Just $ Ae.toJSON part.framesAssetPointersRtuav
        , part.videoContainerAssetPointer
        , part.audioStartTimestampRtuav
        )
        St.insertRealTimeUserAVMMPart

      insertAudioAsset uidAv part.audioAssetPointer


insertAudioAsset :: Int64 -> Jd.AudioAssetPointer -> Tx.Transaction StatW
insertAudioAsset uidParent ptr = do
  uidAudio <- Tx.statement
    ( uidParent
    , ptr.expiryDatetimeAap
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