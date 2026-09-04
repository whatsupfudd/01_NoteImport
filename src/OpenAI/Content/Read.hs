module OpenAI.Content.Read (loadByMsg, loadOne, loadPart) where

import Control.Monad (forM)

import Data.Either (partitionEithers)
import Data.Int (Int32, Int64)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Data.Aeson ((.=))
import qualified Data.Aeson as Ae

import Hasql.Statement (Statement)
import qualified Hasql.Transaction as Tx

import OpenAI.Content.Kind (KindPart(..))
import qualified OpenAI.Content.Kind as Ck
import qualified OpenAI.Content.Types as Ctp
import qualified OpenAI.Conversation.Deserialize.ContentStmt as Cs
import qualified OpenAI.Conversation.Deserialize.ConversationStmt as StC
import qualified OpenAI.Utils as Ut


loadByMsg :: Int64 -> Tx.Transaction (Either [Ctp.IssueC] [(Int32, Ctp.Payload)])
loadByMsg uidMsg = do
  rows <- Tx.statement uidMsg Cs.selectContentsByMsg
  loaded <- forM (sortOn seqRowC $ V.toList rows) loadTagged
  let
    (issues, payloads) = partitionEithers loaded
  pure $
    case issues of
      [] -> Right payloads
      _ -> Left issues

-- Used to load a row of content associated with a message (see loadByMsg).
loadOne :: Cs.CRaw -> Tx.Transaction (Either Ctp.IssueC Ctp.Payload)
loadOne row =
  let
    uidC = uidRowC row
    kindTxt = kindRowC row
  in
  case Ck.kindFromText kindTxt of
    Ctp.CodeKC ->
      loadRequired "code" uidC Cs.selectCode $ \(lang, format, text) ->
        Ctp.CodePL {
            langCode = lang
            , formatRef = format
            , textCode = text
          }

    Ctp.ExecOutKC ->
      loadRequired "execution_output" uidC Cs.selectExecOut $ \text ->
        Ctp.ExecOutPL {
            textOut = text
          }

    Ctp.ModelCtxKC ->
      loadRequired "model_editable_context" uidC Cs.selectModelCtx $ \(model, repo, repoSummary, structured) ->
        Ctp.ModelCtxPL {
            modelSlug = model
            , repoJson = repo
            , rsJson = repoSummary
            , scJson = structured
          }

    Ctp.MultiTextKC ->
      loadMulti uidC

    Ctp.ReasoningKC ->
      loadRequired "reasoning_recap" uidC Cs.selectReasoning $ \text ->
        Ctp.ReasoningPL {
            textReasoning = text
          }

    Ctp.SystemErrKC ->
      loadRequired "system_error" uidC Cs.selectSystemErr $ \(name, text) ->
        Ctp.SystemErrPL {
            nameErr = name
            , textErr = text
          }

    Ctp.TetherBrowseKC ->
      loadRequired "tether_browsing_display" uidC Cs.selectTetherBrowse $ \(results, summary, assets, tetherId) ->
        Ctp.TetherBrowsePL {
            resultsJson = results
            , summaryJson = summary
            , assetsJson = assets
            , tetherId = tetherId
          }

    Ctp.TetherQuoteKC ->
      loadRequired "tether_quote" uidC Cs.selectTetherQuote $ \(url, domain, text, title, tetherId) ->
        Ctp.TetherQuotePL {
            urlQuote = url
            , domainQuote = domain
            , textQuote = text
            , titleQuote = title
            , tetherId = tetherId
          }

    Ctp.TextKC ->
      loadRequired "text" uidC Cs.selectText $ \parts ->
        Ctp.TextPL {
            partsText = V.toList parts
          }

    Ctp.ThoughtsKC ->
      loadThoughts uidC

    Ctp.OtherKC kind ->
      loadOther uidC kind


loadPart :: Cs.PartRaw -> Tx.Transaction (Either Ctp.IssueC Ctp.PartPL)
loadPart row =
  let
    uidPart = uidRowPart row
    kindTxt = kindRowPart row
  in
  case Ck.partFromText kindTxt of
    Left issue ->
      pure $ Left issue

    Right Ck.TextKP ->
      loadRequired "multimodal text part" uidPart Cs.selectTextPart $ \text ->
        Ctp.TextPP {
            textPart = text
          }

    Right AudioTransKP ->
      loadRequired "multimodal audio transcription part" uidPart Cs.selectAudioTransPart $ \(text, direction, decodingId) ->
        Ctp.AudioTransPP {
            textAudio = text
            , direction = direction
            , decodingId = decodingId
          }

    Right Ck.AudioAssetKP ->
      loadAudioPart uidPart

    Right Ck.ImageAssetKP ->
      loadImagePart uidPart

    Right Ck.RealtimeAvKP ->
      loadRealtimePart uidPart

    Right (Ck.OtherKP kind) ->
      pure . Left $ Ctp.UnsupportedIC ("unsupported multimodal part kind " <> kind <> " at part " <> renderI64 uidPart)


loadTagged :: Cs.CRaw -> Tx.Transaction (Either Ctp.IssueC (Int32, Ctp.Payload))
loadTagged row = do
  result <- loadOne row
  pure $ fmap ((,) $ seqRowC row) result


loadMulti :: Int64 -> Tx.Transaction (Either Ctp.IssueC Ctp.Payload)
loadMulti uidC = do
  rows <- Tx.statement uidC Cs.selectPartsByContent
  loaded <- forM (sortOn seqRowPart $ V.toList rows) loadPart
  let
    (issues, parts) = partitionEithers loaded
  pure $
    case issues of
      [] ->
        Right Ctp.MultiPL {
            parts = parts
          }
      _ ->
        Left . Ctp.PartialMultiIC $
          "invalid multimodal content " <> renderI64 uidC <> ": " <> T.intercalate "; " (map renderIssue issues)


loadThoughts :: Int64 -> Tx.Transaction (Either Ctp.IssueC Ctp.Payload)
loadThoughts uidC = do
  mbSource <- Tx.statement uidC Cs.selectThoughts
  case mbSource of
    Nothing ->
      pure . Left $ missingRow "thoughts" uidC

    Just sourceId -> do
      rows <- Tx.statement uidC Cs.selectThoughtsRaw
      let
        -- Note: selectThoughtsRaw already sorts by seqnbr, uid.
        thoughts = map thoughtFromRow $ V.toList rows
      pure . Right $
        Ctp.ThoughtsPL {
            sourceId = sourceId
            , thoughts = thoughts
          }


loadOther :: Int64 -> Text -> Tx.Transaction (Either Ctp.IssueC Ctp.Payload)
loadOther uidC kind = do
  mbRaw <- Tx.statement uidC Cs.selectOther
  let
    raw =
      case mbRaw of
        Just value -> value
        Nothing -> missingOtherValue uidC kind
  pure . Right $
    Ctp.OtherPL {
        kindOther = kind
        , rawOther = raw
      }


loadAudioPart :: Int64 -> Tx.Transaction (Either Ctp.IssueC Ctp.PartPL)
loadAudioPart uidPart = do
  mbRow <- Tx.statement uidPart Cs.selectAudioAssetPart
  case mbRow of
    Nothing -> pure . Left $ missingRow "multimodal audio asset pointer" uidPart
    Just row -> do
      ptr <- loadAudioPtrRow row
      pure . Right $ Ctp.AudioAssetPP { ptrAudio = ptr }


loadAudioByUid :: Int64 -> Tx.Transaction (Either Ctp.IssueC Ctp.AudioPtr)
loadAudioByUid uidAudio = do
  mbRow <- Tx.statement uidAudio Cs.selectAudioAssetPart  -- Cs.selectAudioAssetByUid
  case mbRow of
    Nothing -> pure . Left $ missingRow "audio asset pointer" uidAudio
    Just row -> Right <$> loadAudioPtrRow row


loadAudioPtrRow :: Cs.AudioAssetRaw -> Tx.Transaction Ctp.AudioPtr
loadAudioPtrRow (uid, expiry, asset, size, format, direction) = do
  mbMeta <- Tx.statement uid Cs.selectAudioMeta
  pure $ Ctp.AudioPtr (Ut.safeScientific =<< expiry) asset size format direction
      (audioMetaFromRow <$> mbMeta)


loadImagePart :: Int64 -> Tx.Transaction (Either Ctp.IssueC Ctp.PartPL)
loadImagePart uidPart = do
  mbRow <- Tx.statement uidPart Cs.selectImageAssetPart
  case mbRow of
    Nothing ->
      pure . Left $ missingRow "multimodal image asset pointer" uidPart

    Just row -> do
      mbMeta <- loadImageMeta $ uidImageRow row
      pure . Right $
        Ctp.ImageAssetPP {
            ptrImage =
              Ctp.ImagePtr
                (assetImageRow row)
                (sizeImageRow row)
                (widthImageRow row)
                (heightImageRow row)
                (foveaImageRow row)
                mbMeta
          }


loadImageMeta :: Int64 -> Tx.Transaction (Maybe Ctp.ImageMeta)
loadImageMeta uidImage = do
  mbRow <- Tx.statement uidImage Cs.selectImageMeta
  case mbRow of
    Nothing ->
      pure Nothing

    Just row -> do
      mbDalle <- Tx.statement (uidImageMetaRow row) Cs.selectDalle
      mbGeneration <- Tx.statement (uidImageMetaRow row) Cs.selectGeneration
      pure . Just $
        Ctp.ImageMeta
          (dalleFromRow <$> mbDalle)
          (gizmoImageMetaRow row)
          (generationFromRow <$> mbGeneration)
          (heightImageMetaRow row)
          (widthImageMetaRow row)
          (omitGlimpseImageMetaRow row)
          (patchesOverrideImageMetaRow row)
          (keepPatchImageMetaRow row)
          (deltaChannelImageMetaRow row)
          (sanitizedImageMetaRow row)
          (assetLinkImageMetaRow row)
          (watermarkedImageMetaRow row)
          (placeholderImageMetaRow row)


loadRealtimePart :: Int64 -> Tx.Transaction (Either Ctp.IssueC Ctp.PartPL)
loadRealtimePart uidPart = do
  mbRow <- Tx.statement uidPart Cs.selectRealtimeAvPart
  case mbRow of
    Nothing ->
      pure . Left $ missingRow "multimodal realtime audio/video part" uidPart

    Just row -> do
      audioResult <- loadAudioByUid $ uidAudioRealtimeRow row
      pure $
        case audioResult of
          Left issue ->
            Left issue

          Right audio ->
            Right $
              Ctp.RealtimeAvPP {
                  ptrAv =
                    Ctp.AvPtr
                      (Ut.safeScientific =<< expiryRealtimeRow row)
                      [framesRealtimeRow row]
                      (videoRealtimeRow row)
                      audio
                      (Ut.safeScientific =<< startAudioRealtimeRow row)
                }


-- Performs the query to load a content row of a given type, converts it
-- to an in-memory value.
loadRequired :: Text -> Int64 -> Statement Int64 (Maybe row) -> (row -> value)
    -> Tx.Transaction (Either Ctp.IssueC value)
loadRequired label uid statement fromRow = do
  mbRow <- Tx.statement uid statement
  pure $
    case mbRow of
      Nothing -> Left $ missingRow label uid
      Just row -> Right $ fromRow row


thoughtFromRow :: Cs.ThoughtRaw -> Ctp.ThoughtRow
thoughtFromRow (summary, content, chunks, finished) =
  Ctp.ThoughtRow summary content chunks finished


audioMetaFromRow :: Cs.AudioMetaRaw -> Ctp.AudioMeta
audioMetaFromRow (startTimestamp, endTimestamp, pretokenized, interruptions, source, transcription, wordTranscription, start, end) =
  Ctp.AudioMeta
    (Ut.safeScientific =<< startTimestamp)
    (Ut.safeScientific =<< endTimestamp)
    pretokenized
    interruptions
    source
    transcription
    wordTranscription
    (fromMaybe 0 $ Ut.safeScientific start)
    (fromMaybe 0 $ Ut.safeScientific end)


dalleFromRow :: Cs.DalleRaw -> Ctp.DalleMeta
dalleFromRow (genId, prompt, seed, parentId, editOp, title) =
  Ctp.DalleMeta
    genId
    prompt
    seed
    parentId
    editOp
    title


generationFromRow :: Cs.GenerationRaw -> Ctp.GenMeta
generationFromRow row =
  Ctp.GenMeta
    (genIdGenerationRow row)
    (sizeGenerationRow row)
    (seedGenerationRow row)
    (parentGenIdGenerationRow row)
    (heightGenerationRow row)
    (widthGenerationRow row)
    (transparentGenerationRow row)
    (titleGenerationRow row)
    (orientationGenerationRow row)


missingRow :: Text -> Int64 -> Ctp.IssueC
missingRow label uid =
  Ctp.MissingRowIC $ "missing " <> label <> " subtype row for uid " <> renderI64 uid


missingOtherValue :: Int64 -> Text -> Ae.Value
missingOtherValue uid kind =
  Ae.object [
      "content_type" .= kind
      , "_docimp" .= Ae.object [
          "issue" .= ("missing unknown-content payload" :: Text)
          , "content_uid" .= uid
        ]
    ]


renderIssue :: Ctp.IssueC -> Text
renderIssue = T.pack . show


renderI64 :: Int64 -> Text
renderI64 = T.pack . show


uidRowC :: Cs.CRaw -> Int64
uidRowC (uid, _, _) = uid


kindRowC :: Cs.CRaw -> Text
kindRowC (_, kind, _) = kind


seqRowC :: Cs.CRaw -> Int32
seqRowC (_, _, seqC) = seqC


uidRowPart :: Cs.PartRaw -> Int64
uidRowPart (uid, _, _) = uid


kindRowPart :: Cs.PartRaw -> Text
kindRowPart (_, kind, _) = kind


seqRowPart :: Cs.PartRaw -> Int32
seqRowPart (_, _, seqPart) = seqPart


seqThoughtRow :: StC.ThoughtRow -> Int64
seqThoughtRow (seqThought, _, _, _, _) = seqThought


summaryThoughtRow :: StC.ThoughtRow -> Text
summaryThoughtRow (_, summary, _, _, _) = summary


contentThoughtRow :: StC.ThoughtRow -> Text
contentThoughtRow (_, _, content, _, _) = content


chunksThoughtRow :: StC.ThoughtRow -> Ae.Value
chunksThoughtRow (_, _, _, chunks, _) = chunks


finishedThoughtRow :: StC.ThoughtRow -> Bool
finishedThoughtRow (_, _, _, _, finished) = finished


uidAudioRow :: Cs.AudioAssetRaw -> Int64
uidAudioRow (uid, _, _, _, _, _) = uid


expiryAudioRow :: Cs.AudioAssetRaw -> Maybe Double
expiryAudioRow (_, expiry, _, _, _, _) = expiry


assetAudioRow :: Cs.AudioAssetRaw -> Text
assetAudioRow (_, _, asset, _, _, _) = asset


sizeAudioRow :: Cs.AudioAssetRaw -> Int64
sizeAudioRow (_, _, _, size, _, _) = size


formatAudioRow :: Cs.AudioAssetRaw -> Text
formatAudioRow (_, _, _, _, format, _) = format


directionAudioRow :: Cs.AudioAssetRaw -> Maybe Text
directionAudioRow (_, _, _, _, _, direction) = direction


uidImageRow :: Cs.ImageAssetRaw -> Int64
uidImageRow (uid, _, _, _, _, _) = uid


assetImageRow :: Cs.ImageAssetRaw -> Text
assetImageRow (_, asset, _, _, _, _) = asset


sizeImageRow :: Cs.ImageAssetRaw -> Int64
sizeImageRow (_, _, size, _, _, _) = size


widthImageRow :: Cs.ImageAssetRaw -> Int32
widthImageRow (_, _, _, width, _, _) = width


heightImageRow :: Cs.ImageAssetRaw -> Int32
heightImageRow (_, _, _, _, height, _) = height


foveaImageRow :: Cs.ImageAssetRaw -> Maybe Ae.Value
foveaImageRow (_, _, _, _, _, fovea) = fovea


uidImageMetaRow :: Cs.ImageMetaRaw -> Int64
uidImageMetaRow (uid, _, _, _, _, _, _, _, _, _, _, _) = uid


gizmoImageMetaRow :: Cs.ImageMetaRaw -> Maybe Ae.Value
gizmoImageMetaRow (_, gizmo, _, _, _, _, _, _, _, _, _, _) = gizmo


heightImageMetaRow :: Cs.ImageMetaRaw -> Maybe Int32
heightImageMetaRow (_, _, height, _, _, _, _, _, _, _, _, _) = height


widthImageMetaRow :: Cs.ImageMetaRaw -> Maybe Int32
widthImageMetaRow (_, _, _, width, _, _, _, _, _, _, _, _) = width


omitGlimpseImageMetaRow :: Cs.ImageMetaRaw -> Maybe Ae.Value
omitGlimpseImageMetaRow (_, _, _, _, value, _, _, _, _, _, _, _) = value


patchesOverrideImageMetaRow :: Cs.ImageMetaRaw -> Maybe Ae.Value
patchesOverrideImageMetaRow (_, _, _, _, _, value, _, _, _, _, _, _) = value


keepPatchImageMetaRow :: Cs.ImageMetaRaw -> Maybe Ae.Value
keepPatchImageMetaRow (_, _, _, _, _, _, value, _, _, _, _, _) = value


deltaChannelImageMetaRow :: Cs.ImageMetaRaw -> Maybe Ae.Value
deltaChannelImageMetaRow (_, _, _, _, _, _, _, value, _, _, _, _) = value


sanitizedImageMetaRow :: Cs.ImageMetaRaw -> Bool
sanitizedImageMetaRow (_, _, _, _, _, _, _, _, value, _, _, _) = value


assetLinkImageMetaRow :: Cs.ImageMetaRaw -> Maybe Ae.Value
assetLinkImageMetaRow (_, _, _, _, _, _, _, _, _, value, _, _) = value


watermarkedImageMetaRow :: Cs.ImageMetaRaw -> Maybe Ae.Value
watermarkedImageMetaRow (_, _, _, _, _, _, _, _, _, _, value, _) = value


placeholderImageMetaRow :: Cs.ImageMetaRaw -> Maybe Ae.Value
placeholderImageMetaRow (_, _, _, _, _, _, _, _, _, _, _, value) = value


uidAudioRealtimeRow :: Cs.RealtimeAvRaw -> Int64
uidAudioRealtimeRow (uidAudio, _, _, _, _) = uidAudio


expiryRealtimeRow :: Cs.RealtimeAvRaw -> Maybe Double
expiryRealtimeRow (_, expiry, _, _, _) = expiry


framesRealtimeRow :: Cs.RealtimeAvRaw -> Ae.Value
framesRealtimeRow (_, _, frames, _, _) = frames


videoRealtimeRow :: Cs.RealtimeAvRaw -> Maybe Ae.Value
videoRealtimeRow (_, _, _, video, _) = video


startAudioRealtimeRow :: Cs.RealtimeAvRaw -> Maybe Double
startAudioRealtimeRow (_, _, _, _, startAudio) = startAudio


startTimestampAudioMetaRow :: Cs.AudioMetaRaw -> Maybe Double
startTimestampAudioMetaRow (value, _, _, _, _, _, _, _, _) = value


endTimestampAudioMetaRow :: Cs.AudioMetaRaw -> Maybe Double
endTimestampAudioMetaRow (_, value, _, _, _, _, _, _, _) = value


pretokenizedAudioMetaRow :: Cs.AudioMetaRaw -> Maybe Ae.Value
pretokenizedAudioMetaRow (_, _, value, _, _, _, _, _, _) = value


interruptionsAudioMetaRow :: Cs.AudioMetaRaw -> Maybe Ae.Value
interruptionsAudioMetaRow (_, _, _, value, _, _, _, _, _) = value


sourceAudioMetaRow :: Cs.AudioMetaRaw -> Maybe Ae.Value
sourceAudioMetaRow (_, _, _, _, value, _, _, _, _) = value


transcriptionAudioMetaRow :: Cs.AudioMetaRaw -> Maybe Ae.Value
transcriptionAudioMetaRow (_, _, _, _, _, value, _, _, _) = value


wordTranscriptionAudioMetaRow :: Cs.AudioMetaRaw -> Maybe Ae.Value
wordTranscriptionAudioMetaRow (_, _, _, _, _, _, value, _, _) = value


startAudioMetaRow :: Cs.AudioMetaRaw -> Double
startAudioMetaRow (_, _, _, _, _, _, _, value, _) = value


endAudioMetaRow :: Cs.AudioMetaRaw -> Double
endAudioMetaRow (_, _, _, _, _, _, _, _, value) = value


genIdDalleRow :: Cs.DalleRaw -> Maybe Text
genIdDalleRow (value, _, _, _, _, _) = value


promptDalleRow :: Cs.DalleRaw -> Text
promptDalleRow (_, value, _, _, _, _) = value


seedDalleRow :: Cs.DalleRaw -> Maybe Int64
seedDalleRow (_, _, value, _, _, _) = value


parentGenIdDalleRow :: Cs.DalleRaw -> Maybe Text
parentGenIdDalleRow (_, _, _, value, _, _) = value


editOpDalleRow :: Cs.DalleRaw -> Maybe Text
editOpDalleRow (_, _, _, _, value, _) = value


titleDalleRow :: Cs.DalleRaw -> Text
titleDalleRow (_, _, _, _, _, value) = value


genIdGenerationRow :: Cs.GenerationRaw -> Maybe Text
genIdGenerationRow (value, _, _, _, _, _, _, _, _) = value


sizeGenerationRow :: Cs.GenerationRaw -> Text
sizeGenerationRow (_, value, _, _, _, _, _, _, _) = value


seedGenerationRow :: Cs.GenerationRaw -> Maybe Int64
seedGenerationRow (_, _, value, _, _, _, _, _, _) = value


parentGenIdGenerationRow :: Cs.GenerationRaw -> Maybe Text
parentGenIdGenerationRow (_, _, _, value, _, _, _, _, _) = value


heightGenerationRow :: Cs.GenerationRaw -> Int32
heightGenerationRow (_, _, _, _, value, _, _, _, _) = value


widthGenerationRow :: Cs.GenerationRaw -> Int32
widthGenerationRow (_, _, _, _, _, value, _, _, _) = value


transparentGenerationRow :: Cs.GenerationRaw -> Bool
transparentGenerationRow (_, _, _, _, _, _, value, _, _) = value


titleGenerationRow :: Cs.GenerationRaw -> Text
titleGenerationRow (_, _, _, _, _, _, _, value, _) = value


orientationGenerationRow :: Cs.GenerationRaw -> Maybe Text
orientationGenerationRow (_, _, _, _, _, _, _, _, value) = value