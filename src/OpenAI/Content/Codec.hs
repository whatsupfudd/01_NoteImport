{-# LANGUAGE LambdaCase #-}

module OpenAI.Content.Codec (fromJson, toJsonApprox, valuePayload, valuePart) where

import Data.Foldable (toList)
import qualified Data.Map.Strict as Mp
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Data.Aeson ((.=))
import qualified Data.Aeson as Ae
import qualified Data.Aeson.Key as Ak
import qualified Data.Aeson.KeyMap as Km

import qualified OpenAI.Content.Types as Ct
import qualified OpenAI.Json.Reader as Jd


fromJson :: Jd.Content -> Either Ct.IssueC Ct.Payload
fromJson = \case
  Jd.CodeCT lang formatName text ->
    Right $ Ct.CodePL lang formatName text

  Jd.ExecutionOutputCT text ->
    Right $ Ct.ExecOutPL text

  Jd.ModelEditableContextCT modelSlug repository repoSummary structuredContext ->
    Right $ Ct.ModelCtxPL modelSlug repository repoSummary structuredContext

  Jd.MultimodalTextCT parts ->
    Ct.MultiPL <$> traverse fromPartJson parts

  Jd.ReasoningRecapCT text ->
    Right $ Ct.ReasoningPL text

  Jd.SystemErrorCT name text ->
    Right $ Ct.SystemErrPL name text

  Jd.TetherBrowsingDisplayCT results summary assets tetherId ->
    Right $ Ct.TetherBrowsePL results summary (Ae.toJSON <$> assets) tetherId

  Jd.TetherQuoteCT url domain text title tetherId ->
    Right $ Ct.TetherQuotePL url domain text title tetherId

  Jd.TextCT parts ->
    Right $ Ct.TextPL parts

  Jd.ThoughtsCT thoughts sourceId ->
    Right $ Ct.ThoughtsPL sourceId $ map fromThoughtJson thoughts

  Jd.OtherCT kind raw ->
    Right $ Ct.OtherPL kind $ Ae.toJSON raw


toJsonApprox :: Ct.Payload -> Jd.Content
toJsonApprox = \case
  Ct.CodePL lang formatName text ->
    Jd.CodeCT lang formatName text

  Ct.ExecOutPL text ->
    Jd.ExecutionOutputCT text

  Ct.ModelCtxPL modelSlug repository repoSummary structuredContext ->
    Jd.ModelEditableContextCT modelSlug repository repoSummary structuredContext

  Ct.MultiPL parts ->
    Jd.MultimodalTextCT $ map partToJsonApprox parts

  Ct.ReasoningPL text ->
    Jd.ReasoningRecapCT text

  Ct.SystemErrPL name text ->
    Jd.SystemErrorCT name text

  Ct.TetherBrowsePL results summary assets tetherId ->
    Jd.TetherBrowsingDisplayCT results summary (assetsFromValue assets) tetherId

  Ct.TetherQuotePL url domain text title tetherId ->
    Jd.TetherQuoteCT url domain text title tetherId

  Ct.TextPL parts ->
    Jd.TextCT parts

  Ct.ThoughtsPL sourceId thoughts ->
    Jd.ThoughtsCT (map thoughtToJsonApprox thoughts) sourceId

  Ct.OtherPL kind raw ->
    Jd.OtherCT kind $ objectMap raw


valuePayload :: Ct.Payload -> Ae.Value
valuePayload = \case
  Ct.CodePL lang formatName text ->
    Ae.object [
        "kind" .= ("code" :: Text)
        , "language" .= lang
        , "response_format_name" .= formatName
        , "text" .= text
      ]

  Ct.ExecOutPL text ->
    Ae.object [
        "kind" .= ("execution_output" :: Text)
        , "text" .= text
      ]

  Ct.ModelCtxPL modelSlug repository repoSummary structuredContext ->
    Ae.object [
        "kind" .= ("model_editable_context" :: Text)
        , "model_set_context" .= modelSlug
        , "repository" .= repository
        , "repo_summary" .= repoSummary
        , "structured_context" .= structuredContext
      ]

  Ct.MultiPL parts ->
    Ae.object [
        "kind" .= ("multimodal_text" :: Text)
        , "parts" .= map valuePart parts
      ]

  Ct.ReasoningPL text ->
    Ae.object [
        "kind" .= ("reasoning_recap" :: Text)
        , "content" .= text
      ]

  Ct.SystemErrPL name text ->
    Ae.object [
        "kind" .= ("system_error" :: Text)
        , "name" .= name
        , "text" .= text
      ]

  Ct.TetherBrowsePL results summary assets tetherId ->
    Ae.object [
        "kind" .= ("tether_browsing_display" :: Text)
        , "result" .= results
        , "summary" .= summary
        , "assets" .= assets
        , "tether_id" .= tetherId
      ]

  Ct.TetherQuotePL url domain text title tetherId ->
    Ae.object [
        "kind" .= ("tether_quote" :: Text)
        , "url" .= url
        , "domain" .= domain
        , "text" .= text
        , "title" .= title
        , "tether_id" .= tetherId
      ]

  Ct.TextPL parts ->
    Ae.object [
        "kind" .= ("text" :: Text)
        , "parts" .= parts
      ]

  Ct.ThoughtsPL sourceId thoughts ->
    Ae.object [
        "kind" .= ("thoughts" :: Text)
        , "source_analysis_msg_id" .= sourceId
        , "thoughts" .= map valueThought thoughts
      ]

  Ct.OtherPL kind raw ->
    Ae.object [
        "kind" .= kind
        , "raw" .= raw
      ]


valuePart :: Ct.PartPL -> Ae.Value
valuePart = \case
  Ct.TextPP text ->
    Ae.object [
        "kind" .= ("text" :: Text)
        , "text" .= text
      ]

  Ct.AudioTransPP text direction decodingId ->
    Ae.object [
        "kind" .= ("audio_transcription" :: Text)
        , "text" .= text
        , "direction" .= direction
        , "decoding_id" .= decodingId
      ]

  Ct.AudioAssetPP ptr ->
    Ae.object [
        "kind" .= ("audio_asset_pointer" :: Text)
        , "pointer" .= valueAudioPtr ptr
      ]

  Ct.ImageAssetPP ptr ->
    Ae.object [
        "kind" .= ("image_asset_pointer" :: Text)
        , "pointer" .= valueImagePtr ptr
      ]

  Ct.RealtimeAvPP ptr ->
    Ae.object [
        "kind" .= ("real_time_user_av" :: Text)
        , "pointer" .= valueAvPtr ptr
      ]


fromPartJson :: Jd.MultiModalPart -> Either Ct.IssueC Ct.PartPL
fromPartJson = \case
  Jd.TextPT text ->
    Right $ Ct.TextPP text

  Jd.AudioTranscriptionPT text direction decodingId ->
    Right $ Ct.AudioTransPP text direction decodingId

  Jd.AudioAssetPointerPT ptr ->
    Right $ Ct.AudioAssetPP $ fromAudioPtrJson ptr

  Jd.ImageAssetPointerPT asset size width height fovea metadata ->
    Right $ Ct.ImageAssetPP $
      Ct.ImagePtr asset (fromIntegral size) (fromIntegral width) (fromIntegral height) fovea (fromImageMetaJson <$> metadata)

  Jd.RealTimeUserAVPT expiry frames video audio start ->
    Right $ Ct.RealtimeAvPP $ Ct.AvPtr expiry frames video (fromAudioPtrJson audio) start


partToJsonApprox :: Ct.PartPL -> Jd.MultiModalPart
partToJsonApprox = \case
  Ct.TextPP text ->
    Jd.TextPT text

  Ct.AudioTransPP text direction decodingId ->
    Jd.AudioTranscriptionPT text direction decodingId

  Ct.AudioAssetPP ptr ->
    Jd.AudioAssetPointerPT $ audioPtrToJsonApprox ptr

  Ct.ImageAssetPP (Ct.ImagePtr asset size width height fovea metadata) ->
    Jd.ImageAssetPointerPT asset (fromIntegral size) (fromIntegral width) (fromIntegral height) fovea
      (imageMetaToJsonApprox <$> metadata)

  Ct.RealtimeAvPP (Ct.AvPtr expiry frames video audio start) ->
    Jd.RealTimeUserAVPT expiry frames video (audioPtrToJsonApprox audio) start


fromAudioPtrJson :: Jd.AudioAssetPointer -> Ct.AudioPtr
fromAudioPtrJson (Jd.AudioAssetPointer expiry asset size format direction metadata) =
  Ct.AudioPtr expiry asset (fromIntegral size) format direction (fromAudioMetaJson <$> metadata)


audioPtrToJsonApprox :: Ct.AudioPtr -> Jd.AudioAssetPointer
audioPtrToJsonApprox (Ct.AudioPtr expiry asset size format direction metadata) =
  Jd.AudioAssetPointer expiry asset (fromIntegral size) format direction (audioMetaToJsonApprox <$> metadata)


fromImageMetaJson :: Jd.ImageMetadata -> Ct.ImageMeta
fromImageMetaJson
    (Jd.ImageMetadata dalle gizmo generation containerHeight containerWidth omitGlimpse patchesOverride keepPatch
      deltaChannel sanitized assetLink watermarked placeholder) =
  Ct.ImageMeta
    (fromDalleJson <$> dalle)
    gizmo
    (fromGenerationJson <$> generation)
    (fromIntegral <$> containerHeight)
    (fromIntegral <$> containerWidth)
    omitGlimpse
    patchesOverride
    keepPatch
    deltaChannel
    sanitized
    assetLink
    watermarked
    placeholder


imageMetaToJsonApprox :: Ct.ImageMeta -> Jd.ImageMetadata
imageMetaToJsonApprox
    (Ct.ImageMeta dalle gizmo generation containerHeight containerWidth omitGlimpse patchesOverride keepPatch
      deltaChannel sanitized assetLink watermarked placeholder) =
  Jd.ImageMetadata
    (dalleToJsonApprox <$> dalle)
    gizmo
    (generationToJsonApprox <$> generation)
    (fromIntegral <$> containerHeight)
    (fromIntegral <$> containerWidth)
    omitGlimpse
    patchesOverride
    keepPatch
    deltaChannel
    sanitized
    assetLink
    watermarked
    placeholder


fromDalleJson :: Jd.Dalle -> Ct.DalleMeta
fromDalleJson (Jd.Dalle genId prompt seed parentGenId editOp title) =
  Ct.DalleMeta genId prompt (fromIntegral <$> seed) parentGenId editOp title


dalleToJsonApprox :: Ct.DalleMeta -> Jd.Dalle
dalleToJsonApprox (Ct.DalleMeta genId prompt seed parentGenId editOp title) =
  Jd.Dalle genId prompt (fromIntegral <$> seed) parentGenId editOp title


fromGenerationJson :: Jd.Generation -> Ct.GenMeta
fromGenerationJson (Jd.Generation genId size seed parentGenId height width transparent title orientation) =
  Ct.GenMeta genId size (fromIntegral <$> seed) parentGenId (fromIntegral height) (fromIntegral width) transparent title orientation


generationToJsonApprox :: Ct.GenMeta -> Jd.Generation
generationToJsonApprox (Ct.GenMeta genId size seed parentGenId height width transparent title orientation) =
  Jd.Generation genId size (fromIntegral <$> seed) parentGenId (fromIntegral height) (fromIntegral width) transparent title orientation


fromAudioMetaJson :: Jd.AudioMetadata -> Ct.AudioMeta
fromAudioMetaJson
    (Jd.AudioMetadata startTimestamp endTimestamp pretokenized interruptions originalSource transcription wordTranscription start end) =
  Ct.AudioMeta startTimestamp endTimestamp pretokenized interruptions originalSource transcription wordTranscription start end


audioMetaToJsonApprox :: Ct.AudioMeta -> Jd.AudioMetadata
audioMetaToJsonApprox
    (Ct.AudioMeta startTimestamp endTimestamp pretokenized interruptions originalSource transcription wordTranscription start end) =
  Jd.AudioMetadata startTimestamp endTimestamp pretokenized interruptions originalSource transcription wordTranscription start end


fromThoughtJson :: Jd.Thought -> Ct.ThoughtRow
fromThoughtJson (Jd.Thought summary content mbChunks mbFinished) =
  let
    chunks = fromMaybe Ae.Null mbChunks
    finished = fromMaybe False mbFinished
  in
  Ct.ThoughtRow summary content chunks finished


thoughtToJsonApprox :: Ct.ThoughtRow -> Jd.Thought
thoughtToJsonApprox (Ct.ThoughtRow summary content chunks finished) =
  Jd.Thought summary content (Just chunks) (Just finished)


valueAudioPtr :: Ct.AudioPtr -> Ae.Value
valueAudioPtr (Ct.AudioPtr expiry asset size format direction metadata) =
  Ae.object [
      "expiry_datetime" .= expiry
      , "asset_pointer" .= asset
      , "size_bytes" .= size
      , "format" .= format
      , "tool_audio_direction" .= direction
      , "metadata" .= fmap valueAudioMeta metadata
    ]


valueImagePtr :: Ct.ImagePtr -> Ae.Value
valueImagePtr (Ct.ImagePtr asset size width height fovea metadata) =
  Ae.object [
      "asset_pointer" .= asset
      , "size_bytes" .= size
      , "width" .= width
      , "height" .= height
      , "fovea" .= fovea
      , "metadata" .= fmap valueImageMeta metadata
    ]


valueAvPtr :: Ct.AvPtr -> Ae.Value
valueAvPtr (Ct.AvPtr expiry frames video audio start) =
  Ae.object [
      "expiry_datetime" .= expiry
      , "frames_asset_pointers" .= frames
      , "video_container_asset_pointer" .= video
      , "audio_asset_pointer" .= valueAudioPtr audio
      , "audio_start_timestamp" .= start
    ]


valueImageMeta :: Ct.ImageMeta -> Ae.Value
valueImageMeta
    (Ct.ImageMeta dalle gizmo generation containerHeight containerWidth omitGlimpse patchesOverride keepPatch
      deltaChannel sanitized assetLink watermarked placeholder) =
  Ae.object [
      "dalle" .= fmap valueDalle dalle
      , "gizmo" .= gizmo
      , "generation" .= fmap valueGeneration generation
      , "container_pixel_height" .= containerHeight
      , "container_pixel_width" .= containerWidth
      , "emu_omit_glimpse_image" .= omitGlimpse
      , "emu_patches_override" .= patchesOverride
      , "lpe_keep_patch_ijhw" .= keepPatch
      , "lpe_delta_encoding_channel" .= deltaChannel
      , "sanitized" .= sanitized
      , "asset_pointer_link" .= assetLink
      , "watermarked_asset_pointer" .= watermarked
      , "is_no_auth_placeholder" .= placeholder
    ]


valueDalle :: Ct.DalleMeta -> Ae.Value
valueDalle (Ct.DalleMeta genId prompt seed parentGenId editOp title) =
  Ae.object [
      "gen_id" .= genId
      , "prompt" .= prompt
      , "seed" .= seed
      , "parent_gen_id" .= parentGenId
      , "edit_op" .= editOp
      , "serialization_title" .= title
    ]


valueGeneration :: Ct.GenMeta -> Ae.Value
valueGeneration (Ct.GenMeta genId size seed parentGenId height width transparent title orientation) =
  Ae.object [
      "gen_id" .= genId
      , "gen_size" .= size
      , "seed" .= seed
      , "parent_gen_id" .= parentGenId
      , "height" .= height
      , "width" .= width
      , "transparent_background" .= transparent
      , "serialization_title" .= title
      , "orientation" .= orientation
    ]


valueAudioMeta :: Ct.AudioMeta -> Ae.Value
valueAudioMeta
    (Ct.AudioMeta startTimestamp endTimestamp pretokenized interruptions originalSource transcription wordTranscription start end) =
  Ae.object [
      "start_timestamp" .= startTimestamp
      , "end_timestamp" .= endTimestamp
      , "pretokenized_vq" .= pretokenized
      , "interruptions" .= interruptions
      , "original_audio_source" .= originalSource
      , "transcription" .= transcription
      , "word_transcription" .= wordTranscription
      , "start" .= start
      , "end" .= end
    ]


valueThought :: Ct.ThoughtRow -> Ae.Value
valueThought (Ct.ThoughtRow summary content chunks finished) =
  Ae.object [
      "summary" .= summary
      , "content" .= content
      , "chunks" .= chunks
      , "finished" .= finished
    ]


assetsFromValue :: Maybe Ae.Value -> Maybe [Ae.Value]
assetsFromValue Nothing = Nothing
assetsFromValue (Just Ae.Null) = Nothing
assetsFromValue (Just (Ae.Array values)) = Just $ toList values
assetsFromValue (Just value) =
  case Ae.fromJSON value of
    Ae.Success assets -> Just assets
    Ae.Error _ -> Just [value]


objectMap :: Ae.Value -> Mp.Map Text Ae.Value
objectMap (Ae.Object object) =
  Mp.fromList $ map (\(key, value) -> (Ak.toText key, value)) $ Km.toList object
objectMap Ae.Null = Mp.empty
objectMap value = Mp.singleton "_opaque" value