{-# LANGUAGE QuasiQuotes #-}

module OpenAI.Deserialize.ContentStmt where

import Data.Aeson (Value)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import Hasql.Statement (Statement)
import qualified Hasql.TH as TH


type CRaw = (Int64, Text, Int32)
type PartRaw = (Int64, Text, Int32)
type CodeRaw = (Text, Maybe Text, Text)
type ExecOutRaw = Text
type ModelCtxRaw = (Text, Maybe Value, Maybe Value, Maybe Value)
type ReasoningRaw = Text
type SystemErrRaw = (Text, Text)
type TetherBrowseRaw = (Text, Maybe Value, Maybe Value, Maybe Text)
type TetherQuoteRaw = (Text, Text, Text, Text, Maybe Text)
type TextRaw = Vector Text
type ThoughtsRaw = Text
type ThoughtRaw = (Text, Text, Value, Bool)
type OtherRaw = Value
type TextPartRaw = Text
type AudioTransRaw = (Text, Text, Maybe Text)
type AudioAssetRaw = (Int64, Maybe Value, Text, Int64, Text, Maybe Text)
type ImageAssetRaw = (Int64, Text, Int64, Int32, Int32, Maybe Value)
type RealtimeAvRaw = (Int64, Maybe Value, Value, Maybe Value, Maybe Double)

type ImageMetaRaw = ( Int64, Maybe Value, Maybe Int32, Maybe Int32
  , Maybe Value, Maybe Value, Maybe Value, Maybe Value
  , Bool
  , Maybe Value, Maybe Value, Maybe Value
  )

type AudioMetaRaw = ( Maybe Value, Maybe Value, Maybe Value, Maybe Value
  , Maybe Value, Maybe Value, Maybe Value
  , Double, Double
  )

type DalleRaw = (Maybe Text, Text, Maybe Int64, Maybe Text, Maybe Text, Text)
type GenerationRaw = (Maybe Text, Text, Maybe Int64, Maybe Text, Int32, Int32, Bool, Text, Maybe Text)


selectContentsByMsg :: Statement Int64 (Vector CRaw)
selectContentsByMsg =
  [TH.vectorStatement|
    select
      c.uid :: int8,
      c.content_type :: text,
      c.seqnbr :: int4
    from oai.contents c
    where c.message_fk = $1 :: int8
    order by c.seqnbr, c.uid
  |]


selectCode :: Statement Int64 (Maybe CodeRaw)
selectCode =
  [TH.maybeStatement|
    select
      c.language :: text,
      c.response_format_name :: text?,
      c.text :: text
    from oai.code_contents c
    where c.content_fk = $1 :: int8
  |]


selectExecOut :: Statement Int64 (Maybe ExecOutRaw)
selectExecOut =
  [TH.maybeStatement|
    select c.text :: text
    from oai.execution_output_contents c
    where c.content_fk = $1 :: int8
  |]


selectModelCtx :: Statement Int64 (Maybe ModelCtxRaw)
selectModelCtx =
  [TH.maybeStatement|
    select
      c.model_set_context :: text,
      c.repository :: jsonb?,
      c.repo_summary :: jsonb?,
      c.structured_context :: jsonb?
    from oai.model_editable_context_contents c
    where c.content_fk = $1 :: int8
  |]


selectReasoning :: Statement Int64 (Maybe ReasoningRaw)
selectReasoning =
  [TH.maybeStatement|
    select c.content :: text
    from oai.reasoning_recap_contents c
    where c.content_fk = $1 :: int8
  |]


selectSystemErr :: Statement Int64 (Maybe SystemErrRaw)
selectSystemErr =
  [TH.maybeStatement|
    select
      c.name :: text,
      c.text :: text
    from oai.system_error_contents c
    where c.content_fk = $1 :: int8
  |]


selectTetherBrowse :: Statement Int64 (Maybe TetherBrowseRaw)
selectTetherBrowse =
  [TH.maybeStatement|
    select
      c.result :: text,
      c.summary :: jsonb?,
      c.assets :: jsonb?,
      c.tether_id :: text?
    from oai.tether_browsing_display_contents c
    where c.content_fk = $1 :: int8
  |]


selectTetherQuote :: Statement Int64 (Maybe TetherQuoteRaw)
selectTetherQuote =
  [TH.maybeStatement|
    select
      c.url :: text,
      c.domain :: text,
      c.text :: text,
      c.title :: text,
      c.tether_id :: text?
    from oai.tether_quote_contents c
    where c.content_fk = $1 :: int8
  |]


selectText :: Statement Int64 (Maybe TextRaw)
selectText =
  [TH.maybeStatement|
    select c.parts :: text[]
    from oai.text_contents c
    where c.content_fk = $1 :: int8
  |]


selectThoughts :: Statement Int64 (Maybe ThoughtsRaw)
selectThoughts =
  [TH.maybeStatement|
    select c.source_analysis_msg_id :: text
    from oai.thoughts_contents c
    where c.content_fk = $1 :: int8
  |]


selectThoughtsRaw :: Statement Int64 (Vector ThoughtRaw)
selectThoughtsRaw =
  [TH.vectorStatement|
    select
      t.summary :: text,
      t.content :: text,
      t.chunks :: jsonb,
      t.finished :: bool
    from oai.thoughts t
    where t.content_fk = $1 :: int8
    order by t.seqnbr, t.uid
  |]


selectOther :: Statement Int64 (Maybe OtherRaw)
selectOther =
  [TH.maybeStatement|
    select c.raw :: jsonb
    from oai.unknown_contents c
    where c.content_fk = $1 :: int8
  |]


selectPartsByContent :: Statement Int64 (Vector PartRaw)
selectPartsByContent =
  [TH.vectorStatement|
    select
      p.uid :: int8,
      p.part_type :: text,
      p.seqnbr :: int4
    from oai.multimodal_parts p
    where p.content_fk = $1 :: int8
    order by p.seqnbr, p.uid
  |]


selectTextPart :: Statement Int64 (Maybe TextPartRaw)
selectTextPart =
  [TH.maybeStatement|
    select p.text :: text
    from oai.text_mm_parts p
    where p.mmpart_fk = $1 :: int8
  |]


selectAudioTransPart :: Statement Int64 (Maybe AudioTransRaw)
selectAudioTransPart =
  [TH.maybeStatement|
    select
      p.text :: text,
      p.direction :: text,
      p.decoding_id :: text?
    from oai.audio_transcription_mm_parts p
    where p.mmpart_fk = $1 :: int8
  |]


selectAudioAssetPart :: Statement Int64 (Maybe AudioAssetRaw)
selectAudioAssetPart =
  [TH.maybeStatement|
    select
      p.uid :: int8,
      p.expiry_datetime :: jsonb?,
      p.asset_pointer :: text,
      p.size_bytes :: int8,
      p.format :: text,
      p.tool_audio_direction :: text?
    from oai.audio_asset_pointer_mm_parts p
    where p.mmpart_fk = $1 :: int8
  |]


selectImageAssetPart :: Statement Int64 (Maybe ImageAssetRaw)
selectImageAssetPart =
  [TH.maybeStatement|
    select
      p.uid :: int8,
      p.asset_pointer :: text,
      p.size_bytes :: int8,
      p.width :: int4,
      p.height :: int4,
      p.fovea :: jsonb?
    from oai.image_asset_pointer_mm_parts p
    where p.mmpart_fk = $1 :: int8
  |]


selectRealtimeAvPart :: Statement Int64 (Maybe RealtimeAvRaw)
selectRealtimeAvPart =
  [TH.maybeStatement|
    select
      p.uid :: int8,
      p.expiry_datetime :: jsonb?,
      p.frames_asset_pointers :: jsonb,
      p.video_container_asset_pointer :: jsonb?,
      p.audio_start_timestamp :: float8?
    from oai.real_time_user_av_mmpart p
    where p.mmpart_fk = $1 :: int8
  |]


selectImageMeta :: Statement Int64 (Maybe ImageMetaRaw)
selectImageMeta =
  [TH.maybeStatement|
    select
      m.uid :: int8,
      m.gizmo :: jsonb?,
      m.container_pixel_height :: int4?,
      m.container_pixel_width :: int4?,
      m.emu_omit_glimpse_image :: jsonb?,
      m.emu_patches_override :: jsonb?,
      m.lpe_keep_patch_ijhw :: jsonb?,
      m.lpe_delta_encoding_channel :: jsonb?,
      m.sanitized :: bool,
      m.asset_pointer_link :: jsonb?,
      m.watermarked_asset_pointer :: jsonb?,
      m.is_no_auth_placeholder :: jsonb?
    from oai.image_metadata m
    where m.image_asset_pointer_fk = $1 :: int8
  |]


selectAudioMeta :: Statement Int64 (Maybe AudioMetaRaw)
selectAudioMeta =
  [TH.maybeStatement|
    select
      m.start_timestamp :: jsonb?,
      m.end_timestamp :: jsonb?,
      m.pretokenized_vq :: jsonb?,
      m.interruptions :: jsonb?,
      m.original_audio_source :: jsonb?,
      m.transcription :: jsonb?,
      m.word_transcription :: jsonb?,
      m.start :: float8,
      m.end :: float8
    from oai.audio_metadata m
    where m.audio_asset_pointer_fk = $1 :: int8
    order by m.seqnbr, m.uid
    limit 1
  |]


selectDalle :: Statement Int64 (Maybe DalleRaw)
selectDalle =
  [TH.maybeStatement|
    select
      d.gen_id :: text?,
      d.prompt :: text,
      d.seed :: int8?,
      d.parent_gen_id :: text?,
      d.edit_op :: text?,
      d.serialization_title :: text
    from oai.dalle d
    where d.image_metadata_fk = $1 :: int8
  |]


selectGeneration :: Statement Int64 (Maybe GenerationRaw)
selectGeneration =
  [TH.maybeStatement|
    select
      g.gen_id :: text?,
      g.gen_size :: text,
      g.seed :: int8?,
      g.parent_gen_id :: text?,
      g.height :: int4,
      g.width :: int4,
      g.transparent_background :: bool,
      g.serialization_title :: text,
      g.orientation :: text?
    from oai.generation g
    where g.image_metadata_fk = $1 :: int8
  |]