{-# LANGUAGE QuasiQuotes #-}

module OpenAI.Serialize.ContentStmt where

import Data.Aeson (Value)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Vector (Vector)

import Hasql.Statement (Statement)
import qualified Hasql.TH as TH

import qualified OpenAI.Serialize.ConversationStmt as St


-- This module provides the canonical content-writer statement API while the
-- underlying statements remain available from ConversationStmt for legacy
-- callers. Once those callers have migrated, the SQL definitions can move
-- here without changing the content writer API.
insertMessage :: Statement
  ( Int64
  , Text
  , Maybe Double
  , Maybe Double
  , Text
  , Maybe Bool
  , Double
  , Value
  , Text
  , Maybe Text
  , Int32
  )
  Int64
insertMessage =
  [TH.singletonStatement|
    insert into oai.messages
      (node_fk, eid, create_time, update_time, status, end_turn, weight, metadata, recipient, channel, seqnbr)
    values
      ($1 :: int8, $2 :: text, $3 :: float8?, $4 :: float8?, $5 :: text, $6 :: bool?, $7 :: float8, $8 :: jsonb, $9 :: text, $10 :: text?, $11 :: int4)
    returning uid :: int8
  |]

insertAuthor :: Statement (Int64, Text, Maybe Text, Value) ()
insertAuthor =
  [TH.resultlessStatement|
    insert into oai.authors
      (message_fk, role, name, metadata)
    values
      ($1 :: int8, $2 :: text, $3 :: text?, $4 :: jsonb)
  |]

insertContent :: Statement (Int64, Text, Int32) Int64
insertContent =
  [TH.singletonStatement|
    insert into oai.contents
      (message_fk, content_type, seqnbr)
    values
      ($1 :: int8, $2 :: text, $3 :: int4)
    returning uid :: int8
  |]

insertCodeContent :: Statement (Int64, Text, Maybe Text, Text) ()
insertCodeContent =
  [TH.resultlessStatement|
    insert into oai.code_contents
      (content_fk, language, response_format_name, text)
    values
      ($1 :: int8, $2 :: text, $3 :: text?, $4 :: text)
  |]

insertExecutionOutputContent :: Statement (Int64, Text) ()
insertExecutionOutputContent =
  [TH.resultlessStatement|
    insert into oai.execution_output_contents
      (content_fk, text)
    values
      ($1 :: int8, $2 :: text)
  |]

insertModelEditableContextContent :: Statement (Int64, Text, Maybe Value, Maybe Value, Maybe Value) ()
insertModelEditableContextContent =
  [TH.resultlessStatement|
    insert into oai.model_editable_context_contents
      (content_fk, model_set_context, repository, repo_summary, structured_context)
    values
      ($1 :: int8, $2 :: text, $3 :: jsonb?, $4 :: jsonb?, $5 :: jsonb?)
  |]

insertReasoningRecapContent :: Statement (Int64, Text) ()
insertReasoningRecapContent =
  [TH.resultlessStatement|
    insert into oai.reasoning_recap_contents
      (content_fk, content)
    values
      ($1 :: int8, $2 :: text)
  |]

insertSystemErrorContent :: Statement (Int64, Text, Text) ()
insertSystemErrorContent =
  [TH.resultlessStatement|
    insert into oai.system_error_contents
      (content_fk, name, text)
    values
      ($1 :: int8, $2 :: text, $3 :: text)
  |]

insertTetherBrowsingDisplayContent :: Statement (Int64, Text, Maybe Value, Maybe Value, Maybe Text) ()
insertTetherBrowsingDisplayContent =
  [TH.resultlessStatement|
    insert into oai.tether_browsing_display_contents
      (content_fk, results, summary, assets, tether_id)
    values
      ($1 :: int8, $2 :: text, $3 :: jsonb?, $4 :: jsonb?, $5 :: text?)
  |]

insertTetherQuoteContent :: Statement (Int64, Text, Text, Text, Text, Maybe Text) ()
insertTetherQuoteContent =
  [TH.resultlessStatement|
    insert into oai.tether_quote_contents
      (content_fk, url, domain, text, title, tether_id)
    values
      ($1 :: int8, $2 :: text, $3 :: text, $4 :: text, $5 :: text, $6 :: text?)
  |]

insertTextContent :: Statement (Int64, Vector Text) ()
insertTextContent =
  [TH.resultlessStatement|
    insert into oai.text_contents
      (content_fk, parts)
    values
      ($1 :: int8, $2 :: text[])
  |]

insertThoughtsContent :: Statement (Int64, Text) ()
insertThoughtsContent =
  [TH.resultlessStatement|
    insert into oai.thoughts_contents
      (content_fk, source_analysis_msg_id)
    values
      ($1 :: int8, $2 :: text)
  |]

insertThought :: Statement (Int64, Text, Text, Value, Bool, Int32) ()
insertThought =
  [TH.resultlessStatement|
    insert into oai.thoughts
      (thoughts_content_fk, summary, content, chunks, finished, seqnbr)
    values
      ($1 :: int8, $2 :: text, $3 :: text, $4 :: jsonb, $5 :: bool, $6 :: int4)
  |]

insertUnknownContent :: Statement (Int64, Value) ()
insertUnknownContent =
  [TH.resultlessStatement|
    insert into oai.unknown_contents
      (content_fk, opaquevalue)
    values
      ($1 :: int8, $2 :: jsonb)
  |]

insertMultiModalPart :: Statement (Int64, Text, Int32) Int64
insertMultiModalPart =
  [TH.singletonStatement|
    insert into oai.multimodal_parts
      (content_fk, content_type, seqnbr)
    values
      ($1 :: int8, $2 :: text, $3 :: int4)
    returning uid :: int8
  |]

insertTextMMPart :: Statement (Int64, Text) ()
insertTextMMPart =
  [TH.resultlessStatement|
    insert into oai.text_mmpart
      (mmpart_fk, text)
    values
      ($1 :: int8, $2 :: text)
  |]

insertImageAssetPointerMMPart :: Statement (Int64, Text, Int64, Int32, Int32, Maybe Value) Int64
insertImageAssetPointerMMPart =
  [TH.singletonStatement|
    insert into oai.image_asset_pointer_mmpart
      (mmpart_fk, asset_pointer, size_bytes, width, height, fovea)
    values
      ($1 :: int8, $2 :: text, $3 :: int8, $4 :: int4, $5 :: int4, $6 :: jsonb?)
    returning uid :: int8
  |]

insertImageMetadata :: Statement
  ( Int64
  , Maybe Value
  , Maybe Int32
  , Maybe Int32
  , Maybe Value
  , Maybe Value
  , Maybe Value
  , Maybe Value
  , Bool
  , Maybe Value
  , Maybe Value
  , Maybe Value
  )
  Int64
insertImageMetadata =
  [TH.singletonStatement|
    insert into oai.metadatas_imgasset
      (imgptr_fk, gizmo, container_pixel_height, container_pixel_width, emu_omit_glimpse_image,
       emu_patches_override, lpe_keep_patch_ijhw, lpe_delta_encoding_channel, sanitized,
       asset_pointer_link, watermarked_asset_pointer, is_no_auth_placeholder)
    values
      ($1 :: int8, $2 :: jsonb?, $3 :: int4?, $4 :: int4?, $5 :: jsonb?,
       $6 :: jsonb?, $7 :: jsonb?, $8 :: jsonb?, $9 :: bool,
       $10 :: jsonb?, $11 :: jsonb?, $12 :: jsonb?)
    returning uid :: int8
  |]

insertDalle :: Statement (Int64, Maybe Text, Text, Maybe Int64, Maybe Text, Maybe Text, Text) ()
insertDalle =
  [TH.resultlessStatement|
    insert into oai.dalles
      (metadata_fk, gen_id, prompt, seed, parent_gen_id, edit_op, serialization_title)
    values
      ($1 :: int8, $2 :: text?, $3 :: text, $4 :: int8?, $5 :: text?, $6 :: text?, $7 :: text)
  |]

insertGeneration :: Statement (Int64, Maybe Text, Text, Maybe Int64, Maybe Text, Int32, Int32, Bool, Text, Maybe Text) ()
insertGeneration =
  [TH.resultlessStatement|
    insert into oai.generations
      (metadata_fk, gen_id, gen_size, seed, parent_gen_id, height, width,
       transparent_background, serialization_title, orientation)
    values
      ($1 :: int8, $2 :: text?, $3 :: text, $4 :: int8?, $5 :: text?, $6 :: int4, $7 :: int4,
       $8 :: bool, $9 :: text, $10 :: text?)
  |]

insertAudioTranscriptionMMPart :: Statement (Int64, Text, Text, Maybe Text) ()
insertAudioTranscriptionMMPart =
  [TH.resultlessStatement|
    insert into oai.audio_transcription_mmpart
      (mmpart_fk, text, direction, decoding_id)
    values
      ($1 :: int8, $2 :: text, $3 :: text, $4 :: text?)
  |]

insertAudioAssetPointerMMPart :: Statement (Int64, Maybe Value, Text, Int64, Text, Maybe Text) Int64
insertAudioAssetPointerMMPart =
  [TH.singletonStatement|
    insert into oai.audio_asset_pointer_mmpart
      (mmpart_fk, expiry_datetime, asset_pointer, size_bytes, format, tool_audio_direction)
    values
      ($1 :: int8, $2 :: jsonb?, $3 :: text, $4 :: int8, $5 :: text, $6 :: text?)
    returning uid :: int8
  |]

insertAudioMetadata :: Statement
  ( Int64
  , Int32
  , Maybe Value
  , Maybe Value
  , Maybe Value
  , Maybe Value
  , Maybe Value
  , Maybe Value
  , Maybe Value
  , Double
  , Double
  )
  ()
insertAudioMetadata =
  [TH.resultlessStatement|
    insert into oai.metadatas_audioasset
      (assetptr_fk, part_kind,
       start_timestamp, end_timestamp, pretokenized_vq,
       interruptions, original_audio_source, transcription,
       word_transcription, start_stamp, end_stamp)
    values
      ($1 :: int8, $2 :: int4,
       $3 :: jsonb?, $4 :: jsonb?, $5 :: jsonb?,
       $6 :: jsonb?, $7 :: jsonb?, $8 :: jsonb?,
       $9 :: jsonb?, $10 :: float8, $11 :: float8)
  |]

insertRealTimeUserAVMMPart :: Statement (Int64, Maybe Value, Maybe Value, Maybe Value, Maybe Double) Int64
insertRealTimeUserAVMMPart =
  [TH.singletonStatement|
    insert into oai.real_time_user_av_mmpart
      (mmpart_fk, expiry_datetime, frames_asset_pointers, video_container_asset_pointer, audio_start_timestamp)
    values
      ($1 :: int8, $2 :: jsonb?, $3 :: jsonb?, $4 :: jsonb?, $5 :: float8?)
    returning uid :: int8
  |]


-- Aliases used by the newer iterative support logic:

insertCode :: Statement (Int64, Text, Maybe Text, Text) ()
insertCode = insertCodeContent

insertExecOut :: Statement (Int64, Text) ()
insertExecOut = insertExecutionOutputContent

insertModelCtx :: Statement (Int64, Text, Maybe Value, Maybe Value, Maybe Value) ()
insertModelCtx = insertModelEditableContextContent

insertReasoning :: Statement (Int64, Text) ()
insertReasoning = insertReasoningRecapContent

insertSystemErr :: Statement (Int64, Text, Text) ()
insertSystemErr = insertSystemErrorContent

insertTetherBrowse :: Statement (Int64, Text, Maybe Value, Maybe Value, Maybe Text) ()
insertTetherBrowse = insertTetherBrowsingDisplayContent

insertText :: Statement (Int64, Vector Text) ()
insertText = insertTextContent

insertThoughts :: Statement (Int64, Text) ()
insertThoughts = insertThoughtsContent

insertOther :: Statement (Int64, Value) ()
insertOther = insertUnknownContent

insertPart :: Statement (Int64, Text, Int32) Int64
insertPart = insertMultiModalPart

insertTextPart :: Statement (Int64, Text) ()
insertTextPart = insertTextMMPart

insertAudioTransPart :: Statement (Int64, Text, Text, Maybe Text) ()
insertAudioTransPart = insertAudioTranscriptionMMPart

insertAudioAssetPart :: Statement (Int64, Maybe Value, Text, Int64, Text, Maybe Text) Int64
insertAudioAssetPart = insertAudioAssetPointerMMPart

insertImageAssetPart :: Statement (Int64, Text, Int64, Int32, Int32, Maybe Value) Int64
insertImageAssetPart = insertImageAssetPointerMMPart

insertRealtimeAvPart :: Statement (Int64, Maybe Value, Maybe Value, Maybe Value, Maybe Double) Int64
insertRealtimeAvPart = insertRealTimeUserAVMMPart

insertImageMeta :: Statement (Int64, Maybe Value, Maybe Int32, Maybe Int32, Maybe Value, Maybe Value, Maybe Value, Maybe Value,
      Bool, Maybe Value, Maybe Value, Maybe Value) Int64
insertImageMeta = insertImageMetadata

insertAudioMeta :: Statement (Int64, Int32, Maybe Value, Maybe Value, Maybe Value, Maybe Value, Maybe Value, Maybe Value,
      Maybe Value, Double, Double) ()
insertAudioMeta = insertAudioMetadata


-- TODO: Missing logic:

updateMessage :: Statement (Maybe Double, Maybe Double, Text, Maybe Bool, Double, Value, Text, Maybe Text, Int64) ()
updateMessage = undefined


deleteAuthorByMsg :: Statement Int64 ()
deleteAuthorByMsg = undefined


-- This statement relies on ON DELETE CASCADE from oai.contents to every
-- content subtype and from multimodal parent rows to their subtype,
-- metadata, generation, and audio metadata rows.
deleteContentTreeByMsg :: Statement Int64 ()
deleteContentTreeByMsg = undefined
