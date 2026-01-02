{-# LANGUAGE QuasiQuotes #-}

module OpenAI.Serialize.Statements where

import Data.Text (Text)
import Data.Int (Int32, Int64)
import Data.UUID (UUID)
import Data.Time.Clock (UTCTime)
import qualified Data.Vector as V
import Data.Vector (Vector)

import qualified Data.Aeson as Ae
import Hasql.Statement (Statement)
import qualified Hasql.TH as TH


insertConversation :: Statement (Text, Text,Double, Double) Int64
insertConversation =
  [TH.singletonStatement|
    insert into oai.conversations
      (title, eid, create_time, update_time)
    values
      ($1 :: text, $2 :: text, $3 :: float8, $4 :: float8)
    returning uid :: int8
  |]


insertNodeStmt :: Statement (Int64, Text, Maybe Int64) Int64
insertNodeStmt =
  [TH.singletonStatement|
    insert into oai.nodes
      (conversation_fk, eid, parent_fk)
    values
      ($1 :: int8, $2 :: text, $3 :: int8?)
    returning uid :: int8
  |]


insertMessageStmt :: Statement (Int64, Text, Maybe Double, Maybe Double, Text, Maybe Bool, Double, Ae.Value, Text, Maybe Text) Int64
insertMessageStmt =
  [TH.singletonStatement|
    insert into oai.messages
      (node_fk, eid, create_time, update_time, status, end_turn, weight, metadata, recipient, channel)
    values
      ($1 :: int8, $2 :: text, $3 :: float8?, $4 :: float8?, $5 :: text, $6 :: bool?, $7 :: float8, $8 :: jsonb, $9 :: text, $10 :: text?)
    returning uid :: int8
  |]

insertAuthorStmt :: Statement (Int64, Text, Maybe Text, Ae.Value) ()
insertAuthorStmt =
  [TH.resultlessStatement|
    insert into oai.authors
      (message_fk, role, name, metadata)
    values
      ($1 :: int8, $2 :: text, $3 :: text?, $4 :: jsonb)
  |]

insertContentStmt :: Statement (Int64, Text) Int64
insertContentStmt =
  [TH.singletonStatement|
    insert into oai.contents
      (message_fk, content_type)
    values
      ($1 :: int8, $2 :: text)
    returning uid :: int8
  |]

insertCodeContentStmt :: Statement (Int64, Text, Maybe Text, Text) ()
insertCodeContentStmt =
  [TH.resultlessStatement|
    insert into oai.code_contents
      (content_fk, language, response_format_name, text)
    values
      ($1 :: int8, $2 :: text, $3 :: text?, $4 :: text)
  |]

insertExecutionOutputContentStmt :: Statement (Int64, Text) ()
insertExecutionOutputContentStmt =
  [TH.resultlessStatement|
    insert into oai.execution_output_contents
      (content_fk, text)
    values
      ($1 :: int8, $2 :: text)
  |]


insertModelEditableContextStmt :: Statement (Int64, Text, Maybe Ae.Value, Maybe Ae.Value, Maybe Ae.Value) ()
insertModelEditableContextStmt =
  [TH.resultlessStatement|
    insert into oai.model_editable_context_contents
      (content_fk, model_set_context, repository, repo_summary, structured_context)
    values
      ($1 :: int8, $2 :: text, $3 :: jsonb?, $4 :: jsonb?, $5 :: jsonb?)
  |]

insertReasoningRecapContentStmt :: Statement (Int64, Text) ()
insertReasoningRecapContentStmt =
  [TH.resultlessStatement|
    insert into oai.reasoning_recap_contents
      (content_fk, content)
    values
      ($1 :: int8, $2 :: text)
  |]

insertSystemErrorContentStmt :: Statement (Int64, Text, Text) ()
insertSystemErrorContentStmt =
  [TH.resultlessStatement|
    insert into oai.system_error_contents
      (content_fk, name, text)
    values
      ($1 :: int8, $2 :: text, $3 :: text)
  |]

insertTetherBrowsingDisplayContentStmt :: Statement (Int64, Text, Maybe Ae.Value, Maybe Ae.Value, Maybe Text) ()
insertTetherBrowsingDisplayContentStmt =
  [TH.resultlessStatement|
    insert into oai.tether_browsing_display_contents
      (content_fk, results, summary, assets, tether_id)
    values
    ($1 :: int8, $2 :: text, $3 :: jsonb?, $4 :: jsonb?, $5 :: text?)
  |]

insertTetherQuoteContentStmt :: Statement (Int64, Text, Text, Text, Text, Maybe Text) ()
insertTetherQuoteContentStmt =
  [TH.resultlessStatement|
    insert into oai.tether_quote_contents
      (content_fk, url, domain, text, title, tether_id)
    values
    ($1 :: int8, $2 :: text, $3 :: text, $4 :: text, $5 :: text, $6 :: text?)
  |]


insertTextContentStmt :: Statement (Int64, V.Vector Text) ()
insertTextContentStmt =
  [TH.resultlessStatement|
    insert into oai.text_contents
      (content_fk, parts)
    values
      ($1 :: int8, $2 :: text[])
  |]

insertThoughtsContentStmt :: Statement (Int64, Text) ()
insertThoughtsContentStmt =
  [TH.resultlessStatement|
    insert into oai.thoughts_contents
      (content_fk, source_analysis_msg_id)
    values
      ($1 :: int8, $2 :: text)
  |]

insertThoughtStmt :: Statement (Int64, Text, Text, Ae.Value, Bool) ()
insertThoughtStmt =
  [TH.resultlessStatement|
    insert into oai.thoughts
      (thoughts_content_fk, summary, content, chunks, finished)
    values
      ($1 :: int8, $2 :: text, $3 :: text, $4 :: jsonb, $5 :: bool)
  |]


-- New TH statements for multimodal_text

insertMultiModalPartStmt :: Statement (Int64, Text) Int64
insertMultiModalPartStmt =
  [TH.singletonStatement|
    insert into oai.multimodal_parts
      (content_fk, content_type)
    values
      ($1 :: int8, $2 :: text)
    returning uid :: int8
  |]


insertTextMMPartStmt :: Statement (Int64, Text) ()
insertTextMMPartStmt =
  [TH.resultlessStatement|
    insert into oai.text_mmpart
      (mmpart_fk, text)
    values
      ($1 :: int8, $2 :: text)
  |]


insertImageAssetPointerMMPartStmt :: Statement (Int64, Text, Int64, Int32, Int32, Maybe Ae.Value) Int64
insertImageAssetPointerMMPartStmt =
  [TH.singletonStatement|
    insert into oai.image_asset_pointer_mmpart
      (mmpart_fk, asset_pointer, size_bytes, width, height, fovea)
    values
      ($1 :: int8, $2 :: text, $3 :: int8, $4 :: int4, $5 :: int4, $6 :: jsonb?)
    returning uid :: int8
  |]


insertImageMetadataStmt :: Statement (Int64, Maybe Ae.Value, Maybe Int32, Maybe Int32, Maybe Ae.Value
              , Maybe Ae.Value, Maybe Ae.Value, Maybe Ae.Value, Bool, Maybe Ae.Value, Maybe Ae.Value, Maybe Ae.Value) Int64
insertImageMetadataStmt =
  [TH.singletonStatement|
    insert into oai.metadatas_imgasset
      (imgptr_fk, gizmo, container_pixel_height, container_pixel_width, emu_omit_glimpse_image
        , emu_patches_override, lpe_keep_patch_ijhw, lpe_delta_encoding_channel, sanitized
        , asset_pointer_link, watermarked_asset_pointer, is_no_auth_placeholder)
    values
      ($1 :: int8, $2 :: jsonb?, $3 :: int4?, $4 :: int4?, $5 :: jsonb?
        , $6 :: jsonb?, $7 :: jsonb?, $8 :: jsonb?, $9 :: bool
        , $10 :: jsonb?, $11 :: jsonb?, $12 :: jsonb?)
    returning uid :: int8
  |]


insertDalleStmt :: Statement (Int64, Maybe Text, Text, Maybe Int64, Maybe Text, Maybe Text, Text) ()
insertDalleStmt =
  [TH.resultlessStatement|
    insert into oai.dalles
      (metadata_fk, gen_id, prompt, seed, parent_gen_id, edit_op, serialization_title)
    values
      ($1 :: int8, $2 :: text?, $3 :: text, $4 :: int8?, $5 :: text?, $6 :: text?, $7 :: text)
  |]

insertGenerationStmt :: Statement (Int64,Maybe Text, Text, Maybe Int64, Maybe Text, Int32, Int32, Bool, Text, Maybe Text) ()
insertGenerationStmt =
  [TH.resultlessStatement|
    insert into oai.generations
      (metadata_fk, gen_id, gen_size, seed, parent_gen_id, height, width
        , transparent_background, serialization_title, orientation)
    values
      ($1 :: int8, $2 :: text?, $3 :: text, $4 :: int8?, $5 :: text?, $6 :: int4, $7 :: int4
        , $8 :: bool, $9 :: text, $10 :: text?)
  |]

insertAudioTranscriptionMMPartStmt :: Statement (Int64, Text, Text, Maybe Text) ()
insertAudioTranscriptionMMPartStmt =
  [TH.resultlessStatement|
    insert into oai.audio_transcription_mmpart
      (mmpart_fk, text, direction, decoding_id)
    values
      ($1 :: int8, $2 :: text, $3 :: text, $4 :: text?)
  |]

insertAudioAssetPointerMMPartStmt :: Statement (Int64, Maybe Ae.Value, Text, Int64, Text, Maybe Text) Int64
insertAudioAssetPointerMMPartStmt =
  [TH.singletonStatement|
    insert into oai.audio_asset_pointer_mmpart
      (mmpart_fk, expiry_datetime, asset_pointer, size_bytes, format, tool_audio_direction)
    values ($1 :: int8, $2 :: jsonb?, $3 :: text, $4 :: int8, $5 :: text, $6 :: text?)
    returning uid :: int8
  |]

insertAudioMetadataStmt :: Statement (Int64, Int32
    , Maybe Ae.Value, Maybe Ae.Value, Maybe Ae.Value
    , Maybe Ae.Value, Maybe Ae.Value, Maybe Ae.Value
    , Maybe Ae.Value, Double, Double) ()
insertAudioMetadataStmt =
  [TH.resultlessStatement|
    insert into oai.metadatas_audioasset
      (assetptr_fk, part_kind
        , start_timestamp, end_timestamp, pretokenized_vq
        , interruptions, original_audio_source, transcription
        , word_transcription, start_stamp, end_stamp)
    values
      ($1 :: int8, $2 :: int4
      , $3 :: jsonb?, $4 :: jsonb?, $5 :: jsonb?
      , $6 :: jsonb?, $7 :: jsonb?, $8 :: jsonb?
      , $9 :: jsonb?, $10 :: float8, $11 :: float8)
  |]

insertRealTimeUserAVMMPartStmt :: Statement (Int64, Maybe Ae.Value, Maybe Ae.Value, Maybe Ae.Value, Maybe Double) Int64
insertRealTimeUserAVMMPartStmt =
  [TH.singletonStatement|
    insert into oai.real_time_user_av_mmpart
      (mmpart_fk, expiry_datetime, frames_asset_pointers, video_container_asset_pointer, audio_start_timestamp)
    values ($1 :: int8, $2 :: jsonb?, $3 :: jsonb?, $4 :: jsonb?, $5 :: float8?)
    returning uid :: int8
  |]


--- Discussion Context serialisation:
insertContext :: Statement (Text, Text) (Int64, UUID)
insertContext =
  [TH.singletonStatement|
    insert into oai.discourse
      (title, conversation_eid)
    values
      ($1 :: text, $2 :: text)
    returning uid :: int8, uuid :: uuid
  |]

insertContextIssue :: Statement (Int64, Int32, Text) ()
insertContextIssue =
  [TH.resultlessStatement|
    insert into oai.discourse_issue (context_fk, seq, text)
    values ($1 :: int8, $2 :: int4, $3 :: text)
  |]

insertMessage :: Statement (Int64, Int32, Text, Maybe UTCTime, Maybe UTCTime) Int64
insertMessage =
  [TH.singletonStatement|
    insert into oai.messagefsm (discourse_fk, seq, kind, created_at, updated_at)
    values (
      $1 :: int8,
      $2 :: int4,
      ($3 :: text)::oai.message_kind,
      $4 :: timestamptz?,
      $5 :: timestamptz?
    )
    returning uid :: int8
  |]


insertUserMessage :: Statement (Int64, Text) ()
insertUserMessage =
  [TH.resultlessStatement|
    insert into oai.user_message (message_fk, text)
    values ($1 :: int8, $2 :: text)
  |]

insertResponseAst :: Statement Text Int64
insertResponseAst =
  [TH.singletonStatement|
    insert into oai.response_ast (text)
    values ($1 :: text)
    returning uid :: int8
  |]

insertAssistantMessage :: Statement (Int64, Maybe Int64) ()
insertAssistantMessage =
  [TH.resultlessStatement|
    insert into oai.assistant_message (message_fk, response_fk)
    values ($1 :: int8, $2 :: int8?)
  |]

insertSystemMessage :: Statement (Int64, Text) ()
insertSystemMessage =
  [TH.resultlessStatement|
    insert into oai.system_message (message_fk, text)
    values ($1 :: int8, $2 :: text)
  |]

insertToolMessage :: Statement (Int64, Text) ()
insertToolMessage =
  [TH.resultlessStatement|
    insert into oai.tool_message (message_fk, text)
    values ($1 :: int8, $2 :: text)
  |]

insertUnknownMessage :: Statement (Int64, Text) ()
insertUnknownMessage =
  [TH.resultlessStatement|
    insert into oai.unknown_message (message_fk, text)
    values ($1 :: int8, $2 :: text)
  |]

insertAttachment :: Statement (Int64, Int32, Text) ()
insertAttachment =
  [TH.resultlessStatement|
    insert into oai.message_attachment (message_fk, seq, value)
    values ($1 :: int8, $2 :: int4, $3 :: text)
  |]

insertSubAction :: Statement (Int64, Int32, Text, Maybe Text) Int64
insertSubAction =
  [TH.singletonStatement|
    insert into oai.sub_action (message_fk, seq, kind, text)
    values (
      $1 :: int8,
      $2 :: int4,
      ($3 :: text)::oai.sub_action_kind,
      $4 :: text?
    )
    returning uid :: int8
  |]

insertReflection :: Statement (Int64, Text, Text, Maybe Bool) Int64
insertReflection =
  [TH.singletonStatement|
    insert into oai.reflection (sub_action_fk, summary, content, finished)
    values ($1 :: int8, $2 :: text, $3 :: text, $4 :: bool?)
    returning uid :: int8
  |]

insertReflectionChunk :: Statement (Int64, Int32, Text) ()
insertReflectionChunk =
  [TH.resultlessStatement|
    insert into oai.reflection_chunk (reflection_fk, seq, text)
    values ($1 :: int8, $2 :: int4, $3 :: text)
  |]

insertCode :: Statement (Int64, Text, Maybe Text, Text) ()
insertCode =
  [TH.resultlessStatement|
    insert into oai.code (sub_action_fk, language, format_name, text)
    values ($1 :: int8, $2 :: text, $3 :: text?, $4 :: text)
  |]

insertToolCall :: Statement (Int64, Text, Text) ()
insertToolCall =
  [TH.resultlessStatement|
    insert into oai.tool_call (sub_action_fk, tool_name, tool_input)
    values ($1 :: int8, $2 :: text, $3 :: text)
  |]
