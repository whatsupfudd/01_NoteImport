{-# LANGUAGE QuasiQuotes #-}

module OpenAI.Serialize.ConversationStmt where

import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import Hasql.Statement (Statement)
import qualified Hasql.TH as TH


insertConversation :: Statement (Text, Text, Double, Double) Int64
insertConversation =
  [TH.singletonStatement|
    insert into oai.conversations
      (title, eid, create_time, update_time)
    values
      ($1 :: text, $2 :: text, $3 :: float8, $4 :: float8)
    returning uid :: int8
  |]


insertConversationPrevious :: Statement (Int64, Double, Text) ()
insertConversationPrevious =
  [TH.resultlessStatement|
    insert into oai.conversation_previous
      (conversation_fk, update_time, title)
    values
      ($1 :: int8, $2 :: float8, $3 :: text)
    on conflict (conversation_fk, update_time) do nothing
  |]


updateConversation :: Statement (Text, Double, Int64) ()
updateConversation =
  [TH.resultlessStatement|
    update oai.conversations
    set title = $1 :: text,
        update_time = $2 :: float8
    where uid = $3 :: int8
  |]


insertConversationIngest :: Statement (Int64, Maybe Text, Maybe ByteString, Text) ()
insertConversationIngest =
  [TH.resultlessStatement|
    insert into oai.conversation_ingest
      (conversation_fk, source_key, source_sha256, ingest_type)
    values
      ($1 :: int8, $2 :: text?, $3 :: bytea?, $4 :: text)
    on conflict (conversation_fk, source_sha256) do nothing
  |]


insertNode :: Statement (Int64, Text, Maybe Int64, Int32, Int32, Int32) Int64
insertNode =
  [TH.singletonStatement|
    insert into oai.nodes
      (conversation_fk, eid, parent_fk, seqnbr, child_seq, preorder_seq)
    values
      ($1 :: int8, $2 :: text, $3 :: int8?, $4 :: int4, $5 :: int4, $6 :: int4)
    returning uid :: int8
  |]


updateNodeOrder :: Statement (Maybe Int64, Int32, Int32, Int32, Int64) ()
updateNodeOrder =
  [TH.resultlessStatement|
    update oai.nodes
    set parent_fk = $1 :: int8?,
        seqnbr = $2 :: int4,
        child_seq = $3 :: int4,
        preorder_seq = $4 :: int4
    where uid = $5 :: int8
  |]


insertMessagePrevious :: Statement (Int64, Maybe Double, Maybe Double, ByteString, Value) ()
insertMessagePrevious =
  [TH.resultlessStatement|
    insert into oai.message_previous
      (message_fk, create_time, update_time, content_hash, payload)
    values
      ($1 :: int8, $2 :: float8?, $3 :: float8?, $4 :: bytea, $5 :: jsonb)
  |]


selectMessagePayload :: Statement Int64 Value
selectMessagePayload =
  [TH.singletonStatement|
    select jsonb_build_object(
      'message', to_jsonb(m),

      'authors', coalesce(
        (
          select jsonb_agg(to_jsonb(a) order by a.uid)
          from oai.authors a
          where a.message_fk = m.uid
        ),
        '[]'::jsonb
      ),

      'contents', coalesce(
        (
          select jsonb_agg(to_jsonb(c) order by c.seqnbr, c.uid)
          from oai.contents c
          where c.message_fk = m.uid
        ),
        '[]'::jsonb
      ),

      'code_contents', coalesce(
        (
          select jsonb_agg(to_jsonb(x) order by c.seqnbr, c.uid)
          from oai.code_contents x
          join oai.contents c on c.uid = x.content_fk
          where c.message_fk = m.uid
        ),
        '[]'::jsonb
      ),

      'execution_output_contents', coalesce(
        (
          select jsonb_agg(to_jsonb(x) order by c.seqnbr, c.uid)
          from oai.execution_output_contents x
          join oai.contents c on c.uid = x.content_fk
          where c.message_fk = m.uid
        ),
        '[]'::jsonb
      ),

      'model_editable_context_contents', coalesce(
        (
          select jsonb_agg(to_jsonb(x) order by c.seqnbr, c.uid)
          from oai.model_editable_context_contents x
          join oai.contents c on c.uid = x.content_fk
          where c.message_fk = m.uid
        ),
        '[]'::jsonb
      ),

      'reasoning_recap_contents', coalesce(
        (
          select jsonb_agg(to_jsonb(x) order by c.seqnbr, c.uid)
          from oai.reasoning_recap_contents x
          join oai.contents c on c.uid = x.content_fk
          where c.message_fk = m.uid
        ),
        '[]'::jsonb
      ),

      'system_error_contents', coalesce(
        (
          select jsonb_agg(to_jsonb(x) order by c.seqnbr, c.uid)
          from oai.system_error_contents x
          join oai.contents c on c.uid = x.content_fk
          where c.message_fk = m.uid
        ),
        '[]'::jsonb
      ),

      'tether_browsing_display_contents', coalesce(
        (
          select jsonb_agg(to_jsonb(x) order by c.seqnbr, c.uid)
          from oai.tether_browsing_display_contents x
          join oai.contents c on c.uid = x.content_fk
          where c.message_fk = m.uid
        ),
        '[]'::jsonb
      ),

      'tether_quote_contents', coalesce(
        (
          select jsonb_agg(to_jsonb(x) order by c.seqnbr, c.uid)
          from oai.tether_quote_contents x
          join oai.contents c on c.uid = x.content_fk
          where c.message_fk = m.uid
        ),
        '[]'::jsonb
      ),

      'text_contents', coalesce(
        (
          select jsonb_agg(to_jsonb(x) order by c.seqnbr, c.uid)
          from oai.text_contents x
          join oai.contents c on c.uid = x.content_fk
          where c.message_fk = m.uid
        ),
        '[]'::jsonb
      ),

      'thoughts_contents', coalesce(
        (
          select jsonb_agg(to_jsonb(x) order by c.seqnbr, c.uid)
          from oai.thoughts_contents x
          join oai.contents c on c.uid = x.content_fk
          where c.message_fk = m.uid
        ),
        '[]'::jsonb
      ),

      'thoughts', coalesce(
        (
          select jsonb_agg(to_jsonb(x) order by c.seqnbr, x.seqnbr)
          from oai.thoughts x
          join oai.thoughts_contents h on h.content_fk = x.thoughts_content_fk
          join oai.contents c on c.uid = h.content_fk
          where c.message_fk = m.uid
        ),
        '[]'::jsonb
      ),

      'unknown_contents', coalesce(
        (
          select jsonb_agg(to_jsonb(x) order by c.seqnbr, c.uid)
          from oai.unknown_contents x
          join oai.contents c on c.uid = x.content_fk
          where c.message_fk = m.uid
        ),
        '[]'::jsonb
      ),

      'multimodal_parts', coalesce(
        (
          select jsonb_agg(to_jsonb(p) order by c.seqnbr, p.seqnbr, p.uid)
          from oai.multimodal_parts p
          join oai.contents c on c.uid = p.content_fk
          where c.message_fk = m.uid
        ),
        '[]'::jsonb
      ),

      'text_mmpart', coalesce(
        (
          select jsonb_agg(to_jsonb(x) order by c.seqnbr, p.seqnbr, p.uid)
          from oai.text_mmpart x
          join oai.multimodal_parts p on p.uid = x.mmpart_fk
          join oai.contents c on c.uid = p.content_fk
          where c.message_fk = m.uid
        ),
        '[]'::jsonb
      ),

      'audio_transcription_mmpart', coalesce(
        (
          select jsonb_agg(to_jsonb(x) order by c.seqnbr, p.seqnbr, p.uid)
          from oai.audio_transcription_mmpart x
          join oai.multimodal_parts p on p.uid = x.mmpart_fk
          join oai.contents c on c.uid = p.content_fk
          where c.message_fk = m.uid
        ),
        '[]'::jsonb
      ),

      'image_asset_pointer_mmpart', coalesce(
        (
          select jsonb_agg(to_jsonb(x) order by c.seqnbr, p.seqnbr, p.uid)
          from oai.image_asset_pointer_mmpart x
          join oai.multimodal_parts p on p.uid = x.mmpart_fk
          join oai.contents c on c.uid = p.content_fk
          where c.message_fk = m.uid
        ),
        '[]'::jsonb
      ),

      'image_metadatas', coalesce(
        (
          select jsonb_agg(to_jsonb(x) order by c.seqnbr, p.seqnbr, p.uid)
          from oai.metadatas_imgasset x
          join oai.image_asset_pointer_mmpart i on i.uid = x.imgptr_fk
          join oai.multimodal_parts p on p.uid = i.mmpart_fk
          join oai.contents c on c.uid = p.content_fk
          where c.message_fk = m.uid
        ),
        '[]'::jsonb
      ),

      'dalles', coalesce(
        (
          select jsonb_agg(to_jsonb(x) order by c.seqnbr, p.seqnbr, p.uid)
          from oai.dalles x
          join oai.metadatas_imgasset md on md.uid = x.metadata_fk
          join oai.image_asset_pointer_mmpart i on i.uid = md.imgptr_fk
          join oai.multimodal_parts p on p.uid = i.mmpart_fk
          join oai.contents c on c.uid = p.content_fk
          where c.message_fk = m.uid
        ),
        '[]'::jsonb
      ),

      'generations', coalesce(
        (
          select jsonb_agg(to_jsonb(x) order by c.seqnbr, p.seqnbr, p.uid)
          from oai.generations x
          join oai.metadatas_imgasset md on md.uid = x.metadata_fk
          join oai.image_asset_pointer_mmpart i on i.uid = md.imgptr_fk
          join oai.multimodal_parts p on p.uid = i.mmpart_fk
          join oai.contents c on c.uid = p.content_fk
          where c.message_fk = m.uid
        ),
        '[]'::jsonb
      ),

      'audio_asset_pointer_mmpart', coalesce(
        (
          select jsonb_agg(to_jsonb(x) order by c.seqnbr, p.seqnbr, p.uid)
          from oai.audio_asset_pointer_mmpart x
          join oai.multimodal_parts p on p.uid = x.mmpart_fk
          join oai.contents c on c.uid = p.content_fk
          where c.message_fk = m.uid
        ),
        '[]'::jsonb
      ),

      'real_time_user_av_mmpart', coalesce(
        (
          select jsonb_agg(to_jsonb(x) order by c.seqnbr, p.seqnbr, p.uid)
          from oai.real_time_user_av_mmpart x
          join oai.multimodal_parts p on p.uid = x.mmpart_fk
          join oai.contents c on c.uid = p.content_fk
          where c.message_fk = m.uid
        ),
        '[]'::jsonb
      ),

      'audio_metadatas', coalesce(
        (
          select jsonb_agg(to_jsonb(x) order by x.assetptr_fk, x.part_kind)
          from oai.metadatas_audioasset x
          where exists (
            select 1
            from oai.audio_asset_pointer_mmpart a
            join oai.multimodal_parts p on p.uid = a.mmpart_fk
            join oai.contents c on c.uid = p.content_fk
            where a.uid = x.assetptr_fk
              and c.message_fk = m.uid
          )
          or exists (
            select 1
            from oai.real_time_user_av_mmpart r
            join oai.multimodal_parts p on p.uid = r.mmpart_fk
            join oai.contents c on c.uid = p.content_fk
            where r.uid = x.assetptr_fk
              and c.message_fk = m.uid
          )
        ),
        '[]'::jsonb
      )
    ) :: jsonb
    from oai.messages m
    where m.uid = $1 :: int8
  |]


-- Canonical message/content statements now live in
-- OpenAI.Serialize.ContentStmt. These aliases preserve the previous
-- ConversationStmt API while call sites migrate to the dedicated module.
{- Deprecated, use OpenAI.Serialize.ContentStmt instead.
insertMessage :: Statement (Int64, Text, Maybe Double, Maybe Double, Text, Maybe Bool, Double, Value, Text, Maybe Text, Int32) Int64
insertMessage = Cs.insertMessage


updateMessage :: Statement (Maybe Double, Maybe Double, Text, Maybe Bool, Double, Value, Text, Maybe Text, Int64) ()
updateMessage = Cs.updateMessage


deleteAuthorByMsg :: Statement Int64 ()
deleteAuthorByMsg = Cs.deleteAuthorByMsg


deleteContentsByMsg :: Statement Int64 ()
deleteContentsByMsg = Cs.deleteContentTreeByMsg


deleteContentTreeByMsg :: Statement Int64 ()
deleteContentTreeByMsg = Cs.deleteContentTreeByMsg


insertAuthor :: Statement (Int64, Text, Maybe Text, Value) ()
insertAuthor = Cs.insertAuthor


insertContent :: Statement (Int64, Text, Int32) Int64
insertContent = Cs.insertContent


insertCodeContent :: Statement (Int64, Text, Maybe Text, Text) ()
insertCodeContent = Cs.insertCode


insertExecutionOutputContent :: Statement (Int64, Text) ()
insertExecutionOutputContent = Cs.insertExecOut


insertModelEditableContextContent :: Statement (Int64, Text, Maybe Value, Maybe Value, Maybe Value) ()
insertModelEditableContextContent = Cs.insertModelCtx


insertReasoningRecapContent :: Statement (Int64, Text) ()
insertReasoningRecapContent = Cs.insertReasoning


insertSystemErrorContent :: Statement (Int64, Text, Text) ()
insertSystemErrorContent = Cs.insertSystemErr


insertTetherBrowsingDisplayContent :: Statement (Int64, Text, Maybe Value, Maybe Value, Maybe Text) ()
insertTetherBrowsingDisplayContent = Cs.insertTetherBrowse


insertTetherQuoteContent :: Statement (Int64, Text, Text, Text, Text, Maybe Text) ()
insertTetherQuoteContent = Cs.insertTetherQuote


insertTextContent :: Statement (Int64, Vector Text) ()
insertTextContent = Cs.insertText


insertThoughtsContent :: Statement (Int64, Text) ()
insertThoughtsContent = Cs.insertThoughts


insertThought :: Statement (Int64, Text, Text, Value, Bool, Int32) ()
insertThought = Cs.insertThought


insertUnknownContent :: Statement (Int64, Value) ()
insertUnknownContent = Cs.insertOther


insertMultiModalPart :: Statement (Int64, Text, Int32) Int64
insertMultiModalPart = Cs.insertPart


insertTextMMPart :: Statement (Int64, Text) ()
insertTextMMPart = Cs.insertTextPart


insertImageAssetPointerMMPart :: Statement (Int64, Text, Int64, Int32, Int32, Maybe Value) Int64
insertImageAssetPointerMMPart = Cs.insertImageAssetPart


insertImageMetadata :: Statement (Int64, Maybe Value, Maybe Int32, Maybe Int32, Maybe Value, Maybe Value, Maybe Value, Maybe Value,
      Bool, Maybe Value, Maybe Value, Maybe Value) Int64
insertImageMetadata = Cs.insertImageMeta


insertDalle :: Statement (Int64, Maybe Text, Text, Maybe Int64, Maybe Text, Maybe Text, Text) ()
insertDalle = Cs.insertDalle


insertGeneration :: Statement (Int64, Maybe Text, Text, Maybe Int64, Maybe Text, Int32, Int32, Bool, Text, Maybe Text) ()
insertGeneration = Cs.insertGeneration


insertAudioTranscriptionMMPart :: Statement (Int64, Text, Text, Maybe Text) ()
insertAudioTranscriptionMMPart = Cs.insertAudioTransPart


insertAudioAssetPointerMMPart :: Statement (Int64, Maybe Value, Text, Int64, Text, Maybe Text) Int64
insertAudioAssetPointerMMPart = Cs.insertAudioAssetPart


insertAudioMetadata :: Statement (Int64, Int32, Maybe Value, Maybe Value, Maybe Value, Maybe Value, Maybe Value, Maybe Value,
      Maybe Value, Double, Double) ()
insertAudioMetadata = Cs.insertAudioMeta


insertRealTimeUserAVMMPart :: Statement (Int64, Maybe Value, Maybe Value, Maybe Value, Maybe Double) Int64
insertRealTimeUserAVMMPart = Cs.insertRealtimeAvPart


-- Older return-oriented and *Stmt names remain available until all serializer
-- call sites use the canonical action-first names.

insertNodeStmt :: Statement (Int64, Text, Maybe Int64, Int32, Int32, Int32) Int64
insertNodeStmt = insertNode


insertNodeRetUid :: Statement (Int64, Text, Maybe Int64, Int32, Int32, Int32) Int64
insertNodeRetUid = insertNode


insertMessageStmt :: Statement (Int64, Text, Maybe Double, Maybe Double, Text, Maybe Bool, Double, Value, Text, Maybe Text, Int32) Int64
insertMessageStmt = Cs.insertMessage


insertMessageRetUid :: Statement (Int64, Text, Maybe Double, Maybe Double, Text, Maybe Bool, Double, Value, Text, Maybe Text, Int32) Int64
insertMessageRetUid = Cs.insertMessage


insertAuthorStmt :: Statement (Int64, Text, Maybe Text, Value) ()
insertAuthorStmt = Cs.insertAuthor


insertContentStmt :: Statement (Int64, Text, Int32) Int64
insertContentStmt = Cs.insertContent


insertContentRetUid :: Statement (Int64, Text, Int32) Int64
insertContentRetUid = Cs.insertContent


insertCodeContentStmt :: Statement (Int64, Text, Maybe Text, Text) ()
insertCodeContentStmt = Cs.insertCode


insertExecutionOutputContentStmt :: Statement (Int64, Text) ()
insertExecutionOutputContentStmt = Cs.insertExecOut


insertModelEditableContextStmt :: Statement (Int64, Text, Maybe Value, Maybe Value, Maybe Value) ()
insertModelEditableContextStmt = Cs.insertModelCtx


insertReasoningRecapContentStmt :: Statement (Int64, Text) ()
insertReasoningRecapContentStmt = Cs.insertReasoning


insertSystemErrorContentStmt :: Statement (Int64, Text, Text) ()
insertSystemErrorContentStmt = Cs.insertSystemErr


insertTetherBrowsingDisplayContentStmt :: Statement (Int64, Text, Maybe Value, Maybe Value, Maybe Text) ()
insertTetherBrowsingDisplayContentStmt = Cs.insertTetherBrowse


insertTetherQuoteContentStmt :: Statement (Int64, Text, Text, Text, Text, Maybe Text) ()
insertTetherQuoteContentStmt = Cs.insertTetherQuote


insertTextContentStmt :: Statement (Int64, Vector Text) ()
insertTextContentStmt = Cs.insertText


insertThoughtsContentStmt :: Statement (Int64, Text) ()
insertThoughtsContentStmt = Cs.insertThoughts


insertThoughtStmt :: Statement (Int64, Text, Text, Value, Bool, Int32) ()
insertThoughtStmt = Cs.insertThought


insertUnknownContentStmt :: Statement (Int64, Value) ()
insertUnknownContentStmt = Cs.insertOther


insertMultiModalPartStmt :: Statement (Int64, Text, Int32) Int64
insertMultiModalPartStmt = Cs.insertPart


insertTextMMPartStmt :: Statement (Int64, Text) ()
insertTextMMPartStmt = Cs.insertTextPart


insertImageAssetPointerMMPartStmt :: Statement (Int64, Text, Int64, Int32, Int32, Maybe Value) Int64
insertImageAssetPointerMMPartStmt = Cs.insertImageAssetPart


insertImageMetadataStmt :: Statement (Int64, Maybe Value, Maybe Int32, Maybe Int32, Maybe Value, Maybe Value, Maybe Value, Maybe Value,
      Bool, Maybe Value, Maybe Value, Maybe Value) Int64
insertImageMetadataStmt = Cs.insertImageMeta


insertDalleStmt :: Statement (Int64, Maybe Text, Text, Maybe Int64, Maybe Text, Maybe Text, Text) ()
insertDalleStmt = Cs.insertDalle


insertGenerationStmt :: Statement (Int64, Maybe Text, Text, Maybe Int64, Maybe Text, Int32, Int32, Bool, Text, Maybe Text) ()
insertGenerationStmt = Cs.insertGeneration


insertAudioTranscriptionMMPartStmt :: Statement (Int64, Text, Text, Maybe Text) ()
insertAudioTranscriptionMMPartStmt = Cs.insertAudioTransPart


insertAudioAssetPointerMMPartStmt :: Statement (Int64, Maybe Value, Text, Int64, Text, Maybe Text) Int64
insertAudioAssetPointerMMPartStmt = Cs.insertAudioAssetPart


insertAudioMetadataStmt :: Statement (Int64, Int32, Maybe Value, Maybe Value, Maybe Value, Maybe Value, Maybe Value, Maybe Value,
      Maybe Value, Double, Double) ()
insertAudioMetadataStmt = Cs.insertAudioMeta


insertRealTimeUserAVMMPartStmt :: Statement (Int64, Maybe Value, Maybe Value, Maybe Value, Maybe Double) Int64
insertRealTimeUserAVMMPartStmt = Cs.insertRealtimeAvPart
-}