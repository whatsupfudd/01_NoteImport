{-# LANGUAGE QuasiQuotes #-}

module OpenAI.Deserialize.ConversationStmt where

import Data.Text (Text)
import Data.Int (Int32, Int64)
import Data.UUID (UUID)
import Data.Time.Clock (UTCTime)
import qualified Data.Vector as V
import Data.Vector (Vector)

import qualified Data.Aeson as Ae
import Data.Aeson (Value)

import Hasql.Statement (Statement)
import qualified Hasql.TH as TH


-- -----------------------------
-- Row types
-- -----------------------------

type ConversationRow = (Int64, Text, Text, Double, Double)

type NodeRow = (Int64, Text, Maybe Int64)

-- node_uid, message fields..., author fields...
type MessageRow =
  ( Int64        -- node uid
  , Int64        -- message uid
  , Text         -- message eid
  , Maybe Double -- create_time
  , Maybe Double -- update_time
  , Text         -- status
  , Maybe Bool   -- end_turn
  , Double       -- weight
  , Value        -- metadata
  , Text         -- recipient
  , Maybe Text   -- channel
  , Int64        -- author uid
  , Text         -- author role
  , Maybe Text   -- author name
  , Value        -- author metadata
  )

-- content_uid, message_uid, content_type
type ContentRow = (Int64, Int64, Text)

-- Specialised content rows keyed by content_fk

type CodeRow = (Int64, Text, Maybe Text, Text)

type ExecRow = (Int64, Text)

type MecRow = (Int64, Text, Maybe Value, Maybe Value, Maybe Value)

type RrcRow = (Int64, Text)

type SesRow = (Int64, Text, Text)

type TbdRow = (Int64, Text, Maybe Value, Maybe Value, Maybe Text)

type TqRow = (Int64, Text, Text, Text, Text, Maybe Text)

type TextRow = (Int64, Vector Text)

type ThoughtsHdrRow = (Int64, Text)

type ThoughtRow = (Int64, Text, Text, Value, Bool)

type UnknownRow = (Int64, Value)

-- Multimodal

type MmPartRow = (Int64, Int64, Text) -- mmpart_uid, content_fk, content_type

type MmTextRow = (Int64, Text) -- mmpart_fk, text

type MmAtRow = (Int64, Text, Text, Maybe Text) -- mmpart_fk, text, direction, decoding_id

-- mmpart_uid, imgptr_uid, asset_pointer, size_bytes, width, height, fovea?
type MmImgPtrRow = (Int64, Int64, Text, Int64, Int32, Int32, Maybe Value)

-- imgptr_fk, metadata_uid, gizmo?, cph?, cpw?, emu_omit?, emu_patches?, lpe_keep?, lpe_delta?, sanitized, asset_pointer_link?, watermarked?, is_no_auth?
type MmImgMdRow = (Int64, Int64, Maybe Value, Maybe Int32, Maybe Int32, Maybe Value, Maybe Value, Maybe Value, Maybe Value, Bool, Maybe Value, Maybe Value, Maybe Value)

-- metadata_fk, gen_id?, prompt, seed?, parent_gen_id?, edit_op?, serialization_title
type DalleRow = (Int64, Maybe Text, Text, Maybe Int32, Maybe Text, Maybe Text, Text)

-- metadata_fk, gen_id?, gen_size, seed?, parent_gen_id?, height, width, transparent_background, serialization_title, orientation?
type GenerationRow = (Int64, Maybe Text, Text, Maybe Int32, Maybe Text, Int32, Int32, Bool, Text, Maybe Text)

-- mmpart_uid, aap_uid, expiry_datetime?, asset_pointer, size_bytes, format, tool_audio_direction?
type AapRow = (Int64, Int64, Maybe Value, Text, Int64, Text, Maybe Text)

-- mmpart_uid, rtuav_uid, expiry_datetime?, frames_asset_pointers, video_container_asset_pointer?, audio_start_timestamp?
type RtuavRow = (Int64, Int64, Maybe Value, Value, Maybe Value, Maybe Double)

-- assetptr_fk, start_timestamp?, end_timestamp?, pretokenized_vq?, interruptions?, original_audio_source?, transcription?, word_transcription?, start_stamp, end_stamp
type AudioMetaRow = (Int64, Maybe Value, Maybe Value, Maybe Value, Maybe Value, Maybe Value, Maybe Value, Maybe Value, Double, Double)


fetchAllConversationsRows :: Statement () (Vector (Int64, Text) )
fetchAllConversationsRows =
  [TH.vectorStatement|
    select uid::int8, title::text from oai.conversations
  |]


selectConversationByEid :: Statement Text (Maybe ConversationRow)
selectConversationByEid =
  [TH.maybeStatement|
    select
      d.uid :: int8,
      d.title :: text,
      d.eid :: text,
      d.create_time :: float8,
      d.update_time :: float8
    from oai.conversations d
    where d.eid = $1 :: text
  |]


selectConversationByUid :: Statement Int64 (Maybe ConversationRow)
selectConversationByUid =
  [TH.maybeStatement|
    select
      d.uid :: int8,
      d.title :: text,
      d.eid :: text,
      d.create_time :: float8,
      d.update_time :: float8
    from oai.conversations d
    where d.uid = $1 :: int8
  |]


selectNodes :: Statement Int64 (Vector NodeRow)
selectNodes =
  [TH.vectorStatement|
    select
      n.uid :: int8,
      n.eid :: text,
      n.parent_fk :: int8?
    from oai.nodes n
    where n.conversation_fk = $1 :: int8
    order by n.seqnbr
  |]

selectMessagesWithAuthor :: Statement Int64 (Vector MessageRow)
selectMessagesWithAuthor =
  [TH.vectorStatement|
    select
      n.uid :: int8,
      m.uid :: int8,
      m.eid :: text,
      m.create_time :: float8?,
      m.update_time :: float8?,
      m.status :: text,
      m.end_turn :: bool?,
      m.weight :: float8,
      m.metadata :: jsonb,
      m.recipient :: text,
      m.channel :: text?,
      a.uid :: int8,
      a.role :: text,
      a.name :: text?,
      a.metadata :: jsonb
    from oai.nodes n
      join oai.messages m on m.node_fk = n.uid
      join oai.authors a on a.message_fk = m.uid
    where n.conversation_fk = $1 :: int8
    order by n.seqnbr
  |]

selectContents :: Statement Int64 (Vector ContentRow)
selectContents =
  [TH.vectorStatement|
    select
      c.uid :: int8,
      c.message_fk :: int8,
      c.content_type :: text
    from oai.contents c
      join oai.messages m on m.uid = c.message_fk
      join oai.nodes n on n.uid = m.node_fk
    where n.conversation_fk = $1 :: int8
    order by c.message_fk, c.seqnbr
  |]

selectCodeContents :: Statement Int64 (Vector CodeRow)
selectCodeContents =
  [TH.vectorStatement|
    select
      cc.content_fk :: int8,
      cc.language :: text,
      cc.response_format_name :: text?,
      cc.text :: text
    from oai.code_contents cc
      join oai.contents c on c.uid = cc.content_fk
      join oai.messages m on m.uid = c.message_fk
      join oai.nodes n on n.uid = m.node_fk
    where n.conversation_fk = $1 :: int8
    order by c.message_fk, c.uid
  |]

selectExecutionOutputContents :: Statement Int64 (Vector ExecRow)
selectExecutionOutputContents =
  [TH.vectorStatement|
    select
      eoc.content_fk :: int8,
      eoc.text :: text
    from oai.execution_output_contents eoc
      join oai.contents c on c.uid = eoc.content_fk
      join oai.messages m on m.uid = c.message_fk
      join oai.nodes n on n.uid = m.node_fk
    where n.conversation_fk = $1 :: int8
    order by c.message_fk, c.uid
  |]

selectModelEditableContextContents :: Statement Int64 (Vector MecRow)
selectModelEditableContextContents =
  [TH.vectorStatement|
    select
      mec.content_fk :: int8,
      mec.model_set_context :: text,
      mec.repository :: jsonb?,
      mec.repo_summary :: jsonb?,
      mec.structured_context :: jsonb?
    from oai.model_editable_context_contents mec
      join oai.contents c on c.uid = mec.content_fk
      join oai.messages m on m.uid = c.message_fk
      join oai.nodes n on n.uid = m.node_fk
    where n.conversation_fk = $1 :: int8
    order by c.message_fk, c.uid
  |]

selectReasoningRecapContents :: Statement Int64 (Vector RrcRow)
selectReasoningRecapContents =
  [TH.vectorStatement|
    select
      rrc.content_fk :: int8,
      rrc.content :: text
    from oai.reasoning_recap_contents rrc
      join oai.contents c on c.uid = rrc.content_fk
      join oai.messages m on m.uid = c.message_fk
      join oai.nodes n on n.uid = m.node_fk
    where n.conversation_fk = $1 :: int8
    order by c.message_fk, c.uid
  |]

selectSystemErrorContents :: Statement Int64 (Vector SesRow)
selectSystemErrorContents =
  [TH.vectorStatement|
    select
      sec.content_fk :: int8,
      sec.name :: text,
      sec.text :: text
    from oai.system_error_contents sec
      join oai.contents c on c.uid = sec.content_fk
      join oai.messages m on m.uid = c.message_fk
      join oai.nodes n on n.uid = m.node_fk
    where n.conversation_fk = $1 :: int8
    order by c.message_fk, c.uid
  |]

selectTetherBrowsingDisplayContents :: Statement Int64 (Vector TbdRow)
selectTetherBrowsingDisplayContents =
  [TH.vectorStatement|
    select
      tbd.content_fk :: int8,
      tbd.results :: text,
      tbd.summary :: jsonb?,
      tbd.assets :: jsonb?,
      tbd.tether_id :: text?
    from oai.tether_browsing_display_contents tbd
      join oai.contents c on c.uid = tbd.content_fk
      join oai.messages m on m.uid = c.message_fk
      join oai.nodes n on n.uid = m.node_fk
    where n.conversation_fk = $1 :: int8
    order by c.message_fk, c.uid
  |]

selectTetherQuoteContents :: Statement Int64 (Vector TqRow)
selectTetherQuoteContents =
  [TH.vectorStatement|
    select
      tq.content_fk :: int8,
      tq.url :: text,
      tq.domain :: text,
      tq.text :: text,
      tq.title :: text,
      tq.tether_id :: text?
    from oai.tether_quote_contents tq
      join oai.contents c on c.uid = tq.content_fk
      join oai.messages m on m.uid = c.message_fk
      join oai.nodes n on n.uid = m.node_fk
    where n.conversation_fk = $1 :: int8
    order by c.message_fk, c.uid
  |]

selectTextContents :: Statement Int64 (Vector TextRow)
selectTextContents =
  [TH.vectorStatement|
    select
      tc.content_fk :: int8,
      tc.parts :: text[]
    from oai.text_contents tc
      join oai.contents c on c.uid = tc.content_fk
      join oai.messages m on m.uid = c.message_fk
      join oai.nodes n on n.uid = m.node_fk
    where n.conversation_fk = $1 :: int8
    order by c.message_fk, c.uid
  |]

selectThoughtsContents :: Statement Int64 (Vector ThoughtsHdrRow)
selectThoughtsContents =
  [TH.vectorStatement|
    select
      tc.content_fk :: int8,
      tc.source_analysis_msg_id :: text
    from oai.thoughts_contents tc
      join oai.contents c on c.uid = tc.content_fk
      join oai.messages m on m.uid = c.message_fk
      join oai.nodes n on n.uid = m.node_fk
    where n.conversation_fk = $1 :: int8
    order by c.message_fk, c.uid
  |]

selectThoughts :: Statement Int64 (Vector ThoughtRow)
selectThoughts =
  [TH.vectorStatement|
    select
      t.thoughts_content_fk :: int8,
      t.summary :: text,
      t.content :: text,
      t.chunks :: jsonb,
      t.finished :: bool
    from oai.thoughts t
      join oai.thoughts_contents tc on tc.content_fk = t.thoughts_content_fk
      join oai.contents c on c.uid = tc.content_fk
      join oai.messages m on m.uid = c.message_fk
      join oai.nodes n on n.uid = m.node_fk
    where n.conversation_fk = $1 :: int8
    order by c.message_fk, n.seqnbr, m.seqnbr, c.seqnbr, t.seqnbr
  |]

selectUnknownContents :: Statement Int64 (Vector UnknownRow)
selectUnknownContents =
  [TH.vectorStatement|
    select
      uc.content_fk :: int8,
      uc.opaquevalue :: jsonb
    from oai.unknown_contents uc
      join oai.contents c on c.uid = uc.content_fk
      join oai.messages m on m.uid = c.message_fk
      join oai.nodes n on n.uid = m.node_fk
    where n.conversation_fk = $1 :: int8
    order by c.message_fk, c.uid
  |]

-- Multimodal

selectMultiModalParts :: Statement Int64 (Vector MmPartRow)
selectMultiModalParts =
  [TH.vectorStatement|
    select
      mp.uid :: int8,
      mp.content_fk :: int8,
      mp.content_type :: text
    from oai.multimodal_parts mp
      join oai.contents c on c.uid = mp.content_fk
      join oai.messages m on m.uid = c.message_fk
      join oai.nodes n on n.uid = m.node_fk
    where n.conversation_fk = $1 :: int8
    order by mp.content_fk, mp.seqnbr
  |]

selectTextMmParts :: Statement Int64 (Vector MmTextRow)
selectTextMmParts =
  [TH.vectorStatement|
    select
      t.mmpart_fk :: int8,
      t.text :: text
    from oai.text_mmpart t
      join oai.multimodal_parts mp on mp.uid = t.mmpart_fk
      join oai.contents c on c.uid = mp.content_fk
      join oai.messages m on m.uid = c.message_fk
      join oai.nodes n on n.uid = m.node_fk
    where n.conversation_fk = $1 :: int8
    order by mp.content_fk, mp.uid
  |]

selectAudioTranscriptionMmParts :: Statement Int64 (Vector MmAtRow)
selectAudioTranscriptionMmParts =
  [TH.vectorStatement|
    select
      at.mmpart_fk :: int8,
      at.text :: text,
      at.direction :: text,
      at.decoding_id :: text?
    from oai.audio_transcription_mmpart at
      join oai.multimodal_parts mp on mp.uid = at.mmpart_fk
      join oai.contents c on c.uid = mp.content_fk
      join oai.messages m on m.uid = c.message_fk
      join oai.nodes n on n.uid = m.node_fk
    where n.conversation_fk = $1 :: int8
    order by mp.content_fk, mp.uid
  |]

selectImageAssetPointerMmParts :: Statement Int64 (Vector MmImgPtrRow)
selectImageAssetPointerMmParts =
  [TH.vectorStatement|
    select
      mp.uid :: int8,
      ip.uid :: int8,
      ip.asset_pointer :: text,
      ip.size_bytes :: int8,
      ip.width :: int4,
      ip.height :: int4,
      ip.fovea :: jsonb?
    from oai.image_asset_pointer_mmpart ip
      join oai.multimodal_parts mp on mp.uid = ip.mmpart_fk
      join oai.contents c on c.uid = mp.content_fk
      join oai.messages m on m.uid = c.message_fk
      join oai.nodes n on n.uid = m.node_fk
    where n.conversation_fk = $1 :: int8
    order by mp.content_fk, mp.uid
  |]

selectImageAssetMetadatas :: Statement Int64 (Vector MmImgMdRow)
selectImageAssetMetadatas =
  [TH.vectorStatement|
    select
      md.imgptr_fk :: int8,
      md.uid :: int8,
      md.gizmo :: jsonb?,
      md.container_pixel_height :: int4?,
      md.container_pixel_width :: int4?,
      md.emu_omit_glimpse_image :: jsonb?,
      md.emu_patches_override :: jsonb?,
      md.lpe_keep_patch_ijhw :: jsonb?,
      md.lpe_delta_encoding_channel :: jsonb?,
      md.sanitized :: bool,
      md.asset_pointer_link :: jsonb?,
      md.watermarked_asset_pointer :: jsonb?,
      md.is_no_auth_placeholder :: jsonb?
    from oai.metadatas_imgasset md
      join oai.image_asset_pointer_mmpart ip on ip.uid = md.imgptr_fk
      join oai.multimodal_parts mp on mp.uid = ip.mmpart_fk
      join oai.contents c on c.uid = mp.content_fk
      join oai.messages m on m.uid = c.message_fk
      join oai.nodes n on n.uid = m.node_fk
    where n.conversation_fk = $1 :: int8
    order by mp.content_fk, mp.uid
  |]

selectDalles :: Statement Int64 (Vector DalleRow)
selectDalles =
  [TH.vectorStatement|
    select
      d.metadata_fk :: int8,
      d.gen_id :: text?,
      d.prompt :: text,
      d.seed :: int4?,
      d.parent_gen_id :: text?,
      d.edit_op :: text?,
      d.serialization_title :: text
    from oai.dalles d
      join oai.metadatas_imgasset md on md.uid = d.metadata_fk
      join oai.image_asset_pointer_mmpart ip on ip.uid = md.imgptr_fk
      join oai.multimodal_parts mp on mp.uid = ip.mmpart_fk
      join oai.contents c on c.uid = mp.content_fk
      join oai.messages m on m.uid = c.message_fk
      join oai.nodes n on n.uid = m.node_fk
    where n.conversation_fk = $1 :: int8
    order by mp.content_fk, mp.uid
  |]

selectGenerations :: Statement Int64 (Vector GenerationRow)
selectGenerations =
  [TH.vectorStatement|
    select
      g.metadata_fk :: int8,
      g.gen_id :: text?,
      g.gen_size :: text,
      g.seed :: int4?,
      g.parent_gen_id :: text?,
      g.height :: int4,
      g.width :: int4,
      g.transparent_background :: bool,
      g.serialization_title :: text,
      g.orientation :: text?
    from oai.generations g
      join oai.metadatas_imgasset md on md.uid = g.metadata_fk
      join oai.image_asset_pointer_mmpart ip on ip.uid = md.imgptr_fk
      join oai.multimodal_parts mp on mp.uid = ip.mmpart_fk
      join oai.contents c on c.uid = mp.content_fk
      join oai.messages m on m.uid = c.message_fk
      join oai.nodes n on n.uid = m.node_fk
    where n.conversation_fk = $1 :: int8
    order by mp.content_fk, mp.uid
  |]

selectAudioAssetPointerMmParts :: Statement Int64 (Vector AapRow)
selectAudioAssetPointerMmParts =
  [TH.vectorStatement|
    select
      mp.uid :: int8,
      aap.uid :: int8,
      aap.expiry_datetime :: jsonb?,
      aap.asset_pointer :: text,
      aap.size_bytes :: int8,
      aap.format :: text,
      aap.tool_audio_direction :: text?
    from oai.audio_asset_pointer_mmpart aap
      join oai.multimodal_parts mp on mp.uid = aap.mmpart_fk
      join oai.contents c on c.uid = mp.content_fk
      join oai.messages m on m.uid = c.message_fk
      join oai.nodes n on n.uid = m.node_fk
    where n.conversation_fk = $1 :: int8
    order by mp.content_fk, mp.uid
  |]

selectAudioMetadataForAap :: Statement Int64 (Vector AudioMetaRow)
selectAudioMetadataForAap =
  [TH.vectorStatement|
    select
      ma.assetptr_fk :: int8,
      ma.start_timestamp :: jsonb?,
      ma.end_timestamp :: jsonb?,
      ma.pretokenized_vq :: jsonb?,
      ma.interruptions :: jsonb?,
      ma.original_audio_source :: jsonb?,
      ma.transcription :: jsonb?,
      ma.word_transcription :: jsonb?,
      ma.start_stamp :: float8,
      ma.end_stamp :: float8
    from oai.metadatas_audioasset ma
      join oai.audio_asset_pointer_mmpart aap on aap.uid = ma.assetptr_fk
      join oai.multimodal_parts mp on mp.uid = aap.mmpart_fk
      join oai.contents c on c.uid = mp.content_fk
      join oai.messages m on m.uid = c.message_fk
      join oai.nodes n on n.uid = m.node_fk
    where n.conversation_fk = $1 :: int8
      and ma.part_kind = 1
    order by mp.content_fk, mp.uid
  |]

selectRealTimeUserAVMmParts :: Statement Int64 (Vector RtuavRow)
selectRealTimeUserAVMmParts =
  [TH.vectorStatement|
    select
      mp.uid :: int8,
      r.uid :: int8,
      r.expiry_datetime :: jsonb?,
      r.frames_asset_pointers :: jsonb,
      r.video_container_asset_pointer :: jsonb?,
      r.audio_start_timestamp :: float8?
    from oai.real_time_user_av_mmpart r
      join oai.multimodal_parts mp on mp.uid = r.mmpart_fk
      join oai.contents c on c.uid = mp.content_fk
      join oai.messages m on m.uid = c.message_fk
      join oai.nodes n on n.uid = m.node_fk
    where n.conversation_fk = $1 :: int8
    order by mp.content_fk, mp.uid
  |]

selectAudioMetadataForRtuav :: Statement Int64 (Vector AudioMetaRow)
selectAudioMetadataForRtuav =
  [TH.vectorStatement|
    select
      ma.assetptr_fk :: int8,
      ma.start_timestamp :: jsonb?,
      ma.end_timestamp :: jsonb?,
      ma.pretokenized_vq :: jsonb?,
      ma.interruptions :: jsonb?,
      ma.original_audio_source :: jsonb?,
      ma.transcription :: jsonb?,
      ma.word_transcription :: jsonb?,
      ma.start_stamp :: float8,
      ma.end_stamp :: float8
    from oai.metadatas_audioasset ma
      join oai.real_time_user_av_mmpart r on r.uid = ma.assetptr_fk
      join oai.multimodal_parts mp on mp.uid = r.mmpart_fk
      join oai.contents c on c.uid = mp.content_fk
      join oai.messages m on m.uid = c.message_fk
      join oai.nodes n on n.uid = m.node_fk
    where n.conversation_fk = $1 :: int8
      and ma.part_kind = 2
    order by mp.content_fk, mp.uid
  |]

-- Incremental update statements:

selectConversationForUpdate :: Statement UUID (Maybe (Int64, Text, UTCTime))
selectConversationForUpdate =
  [TH.maybeStatement|
    select
      uid :: int8,
      title :: text,
      update_time :: timestamptz
    from oai.conversations
    where eid = $1 :: uuid
    for update
  |]


selectMaxNodeSeq :: Statement Int64 Int32
selectMaxNodeSeq =
  [TH.singletonStatement|
    select coalesce(max(seqnbr), -1) :: int4
    from oai.nodes
    where conversation_fk = $1 :: int8
  |]

selectNodeUidBySeq :: Statement (Int64, Int32) (Maybe Int64)
selectNodeUidBySeq =
  [TH.maybeStatement|
    select uid :: int8
    from oai.nodes
    where conversation_fk = $1 :: int8
      and seqnbr = $2 :: int4
  |]
