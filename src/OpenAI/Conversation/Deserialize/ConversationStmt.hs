{-# LANGUAGE QuasiQuotes #-}

module OpenAI.Conversation.Deserialize.ConversationStmt where

import Data.Aeson (Value)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import Hasql.Statement (Statement)
import qualified Hasql.TH as TH


type ConversationRow = (Int64, Text, Text, Double, Double)
type ConvKeyRow = (Int64, Text, Text)
type NodeRow = (Int64, Text, Maybe Int64, Int32, Int32, Int32)
type MessageRow =
  ( Int64, Int64, Text, Maybe Double, Maybe Double, Text, Maybe Bool, Maybe Double, Value, Text, Maybe Text
  , Int64, Text, Maybe Text, Value
  )
type ContentRow = (Int64, Int64, Text)
type NodeSnapRow = (Int64, Text, Maybe Int64, Maybe Text, Int32, Int32, Int32)
type MsgSnapRow =
  ( Int64, Int64, Text, Maybe Double, Maybe Double, Text, Maybe Bool, Maybe Double, Value, Text, Maybe Text, Int32
  )

type ContentSnapRow = (Int64, Int64, Text, Int32, Value)
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
type MmPartRow = (Int64, Int64, Text)
type MmTextRow = (Int64, Text)
type MmAtRow = (Int64, Text, Text, Maybe Text)
type MmImgPtrRow = (Int64, Int64, Text, Int64, Int32, Int32, Maybe Value)

type MmImgMdRow =
  ( Int64, Int64, Maybe Value, Maybe Int32, Maybe Int32, Maybe Value, Maybe Value, Maybe Value
  , Maybe Value, Bool, Maybe Value, Maybe Value, Maybe Value
  )

type DalleRow = (Int64, Maybe Text, Text, Maybe Int32, Maybe Text, Maybe Text, Text)
type GenerationRow = (Int64, Maybe Text, Text, Maybe Int32, Maybe Text, Int32, Int32, Bool, Text, Maybe Text)
type AapRow = (Int64, Int64, Maybe Double, Text, Int64, Text, Maybe Text)
type RtuavRow = (Int64, Int64, Maybe Double, Value, Maybe Value, Maybe Double)
type AudioMetaRow =
  (Int64, Maybe Double, Maybe Double, Maybe Value, Maybe Value, Maybe Value, Maybe Value, Maybe Value, Double, Double)


fetchAllConversationsRows :: Statement () (Vector (Int64, Text))
fetchAllConversationsRows =
  [TH.vectorStatement|
    select
      c.uid :: int8,
      c.title :: text
    from oai.conversations c
    order by c.uid
  |]


selectAllConversationsDetailedRows :: Statement () (Vector ConversationRow)
selectAllConversationsDetailedRows =
  [TH.vectorStatement|
    select
      c.uid :: int8,
      c.eid :: text,
      c.title :: text,
      c.create_time :: float8,
      c.update_time :: float8
    from oai.conversations c
    order by c.eid, c.uid
  |]


fetchAllConversationEids :: Statement () (Vector Text)
fetchAllConversationEids =
  [TH.vectorStatement|
    select
      c.eid :: text
    from oai.conversations c
    order by c.eid
  |]


selectConvKeys :: Statement () (Vector ConvKeyRow)
selectConvKeys =
  [TH.vectorStatement|
    select
      c.uid :: int8,
      c.eid :: text,
      c.title :: text
    from oai.conversations c
    order by c.eid, c.uid
  |]


selectConversationByEid :: Statement Text (Maybe ConversationRow)
selectConversationByEid =
  [TH.maybeStatement|
    select
      c.uid :: int8,
      c.title :: text,
      c.eid :: text,
      c.create_time :: float8,
      c.update_time :: float8
    from oai.conversations c
    where c.eid = $1 :: text
  |]


selectConversationByUid :: Statement Int64 (Maybe ConversationRow)
selectConversationByUid =
  [TH.maybeStatement|
    select
      c.uid :: int8,
      c.title :: text,
      c.eid :: text,
      c.create_time :: float8,
      c.update_time :: float8
    from oai.conversations c
    where c.uid = $1 :: int8
  |]


selectConversationForUpdate :: Statement Text (Maybe (Int64, Text, Double))
selectConversationForUpdate =
  [TH.maybeStatement|
    select
      c.uid :: int8,
      c.title :: text,
      c.update_time :: float8
    from oai.conversations c
    where c.eid = $1 :: text
    for update
  |]


selectNodes :: Statement Int64 (Vector NodeRow)
selectNodes =
  [TH.vectorStatement|
    select
      n.uid :: int8,
      n.eid :: text,
      n.parent_fk :: int8?,
      n.seqnbr :: int4,
      n.child_seq :: int4,
      n.preorder_seq :: int4
    from oai.nodes n
    where n.conversation_fk = $1 :: int8
    order by n.preorder_seq, n.seqnbr, n.uid
  |]


selectNodeByEid :: Statement (Int64, Text) (Maybe (Int64, Int32, Int32, Int32))
selectNodeByEid =
  [TH.maybeStatement|
    select
      n.uid :: int8,
      n.seqnbr :: int4,
      n.child_seq :: int4,
      n.preorder_seq :: int4
    from oai.nodes n
    where n.conversation_fk = $1 :: int8
      and n.eid = $2 :: text
  |]


selectNodeMap :: Statement Int64 (Vector (Text, Int64, Maybe Int64, Int32, Int32, Int32))
selectNodeMap =
  [TH.vectorStatement|
    select
      n.eid :: text,
      n.uid :: int8,
      n.parent_fk :: int8?,
      n.seqnbr :: int4,
      n.child_seq :: int4,
      n.preorder_seq :: int4
    from oai.nodes n
    where n.conversation_fk = $1 :: int8
    order by n.preorder_seq, n.seqnbr, n.uid
  |]


selectMaxNodeSeq :: Statement Int64 Int32
selectMaxNodeSeq =
  [TH.singletonStatement|
    select coalesce(max(n.seqnbr), -1) :: int4
    from oai.nodes n
    where n.conversation_fk = $1 :: int8
  |]


selectNodeUidBySeq :: Statement (Int64, Int32) (Maybe Int64)
selectNodeUidBySeq =
  [TH.maybeStatement|
    select
      n.uid :: int8
    from oai.nodes n
    where n.conversation_fk = $1 :: int8
      and n.seqnbr = $2 :: int4
  |]


selectNodeSnaps :: Statement Int64 (Vector NodeSnapRow)
selectNodeSnaps =
  [TH.vectorStatement|
    select
      n.uid :: int8,
      n.eid :: text,
      n.parent_fk :: int8?,
      p.eid :: text?,
      n.seqnbr :: int4,
      n.child_seq :: int4,
      n.preorder_seq :: int4
    from oai.nodes n
      left join oai.nodes p
        on p.uid = n.parent_fk
        and p.conversation_fk = n.conversation_fk
    where n.conversation_fk = $1 :: int8
    order by n.preorder_seq, n.seqnbr, n.uid
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
      m.weight :: float8?,
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
    order by n.preorder_seq, n.seqnbr, n.uid, m.seqnbr, m.uid
  |]


selectMessageByEid :: Statement (Int64, Text) (Maybe (Int64, Maybe Double, Maybe Double))
selectMessageByEid =
  [TH.maybeStatement|
    select
      m.uid :: int8,
      m.create_time :: float8?,
      m.update_time :: float8?
    from oai.messages m
      join oai.nodes n on n.uid = m.node_fk
    where n.conversation_fk = $1 :: int8
      and m.eid = $2 :: text
  |]


selectMsgSnaps :: Statement Int64 (Vector MsgSnapRow)
selectMsgSnaps =
  [TH.vectorStatement|
    select
      m.uid :: int8,
      n.uid :: int8,
      m.eid :: text,
      m.create_time :: float8?,
      m.update_time :: float8?,
      m.status :: text,
      m.end_turn :: bool?,
      m.weight :: float8?,
      m.metadata :: jsonb,
      m.recipient :: text,
      m.channel :: text?,
      m.seqnbr :: int4
    from oai.nodes n
      join oai.messages m on m.node_fk = n.uid
    where n.conversation_fk = $1 :: int8
    order by n.preorder_seq, n.seqnbr, n.uid, m.seqnbr, m.uid
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
    order by n.preorder_seq, n.seqnbr, n.uid, m.seqnbr, m.uid, c.seqnbr, c.uid
  |]


-- Requires oai.content_snap_v to expose:
-- uid, message_fk, content_type, seqnbr, payload.
-- The payload must be canonical JSONB assembled from the relevant content subtype rows.
selectContentSnaps :: Statement Int64 (Vector ContentSnapRow)
selectContentSnaps =
  [TH.vectorStatement|
    select
      cs.uid :: int8,
      cs.message_fk :: int8,
      cs.content_type :: text,
      cs.seqnbr :: int4,
      cs.payload :: jsonb
    from oai.content_snap_v cs
      join oai.messages m on m.uid = cs.message_fk
      join oai.nodes n on n.uid = m.node_fk
    where n.conversation_fk = $1 :: int8
    order by n.preorder_seq, n.seqnbr, n.uid, m.seqnbr, m.uid, cs.seqnbr, cs.uid
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
    order by n.preorder_seq, n.seqnbr, m.seqnbr, c.seqnbr, c.uid
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
    order by n.preorder_seq, n.seqnbr, m.seqnbr, c.seqnbr, c.uid
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
    order by n.preorder_seq, n.seqnbr, m.seqnbr, c.seqnbr, c.uid
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
    order by n.preorder_seq, n.seqnbr, m.seqnbr, c.seqnbr, c.uid
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
    order by n.preorder_seq, n.seqnbr, m.seqnbr, c.seqnbr, c.uid
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
    order by n.preorder_seq, n.seqnbr, m.seqnbr, c.seqnbr, c.uid
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
    order by n.preorder_seq, n.seqnbr, m.seqnbr, c.seqnbr, c.uid
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
    order by n.preorder_seq, n.seqnbr, m.seqnbr, c.seqnbr, c.uid
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
    order by n.preorder_seq, n.seqnbr, m.seqnbr, c.seqnbr, c.uid
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
    order by n.preorder_seq, n.seqnbr, m.seqnbr, c.seqnbr, t.seqnbr, t.uid
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
    order by n.preorder_seq, n.seqnbr, m.seqnbr, c.seqnbr, c.uid
  |]


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
    order by n.preorder_seq, n.seqnbr, m.seqnbr, c.seqnbr, mp.seqnbr, mp.uid
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
    order by n.preorder_seq, n.seqnbr, m.seqnbr, c.seqnbr, mp.seqnbr, mp.uid
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
    order by n.preorder_seq, n.seqnbr, m.seqnbr, c.seqnbr, mp.seqnbr, mp.uid
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
    order by n.preorder_seq, n.seqnbr, m.seqnbr, c.seqnbr, mp.seqnbr, mp.uid
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
    order by n.preorder_seq, n.seqnbr, m.seqnbr, c.seqnbr, mp.seqnbr, mp.uid, md.uid
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
    order by n.preorder_seq, n.seqnbr, m.seqnbr, c.seqnbr, mp.seqnbr, mp.uid, d.metadata_fk
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
    order by n.preorder_seq, n.seqnbr, m.seqnbr, c.seqnbr, mp.seqnbr, mp.uid, g.metadata_fk
  |]


selectAudioAssetPointerMmParts :: Statement Int64 (Vector AapRow)
selectAudioAssetPointerMmParts =
  [TH.vectorStatement|
    select
      mp.uid :: int8,
      aap.uid :: int8,
      aap.expiry_datetime :: float8?,
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
    order by n.preorder_seq, n.seqnbr, m.seqnbr, c.seqnbr, mp.seqnbr, mp.uid
  |]


selectAudioMetadataForAap :: Statement Int64 (Vector AudioMetaRow)
selectAudioMetadataForAap =
  [TH.vectorStatement|
    select
      ma.assetptr_fk :: int8,
      ma.start_timestamp :: float8?,
      ma.end_timestamp :: float8?,
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
    order by n.preorder_seq, n.seqnbr, m.seqnbr, c.seqnbr, mp.seqnbr, mp.uid, ma.part_kind
  |]


selectRealTimeUserAVMmParts :: Statement Int64 (Vector RtuavRow)
selectRealTimeUserAVMmParts =
  [TH.vectorStatement|
    select
      mp.uid :: int8,
      r.uid :: int8,
      r.expiry_datetime :: float8?,
      r.frames_asset_pointers :: jsonb,
      r.video_container_asset_pointer :: jsonb?,
      r.audio_start_timestamp :: float8?
    from oai.real_time_user_av_mmpart r
      join oai.multimodal_parts mp on mp.uid = r.mmpart_fk
      join oai.contents c on c.uid = mp.content_fk
      join oai.messages m on m.uid = c.message_fk
      join oai.nodes n on n.uid = m.node_fk
    where n.conversation_fk = $1 :: int8
    order by n.preorder_seq, n.seqnbr, m.seqnbr, c.seqnbr, mp.seqnbr, mp.uid
  |]


selectAudioMetadataForRtuav :: Statement Int64 (Vector AudioMetaRow)
selectAudioMetadataForRtuav =
  [TH.vectorStatement|
    select
      ma.assetptr_fk :: int8,
      ma.start_timestamp :: float8?,
      ma.end_timestamp :: float8?,
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
    order by n.preorder_seq, n.seqnbr, m.seqnbr, c.seqnbr, mp.seqnbr, mp.uid, ma.start_stamp
  |]