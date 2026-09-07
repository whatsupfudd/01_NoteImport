{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module OpenAI.Delta.Snap (
  ConvSnap(..), NodeSnap(..), MsgSnap(..), ContentSnap(..), fromJson, fromDb
) where

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as Lbs
import qualified Data.ByteString.Builder as BB
import Data.Int (Int32, Int64)
import Data.List (group, sort, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Mp
import Data.Maybe (catMaybes, fromMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Tx
import qualified Data.Vector as V

import Data.Aeson ((.=))
import qualified Data.Aeson as Ae
import qualified Data.Aeson.Encoding as Ae
import qualified Data.Aeson.Key as Ak
import qualified Data.Aeson.KeyMap as Km

import qualified Crypto.Hash as Ch

import qualified OpenAI.Conversation as Cv
import OpenAI.Delta.Types (Conflict(..), Hash(..))
import qualified OpenAI.Conversation.Json.Schema as Jd
import qualified OpenAI.Conversation.Json.V1.Schema as Jv1
import qualified OpenAI.Conversation.Json.Node.Order as Oor
import qualified OpenAI.Conversation.Json.Node as Nd


data ConvSnap = ConvSnap {
    eidConv :: Text
    , uidConv :: Maybe Int64
    , titleConv :: Text
    , timeCreateCv :: Scientific
    , timeUpdateCv :: Scientific
    , nodes :: [NodeSnap]
  }
  deriving stock (Eq, Show)


data NodeSnap = NodeSnap {
    eidNode :: Text
    , uidNode :: Maybe Int64
    , eidParent :: Maybe Text
    , uidParent :: Maybe Int64
    , seqNode :: Int32
    , seqChild :: Int32
    , seqPre :: Int32
    , msg :: Maybe MsgSnap
    , hashNode :: Hash
  }
  deriving stock (Eq, Show)


data MsgSnap = MsgSnap {
    eidMsg :: Text
    , uidMsg :: Maybe Int64
    , timeCreate :: Maybe Scientific
    , timeUpdate :: Maybe Scientific
    , status :: Text
    , endTurn :: Maybe Bool
    , weight :: Maybe Scientific
    , metadata :: Ae.Value
    , recipient :: Text
    , channel :: Maybe Text
    , contents :: [ContentSnap]
    , hashMsg :: Hash
  }
  deriving stock (Eq, Show)


data ContentSnap = ContentSnap {
    uidContent :: Maybe Int64
    , seqContent :: Int32
    , typeContent :: Text
    , payload :: Ae.Value
    , hashContent :: Hash
  }
  deriving stock (Eq, Show)


fromJson :: Jv1.Conversation -> Either [Conflict] ConvSnap
fromJson conv =
  case Oor.buildNodeOrd conv.nodeMapCv of
    Left issues ->
      Left $ map ordConflict issues

    Right ords ->
      let
        issues0 = topConflictsJs conv <> nodeKeyConflictsJs conv.nodeMapCv <> msgDupConflictsJs conv.nodeMapCv
        ordMap = Mp.fromList [(ord.eidNode, ord) | ord <- ords]
        nodeRez = map (nodeSnapJs conv.nodeMapCv ordMap) ords
      in
      case collectE $ map leftRightMerge nodeRez of
        Left issues1 ->
          Left $ issues0 <> issues1
        Right nodeSnaps ->
          if null issues0
            then
              Right ConvSnap {
                  eidConv = conv.convIdCv
                  , uidConv = Nothing
                  , titleConv = conv.titleCv
                  , timeCreateCv = conv.createTimeCv
                  , timeUpdateCv = conv.updateTimeCv
                  , nodes = nodeSnaps
                }
            else Left issues0
  where
  -- TODO: implement a proper left/right merge
  leftRightMerge :: Either [Conflict] NodeSnap -> Either [Conflict] NodeSnap
  leftRightMerge = id


fromDb :: Cv.ConversationDb -> Either [Conflict] ConvSnap
fromDb conv =
  let
    issues0 = topConflictsDb conv <> nodeKeyConflictsDb conv.nodesCv <> msgDupConflictsDb conv.nodesCv
    nodesDb = sortOn (\nd -> (nd.seqPreNd, nd.seqNodeNd, nd.uidNd)) $ Mp.elems conv.nodesCv
    eidByUid = Mp.fromList [(nd.uidNd, nd.eidNd) | nd <- nodesDb]
    nodeRez = map (nodeSnapDb eidByUid) nodesDb
  in
  case collectE nodeRez of
    Left issues1 ->
      Left $ issues0 <> issues1

    Right nodeSnaps ->
      if null issues0
        then
          Right ConvSnap {
              eidConv = conv.eidCv
              , uidConv = Just conv.uidCv
              , titleConv = conv.titleCv
              , timeCreateCv = conv.createTimeCv
              , timeUpdateCv = conv.updateTimeCv
              , nodes = nodeSnaps
            }
        else Left issues0


topConflictsJs :: Jv1.Conversation -> [Conflict]
topConflictsJs conv =
  finiteConflict "conversation.create_time" conv.createTimeCv
    <> finiteConflict "conversation.update_time" conv.updateTimeCv
    <> emptyTxtConflict "conversation.eid" conv.convIdCv


topConflictsDb :: Cv.ConversationDb -> [Conflict]
topConflictsDb conv =
  finiteConflict "conversation.create_time" conv.createTimeCv
    <> finiteConflict "conversation.update_time" conv.updateTimeCv
    <> emptyTxtConflict "conversation.eid" conv.eidCv


nodeKeyConflictsJs :: Map Text Nd.Node -> [Conflict]
nodeKeyConflictsJs mapping =
  [ BrokenShapeC $
      "json node key/id mismatch: key=" <> eidKey <> ", id=" <> node.idNd
  | (eidKey, node) <- Mp.toList mapping
  , eidKey /= node.idNd
  ]


nodeKeyConflictsDb :: Map Text Cv.NodeDb -> [Conflict]
nodeKeyConflictsDb mapping =
  [ BrokenShapeC $
      "db node key/id mismatch: key=" <> eidKey <> ", id=" <> node.eidNd
  | (eidKey, node) <- Mp.toList mapping
  , eidKey /= node.eidNd
  ]


msgDupConflictsJs :: Map Text Nd.Node -> [Conflict]
msgDupConflictsJs mapping =
  map DuplicateEidC $ dupTxts $ catMaybes [(.idMsg) <$> node.messageNd | node <- Mp.elems mapping]


msgDupConflictsDb :: Map Text Cv.NodeDb -> [Conflict]
msgDupConflictsDb mapping =
  map DuplicateEidC $ dupTxts $ catMaybes [(.eidMsg) <$> node.messageNd | node <- Mp.elems mapping]


nodeSnapJs :: Map Text Nd.Node -> Map Text Oor.NodeOrd -> Oor.NodeOrd -> Either [Conflict] NodeSnap
nodeSnapJs mapping ordMap ord =
  case Mp.lookup ord.eidNode mapping of
    Nothing ->
      Left [MissingJsonNodeC ord.eidNode]

    Just node ->
      let
        issues0 =
          parentConflictJs node ord
            <> maybe [] (const []) (Mp.lookup ord.eidNode ordMap)
        msgRez = maybe (Right Nothing) (fmap Just . msgSnapJs) node.messageNd
      in
      case msgRez of
        Left issues1 ->
          Left $ issues0 <> issues1

        Right msgSnap ->
          if null issues0
            then
              let
                hashNode0 = hashNodeVal ord.eidNode ord.eidParent Nothing ord.seqNode ord.seqChild ord.seqPre msgSnap
              in
              Right NodeSnap {
                  eidNode = ord.eidNode
                  , uidNode = Nothing
                  , eidParent = ord.eidParent
                  , uidParent = Nothing
                  , seqNode = ord.seqNode
                  , seqChild = ord.seqChild
                  , seqPre = ord.seqPre
                  , msg = msgSnap
                  , hashNode = hashNode0
                }
            else Left issues0


nodeSnapDb :: Map Int64 Text -> Cv.NodeDb -> Either [Conflict] NodeSnap
nodeSnapDb eidByUid node = do
  let
    eidParent0 =
      case node.parentFkNd of
        Nothing -> Right Nothing
        Just uidParent0 ->
          case Mp.lookup uidParent0 eidByUid of
            Nothing ->
              Left [BrokenShapeC $
                "db node parent uid not found for node " <> node.eidNd <> ": " <> Tx.pack (show uidParent0)]
            Just eidParent1 ->
              Right $ Just eidParent1
  eidParent1 <- eidParent0
  msgSnap <- maybe (Right Nothing) (fmap Just . msgSnapDb) node.messageNd
  let
    hashNode0 = hashNodeVal node.eidNd eidParent1 node.parentFkNd node.seqNodeNd node.seqChildNd node.seqPreNd msgSnap
  pure NodeSnap {
      eidNode = node.eidNd
      , uidNode = Just node.uidNd
      , eidParent = eidParent1
      , uidParent = node.parentFkNd
      , seqNode = node.seqNodeNd
      , seqChild = node.seqChildNd
      , seqPre = node.seqPreNd
      , msg = msgSnap
      , hashNode = hashNode0
    }


parentConflictJs :: Nd.Node -> Oor.NodeOrd -> [Conflict]
parentConflictJs node ord
  | node.parentNd == ord.eidParent = []
  | otherwise =
      [BrokenShapeC $
        "json node parent mismatch for " <> ord.eidNode
          <> ": node.parent=" <> showTxt node.parentNd
          <> ", ord.parent=" <> showTxt ord.eidParent]


msgSnapJs :: Jd.Message -> Either [Conflict] MsgSnap
msgSnapJs msg = do
  let
    issues0 =
      emptyTxtConflict "message.eid" msg.idMsg
        <> finiteConflictMb ("message.create_time:" <> msg.idMsg) (Just msg.createTimeMsg)
        <> finiteConflictMb ("message.update_time:" <> msg.idMsg) msg.updateTimeMsg
    content0 = contentSnapJs 0 msg.contentMsg
    contents0 = [content0]
    metadata0 = Ae.toJSON msg.metadataMsg
    hashMsg0 = hashMsgJsVal msg contents0
  if null issues0
    then
      Right MsgSnap {
          eidMsg = msg.idMsg
          , uidMsg = Nothing
          , timeCreate = Just msg.createTimeMsg
          , timeUpdate = msg.updateTimeMsg
          , status = fromMaybe "<unknown>"msg.statusMsg
          , endTurn = msg.endTurnMsg
          , weight = msg.weightMsg
          , metadata = metadata0
          , recipient = fromMaybe "<unknown>" msg.recipientMsg
          , channel = msg.channelMsg
          , contents = contents0
          , hashMsg = hashMsg0
        }
    else Left issues0


msgSnapDb :: Cv.MessageDb -> Either [Conflict] MsgSnap
msgSnapDb msg = do
  let
    issues0 =
      emptyTxtConflict "message.eid" msg.eidMsg
        <> finiteConflictMb ("message.create_time:" <> msg.eidMsg) msg.createTimeMsg
        <> finiteConflictMb ("message.update_time:" <> msg.eidMsg) msg.updateTimeMsg
    contents0 = zipWith contentSnapDb [0 ..] $ V.toList msg.contentsMsg
    hashMsg0 = hashMsgDbVal msg contents0
  if null issues0
    then
      Right MsgSnap {
          eidMsg = msg.eidMsg
          , uidMsg = Just msg.uidMsg
          , timeCreate = msg.createTimeMsg
          , timeUpdate = msg.updateTimeMsg
          , status = msg.statusMsg
          , endTurn = msg.endTurnMsg
          , weight = msg.weightMsg
          , metadata = msg.metadataMsg
          , recipient = msg.recipientMsg
          , channel = msg.channelMsg
          , contents = contents0
          , hashMsg = hashMsg0
        }
    else Left issues0


contentSnapJs :: Int32 -> Jd.Content -> ContentSnap
contentSnapJs seq0 content =
  let
    (typ0, payload0) = contentPairJs content
    hash0 = hashContentVal typ0 payload0
  in
  ContentSnap {
      uidContent = Nothing
      , seqContent = seq0
      , typeContent = typ0
      , payload = payload0
      , hashContent = hash0
    }


contentSnapDb :: Int32 -> Cv.ContentDb -> ContentSnap
contentSnapDb seq0 content =
  let
    (typ0, payload0) = contentPairDb content
    hash0 = hashContentVal typ0 payload0
  in
  ContentSnap {
      uidContent = Nothing
      , seqContent = seq0
      , typeContent = typ0
      , payload = payload0
      , hashContent = hash0
    }


contentPairJs :: Jd.Content -> (Text, Ae.Value)
contentPairJs = \case
  Jd.CodeCT cBlock -> -- lang fmt txt
    ("code", Ae.object ["language" .= cBlock.languageCP, "format" .= cBlock.responseFormatNameCP, "text" .= cBlock.textCP])

  Jd.ExecutionOutputCT execOut ->
    ("execution_output", Ae.object ["text" .= execOut.textEO])

  Jd.MultimodalTextCT mmt ->
    ("multimodal_text", Ae.object ["parts" .= map partValJs mmt.partsMmt])

  Jd.ModelEditableContextCT modelCtx -> -- model repo rs sc
    ("model_editable_context",
      Ae.object [
          "model_set_context" .= modelCtx.modelSetMEC
          , "repository" .= modelCtx.repositoryMEC
          , "repo_summary" .= modelCtx.repoSummaryMEC
          , "structured_context" .= modelCtx.structuredMEC
        ])

  Jd.ReasoningRecapCT recap -> -- txt
    ("reasoning_recap", Ae.object ["content" .= recap.contentRR])

  Jd.SystemErrorCT sysErr -> -- name0 txt
    ("system_error", Ae.object ["name" .= sysErr.nameSER, "text" .= sysErr.textSER])

  Jd.TetherBrowsingDisplayCT tbDisplay -> -- result0 summary0 assets0 tether0
    ("tether_browsing_display",
      Ae.object [
          "result" .= tbDisplay.resultTbd
          , "summary" .= tbDisplay.summaryTbd
          , "assets" .= tbDisplay.assetsTbd
          , "tether_id" .= tbDisplay.tetherIDTbd
        ])

  Jd.TetherQuoteCT tq -> -- url0 domain0 txt title0 tether0
    ("tether_quote",
      Ae.object [
          "url" .= tq.urlTq
          , "domain" .= tq.domainTq
          , "text" .= tq.textTq
          , "title" .= tq.titleTq
          , "tether_id" .= tq.tetherIDTq
        ])

  Jd.TextCT parts ->
    ("text", Ae.object ["parts" .= parts.partsTP])

  Jd.ThoughtsCT thoughts -> -- thoughts0 src0
    ("thoughts",
      Ae.object [
          "source_analysis_msg_id" .= thoughts.sourceAnalysisMsgIdTP
          , "thoughts" .= map thoughtValJs thoughts.thoughtsTP
        ])

  Jd.OtherCT info ->
    (info.contentTypeOpl, Ae.toJSON info.rawOpl)


contentPairDb :: Cv.ContentDb -> (Text, Ae.Value)
contentPairDb = \case
  Cv.CodeCT_Db lang fmt txt ->
    ("code", Ae.object ["language" .= lang, "format" .= fmt, "text" .= txt])

  Cv.ExecutionOutputCT_Db txt ->
    ("execution_output", Ae.object ["text" .= txt])

  Cv.MultimodalTextCT_Db parts ->
    ("multimodal_text", Ae.object ["parts" .= map partValDb (V.toList parts)])

  Cv.ModelEditableContextCT_Db model repo rs sc ->
    ("model_editable_context",
      Ae.object [
          "model_set_context" .= model
          , "repository" .= repo
          , "repo_summary" .= rs
          , "structured_context" .= sc
        ])

  Cv.ReasoningRecapCT_Db txt ->
    ("reasoning_recap", Ae.object ["content" .= txt])

  Cv.SystemErrorCT_Db name0 txt ->
    ("system_error", Ae.object ["name" .= name0, "text" .= txt])

  Cv.TetherBrowsingDisplayCT_Db result0 summary0 assets0 tether0 ->
    ("tether_browsing_display",
      Ae.object [
          "result" .= result0
          , "summary" .= summary0
          , "assets" .= assets0
          , "tether_id" .= tether0
        ])

  Cv.TetherQuoteCT_Db url0 domain0 txt title0 tether0 ->
    ("tether_quote",
      Ae.object [
          "url" .= url0
          , "domain" .= domain0
          , "text" .= txt
          , "title" .= title0
          , "tether_id" .= tether0
        ])

  Cv.TextCT_Db parts ->
    ("text", Ae.object ["parts" .= V.toList parts])

  Cv.ThoughtsCT_Db src0 thoughts0 ->
    ("thoughts",
      Ae.object [
          "source_analysis_msg_id" .= src0
          , "thoughts" .= map thoughtValDb (V.toList thoughts0)
        ])

  Cv.UnknownCT_Db typ0 raw0 ->
    (typ0, raw0)


partValJs :: Jd.MultiModalPart -> Ae.Value
partValJs = \case
  Jd.TextPT txt ->
    Ae.object ["kind" .= ("text" :: Text), "text" .= txt]

  Jd.AudioTranscriptionPT transcript ->
    Ae.object [
        "kind" .= ("audio_transcription" :: Text)
        , "text" .= transcript.textAtp
        , "direction" .= transcript.directionAtp
        , "decoding_id" .= transcript.decodingIdAtp
      ]

  Jd.AudioAssetPointerPT audioPtr ->
    Ae.object [
        "kind" .= ("audio_asset_pointer" :: Text)
        , "ptr" .= audioPtrValJs audioPtr
      ]

  Jd.ImageAssetPointerPT imgPtr -> -- asset0 size0 width0 height0 fovea0 meta0
    Ae.object [
        "kind" .= ("image_asset_pointer" :: Text)
        , "asset_pointer" .= imgPtr.assetPointerPap
        , "size_bytes" .= imgPtr.sizeBytesPap
        , "width" .= imgPtr.widthPap
        , "height" .= imgPtr.heightPap
        , "fovea" .= imgPtr.foveaPap
        , "metadata" .= fmap imgMetaValJs imgPtr.metadataPap
      ]

  Jd.RealTimeUserAVPT rtUser -> -- expiry0 frames0 video0 _audio0 start0
    Ae.object [
        "kind" .= ("realtime_user_av" :: Text)
        , "expiry_datetime" .= rtUser.expiryDatetimeRtuav
        , "frames_asset_pointers" .= rtUser.framesApRtuav
        , "video_container_asset_pointer" .= rtUser.videoContainerApRtuav
        , "audio_start_timestamp" .= rtUser.audioStartTimestampRtuav
        , "audio_asset_pointer_omitted" .= True
      ]


partValDb :: Cv.MultiModalPartDb -> Ae.Value
partValDb = \case
  Cv.TextPT_Db txt ->
    Ae.object ["kind" .= ("text" :: Text), "text" .= txt]

  Cv.AudioTranscriptionPT_Db atp ->
    Ae.object [
        "kind" .= ("audio_transcription" :: Text)
        , "text" .= atp.textAtp
        , "direction" .= atp.directionAtp
        , "decoding_id" .= atp.decodingIdAtp
      ]

  Cv.AudioAssetPointerPT_Db ptr ->
    Ae.object [
        "kind" .= ("audio_asset_pointer" :: Text)
        , "ptr" .= audioPtrValDb ptr
      ]

  Cv.ImageAssetPointerPT_Db iap ->
    Ae.object [
        "kind" .= ("image_asset_pointer" :: Text)
        , "asset_pointer" .= iap.assetPointerIap
        , "size_bytes" .= iap.sizeBytesIap
        , "width" .= iap.widthIap
        , "height" .= iap.heightIap
        , "fovea" .= iap.foveaIap
        , "metadata" .= fmap imgMetaValDb iap.metadataIap
      ]

  Cv.RealTimeUserAVPT_Db rtuav ->
    Ae.object [
        "kind" .= ("realtime_user_av" :: Text)
        , "expiry_datetime" .= rtuav.expiryDatetimeRtuav
        , "frames_asset_pointers" .= rtuav.framesAssetPointersRtuav
        , "video_container_asset_pointer" .= rtuav.videoContainerAssetPointerRtuav
        , "audio_start_timestamp" .= rtuav.audioStartTimestampRtuav
        , "audio_asset_pointer_omitted" .= True
      ]


audioPtrValJs :: Jd.AudioAssetPointer -> Ae.Value
audioPtrValJs ptr =
  Ae.object [
      "expiry_datetime" .= ptr.expiryDatetimeAap
      , "asset_pointer" .= ptr.assetPointerAap
      , "size_bytes" .= ptr.sizeBytesAap
      , "format" .= ptr.formatAap
      , "tool_audio_direction" .= ptr.toolAudioDirectionAap
      , "metadata" .= fmap audioMetaValJs ptr.metadataAap
    ]


audioPtrValDb :: Cv.AudioAssetPointerDb -> Ae.Value
audioPtrValDb ptr =
  Ae.object [
      "expiry_datetime" .= ptr.expiryDatetimeAap
      , "asset_pointer" .= ptr.assetPointerAap
      , "size_bytes" .= ptr.sizeBytesAap
      , "format" .= ptr.formatAap
      , "tool_audio_direction" .= ptr.toolAudioDirectionAap
      , "metadata" .= fmap audioMetaValDb ptr.metadataAap
    ]


imgMetaValJs :: Jd.ImageMetadata -> Ae.Value
imgMetaValJs meta0 =
  Ae.object [
      "dalle" .= fmap dalleValJs meta0.dalleMd
      , "gizmo" .= meta0.gizmoMd
      , "generation" .= fmap generationValJs meta0.generationMd
      , "container_pixel_height" .= meta0.containerPixelHeightMd
      , "container_pixel_width" .= meta0.containerPixelWidthMd
      , "emu_omit_glimpse_image" .= meta0.emuOmitGlimpseImageMd
      , "emu_patches_override" .= meta0.emuPatchesOverrideMd
      , "lpe_keep_patch_ijhw" .= meta0.lpeKeepPatchIjhwMd
      , "lpe_delta_encoding_channel" .= meta0.lpeDeltaEncodingChannelMd
      , "sanitized" .= meta0.sanitizedMd
      , "asset_pointer_link" .= meta0.assetPointerLinkMd
      , "watermarked_asset_pointer" .= meta0.watermarkedAssetPointerMd
      , "is_no_auth_placeholder" .= meta0.isNoAuthPlaceholderMd
    ]


imgMetaValDb :: Cv.ImageMetadataDb -> Ae.Value
imgMetaValDb meta0 =
  Ae.object [
      "dalle" .= fmap dalleValDb meta0.dalleMd
      , "gizmo" .= meta0.gizmoMd
      , "generation" .= fmap generationValDb meta0.generationMd
      , "container_pixel_height" .= meta0.containerPixelHeightMd
      , "container_pixel_width" .= meta0.containerPixelWidthMd
      , "emu_omit_glimpse_image" .= meta0.emuOmitGlimpseImageMd
      , "emu_patches_override" .= meta0.emuPatchesOverrideMd
      , "lpe_keep_patch_ijhw" .= meta0.lpeKeepPatchIjhwMd
      , "lpe_delta_encoding_channel" .= meta0.lpeDeltaEncodingChannelMd
      , "sanitized" .= meta0.sanitizedMd
      , "asset_pointer_link" .= meta0.assetPointerLinkMd
      , "watermarked_asset_pointer" .= meta0.watermarkedAssetPointerMd
      , "is_no_auth_placeholder" .= meta0.isNoAuthPlaceholderMd
    ]


dalleValJs :: Jd.Dalle -> Ae.Value
dalleValJs dalle0 =
  Ae.object [
      "gen_id" .= dalle0.genIdDa
      , "prompt" .= dalle0.promptDa
      , "seed" .= dalle0.seedDa
      , "parent_gen_id" .= dalle0.parentGenIdDa
      , "edit_op" .= dalle0.editOpDa
      , "serialization_title" .= dalle0.serializationTitleDa
    ]


dalleValDb :: Cv.DalleDb -> Ae.Value
dalleValDb dalle0 =
  Ae.object [
      "gen_id" .= dalle0.genIdDa
      , "prompt" .= dalle0.promptDa
      , "seed" .= dalle0.seedDa
      , "parent_gen_id" .= dalle0.parentGenIdDa
      , "edit_op" .= dalle0.editOpDa
      , "serialization_title" .= dalle0.serializationTitleDa
    ]


generationValJs :: Jd.Generation -> Ae.Value
generationValJs gen0 =
  Ae.object [
      "gen_id" .= gen0.genIdGe
      , "gen_size" .= gen0.genSizeGe
      , "seed" .= gen0.seedGe
      , "parent_gen_id" .= gen0.parentGenIdGe
      , "height" .= gen0.heightGe
      , "width" .= gen0.widthGe
      , "transparent_background" .= gen0.transparentBackgroundGe
      , "serialization_title" .= gen0.serializationTitleGe
      , "orientation" .= gen0.orientationGe
    ]


generationValDb :: Cv.GenerationDb -> Ae.Value
generationValDb gen0 =
  Ae.object [
      "gen_id" .= gen0.genIdGe
      , "gen_size" .= gen0.genSizeGe
      , "seed" .= gen0.seedGe
      , "parent_gen_id" .= gen0.parentGenIdGe
      , "height" .= gen0.heightGe
      , "width" .= gen0.widthGe
      , "transparent_background" .= gen0.transparentBackgroundGe
      , "serialization_title" .= gen0.serializationTitleGe
      , "orientation" .= gen0.orientationGe
    ]


audioMetaValJs :: Jd.AudioMetadata -> Ae.Value
audioMetaValJs meta0 =
  Ae.object [
      "start_timestamp" .= meta0.startTimestampAm
      , "end_timestamp" .= meta0.endTimestampAm
      , "pretokenized_vq" .= meta0.pretokenizedVqAm
      , "interruptions" .= meta0.interruptionsAm
      , "original_audio_source" .= meta0.originalAudioSourceAm
      , "transcription" .= meta0.transcriptionAm
      , "word_transcription" .= meta0.wordTranscriptionAm
      , "start" .= meta0.startAm
      , "end" .= meta0.endAm
    ]


audioMetaValDb :: Cv.AudioMetadataDb -> Ae.Value
audioMetaValDb meta0 =
  Ae.object [
      "start_timestamp" .= meta0.startTimestampAm
      , "end_timestamp" .= meta0.endTimestampAm
      , "pretokenized_vq" .= meta0.pretokenizedVqAm
      , "interruptions" .= meta0.interruptionsAm
      , "original_audio_source" .= meta0.originalAudioSourceAm
      , "transcription" .= meta0.transcriptionAm
      , "word_transcription" .= meta0.wordTranscriptionAm
      , "start" .= meta0.startStampAm
      , "end" .= meta0.endStampAm
    ]


thoughtValJs :: Jd.ThoughtContent -> Ae.Value
thoughtValJs th =
  Ae.object [
      "summary" .= th.summaryTC
      , "content" .= th.contentTC
      , "chunks" .= Ae.toJSON th.chunksTC
      , "finished" .= th.finishedTC
    ]


thoughtValDb :: Cv.ThoughtDb -> Ae.Value
thoughtValDb th =
  Ae.object [
      "summary" .= th.summaryTh
      , "content" .= th.contentTh
      , "chunks" .= th.chunksTh
      , "finished" .= th.finishedTh
    ]


hashContentVal :: Text -> Ae.Value -> Hash
hashContentVal typ0 payload0 =
  hashVal $ Ae.object ["type" .= typ0, "payload" .= payload0]


hashMsgJsVal :: Jd.Message -> [ContentSnap] -> Hash
hashMsgJsVal msg0 contents0 =
  hashVal $
    Ae.object [
        "eid" .= msg0.idMsg
        , "author" .= authorValJs msg0.authorMsg
        , "create_time" .= msg0.createTimeMsg
        , "update_time" .= msg0.updateTimeMsg
        , "status" .= msg0.statusMsg
        , "end_turn" .= msg0.endTurnMsg
        , "weight" .= msg0.weightMsg
        , "metadata" .= msg0.metadataMsg
        , "recipient" .= msg0.recipientMsg
        , "channel" .= msg0.channelMsg
        , "contents" .= map contentValSnap contents0
      ]


hashMsgDbVal :: Cv.MessageDb -> [ContentSnap] -> Hash
hashMsgDbVal msg0 contents0 =
  hashVal $
    Ae.object [
        "eid" .= msg0.eidMsg
        , "author" .= authorValDb msg0.authorMsg
        , "create_time" .= msg0.createTimeMsg
        , "update_time" .= msg0.updateTimeMsg
        , "status" .= msg0.statusMsg
        , "end_turn" .= msg0.endTurnMsg
        , "weight" .= msg0.weightMsg
        , "metadata" .= msg0.metadataMsg
        , "recipient" .= msg0.recipientMsg
        , "channel" .= msg0.channelMsg
        , "contents" .= map contentValSnap contents0
      ]


hashNodeVal :: Text -> Maybe Text -> Maybe Int64 -> Int32 -> Int32 -> Int32 -> Maybe MsgSnap -> Hash
hashNodeVal eidNode0 eidParent0 _uidParent0 seqNode0 seqChild0 seqPre0 msg0 =
  hashVal $
    Ae.object [
        "eid" .= eidNode0
        , "parent_eid" .= eidParent0
        , "seq_node" .= seqNode0
        , "seq_child" .= seqChild0
        , "seq_pre" .= seqPre0
        , "message" .= fmap msgRefVal msg0
      ]


authorValJs :: Jd.Author -> Ae.Value
authorValJs author0 =
  Ae.object [
      "role" .= author0.roleAu
      , "name" .= author0.nameAu
      , "metadata" .= author0.metadataAu
    ]


authorValDb :: Cv.AuthorDb -> Ae.Value
authorValDb author0 =
  Ae.object [
      "role" .= author0.roleAu
      , "name" .= author0.nameAu
      , "metadata" .= author0.metadataAu
    ]


contentValSnap :: ContentSnap -> Ae.Value
contentValSnap content0 =
  Ae.object [
      "seq" .= content0.seqContent
      , "type" .= content0.typeContent
      , "payload" .= content0.payload
    ]


msgRefVal :: MsgSnap -> Ae.Value
msgRefVal msg0 =
  Ae.object [
      "eid" .= msg0.eidMsg
      , "hash" .= hashTxt msg0.hashMsg
    ]


hashVal :: Ae.Value -> Hash
hashVal aValue =
  let
    encoding = Ae.toEncoding . canonVal $ aValue
    decoding = BB.toLazyByteString . Ae.fromEncoding $ encoding
    hashBS :: BS.ByteString
    hashBS = BA.convert . (Ch.hashlazy :: Lbs.ByteString -> Ch.Digest Ch.SHA256) $  decoding
  in
  -- 
  Hash hashBS


canonVal :: Ae.Value -> Ae.Value
canonVal = \case
  Ae.Object obj ->
    Ae.Object $ Km.fromList $ sortOn (Ak.toText . fst) [(key, canonVal val) | (key, val) <- Km.toList obj]
  Ae.Array arr -> Ae.Array $ V.map canonVal arr
  other -> other


ordConflict :: Oor.OrdIssue -> Conflict
ordConflict = \case
  Oor.MissingRootOI ->
    BrokenShapeC "missing root node"

  Oor.MissingNodeOI eid0 ->
    MissingJsonNodeC eid0

  Oor.MissingParentOI child0 parent0 ->
    BrokenShapeC $ "missing parent for node " <> child0 <> ": " <> parent0

  Oor.CycleOI eid0 ->
    BrokenShapeC $ "cycle detected at node " <> eid0

  Oor.DuplicateChildOI parent0 child0 ->
    BrokenShapeC $ "duplicate child reference under " <> parent0 <> ": " <> child0

  Oor.BranchOI parent0 children0 ->
    BrokenShapeC $ "ordering failed on branch under " <> parent0 <> ": " <> Tx.intercalate "," children0


-- TODO: to deprecate as Scientific is already fine.
finiteConflict :: Text -> Scientific -> [Conflict]
finiteConflict label0 value0 = []


finiteConflictMb :: Text -> Maybe Scientific -> [Conflict]
finiteConflictMb label0 =
  maybe [] (finiteConflict label0)


emptyTxtConflict :: Text -> Text -> [Conflict]
emptyTxtConflict label0 txt0
  | Tx.null txt0 = [BrokenShapeC $ "empty text at " <> label0]
  | otherwise = []


dupTxts :: [Text] -> [Text]
dupTxts txts =
  [head grp | grp <- group $ sort txts, length grp > 1]


collectE :: [Either [Conflict] a] -> Either [Conflict] [a]
collectE rezs =
  let
    step (errs0, oksRev0) rez0 =
      case rez0 of
        Left errs1 -> (errs0 <> errs1, oksRev0)
        Right ok0 -> (errs0, ok0 : oksRev0)

    (errs2, oksRev2) = foldl step ([], []) rezs
  in
  if null errs2
    then Right $ reverse oksRev2
    else Left errs2


hashTxt :: Hash -> Text
hashTxt hash0 =
  Tx.pack $ show hash0.bytesHash


showTxt :: Show a => a -> Text
showTxt =
  Tx.pack . show