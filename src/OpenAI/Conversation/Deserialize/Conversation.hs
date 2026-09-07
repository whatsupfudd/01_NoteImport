module OpenAI.Conversation.Deserialize.Conversation where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Char (toLower)
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Mp
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as St
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Aeson (Value)
import qualified Data.Aeson as Ae

import qualified Hasql.Pool as Hp
import qualified Hasql.Session as Ses
import Hasql.Statement (Statement)
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as Txs

import qualified OpenAI.Conversation as Cv
import qualified OpenAI.Conversation.Deserialize.ConversationStmt as Dst
import qualified OpenAI.Utils as Ut


type CodeVal = (Text, Maybe Text, Text)
type MecVal = (Text, Maybe Value, Maybe Value, Maybe Value)
type SystemErrVal = (Text, Text)
type TetherBrowseVal = (Text, Maybe Value, Maybe Value, Maybe Text)
type TetherQuoteVal = (Text, Text, Text, Text, Maybe Text)
type AudioTransVal = (Text, Text, Maybe Text)
type ImagePtrVal = (Int64, Text, Int64, Int32, Int32, Maybe Value)


data ContentIx = ContentIx {
    codesCi :: Map Int64 CodeVal
    , execsCi :: Map Int64 Text
    , modelContextsCi :: Map Int64 MecVal
    , reasoningRecapsCi :: Map Int64 Text
    , systemErrorsCi :: Map Int64 SystemErrVal
    , tetherBrowsesCi :: Map Int64 TetherBrowseVal
    , tetherQuotesCi :: Map Int64 TetherQuoteVal
    , textsCi :: Map Int64 (Vector Text)
    , thoughtHeadersCi :: Map Int64 Text
    , thoughtsCi :: Map Int64 (Vector Cv.ThoughtDb)
    , unknownsCi :: Map Int64 Value
    , multiPartsCi :: Map Int64 (Vector Cv.MultiModalPartDb)
  }


data MultiIx = MultiIx {
    textsMi :: Map Int64 Text
    , audioTransMi :: Map Int64 AudioTransVal
    , imagePtrsMi :: Map Int64 ImagePtrVal
    , imageMetasMi :: Map Int64 Cv.ImageMetadataDb
    , audioPtrsMi :: Map Int64 Cv.AudioAssetPointerDb
    , realtimeMi :: Map Int64 Cv.RealTimeUserAVDb
  }


fetchAllConversations :: Hp.Pool -> IO (Either Hp.UsageError (Map Text Int64))
fetchAllConversations pool = do
  dbRez <- Hp.use pool $ Ses.statement () Dst.fetchAllConversationsRows
  pure $ fmap (Mp.fromList . map (\(uid, title) -> (title, uid)) . V.toList) dbRez


fetchAllConversationEids :: Hp.Pool -> IO (Either Hp.UsageError (Set Text))
fetchAllConversationEids pool = do
  dbRez <- Hp.use pool $ Ses.statement () Dst.fetchAllConversationEids
  pure $ fmap (St.fromList . V.toList) dbRez


getConversationByEid :: Hp.Pool -> Text -> IO (Either Hp.UsageError (Either String (Maybe Cv.ConversationDb)))
getConversationByEid pool targetEid =
  Hp.use pool $ Txs.transaction Txs.RepeatableRead Txs.Read $ do
    mbConv <- Tx.statement targetEid Dst.selectConversationByEid
    case mbConv of
      Nothing -> pure $ Right Nothing
      Just convRow -> loadConversationTx convRow


getConversationByUid :: Hp.Pool -> Int64 -> IO (Either Hp.UsageError (Either String (Maybe Cv.ConversationDb)))
getConversationByUid pool uid =
  Hp.use pool $ Txs.transaction Txs.RepeatableRead Txs.Read $ do
    mbConv <- Tx.statement uid Dst.selectConversationByUid
    case mbConv of
      Nothing -> pure $ Right Nothing
      Just convRow -> loadConversationTx convRow


getConversationBody :: Hp.Pool -> Dst.ConversationRow -> IO (Either Hp.UsageError (Either String (Maybe Cv.ConversationDb)))
getConversationBody pool convRow =
  Hp.use pool $ Txs.transaction Txs.RepeatableRead Txs.Read $ loadConversationTx convRow


loadConversationTx :: Dst.ConversationRow -> Tx.Transaction (Either String (Maybe Cv.ConversationDb))
loadConversationTx convRow@(convUid, _, _, _, _) = do
  nodeRows <- Tx.statement convUid Dst.selectNodes
  msgRows <- Tx.statement convUid Dst.selectMessagesWithAuthor
  contentRows <- Tx.statement convUid Dst.selectContents
  codeRows <- Tx.statement convUid Dst.selectCodeContents
  execRows <- Tx.statement convUid Dst.selectExecutionOutputContents
  mecRows <- Tx.statement convUid Dst.selectModelEditableContextContents
  rrcRows <- Tx.statement convUid Dst.selectReasoningRecapContents
  sesRows <- Tx.statement convUid Dst.selectSystemErrorContents
  tbdRows <- Tx.statement convUid Dst.selectTetherBrowsingDisplayContents
  tqRows <- Tx.statement convUid Dst.selectTetherQuoteContents
  txtRows <- Tx.statement convUid Dst.selectTextContents
  thHdrRows <- Tx.statement convUid Dst.selectThoughtsContents
  thRows <- Tx.statement convUid Dst.selectThoughts
  unkRows <- Tx.statement convUid Dst.selectUnknownContents
  mmPartRows <- Tx.statement convUid Dst.selectMultiModalParts
  mmTextRows <- Tx.statement convUid Dst.selectTextMmParts
  mmAtRows <- Tx.statement convUid Dst.selectAudioTranscriptionMmParts
  mmImgPtrRows <- Tx.statement convUid Dst.selectImageAssetPointerMmParts
  mmImgMdRows <- Tx.statement convUid Dst.selectImageAssetMetadatas
  dalleRows <- Tx.statement convUid Dst.selectDalles
  genRows <- Tx.statement convUid Dst.selectGenerations
  aapRows <- Tx.statement convUid Dst.selectAudioAssetPointerMmParts
  aapMetaRows <- Tx.statement convUid Dst.selectAudioMetadataForAap
  rtuavRows <- Tx.statement convUid Dst.selectRealTimeUserAVMmParts
  rtuavMetaRows <- Tx.statement convUid Dst.selectAudioMetadataForRtuav

  pure $
    Just <$> buildConversationDb convRow nodeRows msgRows contentRows codeRows execRows mecRows rrcRows sesRows tbdRows tqRows
      txtRows thHdrRows thRows unkRows mmPartRows mmTextRows mmAtRows mmImgPtrRows mmImgMdRows dalleRows genRows aapRows
      aapMetaRows rtuavRows rtuavMetaRows


runStmt :: Hp.Pool -> Statement a b -> a -> ExceptT Hp.UsageError IO b
runStmt pool stmt value = do
  result <- lift $ Hp.use pool $ Ses.statement value stmt
  case result of
    Left err -> throwE err
    Right loaded -> pure loaded


buildConversationDb :: Dst.ConversationRow -> Vector Dst.NodeRow -> Vector Dst.MessageRow -> Vector Dst.ContentRow
      -> Vector Dst.CodeRow -> Vector Dst.ExecRow -> Vector Dst.MecRow -> Vector Dst.RrcRow -> Vector Dst.SesRow
      -> Vector Dst.TbdRow -> Vector Dst.TqRow -> Vector Dst.TextRow -> Vector Dst.ThoughtsHdrRow -> Vector Dst.ThoughtRow
      -> Vector Dst.UnknownRow -> Vector Dst.MmPartRow -> Vector Dst.MmTextRow -> Vector Dst.MmAtRow
      -> Vector Dst.MmImgPtrRow -> Vector Dst.MmImgMdRow -> Vector Dst.DalleRow -> Vector Dst.GenerationRow
      -> Vector Dst.AapRow -> Vector Dst.AudioMetaRow -> Vector Dst.RtuavRow -> Vector Dst.AudioMetaRow
      -> Either String Cv.ConversationDb
buildConversationDb (convUid, title, eid, timeCreate, timeUpdate) nodeRows msgRows contentRows codeRows execRows mecRows
    rrcRows sesRows tbdRows tqRows txtRows thHdrRows thRows unkRows mmPartRows mmTextRows mmAtRows mmImgPtrRows
    mmImgMdRows dalleRows genRows aapRows aapMetaRows rtuavRows _rtuavMetaRows = do
  validateUnique "node uid" [uid | (uid, _, _, _, _, _) <- V.toList nodeRows]
  validateUnique "node eid" [eidNode | (_, eidNode, _, _, _, _) <- V.toList nodeRows]
  validateUnique "message uid" [uidMsg | (_, uidMsg, _, _, _, _, _, _, _, _, _, _, _, _, _) <- V.toList msgRows]
  validateUnique "message eid" [eidMsg | (_, _, eidMsg, _, _, _, _, _, _, _, _, _, _, _, _) <- V.toList msgRows]
  validateUnique "message node uid" [uidNode | (uidNode, _, _, _, _, _, _, _, _, _, _, _, _, _, _) <- V.toList msgRows]
  validateUnique "content uid" [uidContent | (uidContent, _, _) <- V.toList contentRows]
  validateUnique "multimodal part uid" [uidPart | (uidPart, _, _) <- V.toList mmPartRows]

  let
    codeMap = Mp.fromList [(uid, (lang, formatName, text)) | (uid, lang, formatName, text) <- V.toList codeRows]
    execMap = Mp.fromList [(uid, text) | (uid, text) <- V.toList execRows]
    mecMap = Mp.fromList [(uid, (modelSet, repository, repoSummary, structured)) |
        (uid, modelSet, repository, repoSummary, structured) <- V.toList mecRows]
    rrcMap = Mp.fromList [(uid, text) | (uid, text) <- V.toList rrcRows]
    sesMap = Mp.fromList [(uid, (name, text)) | (uid, name, text) <- V.toList sesRows]
    tbdMap = Mp.fromList [(uid, (result, summary, assets, tetherId)) |
        (uid, result, summary, assets, tetherId) <- V.toList tbdRows]
    tqMap = Mp.fromList [(uid, (url, domain, text, quoteTitle, tetherId)) |
        (uid, url, domain, text, quoteTitle, tetherId) <- V.toList tqRows]
    txtMap = Mp.fromList [(uid, parts) | (uid, parts) <- V.toList txtRows]
    thHdrMap = Mp.fromList [(uid, sourceId) | (uid, sourceId) <- V.toList thHdrRows]
    unkMap = Mp.fromList [(uid, raw) | (uid, raw) <- V.toList unkRows]
    thoughtsMap = groupVecByKey thRows thoughtFromRow
    mmTextMap = Mp.fromList [(uid, text) | (uid, text) <- V.toList mmTextRows]
    mmAtMap = Mp.fromList [(uid, (text, direction, decodingId)) |
        (uid, text, direction, decodingId) <- V.toList mmAtRows]
    mmImgPtrMap = Mp.fromList [(uidPart, (uidPtr, assetPtr, sizeBytes, width, height, fovea)) |
        (uidPart, uidPtr, assetPtr, sizeBytes, width, height, fovea) <- V.toList mmImgPtrRows]
    mmImgMdMap = buildImageMetadataMap mmImgMdRows dalleRows genRows
    aapMetaMap = Mp.fromList [(uidOwner, mkAudioMeta row) |
        row@(uidOwner, _, _, _, _, _, _, _, _, _) <- V.toList aapMetaRows]
    aapMap = Mp.fromList [(uidPart, audioPointerFromRow aapMetaMap row) | row@(uidPart, _, _, _, _, _, _) <- V.toList aapRows]
    rtuavMap = Mp.fromList [(uidPart, realtimeFromRow row) | row@(uidPart, _, _, _, _, _) <- V.toList rtuavRows]
    multiIx = MultiIx {
        textsMi = mmTextMap
        , audioTransMi = mmAtMap
        , imagePtrsMi = mmImgPtrMap
        , imageMetasMi = mmImgMdMap
        , audioPtrsMi = aapMap
        , realtimeMi = rtuavMap
      }

  validateMultiRows multiIx mmPartRows

  let
    mmPartsByContent = groupVecByKey mmPartRows $ \(uidPart, uidContent, kindPart) ->
      (uidContent, mkMultiModalPartIx multiIx uidPart kindPart)
    contentIx = ContentIx {
        codesCi = codeMap
        , execsCi = execMap
        , modelContextsCi = mecMap
        , reasoningRecapsCi = rrcMap
        , systemErrorsCi = sesMap
        , tetherBrowsesCi = tbdMap
        , tetherQuotesCi = tqMap
        , textsCi = txtMap
        , thoughtHeadersCi = thHdrMap
        , thoughtsCi = thoughtsMap
        , unknownsCi = unkMap
        , multiPartsCi = mmPartsByContent
      }

  validateContentRows contentIx contentRows

  let
    contentsByMessage = groupVecByKey contentRows $ \(uidContent, uidMsg, kindContent) ->
      (uidMsg, mkContentIx contentIx uidContent kindContent)
    msgMapByNode = Mp.fromList [
        (uidNode, messageFromRow contentsByMessage row)
        | row@(uidNode, _, _, _, _, _, _, _, _, _, _, _, _, _, _) <- V.toList msgRows
      ]
    nodesMap = Mp.fromList [
        (node.eidNd, attachMessageNode msgMapByNode node)
        | row <- V.toList nodeRows
        , let node = nodeFromRow row
      ]

  pure Cv.ConversationDb {
      Cv.uidCv = convUid
      , Cv.titleCv = title
      , Cv.eidCv = eid
      , Cv.createTimeCv = fromMaybe 0 $ Ut.safeScientific timeCreate
      , Cv.updateTimeCv = fromMaybe 0 $ Ut.safeScientific timeUpdate
      , Cv.nodesCv = nodesMap
    }


thoughtFromRow :: Dst.ThoughtRow -> (Int64, Cv.ThoughtDb)
thoughtFromRow (uidContent, summary, content, chunks, finished) =
  (uidContent, Cv.ThoughtDb {
      Cv.summaryTh = summary
      , Cv.contentTh = content
      , Cv.chunksTh = chunks
      , Cv.finishedTh = finished
    })


messageFromRow :: Map Int64 (Vector Cv.ContentDb) -> Dst.MessageRow -> Cv.MessageDb
messageFromRow contentsByMessage (_, uidMsg, eidMsg, timeCreate, timeUpdate, status, endTurn, weight, metadata, recipient,
    channel, uidAuthor, roleAuthor, nameAuthor, metadataAuthor) =
  Cv.MessageDb {
      Cv.uidMsg = uidMsg
      , Cv.eidMsg = eidMsg
      , Cv.createTimeMsg = Ut.safeScientific =<< timeCreate
      , Cv.updateTimeMsg = Ut.safeScientific =<< timeUpdate
      , Cv.statusMsg = status
      , Cv.endTurnMsg = endTurn
      , Cv.weightMsg = Ut.safeScientific =<< weight
      , Cv.metadataMsg = metadata
      , Cv.recipientMsg = recipient
      , Cv.channelMsg = channel
      , Cv.authorMsg = Cv.AuthorDb {
          Cv.uidAu = uidAuthor
          , Cv.roleAu = roleAuthor
          , Cv.nameAu = nameAuthor
          , Cv.metadataAu = metadataAuthor
        }
      , Cv.contentsMsg = Mp.findWithDefault V.empty uidMsg contentsByMessage
    }


nodeFromRow :: Dst.NodeRow -> Cv.NodeDb
nodeFromRow (uid, eid, parentFk, seqNode, seqChild, seqPre) =
  Cv.NodeDb {
      Cv.uidNd = uid
      , Cv.eidNd = eid
      , Cv.parentFkNd = parentFk
      , Cv.seqNodeNd = seqNode
      , Cv.seqChildNd = seqChild
      , Cv.seqPreNd = seqPre
      , Cv.messageNd = Nothing
    }


attachMessageNode :: Map Int64 Cv.MessageDb -> Cv.NodeDb -> Cv.NodeDb
attachMessageNode msgMap node =
  node {Cv.messageNd = Mp.lookup node.uidNd msgMap}


mkContent :: Int64 -> Text -> Map Int64 CodeVal -> Map Int64 Text -> Map Int64 MecVal -> Map Int64 Text
      -> Map Int64 SystemErrVal -> Map Int64 TetherBrowseVal -> Map Int64 TetherQuoteVal -> Map Int64 (Vector Text)
      -> Map Int64 Text -> Map Int64 (Vector Cv.ThoughtDb) -> Map Int64 Value
      -> Map Int64 (Vector Cv.MultiModalPartDb) -> Cv.ContentDb
mkContent uidContent kindContent codeMap execMap mecMap rrcMap sesMap tbdMap tqMap txtMap thHdrMap thoughtsMap
    unkMap mmPartsByContent =
  mkContentIx ContentIx {
      codesCi = codeMap
      , execsCi = execMap
      , modelContextsCi = mecMap
      , reasoningRecapsCi = rrcMap
      , systemErrorsCi = sesMap
      , tetherBrowsesCi = tbdMap
      , tetherQuotesCi = tqMap
      , textsCi = txtMap
      , thoughtHeadersCi = thHdrMap
      , thoughtsCi = thoughtsMap
      , unknownsCi = unkMap
      , multiPartsCi = mmPartsByContent
    } uidContent kindContent


mkContentIx :: ContentIx -> Int64 -> Text -> Cv.ContentDb
mkContentIx index uidContent kindContent =
  case normalizeTyp kindContent of
    "code" ->
      case Mp.lookup uidContent index.codesCi of
        Just (language, formatName, text) -> Cv.CodeCT_Db language formatName text
        Nothing -> missingContent kindContent

    "execution_output" ->
      case Mp.lookup uidContent index.execsCi of
        Just text -> Cv.ExecutionOutputCT_Db text
        Nothing -> missingContent kindContent

    "multimodal_text" ->
      Cv.MultimodalTextCT_Db $ Mp.findWithDefault V.empty uidContent index.multiPartsCi

    "model_editable_context" ->
      case Mp.lookup uidContent index.modelContextsCi of
        Just (modelSet, repository, repoSummary, structured) ->
          Cv.ModelEditableContextCT_Db modelSet repository repoSummary structured
        Nothing -> missingContent kindContent

    "reasoning_recap" ->
      case Mp.lookup uidContent index.reasoningRecapsCi of
        Just text -> Cv.ReasoningRecapCT_Db text
        Nothing -> missingContent kindContent

    "system_error" ->
      case Mp.lookup uidContent index.systemErrorsCi of
        Just (name, text) -> Cv.SystemErrorCT_Db name text
        Nothing -> missingContent kindContent

    "tether_browsing_display" ->
      case Mp.lookup uidContent index.tetherBrowsesCi of
        Just (result, summary, assets, tetherId) -> Cv.TetherBrowsingDisplayCT_Db result summary assets tetherId
        Nothing -> missingContent kindContent

    "tether_quote" ->
      case Mp.lookup uidContent index.tetherQuotesCi of
        Just (url, domain, text, title, tetherId) -> Cv.TetherQuoteCT_Db url domain text title tetherId
        Nothing -> missingContent kindContent

    "text" ->
      case Mp.lookup uidContent index.textsCi of
        Just parts -> Cv.TextCT_Db parts
        Nothing -> missingContent kindContent

    "thoughts" ->
      case Mp.lookup uidContent index.thoughtHeadersCi of
        Nothing -> missingContent kindContent
        Just sourceId ->
          Cv.ThoughtsCT_Db sourceId $ Mp.findWithDefault V.empty uidContent index.thoughtsCi

    _ ->
      Cv.UnknownCT_Db kindContent $ Mp.findWithDefault Ae.Null uidContent index.unknownsCi


missingContent :: Text -> Cv.ContentDb
missingContent kindContent =
  Cv.UnknownCT_Db kindContent Ae.Null


validateContentRows :: ContentIx -> Vector Dst.ContentRow -> Either String ()
validateContentRows index =
  V.mapM_ $ \(uidContent, _, kindContent) ->
    case normalizeTyp kindContent of
      "code" -> requireKey "code content" uidContent index.codesCi
      "execution_output" -> requireKey "execution-output content" uidContent index.execsCi
      "multimodal_text" -> Right ()
      "model_editable_context" -> requireKey "model-editable-context content" uidContent index.modelContextsCi
      "reasoning_recap" -> requireKey "reasoning-recap content" uidContent index.reasoningRecapsCi
      "system_error" -> requireKey "system-error content" uidContent index.systemErrorsCi
      "tether_browsing_display" -> requireKey "tether-browsing content" uidContent index.tetherBrowsesCi
      "tether_quote" -> requireKey "tether-quote content" uidContent index.tetherQuotesCi
      "text" -> requireKey "text content" uidContent index.textsCi
      "thoughts" -> requireKey "thoughts content" uidContent index.thoughtHeadersCi
      _ -> requireKey ("opaque content " <> T.unpack kindContent) uidContent index.unknownsCi


mkMultiModalPart :: Int64 -> Text -> Map Int64 Text -> Map Int64 AudioTransVal -> Map Int64 ImagePtrVal
      -> Map Int64 Cv.ImageMetadataDb -> Map Int64 Cv.AudioAssetPointerDb -> Map Int64 Cv.RealTimeUserAVDb
      -> Cv.MultiModalPartDb
mkMultiModalPart uidPart kindPart textMap audioTransMap imagePtrMap imageMetaMap audioPtrMap realtimeMap =
  mkMultiModalPartIx MultiIx {
      textsMi = textMap
      , audioTransMi = audioTransMap
      , imagePtrsMi = imagePtrMap
      , imageMetasMi = imageMetaMap
      , audioPtrsMi = audioPtrMap
      , realtimeMi = realtimeMap
    } uidPart kindPart


mkMultiModalPartIx :: MultiIx -> Int64 -> Text -> Cv.MultiModalPartDb
mkMultiModalPartIx index uidPart kindPart =
  case normalizeTyp kindPart of
    "text" ->
      Cv.TextPT_Db $ Mp.findWithDefault "" uidPart index.textsMi

    "audio_transcription" ->
      case Mp.lookup uidPart index.audioTransMi of
        Just (text, direction, decodingId) ->
          Cv.AudioTranscriptionPT_Db Cv.AudioTranscriptionDb {
              Cv.textAtp = text
              , Cv.directionAtp = direction
              , Cv.decodingIdAtp = decodingId
            }
        Nothing -> Cv.TextPT_Db "(missing audio transcription)"

    "image_asset_pointer" ->
      case Mp.lookup uidPart index.imagePtrsMi of
        Just (uidImagePtr, assetPtr, sizeBytes, width, height, fovea) ->
          Cv.ImageAssetPointerPT_Db Cv.ImageAssetPointerDb {
              Cv.assetPointerIap = assetPtr
              , Cv.sizeBytesIap = sizeBytes
              , Cv.widthIap = width
              , Cv.heightIap = height
              , Cv.foveaIap = fovea
              , Cv.metadataIap = Mp.lookup uidImagePtr index.imageMetasMi
            }
        Nothing -> Cv.TextPT_Db "(missing image asset pointer)"

    "audio_asset_pointer" ->
      case Mp.lookup uidPart index.audioPtrsMi of
        Just pointer -> Cv.AudioAssetPointerPT_Db pointer
        Nothing -> Cv.TextPT_Db "(missing audio asset pointer)"

    "real_time_user_av" ->
      case Mp.lookup uidPart index.realtimeMi of
        Just realtime -> Cv.RealTimeUserAVPT_Db realtime
        Nothing -> Cv.TextPT_Db "(missing real-time user AV)"

    other ->
      Cv.TextPT_Db $ "(unsupported multimodal part type: " <> other <> ")"


validateMultiRows :: MultiIx -> Vector Dst.MmPartRow -> Either String ()
validateMultiRows index =
  V.mapM_ $ \(uidPart, _, kindPart) ->
    case normalizeTyp kindPart of
      "text" -> requireKey "multimodal text part" uidPart index.textsMi
      "audio_transcription" -> requireKey "multimodal audio-transcription part" uidPart index.audioTransMi
      "image_asset_pointer" -> requireKey "multimodal image-pointer part" uidPart index.imagePtrsMi
      "audio_asset_pointer" -> requireKey "multimodal audio-pointer part" uidPart index.audioPtrsMi
      "real_time_user_av" -> requireKey "multimodal real-time AV part" uidPart index.realtimeMi
      other -> Left $ "unsupported multimodal part type " <> T.unpack other <> " for part uid " <> show uidPart


buildImageMetadataMap :: Vector Dst.MmImgMdRow -> Vector Dst.DalleRow -> Vector Dst.GenerationRow
      -> Map Int64 Cv.ImageMetadataDb
buildImageMetadataMap metadataRows dalleRows generationRows =
  let
    dalleByMetadata = Mp.fromList [(uidMetadata, dalleFromRow row) |
        row@(uidMetadata, _, _, _, _, _, _) <- V.toList dalleRows]
    generationByMetadata = Mp.fromList [(uidMetadata, generationFromRow row) |
        row@(uidMetadata, _, _, _, _, _, _, _, _, _) <- V.toList generationRows]
  in
  Mp.fromList [
      (uidImagePtr, imageMetadataFromRow dalleByMetadata generationByMetadata row)
      | row@(uidImagePtr, _, _, _, _, _, _, _, _, _, _, _, _) <- V.toList metadataRows
    ]


imageMetadataFromRow :: Map Int64 Cv.DalleDb -> Map Int64 Cv.GenerationDb -> Dst.MmImgMdRow -> Cv.ImageMetadataDb
imageMetadataFromRow dalleByMetadata generationByMetadata
    (_, uidMetadata, gizmo, height, width, omitGlimpse, patchesOverride, keepPatch, deltaEncoding, sanitized, assetLink,
      watermarkedLink, noAuthPlaceholder) =
  Cv.ImageMetadataDb {
      Cv.dalleMd = Mp.lookup uidMetadata dalleByMetadata
      , Cv.gizmoMd = gizmo
      , Cv.generationMd = Mp.lookup uidMetadata generationByMetadata
      , Cv.containerPixelHeightMd = fmap fromIntegral height
      , Cv.containerPixelWidthMd = fmap fromIntegral width
      , Cv.emuOmitGlimpseImageMd = omitGlimpse
      , Cv.emuPatchesOverrideMd = patchesOverride
      , Cv.lpeKeepPatchIjhwMd = keepPatch
      , Cv.lpeDeltaEncodingChannelMd = deltaEncoding
      , Cv.sanitizedMd = sanitized
      , Cv.assetPointerLinkMd = assetLink
      , Cv.watermarkedAssetPointerMd = watermarkedLink
      , Cv.isNoAuthPlaceholderMd = noAuthPlaceholder
    }


dalleFromRow :: Dst.DalleRow -> Cv.DalleDb
dalleFromRow (_, genId, prompt, seed, parentGenId, editOp, serializationTitle) =
  Cv.DalleDb {
      Cv.genIdDa = genId
      , Cv.promptDa = prompt
      , Cv.seedDa = fmap fromIntegral seed
      , Cv.parentGenIdDa = parentGenId
      , Cv.editOpDa = editOp
      , Cv.serializationTitleDa = serializationTitle
    }


generationFromRow :: Dst.GenerationRow -> Cv.GenerationDb
generationFromRow (_, genId, genSize, seed, parentGenId, height, width, transparent, serializationTitle, orientation) =
  Cv.GenerationDb {
      Cv.genIdGe = genId
      , Cv.genSizeGe = genSize
      , Cv.seedGe = fmap fromIntegral seed
      , Cv.parentGenIdGe = parentGenId
      , Cv.heightGe = fromIntegral height
      , Cv.widthGe = fromIntegral width
      , Cv.transparentBackgroundGe = transparent
      , Cv.serializationTitleGe = serializationTitle
      , Cv.orientationGe = orientation
    }


audioPointerFromRow :: Map Int64 Cv.AudioMetadataDb -> Dst.AapRow -> Cv.AudioAssetPointerDb
audioPointerFromRow metadataByPointer (_, uidPointer, expiry, assetPtr, sizeBytes, format, direction) =
  Cv.AudioAssetPointerDb {
      Cv.expiryDatetimeAap = expiry
      , Cv.assetPointerAap = assetPtr
      , Cv.sizeBytesAap = sizeBytes
      , Cv.formatAap = format
      , Cv.toolAudioDirectionAap = direction
      , Cv.metadataAap = Mp.lookup uidPointer metadataByPointer
    }


realtimeFromRow :: Dst.RtuavRow -> Cv.RealTimeUserAVDb
realtimeFromRow (_, _, expiry, frames, videoContainer, audioStart) =
  Cv.RealTimeUserAVDb {
      Cv.expiryDatetimeRtuav = expiry
      , Cv.framesAssetPointersRtuav = frames
      , Cv.videoContainerAssetPointerRtuav = videoContainer
      , Cv.audioStartTimestampRtuav = fromMaybe 0 . Ut.safeScientific <$> audioStart
    }


mkAudioMeta :: Dst.AudioMetaRow -> Cv.AudioMetadataDb
mkAudioMeta (_, startTimestamp, endTimestamp, pretokenizedVq, interruptions, originalSource, transcription,
    wordTranscription, startStamp, endStamp) =
  Cv.AudioMetadataDb {
      Cv.startTimestampAm = startTimestamp
      , Cv.endTimestampAm = endTimestamp
      , Cv.pretokenizedVqAm = pretokenizedVq
      , Cv.interruptionsAm = interruptions
      , Cv.originalAudioSourceAm = originalSource
      , Cv.transcriptionAm = transcription
      , Cv.wordTranscriptionAm = wordTranscription
      , Cv.startStampAm = fromMaybe 0 $ Ut.safeScientific startStamp
      , Cv.endStampAm = fromMaybe 0 $ Ut.safeScientific endStamp
    }


normalizeTyp :: Text -> Text
normalizeTyp = T.pack . map toLower . T.unpack . T.strip


groupVecByKey :: Ord key => Vector row -> (row -> (key, value)) -> Map key (Vector value)
groupVecByKey rows project =
  let
    groupedRev = V.foldl' addRow Mp.empty rows
    addRow grouped row =
      let
        (key, value) = project row
      in
      Mp.insertWith (V.++) key (V.singleton value) grouped
  in
  Mp.map V.reverse groupedRev


requireKey :: (Show key, Ord key) => String -> key -> Map key value -> Either String ()
requireKey label key mapping =
  case Mp.lookup key mapping of
    Nothing -> Left $ "missing " <> label <> " row for uid " <> show key
    Just _ -> Right ()


validateUnique :: (Ord value, Show value) => String -> [value] -> Either String ()
validateUnique label values =
  case duplicateValues values of
    [] -> Right ()
    duplicates -> Left $ "duplicate " <> label <> " values: " <> show duplicates


duplicateValues :: Ord value => [value] -> [value]
duplicateValues values =
  Mp.keys $ Mp.filter (> (1 :: Int)) $ Mp.fromListWith (+) [(value, 1 :: Int) | value <- values]
