{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}
{-# HLINT ignore "Use forM_" #-}
module OpenAI.Operations where

import Control.Monad (when, forM_, forM, void)
import Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString.Lazy as Bs
import Data.Int (Int64, Int32)
import qualified Data.List as L
import qualified Data.Map.Strict as Mp
import qualified Data.HashMap.Strict as HM
import Data.Maybe (isJust, fromJust, fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import qualified Data.Vector as V
import Data.Vector (Vector)

import qualified Data.Aeson as Ae


import qualified Hasql.Pool as Hp
import qualified Hasql.Session as Ses
import qualified OpenAI.Statements as St
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as Tx

import OpenAI.Json.Reader
import OpenAI.Types
import OpenAI.Parse (analyzeDiscussion, findRootNode)
import Data.List (find)

addDiscussionSession :: Conversation -> Tx.Transaction Int64
addDiscussionSession disc = Tx.statement (disc.titleCv, disc.convIdCv, disc.createTimeCv, disc.updateTimeCv) St.insertDiscussionStmt

addNodeSession :: Int64 -> Maybe Int64 -> Text -> Node -> Tx.Transaction Int64
addNodeSession discussionID mbParentID node_id node = do
  nuid <- Tx.statement (discussionID, node_id, mbParentID) St.insertNodeStmt
  when (isJust node.messageNd) $
    addMessageSession nuid (fromJust node.messageNd)
  pure nuid

addMessageSession :: Int64 -> Message -> Tx.Transaction ()
addMessageSession node_uid msg = do
  muid <- Tx.statement
    (node_uid, msg.idMsg, msg.createTimeMsg, msg.updateTimeMsg, msg.statusMsg
    , msg.endTurnMsg, msg.weightMsg, Ae.toJSON (HM.fromList $ Mp.toList msg.metadataMsg)
    , msg.recipientMsg, msg.channelMsg
    ) St.insertMessageStmt
  addAuthorSession muid msg.authorMsg
  addContentSession muid msg.contentMsg


addAuthorSession :: Int64 -> Author -> Tx.Transaction ()
addAuthorSession msg_uid au = Tx.statement
    (msg_uid, au.roleAu, au.nameAu
    , Ae.toJSON (HM.fromList $ Mp.toList au.metadataAu)) St.insertAuthorStmt


addContentSession :: Int64 -> Content -> Tx.Transaction ()
addContentSession msg_uid content = do
  cuid <- Tx.statement (msg_uid, contentType content) St.insertContentStmt
  case content of
    CodeCT lang rfn txt -> addCodeContentSession cuid lang rfn txt
    ExecutionOutputCT txt -> addExecutionOutputContentSession cuid txt
    ModelEditableContextCT msc rep rs sc -> addModelEditableContextSession cuid msc rep rs sc
    MultimodalTextCT parts -> forM_ parts (addMultiModalPartSession cuid)
    ReasoningRecapCT cnt -> addReasoningRecapContentSession cuid cnt
    SystemErrorCT name txt -> addSystemErrorContentSession cuid name txt
    TetherBrowsingDisplayCT results summary assets tetherID -> addTetherBrowsingDisplayContentSession cuid results summary assets tetherID
    TetherQuoteCT url domain text title tetherID -> addTetherQuoteContentSession cuid url domain text title tetherID
    TextCT parts -> addTextContentSession cuid parts
    ThoughtsCT ths sam -> addThoughtsContentSession cuid ths sam
    OtherCT _ _ -> return ()  -- Skip


contentType :: Content -> Text
contentType (CodeCT {}) = "code"
contentType (ExecutionOutputCT {}) = "execution_output"
contentType (ModelEditableContextCT {}) = "model_editable_context"
contentType (MultimodalTextCT {}) = "multimodal_text"
contentType (ReasoningRecapCT {}) = "reasoning_recap"
contentType (SystemErrorCT {}) = "system_error"
contentType (TetherBrowsingDisplayCT {}) = "tether_browsing_display"
contentType (TetherQuoteCT {}) = "tether_quote"
contentType (TextCT {}) = "text"
contentType (ThoughtsCT {}) = "thoughts"
contentType (OtherCT ct _) = ct


addCodeContentSession :: Int64 -> Text -> Maybe Text -> Text -> Tx.Transaction ()
addCodeContentSession cuid lang rfn txt = Tx.statement (cuid, lang, rfn, txt) St.insertCodeContentStmt

addExecutionOutputContentSession :: Int64 -> Text -> Tx.Transaction ()
addExecutionOutputContentSession cuid txt = Tx.statement (cuid, txt) St.insertExecutionOutputContentStmt


addModelEditableContextSession :: Int64 -> Text -> Maybe Ae.Value -> Maybe Ae.Value -> Maybe Ae.Value -> Tx.Transaction ()
addModelEditableContextSession cuid msc rep rs sc = Tx.statement
    (cuid, msc, fmap Ae.toJSON rep, fmap Ae.toJSON rs, fmap Ae.toJSON sc) St.insertModelEditableContextStmt

addReasoningRecapContentSession :: Int64 -> Text -> Tx.Transaction ()
addReasoningRecapContentSession cuid cnt = Tx.statement (cuid, cnt) St.insertReasoningRecapContentStmt

addSystemErrorContentSession :: Int64 -> Text -> Text -> Tx.Transaction ()
addSystemErrorContentSession cuid name txt = Tx.statement (cuid, name, txt) St.insertSystemErrorContentStmt

addTetherBrowsingDisplayContentSession :: Int64 -> Text -> Maybe Text -> Maybe [Ae.Value] -> Maybe Text -> Tx.Transaction ()
addTetherBrowsingDisplayContentSession cuid results summary assets tetherID = Tx.statement (cuid, results, fmap Ae.toJSON summary, fmap Ae.toJSON assets, tetherID) St.insertTetherBrowsingDisplayContentStmt

addTetherQuoteContentSession :: Int64 -> Text -> Text -> Text -> Text -> Maybe Text -> Tx.Transaction ()
addTetherQuoteContentSession cuid url domain text title tetherID = Tx.statement (cuid, url, domain, text, title, tetherID) St.insertTetherQuoteContentStmt

addTextContentSession :: Int64 -> [Text] -> Tx.Transaction ()
addTextContentSession cuid parts = Tx.statement (cuid, V.fromList parts) St.insertTextContentStmt

addThoughtsContentSession :: Int64 -> [Thought] -> Text -> Tx.Transaction ()
addThoughtsContentSession cuid ths sam = do
  Tx.statement (cuid, sam) St.insertThoughtsContentStmt
  forM_ ths $ \th -> addThoughtSession cuid th

addThoughtSession :: Int64 -> Thought -> Tx.Transaction ()
addThoughtSession tc_uid th = Tx.statement
    (tc_uid, th.summaryTh, th.contentTh, Ae.toJSON th.chunksTh, fromMaybe False th.finishedTh) St.insertThoughtStmt


-- Recursive node tree insertion
addNodeTreeSession :: Int64 -> Mp.Map Text Node -> Text -> Maybe Int64 -> Tx.Transaction (Either String ())
addNodeTreeSession discussionID mapping nodeEid mbParentID =
  let
    mbNode =  Mp.lookup nodeEid mapping
  in do
  case mbNode of
    Just aNode -> do
      nuid <- addNodeSession discussionID mbParentID nodeEid aNode
      forM_ aNode.childrenNd $ \child_id ->
        addNodeTreeSession discussionID mapping child_id (Just nuid)
      pure $ Right ()
    Nothing -> pure . Left $ "@[addNodeTreeSession] node not found: " <> show nodeEid


-- Main session: wrap in transaction
useTx :: Hp.Pool -> Tx.Transaction tr -> IO (Either Hp.UsageError tr)
useTx pool stmts = Hp.use pool (Tx.transaction Tx.ReadCommitted Tx.Write stmts)   -- 


addDiscussion :: Hp.Pool -> Conversation -> IO (Either Hp.UsageError (Either String Int64))
addDiscussion pool discussion = do
  -- liftIO $ putStrLn $ "@[addDiscussion] adding discussion: " <> show discussion.titleCv
  useTx pool $ do
    discussionID <- addDiscussionSession discussion
    let
      mbRootNode = findRootNode discussion.mappingCv
    case mbRootNode of
      Nothing -> pure . Left $ "@[addDiscussion] no root node found"
      Just rootNode -> do
        eiTreeRez <- addNodeTreeSession discussionID discussion.mappingCv rootNode.idNd Nothing
        case eiTreeRez of
          Left err -> pure . Left $ err <> ": " <> T.unpack ("Title: " <> discussion.titleCv <> ", id: " <> discussion.convIdCv) <> "\n" <> show discussion
          Right () -> pure . Right $ discussionID

-- New stuff for multimodal text content:

{-
assetPointerPap :: String,
    sizeBytesPap :: Int,
    widthPap :: Int,
    heightPap :: Int,
    foveaPap :: Maybe Value,
    metadataPap :: Metadata
-}
addMultiModalPartSession :: Int64 -> MultiModalPart -> Tx.Transaction ()
addMultiModalPartSession contentID part = do
  partID <- Tx.statement (contentID, mmPartContentType part) St.insertMultiModalPartStmt
  case part of
    AudioTranscriptionPT text direction decodingId ->
      Tx.statement (partID, text, direction, decodingId) St.insertAudioTranscriptionMMPartStmt
    AudioAssetPointerPT value -> do
      assetPtrID <- Tx.statement (partID, value.expiryDatetimeAap, value.assetPointerAap, fromIntegral value.sizeBytesAap, value.formatAap, value.toolAudioDirectionAap) St.insertAudioAssetPointerMMPartStmt
      case value.metadataAap of
        Just metadata -> addAudioMetadataSession assetPtrID metadata
        Nothing -> return ()
    TextPT text -> Tx.statement (partID, text) St.insertTextMMPartStmt
    ImageAssetPointerPT assetPointer sizeBytes width height fovea metadata -> do
      imgPartID <- Tx.statement (
            partID, assetPointer, fromIntegral sizeBytes, fromIntegral width, fromIntegral height
            , Ae.toJSON <$> fovea
          ) St.insertImageAssetPointerMMPartStmt
      case metadata of
        Just metadata -> addImageMetadataSession imgPartID metadata
        Nothing -> return ()
    RealTimeUserAVPT expiryDatetime framesAssetPointers videoContainerAssetPointer audioAssetPointer audioStartTimestamp -> 
      let
        framePtrValues = if null framesAssetPointers then Nothing else Just (Ae.toJSON framesAssetPointers)
      in do
      assetPtrID <- Tx.statement (
          partID, expiryDatetime, framePtrValues
          , videoContainerAssetPointer
          , audioStartTimestamp
        ) St.insertRealTimeUserAVMMPartStmt
      case audioAssetPointer.metadataAap of
        Just metadata -> addAudioMetadataSession assetPtrID metadata
        Nothing -> return ()


mmPartContentType :: MultiModalPart -> Text
mmPartContentType (AudioTranscriptionPT {}) = "audio_transcription"
mmPartContentType (AudioAssetPointerPT {}) = "audio_asset_pointer"
mmPartContentType (ImageAssetPointerPT {}) = "image_asset_pointer"
mmPartContentType (RealTimeUserAVPT {}) = "real_time_user_audio_video_asset_pointer"
mmPartContentType (TextPT {}) = "text"


addImageMetadataSession :: Int64 -> ImageMetadata -> Tx.Transaction ()
addImageMetadataSession partID md = do
  dalle_uid <- case md.dalleMd of
    Just d -> Just <$> addDalleSession partID d
    Nothing -> return Nothing
  gen_uid <- case md.generationMd of
    Just g -> Just <$> addGenerationSession partID g
    Nothing -> return Nothing
  Tx.statement (dalle_uid, fmap Ae.toJSON md.gizmoMd, gen_uid, fromIntegral <$> md.containerPixelHeightMd
      , fromIntegral <$> md.containerPixelWidthMd, Ae.toJSON <$> md.emuOmitGlimpseImageMd
      , Ae.toJSON <$> md.emuPatchesOverrideMd, Ae.toJSON <$> md.lpeKeepPatchIjhwMd
      , Ae.toJSON <$> md.lpeDeltaEncodingChannelMd, md.sanitizedMd, Ae.toJSON <$> md.assetPointerLinkMd
      , Ae.toJSON <$> md.watermarkedAssetPointerMd, Ae.toJSON <$> md.isNoAuthPlaceholderMd) St.insertImageMetadataStmt

addDalleSession :: Int64 -> Dalle -> Tx.Transaction Int64
addDalleSession partID d =
  Tx.statement (d.genIdDa, d.promptDa, fromIntegral <$> d.seedDa, d.parentGenIdDa, d.editOpDa, d.serializationTitleDa) St.insertDalleStmt

addGenerationSession :: Int64 -> Generation -> Tx.Transaction Int64
addGenerationSession partID g =
  Tx.statement (g.genIdGe, g.genSizeGe, fromIntegral <$> g.seedGe, g.parentGenIdGe
        , fromIntegral g.heightGe, fromIntegral g.widthGe, g.transparentBackgroundGe, g.serializationTitleGe, g.orientationGe) St.insertGenerationStmt


addAudioMetadataSession :: Int64 -> AudioMetadata -> Tx.Transaction ()
addAudioMetadataSession partID md = do
  Tx.statement (
      partID, Ae.toJSON <$> md.startTimestampAm, Ae.toJSON <$> md.endTimestampAm
      , Ae.toJSON <$> md.pretokenizedVqAm, Ae.toJSON <$> md.interruptionsAm
      , Ae.toJSON <$> md.originalAudioSourceAm, Ae.toJSON <$> md.transcriptionAm
      , Ae.toJSON <$> md.wordTranscriptionAm, md.startAm, md.endAm
    ) St.insertAudioMetadataStmt


-- Reading the DB:
fetchAllDiscussions :: Hp.Pool -> IO (Either Hp.UsageError (Mp.Map Text Int64))
fetchAllDiscussions pool = do
  dbRez <- Hp.use pool (Ses.statement () St.fetchAllDiscussions)
  case dbRez of
    Left ue -> pure . Left $ ue
    Right discussions ->
      let
        discussionMap = Mp.fromList $ map (\(uid, title) -> (title, uid)) (V.toList discussions)
      in
      pure $ Right discussionMap

--- Storing the Discussion context:
-- | Persist a 'Context' (and all nested MessageFsm structures) into Postgres.
--
-- Requirements satisfied:
--   * Uses hasql-transaction to wrap inserts atomically (rollback on failure).
--   * Uses embedded SQL quasiquotes (hasql-th) instead of string SQL.
--   * Targets the schema under the "legal" Postgres schema provided earlier.
--
-- Notes:
--   * We avoid passing custom enum types as parameters directly; instead we pass
--     'text' and cast to enum inside SQL (to keep hasql-th type mappings simple).
--   * Adjust the import for your conversation types.
--

-- | Store a Context atomically.
--
-- Returns the (context_uid, context_uuid) on success.
-- Any failure rolls back the entire insert.
storeDiscussion :: Hp.Pool -> Text -> Text -> Context -> IO (Either String (Int64, UUID))
storeDiscussion pool title convId ctx = do
  r <- Hp.use pool $
    Tx.transaction Tx.ReadCommitted Tx.Write $ do
      (ctxUid, ctxUuid) <- Tx.statement (title, convId) St.insertContext

      -- issues :: [Text]
      forM_ (zip [1 :: Int32 ..] ctx.issues) $ \(i, t) ->
        Tx.statement (ctxUid, i, t) St.insertContextIssue

      -- messages :: [MessageFsm]
      inserted <- forM (zip [1 :: Int32 ..] (reverse ctx.messages)) $ \(i, m) -> do
        let (kindTxt, createdTs, updatedTs) = messageHeaderData m
        msgUid <- Tx.statement (ctxUid, i, kindTxt, createdTs, updatedTs) St.insertMessage

        case m of
          UserMF _ um -> do
            Tx.statement (msgUid, um.textUM) St.insertUserMessage
            addAttachments msgUid um.attachmentsUM

          AssistantMF _ am -> do
            respUid <- case response am of
              Nothing -> pure Nothing
              Just (ResponseAst txt) -> Just <$> Tx.statement txt St.insertResponseAst

            Tx.statement (msgUid, respUid) St.insertAssistantMessage
            addAttachments msgUid am.attachmentsAM
            addSubActions msgUid am.subActions

          SystemMF _ (SystemMessage txt) ->
            Tx.statement (msgUid, txt) St.insertSystemMessage

          ToolMF _ (ToolMessage txt) ->
            Tx.statement (msgUid, txt) St.insertToolMessage

          UnknownMF _ (UnknownMessage txt) ->
            Tx.statement (msgUid, txt) St.insertUnknownMessage

        pure (i, msgUid, messageKey m)

      pure (ctxUid, ctxUuid)
  pure $ either (Left . show) Right r


addAttachments :: Int64 -> [Text] -> Tx.Transaction ()
addAttachments msgUid xs =
  forM_ (zip [1 :: Int32 ..] xs) $ \(i, v) ->
    Tx.statement (msgUid, i, v) St.insertAttachment

addSubActions :: Int64 -> [SubAction] -> Tx.Transaction ()
addSubActions msgUid sas =
  forM_ (zip [1 :: Int32 ..] sas) $ \(i, sa) ->
    case sa of
      ReflectionSA rf -> do
        saUid <- Tx.statement (msgUid, i, "reflection", Nothing) St.insertSubAction
        rfUid <- Tx.statement (saUid, summaryRF rf, contentRF rf, finishedRF rf) St.insertReflection
        forM_ (zip [1 :: Int32 ..] (chunksRF rf)) $ \(j, t) ->
          Tx.statement (rfUid, j, t) St.insertReflectionChunk

      CodeSA cc -> do
        saUid <- Tx.statement (msgUid, i, "code", Nothing) St.insertSubAction
        Tx.statement (saUid, languageCC cc, responseFormatNameCC cc, textCC cc) St.insertCode

      ToolCallSA tc -> do
        saUid <- Tx.statement (msgUid, i, "tool_call", Nothing) St.insertSubAction
        Tx.statement (saUid, toolNameTC tc, toolInputTC tc) St.insertToolCall

      IntermediateSA t ->
        void $ Tx.statement (msgUid, i, "intermediate", Just t) St.insertSubAction


-- -----------------------
-- Keying / timing
-- -----------------------

-- Used only for best-effort matching of currentMsg.
messageKey :: MessageFsm -> (Text, Maybe Double, Maybe Double, Text)
messageKey m =
  let (Timing c u, body, k) = case m of
        UserMF t um -> (t, um.textUM, "user")
        AssistantMF t am ->
          ( t
          , maybe "" (\(ResponseAst x) -> x) am.response
          , "assistant"
          )
        SystemMF t (SystemMessage x) -> (t, x, "system")
        ToolMF t (ToolMessage x) -> (t, x, "tool")
        UnknownMF t (UnknownMessage x) -> (t, x, "unknown")
  in (k, c, u, T.take 2000 body)

messageHeaderData :: MessageFsm -> (Text, Maybe UTCTime, Maybe UTCTime)
messageHeaderData m =
  let Timing c u = case m of
        UserMF t _ -> t
        AssistantMF t _ -> t
        SystemMF t _ -> t
        ToolMF t _ -> t
        UnknownMF t _ -> t
  in (messageKindText m, epochToUtc <$> c, epochToUtc <$> u)

messageKindText :: MessageFsm -> Text
messageKindText m = case m of
  UserMF{} -> "user"
  AssistantMF{} -> "assistant"
  SystemMF{} -> "system"
  ToolMF{} -> "tool"
  UnknownMF{} -> "unknown"

epochToUtc :: Double -> UTCTime
epochToUtc = posixSecondsToUTCTime . realToFrac
