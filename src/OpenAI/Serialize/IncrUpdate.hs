{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module OpenAI.Serialize.IncrUpdate ( incrUpdateConversation )
where

-- | Incremental update of an already-existing conversation in Postgres.
--
-- Assumptions / goals:
--   * The caller already validated the conversation exists in DB (not first entry).
--   * In practice, exported conversations are linear chains (one child per node).
--   * We only insert NEW tail nodes + their message trees (messages/authors/contents/...).
--   * We still update the top-level conversation title/update_time.
--   * We record the prior top-level state in 'oai.conversation_previous'.
--   * We append a trace row in 'oai.conversation_ingest' at the end.
--   * All of the above is wrapped in one transaction.
--
-- Notes:
--   * This module uses conservative per-row inserts; incremental batches are typically small.
--   * If you later want "mid-chain" edits or re-writes, add hashing + selective rewrite.

import Control.Monad (forM_, when)
import Control.Exception (Exception, throwIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import Data.Int (Int32, Int64)
import Data.List (sort)
import qualified Data.Map.Strict as Mp
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.Vector as V

import qualified Data.Aeson as Ae
import Data.Aeson (Value)

import qualified Hasql.Pool as Pool

import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as TxS

-- cryptonite (recommended). If you prefer another SHA256 lib, replace 'sha256'.
import qualified Crypto.Hash as CH
import qualified Data.ByteArray as BA

import qualified OpenAI.Json.Reader as Jd
import qualified OpenAI.Deserialize.ConversationStmt as Dcv
import qualified OpenAI.Serialize.ConversationStmt as Scv


-- ----------------------------
-- Public API
-- ----------------------------


-- | Incrementally update an already-existing conversation.
--
-- * 'sourceKey' is recorded in 'oai.conversation_ingest.source_key'.
-- * The function inserts a 'oai.conversation_previous' snapshot (title/update_time)
--   and then applies the new state.
--
-- Returns Left <err> on failure, Right () on success.
incrUpdateConversation :: Pool.Pool -> Jd.Conversation -> Text -> IO (Either String ())
incrUpdateConversation pool conv sourceKey = do
  r <- Pool.use pool $
    TxS.transaction TxS.ReadCommitted TxS.Write $ incrUpdateTx conv sourceKey
  pure $ case r of
    Left e -> Left (show e)
    Right rez -> Right ()


-- ----------------------------
-- Transaction orchestration
-- ----------------------------


incrUpdateTx :: Jd.Conversation -> Text -> Tx.Transaction (Either String ())
incrUpdateTx conv sourceKey = do
  -- 1) Resolve conversation row + lock it.
  convEid <- either (pure . Left) (pure . Right) (toUuid conv.convIdCv)
  case convEid of
    Left e -> pure (Left e)
    Right ceid -> do
      mb <- Tx.statement ceid Dcv.selectConversationForUpdate
      case mb of
        Nothing -> pure (Left "@[incrUpdateTx] conversation not found (expected existing)")
        Just (cUid, oldTitle, oldUpd) -> do
          -- 2) Store previous top-level state.
          Tx.statement (cUid, oldUpd, oldTitle) Scv.insertConversationPrevious

          -- 3) Update top-level row.
          newUpd <- either (pure . Left) (pure . Right) (toUtc conv.updateTimeCv)
          case newUpd of
            Left e -> pure (Left e)
            Right newUpdAt -> do
              Tx.statement (conv.titleCv, newUpdAt, cUid) Scv.updateConversation

              -- 4) Find last node seq + uid.
              maxSeq <- Tx.statement cUid Dcv.selectMaxNodeSeq
              when (maxSeq < 0) $ do
                Tx.condemn
                -- pure $ Left "@[incrUpdateTx] max seq < 0"

              mbLastNodeUid <- Tx.statement (cUid, maxSeq) Dcv.selectNodeUidBySeq
              case mbLastNodeUid of
                Nothing -> do
                   Tx.condemn
                   pure $ Left "@[incrUpdateTx] last node uid not found for max seq"
                Just lastNodeUid -> do
                  -- 5) Linearize incoming nodes into deterministic chain order.
                  orderedNodeIds <-
                    either (pure . Left) (pure . Right) (linearizeNodeChain conv.mappingCv)
                  case orderedNodeIds of
                    Left e -> pure (Left (T.unpack e))
                    Right nodeIds -> do
                      let startIdx :: Int
                          startIdx = fromIntegral maxSeq + 1

                          newTail = drop startIdx nodeIds

                      -- 6) Insert new tail nodes + message trees.
                      ei <- insertTail cUid lastNodeUid startIdx conv.mappingCv newTail
                      case ei of
                        Left e -> pure (Left e)
                        Right () -> do
                          -- 7) Trace ingest.
                          let sha = sha256 (Ae.encode conv)
                          Tx.statement (cUid, Just sourceKey, Just sha, ("single-file" :: Text)) Scv.insertConversationIngest
                          pure $ Right ()


insertTail :: Int64 -> Int64 -> Int -> Mp.Map NodeIdT Jd.Node -> [NodeIdT]
        -> Tx.Transaction (Either String ())
insertTail conversationUid lastNodeUid startIdx mapping newNodeIds = do
  go lastNodeUid (fromIntegral startIdx) newNodeIds
  where
    go :: Int64 -> Int32 -> [NodeIdT] -> Tx.Transaction (Either String ())
    go _ _ [] = pure (Right ())
    go parentUid seqNbr (nid:rest) =
      case Mp.lookup nid mapping of
        Nothing -> pure (Left ("@[insertTail] node not found in mapping: " <> T.unpack (showNodeId nid)))
        Just node -> do
          -- Insert node
          nUuid <- case toUuid (nodeId node) of
            Left e -> pure (Left e)
            Right u -> pure (Right u)
          case nUuid of
            Left e -> pure (Left e)
            Right nodeUuid -> do
              newNodeUid <- Tx.statement (conversationUid, nodeUuid, Just parentUid, seqNbr) Scv.insertNodeRetUid

              -- Insert message tree (if any)
              eiMsg <-
                case node.messageNd of
                  Nothing -> pure (Right ())
                  Just msg -> insertMessageTree newNodeUid msg

              case eiMsg of
                Left e -> pure (Left e)
                Right () -> go newNodeUid (seqNbr + 1) rest


-- ----------------------------
-- Message tree insertion
-- ----------------------------

insertMessageTree :: Int64 -> Jd.Message -> Tx.Transaction (Either String ())
insertMessageTree nodeUid msg = do
  -- message times (schema: NOT NULL)
  let
    eiCreate = maybe (Left "message.create_time missing") (Right . epochToUtc) msg.createTimeMsg
    eiUpdate = maybe (Left "message.update_time missing") (Right . epochToUtc) msg.updateTimeMsg

  case (eiCreate, eiUpdate) of
    (Left e, _) -> pure (Left ("@[insertMessageTree] " <> e))
    (_, Left e) -> pure (Left ("@[insertMessageTree] " <> e))
    (Right cAt, Right uAt) -> do

      mUid <- Tx.statement (nodeUid, msg.idMsg, cAt, uAt, msg.statusMsg, msg.endTurnMsg
          , msg.weightMsg, Ae.toJSON (HM.fromList $ Mp.toList msg.metadataMsg), msg.recipientMsg, msg.channelMsg, 0) Scv.insertMessageRetUid

      -- author (single)
      let
        au = msg.authorMsg
      Tx.statement (mUid, au.roleAu,au.nameAu, Ae.toJSON (HM.fromList $ Mp.toList au.metadataAu)) Scv.insertAuthor

      -- contents: most exports carry a single 'contentMsg'. We still store as seq=0.
      insertContent mUid 0 msg.contentMsg


insertContent :: Int64 -> Int32 -> Jd.Content -> Tx.Transaction (Either String ())
insertContent messageUid seqNbr content = do
  let (ctype, payload) = contentTypeAndJson content

  cUid <- Tx.statement (messageUid, ctype, seqNbr) Scv.insertContentRetUid

  -- Insert subtype payload
  case payload of
    CodePayload lang fmt txt ->
      Tx.statement (cUid, lang, fmt, txt) Scv.insertCodeContent >> pure (Right ())

    ExecutionOutputPayload txt ->
      Tx.statement (cUid, txt) Scv.insertExecutionOutputContent >> pure (Right ())

    ModelEditableContextPayload msc repo rs sc ->
      Tx.statement (cUid, msc, repo, rs, sc) Scv.insertModelEditableContextContent >> pure (Right ())

    ReasoningRecapPayload txt ->
      Tx.statement (cUid, txt) Scv.insertReasoningRecapContent >> pure (Right ())

    SystemErrorPayload nm txt ->
      Tx.statement (cUid, nm, txt) Scv.insertSystemErrorContent >> pure (Right ())

    TetherBrowsingDisplayPayload results summ assets tid ->
      Tx.statement (cUid, results, summ, assets, tid) Scv.insertTetherBrowsingDisplayContent >> pure (Right ())

    TetherQuotePayload url domain txt title tid ->
      Tx.statement (cUid, url, domain, txt, title, tid) Scv.insertTetherQuoteContent >> pure (Right ())

    TextPayload parts ->
      Tx.statement (cUid, parts) Scv.insertTextContent >> pure (Right ())

    ThoughtsPayload sourceId thoughtsVec -> do
      Tx.statement (cUid, sourceId) Scv.insertThoughtsContent
      -- thoughts rows with seqnbr
      forM_ (zip [0..] (V.toList thoughtsVec)) $ \(s, th) ->
        Tx.statement (cUid, th.summaryTh, th.contentTh, Ae.toJSON th.chunksTh, fromMaybe False th.finishedTh, s) Scv.insertThought
      pure (Right ())

    UnknownPayload opaque ->
      Tx.statement (cUid, opaque) Scv.insertUnknownContent >> pure (Right ())


-- ----------------------------
-- Content mapping (JSON -> DB payload)
-- ----------------------------

-- | Internal representation of payload variants to keep insertContent readable.
-- This deliberately mirrors your subtype tables.

data ContentPayload
  = CodePayload !Text !(Maybe Text) !Text
  | ExecutionOutputPayload !Text
  | ModelEditableContextPayload !Text !(Maybe Value) !(Maybe Value) !(Maybe Value)
  | ReasoningRecapPayload !Text
  | SystemErrorPayload !Text !Text
  | TetherBrowsingDisplayPayload !Text !(Maybe Text) !(Maybe Value) !(Maybe Text)
  | TetherQuotePayload !Text !Text !Text !Text !(Maybe Text)
  | TextPayload !(V.Vector Text)
  | ThoughtsPayload !Text !(V.Vector Jd.Thought)
  | UnknownPayload !Value

contentTypeAndJson :: Jd.Content -> (Text, ContentPayload)
contentTypeAndJson = \case
  Jd.CodeCT lang fmt txt ->
    ("code", CodePayload lang fmt txt)
  Jd.ExecutionOutputCT txt ->
    ("execution_output", ExecutionOutputPayload txt)
  Jd.ModelEditableContextCT msc repo rs sc ->
    ("model_editable_context", ModelEditableContextPayload msc repo rs sc)
  Jd.ReasoningRecapCT txt ->
    ("reasoning_recap", ReasoningRecapPayload txt)
  Jd.SystemErrorCT nm txt ->
    ("system_error", SystemErrorPayload nm txt)
  Jd.TetherBrowsingDisplayCT results mbSummary assets tid ->
    let
      assetsValue = Ae.toJSON <$> assets
    in
    ("tether_browsing_display", TetherBrowsingDisplayPayload results mbSummary assetsValue tid)
  Jd.TetherQuoteCT url domain txt title tid ->
    ("tether_quote", TetherQuotePayload url domain txt title tid)
  Jd.TextCT parts ->
    ("text", TextPayload (V.fromList parts))
  Jd.ThoughtsCT thoughts sourceId ->
    ("thoughts", ThoughtsPayload sourceId (V.fromList thoughts))
  -- Multimodal is supported by tables, but JSON part typing varies; to preserve fidelity,
  -- store it as unknown for now unless/until you map the parts 1:1.
  Jd.MultimodalTextCT parts ->
    ("multimodal_text", UnknownPayload (Ae.toJSON parts))
  Jd.OtherCT contentType raw ->
    ("unknown:" <> contentType, UnknownPayload (Ae.toJSON (HM.fromList $ Mp.toList raw)))
  other -> ("unknown", UnknownPayload (Ae.toJSON other))


-- ----------------------------
-- Linearisation (chain)
-- ----------------------------

-- Your JSON mapping key may be UUID or Text in different stages.
-- We keep the key abstract and require two small accessors.

type NodeIdT = Text

nodeId :: Jd.Node -> NodeIdT
nodeId = Jd.idNd

showNodeId :: NodeIdT -> Text
showNodeId = id

-- | Convert the node mapping into a deterministic chain order.
--
-- Strategy:
--   * Find a root: node with parent == Nothing (fallback).
--   * Walk by children, choosing the smallest child ID if multiple (stable).
--   * Stop on cycles or missing references.
linearizeNodeChain :: Mp.Map NodeIdT Jd.Node -> Either Text [NodeIdT]
linearizeNodeChain mapping = do
  root <-
    case findRoot mapping of
      Nothing -> Left "@[linearizeNodeChain] no root node found"
      Just r -> Right r

  let childMap =
        Mp.fromListWith (++)
          [ (p, [c])
          | (c, n) <- Mp.toList mapping
          , Just p <- [n.parentNd]
          ]

      go seen cur =
        if Mp.member cur seen
          then Left ("@[linearizeNodeChain] cycle detected at " <> showNodeId cur)
          else
            let seen' = Mp.insert cur () seen
                nexts = sort (Mp.findWithDefault [] cur childMap)
            in case nexts of
                [] -> Right [cur]
                (nxt:_) -> (cur :) <$> go seen' nxt

  go Mp.empty (nodeId root)

findRoot :: Mp.Map NodeIdT Jd.Node -> Maybe Jd.Node
findRoot mp =
  case filter (\n -> isNothing n.parentNd) (Mp.elems mp) of
    [] -> Nothing
    (x:_) -> Just x


-- ----------------------------
-- Small typeclass helpers
-- ----------------------------

class ToUuid a where
  toUuid :: a -> Either String UUID

instance ToUuid UUID where
  toUuid = Right

instance ToUuid Text where
  toUuid t = maybe (Left ("invalid uuid: " <> T.unpack t)) Right (UUID.fromText t)


class ToUtc a where
  toUtc :: a -> Either String UTCTime

instance ToUtc UTCTime where
  toUtc = Right

instance ToUtc Double where
  toUtc = Right . epochToUtc

instance ToUtc Text where
  toUtc t =
    case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (T.unpack t) of
      Just u -> Right u
      Nothing -> Left ("invalid UTC time: " <> T.unpack t)

epochToUtc :: Double -> UTCTime
epochToUtc = posixSecondsToUTCTime . realToFrac


sha256 :: BL.ByteString -> ByteString
sha256 bs =
  let d :: CH.Digest CH.SHA256
      d = CH.hashlazy bs
  in BA.convert d
