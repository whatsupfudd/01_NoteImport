{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}
{-# HLINT ignore "Use forM_" #-}
module OpenAI.Serialize.Discussion where

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
import qualified Data.UUID as Uu
import qualified Data.Vector as V
import Data.Vector (Vector)

import qualified Data.Aeson as Ae


import qualified Hasql.Pool as Hp
import qualified Hasql.Session as Ses
import qualified OpenAI.Serialize.DiscussionStmt as St
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as Tx

import OpenAI.Json.Reader
import OpenAI.Types
import OpenAI.Parse (analyzeDiscussion, findRootNode)
import Data.List (find)

-- | useTx: helper to wrap a statement into a transaction.
useTx :: Hp.Pool -> Tx.Transaction tr -> IO (Either Hp.UsageError tr)
useTx pool stmts = Hp.use pool (Tx.transaction Tx.ReadCommitted Tx.Write stmts)   -- 


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
  case Uu.fromString $ T.unpack convId of
    Nothing -> pure . Left $ "@storeDiscussion] invalid UUID: " <> T.unpack convId
    Just oaiid -> do
      r <- Hp.use pool $
        Tx.transaction Tx.ReadCommitted Tx.Write $ do
          (ctxUid, ctxUuid) <- Tx.statement (title, oaiid) St.insertDiscussion

          -- issues :: [Text]
          forM_ (zip [1 :: Int32 ..] ctx.issues) $ \(i, t) ->
            Tx.statement (ctxUid, i, t) St.insertDiscussionIssue

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
