module OpenAI.Serialize.IncrUpdate (ReportRaw(..), updateConversation, incrUpdateConversation) where

import Data.Text (Text)
import qualified Data.Text as T

import qualified Hasql.Pool as Pool
import qualified Hasql.Transaction as Htx
import qualified Hasql.Transaction.Sessions as Hts

import qualified OpenAI.Delta.Apply as Apply
import qualified OpenAI.Delta.Plan as Plan
import OpenAI.Delta.Report (ReportRaw(..))
import qualified OpenAI.Delta.Report as Report
import qualified OpenAI.Delta.Snap.Db as SnapDb
import qualified OpenAI.Delta.Snap.Json as SnapJson
import OpenAI.Delta.Types (Conflict)
import qualified OpenAI.Delta.Validate as Validate
import qualified OpenAI.Deserialize.ConversationStmt as ConvStmt
import qualified OpenAI.Json.Reader as Jd


-- | Reconcile an existing raw OpenAI conversation with its JSON
-- representation.
--
-- The conversation row is locked before loading the database snapshot.
-- Snapshot construction, delta planning and validation perform no writes.
-- All message and content insertion or rewriting is delegated through
-- 'OpenAI.Delta.Apply', which uses the shared 'OpenAI.Serialize.Content'
-- implementation.
--
-- Semantic failures are returned inside the pool result:
--
-- @
-- Right (Left message)
-- @
--
-- Database and session failures remain 'Pool.UsageError':
--
-- @
-- Left usageError
-- @
updateConversation :: Pool.Pool -> Jd.Conversation -> Text -> IO (Either Pool.UsageError (Either Text ReportRaw))
updateConversation pool conversation sourceKey =
  Pool.use pool $ Hts.transaction Hts.ReadCommitted Hts.Write $ updateTx conversation sourceKey


-- | Compatibility alias for callers using the former entry-point name.
incrUpdateConversation :: Pool.Pool -> Jd.Conversation -> Text -> IO (Either Pool.UsageError (Either Text ReportRaw))
incrUpdateConversation = updateConversation


updateTx :: Jd.Conversation -> Text -> Htx.Transaction (Either Text ReportRaw)
updateTx conversation sourceKey = do
  lockedConv <- Htx.statement conversation.convIdCv ConvStmt.selectConversationForUpdate
  case lockedConv of
    Nothing -> pure . Left $ "conversation not found: " <> conversation.convIdCv
    Just (uidConv, _, _) -> do
      dbSnapRez <- SnapDb.load uidConv
      let
        jsonSnapRez = SnapJson.build conversation
      case (dbSnapRez, jsonSnapRez) of
        (Left conflicts, _) -> pure . Left $ renderConflicts "database snapshot" conflicts
        (_, Left conflicts) -> pure . Left $ renderConflicts "JSON snapshot" conflicts
        (Right dbSnap, Right jsonSnap) ->
          case Plan.compute Plan.policyDefault dbSnap jsonSnap of
            Left conflicts -> pure . Left $ renderConflicts "delta planning" conflicts
            Right plannedDelta ->
              case Validate.check plannedDelta of
                Left conflicts -> pure . Left $ renderConflicts "delta validation" conflicts
                Right delta -> do
                  applyRez <- Apply.apply sourceKey conversation jsonSnap delta
                  case applyRez of
                    Left conflicts -> do
                      Htx.condemn
                      pure . Left $ renderConflicts "delta application" conflicts
                    Right applyResult -> pure . Right $ Report.fromApply delta applyResult


renderConflicts :: Text -> [Conflict] -> Text
renderConflicts stage conflicts =
  case conflicts of
    [] -> stage <> " failed without conflict details"
    _ -> stage <> " failed: " <> T.intercalate "; " (map (T.pack . show) conflicts)