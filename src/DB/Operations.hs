module DB.Operations where

import Control.Monad (void)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import qualified Data.Text as T
import qualified Data.Vector as V

import Hasql.Session (statement)
import Hasql.Pool (Pool, use)
import qualified Hasql.Session as S

import DB.Statements as Dbs
import HBDoc.Structure

-- Helpers -----------------------------------------------------
err :: Text -> ApiResult a
err e = ApiResult False Nothing (Just e)

ok :: a -> ApiResult a
ok a = ApiResult True (Just a) Nothing


-- Users/Auth --------------------------------------------------
resolveUser :: Pool -> Text -> IO (Either String (Maybe User))
resolveUser pool email = do
  r <- use pool (statement email qGetUserByEmail)
  case r of
    Left ue -> pure . Left $ "@[resolveUser] err: " <> show ue
    Right aValue -> pure $ Right aValue


canUserIO :: Pool -> Int32 -> Text -> Int32 -> IO (ApiResult Bool)
canUserIO pool u perm d = do
  r <- use pool (statement (u, perm, d) qCanUser)
  either (pure . err . T.pack . show) (pure . ok) r


-- Categorisation --------------------------------------------------
listCategories :: Pool -> IO (ApiResult (V.Vector Category))
listCategories pool = do
  r <- use pool (statement () qListCategories)
  either (pure . err . T.pack . show) (pure . ok) r

fetchNodes :: Pool -> Int32 -> IO (ApiResult (V.Vector NodeOut))
fetchNodes pool taxoID = do
  r <- use pool (statement taxoID fetchNodesForTaxo)
  either (pure . err . T.pack . show) (pure . ok) r

-- Documents --------------------------------------------------
listDocs :: Pool -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Text -> Maybe Day -> Maybe UTCTime -> IO (ApiResult (V.Vector DocRow))
listDocs pool dom st tier q limit offset = do
  r <- use pool (statement (dom, st, tier, q, limit, offset) qListDocs)
  either (pure . err . T.pack . show) (pure . ok) r


getDoc :: Pool -> Int32 -> IO (ApiResult DocDetail)
getDoc pool docId = do
  r <- use pool (statement docId qDocDetail)
  case r of
    Left ue           -> pure $ err (T.pack $ show ue)
    Right Nothing     -> pure $ err "doc_not_found"
    Right (Just dd)   -> pure $ ok dd


-- (Text, Int32, Int32, Int32, Int32, Maybe Text, Bool, Bool, Maybe Day, Int32)
{-
document (title, domain_fk, doc_type_fk, tier_fk, status_fk,
     owner_user_fk, residency, ai_allowed, legal_hold, due_date,
     created_by_user_fk)
  values
    ($1::text, $2::int4, $3::int4 ,$4::int4 ,$5::int4
    , $6::text?, $7::bool, $8::bool, $9::date?
    , $10::int4)
-}
createDoc :: Pool -> Int32
      -> Text
      -> Int32 -> Int32 -> Int32 -> Int32 -> Maybe Int32
      -> Maybe Text -> Bool -> Bool -> Maybe Day
      -> IO (ApiResult Int32)
createDoc pool actor title domainID typeID tierID statusID ownerID residency aiAllowed legalHold due = do
  r <- use pool (statement (
        title
        , domainID, typeID, tierID, statusID, ownerID
        , residency, aiAllowed, legalHold
        , due, actor
      ) qCreateDoc)
  either (pure . err . T.pack . show) (pure . ok) r


updateDocMeta :: Pool -> Text -> Int32 -> Int32 -> Int32 -> Int32 -> Maybe Int32 -> Maybe Text -> Bool -> Bool -> Maybe Day -> Int32 -> IO (ApiResult ())
updateDocMeta pool title dom typ tier status owner residency aiAllowed legalHold due docId = do
  r <- use pool (statement (title, dom, typ, tier, status, owner, residency, aiAllowed, legalHold, due, docId) qUpdateDocMeta)
  either (pure . err . T.pack . show) (const $ pure $ ok ()) r


softDeleteDoc :: Pool -> Int32 -> IO (ApiResult ())
softDeleteDoc pool docId = do
  r <- use pool (statement docId qSoftDeleteDoc)
  either (pure . err . T.pack . show) (const $ pure $ ok ()) r

-- Versions ---------------------------------------------------
saveVersion :: Pool -> Int32 -> Int32 -> Maybe Text -> Maybe Text -> Int32 -> IO (ApiResult Int32)
saveVersion pool docId verNo note contentRef author = do
  r <- use pool (statement (docId, verNo, note, contentRef, author) qInsertVersion)
  either (pure . err . T.pack . show) (pure . ok) r

latestVersion :: Pool -> Int32 -> IO (ApiResult (Maybe DocVersion))
latestVersion pool docId = do
  r <- use pool (statement docId qLatestVersion)
  either (pure . err . T.pack . show) (pure . ok) r

-- Comments ---------------------------------------------------
listCommentsIO :: Pool -> Int32 -> IO (ApiResult [Comment])
listCommentsIO pool docId = do
  r <- use pool (statement docId qListComments)
  either (pure . err . T.pack . show) (pure . ok . V.toList) r

addCommentIO :: Pool -> Int32 -> Int32 -> Maybe Int32 -> Text -> IO (ApiResult Int32)
addCommentIO pool actor docId parent body = do
  r <- use pool (statement (docId, parent, actor, body) qAddComment)
  either (pure . err . T.pack . show) (pure . ok) r

deleteCommentIO :: Pool -> Int32 -> Int32 -> IO (ApiResult ())
deleteCommentIO pool actor commentId = do
  r <- use pool (statement (commentId, actor) qDeleteComment)
  either (pure . err . T.pack . show) (const $ pure $ ok ()) r

-- ACLs -------------------------------------------------------
listAclIO :: Pool -> Int32 -> IO (ApiResult [AclEntry])
listAclIO pool docId = do
  r <- use pool (statement docId qListAcl)
  either (pure . err . T.pack . show) (pure . ok . V.toList) r


addAclIO :: Pool -> Int32 -> Int32 -> Text -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> V.Vector Text -> Maybe Text -> Maybe Text -> IO (ApiResult Int32)
addAclIO pool actor docId principal u g r o rights scope scopeVal = do
  r' <- use pool (statement (docId, principal, u, g, r, o, rights, scope, scopeVal, actor) qAddAcl)
  either (pure . err . T.pack . show) (pure . ok) r'


removeAclIO :: Pool -> Int32 -> IO (ApiResult ())
removeAclIO pool aclId = do
  r <- use pool (statement aclId qRemoveAcl)
  either (pure . err . T.pack . show) (const $ pure $ ok ()) r


-- Reports ----------------------------------------------------
heatmapCountsIO :: Pool -> IO (ApiResult [CountCell])
heatmapCountsIO pool = do
  r <- use pool (statement () qHeatmapCounts)
  either (pure . err . T.pack . show) (pure . ok . V.toList) r

sankeyDomainStatusIO :: Pool -> IO (ApiResult [SankeyAB])
sankeyDomainStatusIO pool = do
  r <- use pool (statement () qSankeyDomainStatus)
  either (pure . err . T.pack . show) (pure . ok . V.toList) r

sankeyStatusTierIO :: Pool -> IO (ApiResult [SankeyAB])
sankeyStatusTierIO pool = do
  r <- use pool (statement () qSankeyStatusTier)
  either (pure . err . T.pack . show) (pure . ok . V.toList) r

-- Audit ------------------------------------------------------
recordAuditIO :: Pool -> Int32 -> Text -> Maybe Int32 -> Maybe Text -> Maybe Int32 -> Maybe Text -> IO (ApiResult ())
recordAuditIO pool actor action mDoc mTargetType mTargetUid mUA = do
  r <- use pool (statement (actor, action, mDoc, mTargetType, mTargetUid, mUA) qAudit)
  either (pure . err . T.pack . show) (const $ pure $ ok ()) r
