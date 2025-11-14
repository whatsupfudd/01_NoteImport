{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module DB.Statements where

import Control.Exception (bracket)
import Data.Profunctor (dimap)
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, UTCTime, toGregorian)
import Data.Vector (Vector)
import qualified Data.Vector as V

import qualified Data.Aeson as Ae

import Hasql.Connection (Settings, acquire, release)
import Hasql.Session (Session, statement)
import qualified Hasql.Session as Ses
import qualified Hasql.Statement as St
import Hasql.Pool (Pool)
import qualified Hasql.Pool as Pool
import Hasql.TH

import HBDoc.Structure

-- Users / Auth -----------------------------------------------
qGetUserByEmail :: St.Statement Text (Maybe User)
qGetUserByEmail = dimap id (\case
    Just (uid, email, fullName, isExternal, orgName) -> Just (User uid email fullName isExternal orgName)
    Nothing -> Nothing
  ) [maybeStatement|
    select u.uid::int4
        , u.email::text
        , u.full_name::text
        , u.is_external::bool
        , o.name::text?
    from kms.usertb u
    left join kms.organization o on o.uid = u.org_fk
    where u.email = $1::text
  |]


qCanUser :: St.Statement (Int32, Text, Int32) Bool
qCanUser = [singletonStatement|
  select kms.can_user($1::int4, $2::text, $3::int4)::bool
|]

-- Documents ---------------------------------------------------
qListDocs :: St.Statement (Maybe Int32, Maybe Int32, Maybe Int32, Maybe Text, Maybe Day, Maybe UTCTime) (Vector DocRow)
qListDocs = dimap id (V.map (\(uid, title, domainCode, docTypeCode, tierCode, statusCode, dueDate, updatedAt) ->
      DocRow uid title domainCode docTypeCode tierCode statusCode dueDate updatedAt
  ))
  [vectorStatement|
    with latest as (
      select document_uid, content_text
      from kms.v_document_latest
    )
    select d.uid::int4
        , d.title::text
        , dm.code::text
        , dt.code::text
        , tr.code::text
        , st.code::text
        , d.due_date::date?
        , d.updated_at::timestamptz
    from kms.document d
    join kms.domaintb dm on dm.uid = d.domain_fk
    join kms.doc_type dt on dt.uid = d.doc_type_fk
    join kms.tier tr on tr.uid = d.tier_fk
    join kms.statustb st on st.uid = d.status_fk
    left join kms.v_document_latest v on v.document_uid = d.uid
    where d.deleted_at is null
      and ($1::int4? is null or d.domain_fk = $1::int4?)
      and ($2::int4? is null or d.status_fk = $2::int4?)
      and ($3::int4? is null or d.tier_fk   = $3::int4?)
      and (
        $4::text? is null or $4::text? = '' or
        d.title ilike ('%' || $4::text? || '%') or
        (v.content_text is not null and to_tsvector('english', v.content_text) @@ plainto_tsquery('english', $4::text?))
      )
      and ($6::timestamptz? is null or d.updated_at <= $6::timestamptz?)
      and ($5::date? is null or d.due_date <= $5::date?)
    order by d.updated_at desc
  |]

qDocDetail :: St.Statement Int32 (Maybe DocDetail)
qDocDetail = dimap id (\case
    Just (uid, title, domainFk, typeFk, tierFk, statusFk, ownerFk, residency, aiAllowed, legalHold, dueDate, createdAt, updatedAt, latestVerUid, latestVerNo) ->
      Just $ DocDetail uid title domainFk typeFk tierFk statusFk ownerFk residency aiAllowed legalHold dueDate createdAt updatedAt latestVerUid latestVerNo
    Nothing -> Nothing
  ) [maybeStatement|
  select d.uid::int4
       , d.title::text
       , d.domain_fk::int4
       , d.doc_type_fk::int4
       , d.tier_fk::int4
       , d.status_fk::int4
       , d.owner_user_fk::int4?
       , d.residency::text?
       , d.ai_allowed::bool
       , d.legal_hold::bool
       , d.due_date::date?
       , d.created_at::timestamptz
       , d.updated_at::timestamptz
       , dv.uid::int4?
       , dv.version_no::int4?
  from kms.document d
  left join LATERAL (
    select v.uid, v.version_no from kms.document_version v
    where v.document_fk = d.uid
    order by v.version_no desc limit 1
  ) dv on true
  where d.uid = $1 :: int4
|]


qCreateDoc :: St.Statement (Text, Int32, Int32, Int32, Int32, Maybe Int32, Maybe Text, Bool, Bool, Maybe Day, Int32) Int32
qCreateDoc = [singletonStatement|
  insert into kms.document
    (title
    , domain_fk, doc_type_fk, tier_fk, status_fk,owner_user_fk
    , residency, ai_allowed, legal_hold, due_date,
     created_by_user_fk)
  values
    ($1::text
    , $2::int4, $3::int4, $4::int4, $5::int4, $6::int4?
    , $7::text?, $8::bool, $9::bool
    , $10::date?
    , $11::int4)
  returning uid::int4
|]


qUpdateDocMeta :: St.Statement (Text, Int32, Int32, Int32, Int32, Maybe Int32, Maybe Text, Bool, Bool, Maybe Day, Int32) ()
qUpdateDocMeta = [resultlessStatement|
  update kms.document set
    title = $1::text
    , domain_fk = $2::int4
    , doc_type_fk = $3::int4
    , tier_fk = $4::int4
    , status_fk = $5::int4
    , owner_user_fk = $6::int4?
    , residency = $7::text?
    , ai_allowed = $8::bool
    , legal_hold = $9::bool
    , due_date = $10::date?
  where uid=$11::int4
|]


qSoftDeleteDoc :: St.Statement Int32 ()
qSoftDeleteDoc = [resultlessStatement|
  update kms.document set deleted_at = now() where uid = $1::int4
|]


-- Versions ----------------------------------------------------
qInsertVersion :: St.Statement (Int32, Int32, Maybe Text, Maybe Text, Int32) Int32
qInsertVersion = [singletonStatement|
  insert into kms.document_version
    (document_fk, version_no, note, content_ref, created_by_user_fk)
  values ($1::int4,$2::int4,$3::text?,$4::text?,$5::int4)
  returning uid::int4
|]


qLatestVersion :: St.Statement Int32 (Maybe DocVersion)
qLatestVersion = dimap id (\case
    Just (uid, docFk, noVer, noteVer, contentRefVer, contentShaVer, createdByVer, createdAt) ->
      Just $ DocVersion uid docFk noVer noteVer contentRefVer contentShaVer createdByVer createdAt
    Nothing -> Nothing
  ) [maybeStatement|
    select
      v.uid::int4, v.document_fk::int4, v.version_no::int4, v.note::text?
      , v.content_ref::text?, v.content_sha256::text?, v.created_by_user_fk::int4?
      , v.created_at::timestamptz
    from kms.document_version v
    where v.document_fk=$1::int4
    order by v.version_no desc limit 1
|]

-- Comments ----------------------------------------------------
qListComments :: St.Statement Int32 (Vector Comment)
qListComments = dimap id (V.map (\(uid, docFk, parentFk, authorFk, body, resolved, createdAt) ->
      Comment uid docFk parentFk authorFk body resolved createdAt
  )) [vectorStatement|
  select
    uid::int4, document_fk::int4, parent_comment_fk::int4?
    , author_user_fk::int4?, body::text, resolved::bool
    , created_at::timestamptz
  from kms.commenttb where document_fk=$1::int4 order by created_at desc
|]


qAddComment :: St.Statement (Int32, Maybe Int32, Int32, Text) Int32
qAddComment = [singletonStatement|
  insert into kms.commenttb (document_fk, parent_comment_fk, author_user_fk, body)
  values ($1::int4,$2::int4?,$3::int4,$4::text)
  returning uid::int4
|]


qDeleteComment :: St.Statement (Int32, Int32) ()
qDeleteComment = [resultlessStatement|
  delete from kms.commenttb where uid = $1::int4 and author_user_fk = $2::int4
|]


-- ACLs --------------------------------------------------------
qListAcl :: St.Statement Int32 (Vector AclEntry)
qListAcl = dimap id (V.map (\(uid, docFk, principal, userFk, groupFk, roleFk, orgFk, rights, scope, scopeValue, createdBy, createdAt) ->
      AclEntry uid docFk principal userFk groupFk roleFk orgFk rights scope scopeValue createdBy createdAt
  )) [vectorStatement|
  select
    uid::int4, document_fk::int4, principal::text, user_fk::int4?
    , group_fk::int4?, role_fk::int4?, org_fk::int4?, rights::text[]
    , scope::text?, scope_value::text?, created_by_user_fk::int4?
    , created_at::timestamptz
  from kms.document_acl
  where
    document_fk = $1::int4
  order by created_at desc
|]


qAddAcl :: St.Statement (Int32, Text, Maybe Int32, Maybe Int32, Maybe Int32, Maybe Int32, Vector Text, Maybe Text, Maybe Text, Int32) Int32
qAddAcl = [singletonStatement|
  insert into kms.document_acl
    (document_fk, principal, user_fk, group_fk, role_fk, org_fk
      , rights, scope, scope_value, created_by_user_fk
    )
  values
    ($1::int4, $2::text, $3::int4?, $4::int4?, $5::int4?, $6::int4?
      , $7::text[], $8::text?, $9::text?, $10::int4
    )
  returning uid::int4
|]


qRemoveAcl :: St.Statement Int32 ()
qRemoveAcl = [resultlessStatement|
  delete from kms.document_acl where uid = $1::int4
|]

-- Reports -----------------------------------------------------
qHeatmapCounts :: St.Statement () (Vector CountCell)
qHeatmapCounts = dimap id (V.map (\(domainFk, statusFk, count) ->
      CountCell domainFk statusFk count
  )) [vectorStatement|
  select
    domain_fk::int4, status_fk::int4, count(*)::int4
  from kms.document where deleted_at is null
  group by domain_fk, status_fk
|]


qSankeyDomainStatus :: St.Statement () (Vector SankeyAB)
qSankeyDomainStatus = dimap id (V.map (\(codeASK, codeBSK, countABSK) ->
      SankeyAB codeASK codeBSK countABSK
  )) [vectorStatement|
  select
    dm.code::text, st.code::text, count(*)::int4
  from kms.document d
  join kms.domaintb dm on dm.uid = d.domain_fk
  join kms.statustb st on st.uid = d.status_fk
  where d.deleted_at is null
  group by dm.code, st.code
|]


qSankeyStatusTier :: St.Statement () (Vector SankeyAB)
qSankeyStatusTier = dimap id (V.map (\(codeASK, codeBSK, countABSK) ->
      SankeyAB codeASK codeBSK countABSK
  )) [vectorStatement|
  select
    st.code::text, tr.code::text, count(*)::int4
  from kms.document d
  join kms.statustb st on st.uid = d.status_fk
  join kms.tier tr on tr.uid = d.tier_fk
  where d.deleted_at is null
  group by st.code, tr.code
|]


-- Audit -------------------------------------------------------
qAudit :: St.Statement (Int32, Text, Maybe Int32, Maybe Text, Maybe Int32, Maybe Text) ()
qAudit = [resultlessStatement|
  insert into kms.audit_event (actor_user_fk, action_code, document_fk, target_type, target_uid, user_agent)
  values ($1::int4, $2::text, $3::int4?, $4::text?, $5::int4?, $6::text?)
|]



qPolicyBlockImport :: St.Statement (Int32, Int32, Int32, Maybe Text) (Maybe Bool)
qPolicyBlockImport = [maybeStatement|
  select block_import::bool
  from kms.policy_rule
  where (domain_fk is null or domain_fk = $1::int4)
    and (tier_fk is null or tier_fk = $2::int4)
    and (status_fk is null or status_fk = $3::int4)
    and ($4::text? is null or residency is null or residency = $4::text?)
  order by priority desc limit 1
|]


qInsertAttachment :: St.Statement (Int32, Text, Text, Int32, Text, Text, Int32) Int32
qInsertAttachment = [singletonStatement|
  insert into kms.attachment
    (document_fk, filename, content_type, size_bytes, storage_ref, sha256, uploaded_by_user_fk)
  values
    ($1::int4, $2::text, $3::text, $4::int4, $5::text, $6::text, $7::int4)
  returning uid::int4
|]



qDocMeta :: St.Statement Int32 (Int32, Int32, Int32, Maybe Text)
qDocMeta = [singletonStatement|
  select domain_fk::int4, tier_fk::int4, status_fk::int4, residency::text?
  from kms.document where uid=$1::int4
|]


qEnsureRoot :: St.Statement (Int32, Int32) Int32
qEnsureRoot = [singletonStatement|
  select kms.ensure_doc_root($1::int4, $2::int4)::int4
|]


qAppendChild :: St.Statement (Int32, Maybe Int32, Text, Maybe Text, Maybe Ae.Value, Int32) Int32
qAppendChild = [singletonStatement|
  select kms.append_child_import($1::int4, $2::int4?, $3::text, $4::text?, $5::jsonb?, $6::int4)::int4
|]


qLinkBlockAttachment :: St.Statement (Int32, Int32, Int32) ()
qLinkBlockAttachment = [resultlessStatement|
  select kms.link_block_attachment($1::int4, $2::int4, $3::int4)::int4
|]


qNextVersionNo :: St.Statement Int32 Int32
qNextVersionNo = [singletonStatement|
  select
    (coalesce(max(version_no),0)+1)::int4
  from kms.document_version
  where document_fk=$1::int4
|]


qInsertVersionV12 :: St.Statement (Int32, Int32, Text, Text, Int32, Text) Int32
qInsertVersionV12 = [singletonStatement|
  insert into kms.document_version
    (document_fk, version_no, note, content_ref, created_by_user_fk, content_text)
  values
    ($1::int4, $2::int4, $3::text, $4::text, $5::int4, $6::text)
  returning uid::int4
|]

--- Categorisation --------------------------------------------
qListCategories :: St.Statement () (Vector Category)
qListCategories = dimap id (V.map (\(uid, name, parent) ->
      Category uid name parent
  )) [vectorStatement|
    select
      uid::int4, label::text, owner_fk::int4?
    from kms.catalog
    order by label asc
  |]

type NodeOut = (Int32, Text, Maybe Int32, Maybe Int32, Maybe UTCTime, Int32)

fetchNodesForTaxo :: St.Statement Int32 (Vector NodeOut)
fetchNodesForTaxo = [vectorStatement|
    with recursive xtree as (
      select
        uid, label, parentid, assetid, lastmod, 1 as depth
        from kms.nodecat
        where arboid = $1::int4 and parentid is null
      union all
      select
        nd.uid, nd.label, nd.parentid, nd.assetid, nd.lastmod, xt.depth+1 as depth
        from kms.nodecat as nd
          inner join xtree xt on xt.uid = nd.parentid
    )
    select uid::int4, label::text, parentid::int4?, assetid::int4?, lastmod::timestamptz?, depth::int4 from xtree
  |]
