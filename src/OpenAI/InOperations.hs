-- | Load a persisted conversation (legal.context tree) from Postgres.
--
-- This module defines DB-optimised in-memory types carrying stable DB identities
-- (uid + uuid) and convenient indexes for later modifications.
--
module OpenAI.InOperations where

import Control.Monad (forM)

import Data.Bifunctor (first)
import Data.Int (Int32, Int64)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Mp
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import qualified Data.Vector as V

import Hasql.Statement (Statement)
import qualified Hasql.Pool as Hp
import qualified Hasql.TH as TH
import qualified Hasql.Session as Ses

import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as TxS

import qualified OpenAI.Statements as St


-- -----------------------------
-- DB-optimised in-memory types
-- -----------------------------

data RefDb = RefDb
  { uidRd :: !Int64
  , uuidRd :: !UUID
  }
  deriving (Show, Eq)

data IssueDb = IssueDb
  { seqIs :: !Int32
  , textIs :: !Text
  }
  deriving (Show, Eq)

data MessageKindDb = 
    UserMK
  | AssistantMK
  | SystemMK
  | ToolMK
  | UnknownMK
  deriving (Show, Eq)

data AttachmentDb = AttachmentDb
  { seqAt :: !Int32
  , valueAt :: !Text
  }
  deriving (Show, Eq)

data MessageBodyDb = 
    UserBody {
      textUB :: !Text
      , summaryUB :: !(Maybe Text)
      }
  | AssistantBody { 
        responseUidAB :: !(Maybe Int64)
      , responseTextAB :: !(Maybe Text)
      , summaryAB :: !(Maybe Text)
      }
  | SystemBody { 
        textSB :: !Text
      }
  | ToolBody { 
        textTB :: !Text
      }
  | UnknownBody { 
        textUB :: !Text
      }
  deriving (Show, Eq)

data SubActionKindDb
  = ReflectionSAK
  | CodeSAK
  | ToolCallSAK
  | IntermediateSAK
  deriving (Show, Eq)

data SubActionBodyDb
  = ReflectionBody { 
        summarySab :: !Text
      , contentSab :: !Text
      , chunksSab :: !(Vector Text)
      , finishedSab :: !(Maybe Bool)
      }
  | CodeBody { 
        languageCb :: !Text
      , formatNameCb :: !(Maybe Text)
      , textCb :: !Text
      }
  | ToolCallBody { 
        toolNameTc :: !Text
      , toolInputTc :: !Text
      }
  | IntermediateBody { 
        textIb :: !Text
      }
  deriving (Show, Eq)

data SubActionDb = SubActionDb
  { refSa :: !RefDb
  , seqSa :: !Int32
  , kindSa :: !SubActionKindDb
  , bodySa :: !SubActionBodyDb
  }
  deriving (Show, Eq)

data MessageDb = MessageDb
  { refMb :: !RefDb
  , seqMb :: !Int32
  , kindMb :: !MessageKindDb
  , createdAtMb :: !(Maybe UTCTime)
  , updatedAtMb :: !(Maybe UTCTime)
  , attachmentsMb :: !(Vector AttachmentDb)
  , bodyMb :: !MessageBodyDb
  , subActionsMb :: !(Vector SubActionDb)
  }
  deriving (Show, Eq)

-- | Convenience indexes to support further edits efficiently.
--
-- messageByUid: uid -> index in 'messages'
-- subActionByUid: uid -> (messageIndex, subActionIndex)

data IndexDb = IndexDb
  { messageByUidIb :: !(Map Int64 Int)
  , subActionByUidIb :: !(Map Int64 (Int, Int))
  }
  deriving (Show, Eq)

data ContextDb = ContextDb
  { refCo :: !RefDb
  , titleCo :: !Text
  , convIdCo :: !Text
  , issuesCo :: !(Vector IssueDb)
  , messagesCo :: !(Vector MessageDb)
  , indexCo :: !IndexDb
  }
  deriving (Show, Eq)


-- -----------------------------
-- Public API
-- -----------------------------

-- | Load a context by its external UUID.
--
-- Returns:
--   * Right Nothing  => context not found
--   * Right (Just x) => loaded successfully
--   * Left err       => DB error or unexpected schema inconsistency
loadDiscourse :: Hp.Pool -> UUID -> IO (Either String (Maybe ContextDb))
loadDiscourse pool discourseUuid = do
  rez <- Hp.use pool $
    TxS.transaction TxS.ReadCommitted TxS.Read (loadDiscourseTx discourseUuid)

  -- Flatten: Pool.use gives Either UsageError a
  pure $ case first show rez of
    Left err -> Left err
    Right (Left err) -> Left err
    Right (Right ctx) -> Right ctx


findDiscourseByConvId :: Hp.Pool -> Text -> IO (Either String (Maybe UUID))
findDiscourseByConvId pool convId = do
  eiRezA <- Hp.use pool $ Ses.statement convId St.selectContextByConvId
  case eiRezA of
    Left err -> pure . Left $ show err
    Right Nothing -> pure . Right $ Nothing
    Right (Just (_, ctxUuid, _, _)) ->
      pure . Right $ Just ctxUuid


loadDiscourseTx :: UUID -> Tx.Transaction (Either String (Maybe ContextDb))
loadDiscourseTx discourseUuid = do
  mCtx <- Tx.statement discourseUuid St.selectContextByUuid
  case mCtx of
    Nothing -> pure (Right Nothing)
    Just (ctxUid, foundUuid, title, convId) -> do
      is <- Tx.statement ctxUid St.selectIssues
      ms <- Tx.statement ctxUid St.selectMessages
      as <- Tx.statement ctxUid St.selectAttachments
      sms <- Tx.statement ctxUid St.selectMessageSummaries
      sas <- Tx.statement ctxUid St.selectSubActions
      rfs <- Tx.statement ctxUid St.selectReflections
      rcs <- Tx.statement ctxUid St.selectReflectionChunks
      cds <- Tx.statement ctxUid St.selectCodes
      tcs <- Tx.statement ctxUid St.selectToolCalls

      pure $ first ("buildContextDb: " <>)
              ( Just <$> buildContextDb (ctxUid, foundUuid, title, convId) is ms sms as sas rfs rcs cds tcs )


-- -----------------------------
-- Embedded SQL (Hasql-TH)
-- -----------------------------


-- -----------------------------
-- Assembly
-- -----------------------------

buildContextDb
 :: (Int64, UUID, Text, Text)
  -> Vector (Int32, Text)
  -> Vector St.MessageRow
  -> Vector St.MessageSummaryRow
  -> Vector St.AttachmentRow
  -> Vector St.SubActionRow
  -> Vector St.ReflectionRow
  -> Vector St.ReflectionChunkRow
  -> Vector St.CodeRow
  -> Vector St.ToolCallRow
  -> Either String ContextDb
buildContextDb (ctxUid, ctxUuid, title, convId) issuesV msgRowsV summariesRv attRowsV saRowsV relectionsRv rfChunkRowsV codeRowsV toolRowsV = do
  let
    issueObjs = V.map (uncurry IssueDb) issuesV

    summaryByMessageUid :: Map Int64 Text
    summaryByMessageUid = Mp.fromList (V.toList summariesRv)

  -- attachments by message uid
    attMap :: Map Int64 (Vector AttachmentDb)
    attMap = groupVecByKey attRowsV $ \(mUid, s, v) -> (mUid, AttachmentDb s v)

  -- reflection chunks by reflection uid
    chunkMap :: Map Int64 (Vector Text)
    chunkMap = groupVecByKey rfChunkRowsV $ \(rfUid, _, t) -> (rfUid, t)

  -- reflection bodies by sub_action uid
    rfBodyBySa :: Map Int64 SubActionBodyDb
    rfBodyBySa = Mp.fromList $ map (\(rfUid, saUid, summ, cont, fin) ->
              let chunksV = Mp.findWithDefault V.empty rfUid chunkMap
              in (saUid, ReflectionBody summ cont chunksV fin)
            )
            (V.toList relectionsRv)

  -- code bodies by sub_action uid
    codeBodyBySa :: Map Int64 SubActionBodyDb
    codeBodyBySa =
        Mp.fromList $
          map (\(saUid, lang, fmt, txt) -> (saUid, CodeBody lang fmt txt)) (V.toList codeRowsV)

  -- tool call bodies by sub_action uid
    toolBodyBySa :: Map Int64 SubActionBodyDb
    toolBodyBySa =
        Mp.fromList $
          map (\(saUid, nm, inp) -> (saUid, ToolCallBody nm inp)) (V.toList toolRowsV)

  -- sub-actions grouped by message uid
    subActionsByMsg :: Map Int64 (Vector (Either String SubActionDb))
    subActionsByMsg =
      groupVecByKey saRowsV $ \(saUid, saUuid, msgUid, s, kindTxt, mText) ->
        let
          kind = parseSubActionKind kindTxt
          eiBody = case kind of
            IntermediateSAK ->
              case mText of
                Just t -> Right (IntermediateBody t)
                Nothing -> Left "@[subActionsByMsg] intermediate sub_action missing text"
            ReflectionSAK ->
              maybe (Left "@[subActionsByMsg] reflection sub_action missing reflection row") Right (Mp.lookup saUid rfBodyBySa)
            CodeSAK ->
              maybe (Left "@[subActionsByMsg] code sub_action missing code row") Right (Mp.lookup saUid codeBodyBySa)
            ToolCallSAK ->
              maybe (Left "@[subActionsByMsg] tool_call sub_action missing tool_call row") Right (Mp.lookup saUid toolBodyBySa)
        in
        case eiBody of
          Left err -> (0, Left err)
          Right body ->
            ( msgUid
            , Right SubActionDb
                { refSa = RefDb saUid saUuid
                , seqSa = s
                , kindSa = kind
                , bodySa = body
                }
              )
        -- groupVecByKey expects pure; we encode Either in the value and fix below
        -- (see post-processing)

  -- Fix subActionsByMsg: collapse Either errors
  subActionsByMsg' <- traverse traverseEitherVec subActionsByMsg

  -- messages
  msgs <- forM (V.toList msgRowsV) $ \row -> buildMessage summaryByMessageUid attMap subActionsByMsg' row
  let
    msgV = V.fromList msgs
    -- build indexes
    idx = buildIndex msgV

  pure $ ContextDb
      { refCo = RefDb ctxUid ctxUuid
      , titleCo = title
      , convIdCo = convId
      , issuesCo = issueObjs
      , messagesCo = msgV
      , indexCo = idx
      }


-- Build one MessageDb from the joined row.
buildMessage :: Map Int64 Text -> Map Int64 (Vector AttachmentDb) -> Map Int64 (Vector SubActionDb) -> St.MessageRow -> Either String MessageDb
buildMessage summaryByMessageUid attMap saMap (mUid, mUuid, s, kindTxt, cAt, uAt, userTxt, respUid, respTxt, sysTxt, toolTxt, unkTxt) = do
  kind <- parseMessageKind kindTxt

  let
    mbSummary :: Maybe Text
    mbSummary = Mp.lookup mUid summaryByMessageUid

  body <- case kind of
    UserMK ->
      case userTxt of
        Just t -> Right (UserBody t mbSummary)
        Nothing -> Left "user message missing user_message.text"
    AssistantMK -> Right (AssistantBody respUid respTxt mbSummary)
    SystemMK -> maybe (Left "system message missing system_message.text") (Right . SystemBody) sysTxt
    ToolMK -> maybe (Left "tool message missing tool_message.text") (Right . ToolBody) toolTxt
    UnknownMK -> maybe (Left "unknown message missing unknown_message.text") (Right . UnknownBody) unkTxt

  let
    atts = Mp.findWithDefault V.empty mUid attMap
    subs = Mp.findWithDefault V.empty mUid saMap

  pure $ MessageDb
      { refMb = RefDb mUid mUuid
      , seqMb = s
      , kindMb = kind
      , createdAtMb = cAt
      , updatedAtMb = uAt
      , attachmentsMb = atts
      , bodyMb = body
      , subActionsMb = subs
      }


parseMessageKind :: Text -> Either String MessageKindDb
parseMessageKind t = case T.toLower t of
  "user" -> Right UserMK
  "assistant" -> Right AssistantMK
  "system" -> Right SystemMK
  "tool" -> Right ToolMK
  "unknown" -> Right UnknownMK
  other -> Left ("unknown message kind: " <> T.unpack other)

parseSubActionKind :: Text -> SubActionKindDb
parseSubActionKind t = case T.toLower t of
  "reflection" -> ReflectionSAK
  "code" -> CodeSAK
  "tool_call" -> ToolCallSAK
  "intermediate" -> IntermediateSAK
  _ -> IntermediateSAK -- be permissive; validation happens when choosing body


-- -----------------------------
-- Grouping helpers
-- -----------------------------

-- | Group an already-ordered vector into a Map of vectors.
--
-- The SQL queries above already order by parent seq then child seq, so we can
-- preserve stable ordering by folding.
--
groupVecByKey :: Ord k => Vector a -> (a -> (k, v)) -> Map k (Vector v)
groupVecByKey inVect f =
  V.foldr'
    (\a m ->
      let (k, v) = f a
      in Mp.insertWith (V.++) k (V.singleton v) m
    )
    Mp.empty
    inVect

-- | The sub-action grouping above temporarily stores Either errors in the body.
{-
traverseEitherVec :: Vector (SubActionKindDb, Either String SubActionBodyDb) -> Either String (Vector SubActionDb)
traverseEitherVec = undefined
-}

-- Actually we encoded SubActionDb.body as Either in the grouping step.
-- To avoid rewriting groupVecByKey with Either, we provide a small adapter.

traverseEitherVec :: Vector (Either String SubActionDb) -> Either String (Vector SubActionDb)
traverseEitherVec v =
  foldl'
    (\acc e ->
      case (acc, e) of
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (Right xs, Right x) -> Right (xs V.++ V.singleton x)
    )
    (Right V.empty)
    (V.toList v)


-- -----------------------------
-- Index building
-- -----------------------------

buildIndex :: Vector MessageDb -> IndexDb
buildIndex msgs =
  let
    messageByUid' = Mp.fromList (zip (map (uidRd . refMb) (V.toList msgs)) [0 ..])
    subActionByUid' =
      foldl' (\m (mi, msg) ->
          let subs = V.toList msg.subActionsMb
              pairs = zip [0 ..] subs
          in foldl'
              (\m2 (si, sa) -> Mp.insert (uidRd sa.refSa) (mi, si) m2)
              m
              pairs
        ) Mp.empty (zip [0 ..] (V.toList msgs))
  in
  IndexDb {
      messageByUidIb = messageByUid'
    , subActionByUidIb = subActionByUid'
    }


-- -----------------------------
-- Time conversion (if needed)
-- -----------------------------

epochToUtc :: Double -> UTCTime
epochToUtc = posixSecondsToUTCTime . realToFrac
