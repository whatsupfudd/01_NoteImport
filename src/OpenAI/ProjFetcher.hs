{-# LANGUAGE QuasiQuotes #-}
module OpenAI.ProjFetcher ( DiscussionDescription(..), saveProjects, saveDescriptionsToGroup ) 
  where

import Control.Applicative (Alternative, (<|>))

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)

import Data.Bifunctor (first)
import Data.Either (lefts)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Time as Dt
import qualified Data.Text as T
import qualified Data.Vector as V

import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)

import Text.HTML.Scalpel

import Hasql.Statement (Statement)
import qualified Hasql.Pool as Hp
import qualified Hasql.TH as TH
import qualified Hasql.Session as Ses
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as TxS


-- | Data structure to hold extracted project information
data ConversationDesc = ConversationDesc {
    uid :: Int64
  , title :: Text
  , eid :: Text
  }
  deriving Show

data DiscussionDescription = DiscussionDescription {
      title :: Text
    , summary :: Text
    , lastUse :: Maybe Dt.Day
    , eid :: Text
    }
  deriving (Eq)

instance Show DiscussionDescription where
  show proj =
    T.unpack proj.title <> "; " <> T.unpack proj.summary <> ", last: " <> show proj.lastUse <> ", (" <> T.unpack proj.eid <> ")"

-- | Top-level function requested by the CTO.
-- Reads a file at the given FilePath and extracts a list of DiscussionDescriptions.
saveProjects :: FilePath -> IO [DiscussionDescription]
saveProjects path = do
    content <- readFile path
    let
      projects = scrapeStringLike content projectListScraper
    pure $ fromMaybe [] projects

-- | Scraper that targets the list items within the section
projectListScraper :: Scraper String [DiscussionDescription]
projectListScraper = chroots ("li" @: [hasClass "group/project-item"]) projectScraper

-- | Individual scraper for a single project (the <li> element)
projectScraper :: Scraper String DiscussionDescription
projectScraper = do
  -- 1. Extract the URL from the 'href' attribute of the 'a' tag
  projectEid <- attr "href" "a"

  -- 2. Extract the Title: The div with class 'font-medium'
  projectTitle <- text $ "div" @: [hasClass "font-medium"]

  -- 3. Extract the Summary: The div with class 'text-token-text-secondary'
  -- We use 'trim' logic because Scalpel's 'text' includes inner spacing
  projectSummary <- text $ "div" @: [hasClass "text-token-text-secondary"]

  -- 4. Extract the Date: The span with class 'whitespace-nowrap'
  projectDate <- text $ "span" @: [hasClass "whitespace-nowrap"]

  let
    eidParts = T.splitOn "/" $ T.pack projectEid
    realEid = last eidParts
  let lastUse = unsafePerformIO $ parseMonthDayWithCurrentYear projectDate

  pure $ DiscussionDescription
      { title   = T.strip $ T.pack projectTitle
      , summary = T.strip $ T.pack projectSummary
      , lastUse = lastUse
      , eid     = realEid
      }


-- | Ensure the group exists (by label), then ensure each description is present
-- as a discourse row, and finally ensure membership rows exist.
--
-- All DB writes are wrapped in a single transaction.
--
-- Returns:
--   * Right () on success
--   * Left err on any failure (transaction rolled back)
--
saveDescriptionsToGroup :: Text -> [DiscussionDescription] -> Hp.Pool -> IO (Either [Hp.UsageError] (Either [String] [()]))
saveDescriptionsToGroup groupLabel descs pool = do
  rezA <- Hp.use pool $ TxS.transaction TxS.ReadCommitted TxS.Write (saveTx groupLabel descs)
  case rezA of
    Left dbErr -> pure . Left $ [ dbErr ]
    Right eiOpRez -> pure . Right $ eiOpRez


saveTx :: Text -> [DiscussionDescription] -> Tx.Transaction (Either [String] [()])
saveTx groupLabel descs = do
  groupUid <- Tx.statement groupLabel upsertDiscourseGroup

  rezIns <- forM descs $ \d -> do
    mbDiscourseUid <- Tx.statement d.eid findConversationByEid
    case mbDiscourseUid of
      Nothing ->
        pure . Left $ "@[saveTx]discourse not found for eid: " <> show d.eid
      Just discourseUid -> do
        Tx.statement (groupUid, discourseUid, d.lastUse) insertGroupMember
        pure . Right $ ()
  case lefts rezIns of
    [] -> pure . Right $ [ () ]
    errs -> pure . Left $ errs

-- Fetchers:
fetchDiscoursesInGroup :: Text -> Hp.Pool -> IO (Either Hp.UsageError (V.Vector ConversationDesc))
fetchDiscoursesInGroup groupLabel pool = do
  rezA <- Hp.use pool $ Ses.statement groupLabel findDiscourseGroupByLabel
  case rezA of
    Left ue -> pure . Left $ ue
    Right Nothing -> pure . Right $ V.empty
    Right (Just (groupID, _, _)) -> do
      rez <- Hp.use pool $ Ses.statement groupID findConversationByGroupId
      case rez of
        Left uerr -> pure . Left $ uerr
        Right rawConvs -> pure . Right $ V.map (\(uid, title, eid) -> ConversationDesc uid title eid) rawConvs


findDiscourseGroupByLabel :: Statement Text (Maybe (Int64, Text, Text))
findDiscourseGroupByLabel =
  [TH.maybeStatement|
    select uid :: int8, title::text, eid::text
    from oai.discourse_group where label = $1 :: text
  |]


-- -----------------------------
-- Statements
-- -----------------------------

-- | Insert group if missing; always return its uid.
--
-- Uses an ON CONFLICT DO UPDATE no-op to get RETURNING uid in both cases.
upsertDiscourseGroup :: Statement Text Int64
upsertDiscourseGroup =
  [TH.singletonStatement|
    insert into oai.discourse_group (label)
    values ($1::text)
    on conflict (label) do update
      set label = excluded.label
    returning uid :: int8
  |]


-- | Find a discourse by eid.
findConversationByEid :: Statement Text (Maybe Int64)
findConversationByEid =
  [TH.maybeStatement|
    select uid :: int8
    from oai.discussions where eid = $1 :: text
  |]


findConversationByGroupId :: Statement Int64 (V.Vector (Int64, Text, Text))
findConversationByGroupId =
  [TH.vectorStatement|
    select b.uid :: int8, b.title :: text, b.eid :: text
    from oai.discourse_group_member a
      join oai.discussions b on b.discussions_fk = a.uid
    where
      discourse_group_fk = $1 :: int8
  |]


-- | Create membership if missing.
insertGroupMember :: Statement (Int64, Int64, Maybe Dt.Day) ()
insertGroupMember =
  [TH.resultlessStatement|
    insert into oai.discourse_group_member (discourse_group_fk, discussions_fk, last_use)
    values ($1 :: int8, $2 :: int8, $3 :: date?)
    on conflict (discourse_group_fk, discussions_fk) do nothing
  |]


parseMonthDayWithCurrentYear :: String -> IO (Maybe Dt.Day)
parseMonthDayWithCurrentYear dateString = do
    now <- Dt.getCurrentTime
    timezone <- Dt.getCurrentTimeZone
    let localTime = Dt.utcToLocalTime timezone now
    let (currentYear, _, _) = Dt.toGregorian $ Dt.localDay localTime

    -- Define the format for month and day parsing (e.g., "Jan 26" or "1 26")
    -- We use a format string that only parses the month and day
    -- %b for abbreviated month name (e.g., Jan), %m for numeric month (e.g., 01)
    -- %e for space-padded day, %d for zero-padded day

    let parseMonthDay format = Dt.parseTimeM True Dt.defaultTimeLocale format dateString :: Maybe Dt.LocalTime

    -- Try parsing with common formats
    let mLocalTime = parseMonthDay "%b %e" <|> parseMonthDay "%B %e" <|> parseMonthDay "%m %d" <|> parseMonthDay "%d %m"
        -- You can add more formats as needed

    case mLocalTime of
        Just lt -> do
            let
              (_, month, day) = Dt.toGregorian $ Dt.localDay lt
              lastYear = currentYear - 1
            -- Combine with the current year
            pure $ Just $ Dt.fromGregorian lastYear month day
        Nothing -> pure Nothing

