{-# LANGUAGE QuasiQuotes #-}
module OpenAI.ProjFetcher ( DiscussionDescription(..), fetchProjects, saveDescriptionsToGroup ) 
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
fetchProjects :: FilePath -> IO [DiscussionDescription]
fetchProjects path = do
    -- We read the file as a String for Scalpel's scrapeStringLike.
    -- Scalpel handles partial HTML (like a lone <section>) gracefully.
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
saveDescriptionsToGroup :: Text -> [DiscussionDescription] -> Hp.Pool -> IO (Either String ())
saveDescriptionsToGroup groupLabel descs pool = do
  rez <- Hp.use pool $ TxS.transaction TxS.ReadCommitted TxS.Write (saveTx groupLabel descs)
  case rez of
    Left dbErr -> pure . Left $ "@[saveDescriptionsToGroup] database error: " <> show dbErr
    Right eiOpRez -> case eiOpRez of
      Left opErr -> pure . Left $ "@[saveDescriptionsToGroup] operation err: " <> opErr
      Right () -> pure . Right $ ()


saveTx :: Text -> [DiscussionDescription] -> Tx.Transaction (Either String ())
saveTx groupLabel descs = do
  groupUid <- Tx.statement groupLabel upsertDiscourseGroup

  rezIns <- forM descs $ \d -> do
    mbDiscourseUid <- Tx.statement d.eid findDiscourseByEid
    case mbDiscourseUid of
      Nothing ->
        pure . Left $ "@[saveTx]discourse not found for eid: " <> show d.eid
      Just discourseUid -> do
        Tx.statement (groupUid, discourseUid, d.lastUse) insertGroupMember
        pure . Right $ ()
  case lefts rezIns of
    [] -> pure . Right $ ()
    errs -> pure . Left $ unlines errs


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
findDiscourseByEid :: Statement Text (Maybe Int64)
findDiscourseByEid =
  [TH.maybeStatement|
    select uid :: int8
    from oai.discussions where eid = $1 :: text
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

