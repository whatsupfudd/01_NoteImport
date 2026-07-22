module OpenAI.Import.Discussion
  ( sync
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Hasql.Pool as Hp
import qualified OpenAI.Import.Report as Ir
import qualified OpenAI.Import.Types as It

sync
  :: Hp.Pool
  -> It.Opts
  -> Text
  -> Ir.Report
  -> IO (Either Hp.UsageError (Either Text Ir.Report))
sync _pool opts eidConv report =
  case opts.scopeOpt of
    It.RawOnlyS -> pure $ Right $ Right report
    It.RawDiscS -> syncLegacy eidConv report
    It.RawDiscSummaryS -> syncLegacy eidConv report

syncLegacy
  :: Text
  -> Ir.Report
  -> IO (Either Hp.UsageError (Either Text Ir.Report))
syncLegacy eidConv report =
  pure $ Right $ Right $ addPendingNote eidConv report

addPendingNote :: Text -> Ir.Report -> Ir.Report
addPendingNote eidConv report =
  let txt =
        "discussion sync pending for conversation eid "
          <> eidConv
          <> "; raw import kept"
      note = Ir.InfoN txt
  in if note `elem` report.notes
       then report
       else report { Ir.notes = report.notes <> [note] }