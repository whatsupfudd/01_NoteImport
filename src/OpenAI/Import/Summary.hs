module OpenAI.Import.Summary
  ( sync
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Hasql.Pool as Hp
import qualified Network.HTTP.Client as Ht
import qualified OpenAI.Deserialize.Discussion as Dds
import qualified OpenAI.Import.Report as Ir
import qualified OpenAI.Import.Types as It
import qualified OpenAI.Summarisation as Sm

sync
  :: Maybe Ht.Manager
  -> Hp.Pool
  -> It.Opts
  -> Text
  -> Ir.Report
  -> IO (Either Hp.UsageError (Either Text Ir.Report))
sync mgrM pgPool opts eidConv report0 =
  case opts.scopeOpt of
    It.RawOnlyS -> pure $ ok report0
    It.RawDiscS -> pure $ ok report0
    It.RawDiscSummaryS ->
      case mgrM of
        Nothing -> pure . ok $
          noteWarn ("summary skipped: no HTTP manager for " <> eidConv) report0
        Just mgr ->
          case report0.uidDisc of
            Nothing -> pure . ok $
              noteInfo ("summary pending: discussion uid unavailable for " <> eidConv) report0
            Just uidDisc -> do
              discRez <- Dds.loadDiscussionByUid pgPool uidDisc
              case discRez of
                Left dbErr -> pure . ok $ noteWarn ( 
                    "summary skipped: could not load discussion "
                    <> showT uidDisc <> " for " <> eidConv
                    <> ", err: " <> showT dbErr
                  ) report0
                Right (Left err) -> pure . ok $ noteWarn ( 
                    "summary skipped: could not load discussion "
                    <> showT uidDisc <> " for " <> eidConv
                    <> ", err: " <> T.pack err
                  ) report0
                Right (Right Nothing) -> pure . ok $ noteWarn ( 
                    "summary skipped: could not load discussion "
                    <> showT uidDisc <> " for " <> eidConv
                  ) report0
                Right (Right (Just disc)) -> do
                  sumRez <- Sm.summarizeDiscourseMessages pgPool mgr disc
                  pure . ok $ mergeSummary eidConv report0 sumRez

ok :: Ir.Report -> Either Hp.UsageError (Either Text Ir.Report)
ok report = Right (Right report)

mergeSummary
  :: Text
  -> Ir.Report
  -> Either [Hp.UsageError] (Either [String] [()])
  -> Ir.Report
mergeSummary eidConv report0 sumRez =
  case sumRez of
    Left dbErrs ->
      noteWarn
        ("summary failed for " <> eidConv <> ": " <> renderShowList dbErrs)
        report0
    Right (Left errs) ->
      noteWarn
        ("summary failed for " <> eidConv <> ": " <> renderStringList errs)
        report0
    Right (Right doneL) ->
      let n = length doneL
          report1 =
            if n > 0
              then bumpSum n report0
              else report0
      in if n > 0
           then noteInfo ("summaries stored: " <> showT n <> " for " <> eidConv) report1
           else noteInfo ("summary current or no eligible messages for " <> eidConv) report1

bumpSum :: Int -> Ir.Report -> Ir.Report
bumpSum n report =
  report
    { Ir.count =
        report.count
          { Ir.sumAddedCnt = report.count.sumAddedCnt + n
          }
    }

noteInfo :: Text -> Ir.Report -> Ir.Report
noteInfo msg report =
  report { Ir.notes = report.notes <> [Ir.InfoN msg]}

noteWarn :: Text -> Ir.Report -> Ir.Report
noteWarn msg report =
  report { Ir.notes = report.notes <> [Ir.WarnN msg]}

showT :: Show a => a -> Text
showT = T.pack . show

renderShowList :: Show a => [a] -> Text
renderShowList xs =
  let ys = take 3 xs
      more = if length xs > length ys then " ..." else ""
  in case ys of
       [] -> "unknown error"
       _ -> T.intercalate "; " (map showT ys) <> more

renderStringList :: [String] -> Text
renderStringList xs =
  let ys = take 3 xs
      more = if length xs > length ys then " ..." else ""
  in case ys of
       [] -> "unknown error"
       _ -> T.intercalate "; " (map T.pack ys) <> more