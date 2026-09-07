module OpenAI.Import.Batch
  ( runBatch
  )
where

import Data.List (foldl', maximumBy, sortOn)
import Data.Ord (comparing)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Mp
import qualified Hasql.Pool as Hp
import qualified Network.HTTP.Client as Ht
import qualified OpenAI.Import.Run as Run
import qualified OpenAI.Import.Types as Typ
import qualified OpenAI.Import.Report as Rpt
import qualified OpenAI.Conversation.Json.Schema as Jd

data ConvIx = ConvIx
  { ixInp :: Int
  , convInp :: Jd.Conversation
  , sourceKey :: Text
  }


data BatchAcc = BatchAcc
  { errsAcc :: [Hp.UsageError]
  , repsAcc :: [(Int, Rpt.Report)]
  }

runBatch :: Maybe Ht.Manager -> Hp.Pool -> Typ.Source -> Typ.Opts -> [(Jd.Conversation, Text)]
    -> IO (Either [Hp.UsageError] Rpt.BatchReport)
runBatch mgr pool source opts convs =
  let
    (convsDedup, repsDup) = dedupConvs convs
  in do
  putStrLn $ "@[runBatch] dedupConvs: " <> show (length convs) <> " -> " <> show (length convsDedup)
  acc <- foldBatch (BatchAcc [] repsDup) convsDedup
  let
    repsAll = orderReports acc.repsAcc
  if null acc.errsAcc && null repsAll then
    pure $ Right (Rpt.batchReport source [])
  else
    pure $ if null repsAll then
        Left (reverse acc.errsAcc)
      else 
        Right (Rpt.batchReport source repsAll)
  where
  stopOnFail = opts.policyOpt.stopOnFailPol
  foldBatch :: BatchAcc -> [ConvIx] -> IO BatchAcc
  foldBatch acc [] = pure acc
  foldBatch acc (convIx : rest) = do
    rez <- Run.runOne mgr pool source opts convIx.convInp convIx.sourceKey
    case rez of
      Left dbErr -> do
        let
          rpt = dbErrReport convIx.convInp dbErr
          acc1 = pushErr dbErr (pushRep convIx.ixInp rpt acc)
        if stopOnFail then
          pure (pushHalted ("batch stopped after database error on " <> convIx.convInp.oaiIdCv) rest acc1)
        else
          foldBatch acc1 rest
      Right rpt -> do
        let
          acc1 = pushRep convIx.ixInp rpt acc
        if stopOnFail && isHardFail rpt then
            pure (pushHalted ("batch stopped after failed import on " <> rpt.eidConv) rest acc1)
        else
          foldBatch acc1 rest


dedupConvs :: [(Jd.Conversation, Text)] -> ([ConvIx], [(Int, Rpt.Report)])
dedupConvs convs =
  let
    convsIx = zipWith (\ix (conv, sourceKey) -> ConvIx ix conv sourceKey) [0 ..] convs
    groups = groupByEid convsIx
    chosen = fmap chooseWinner groups
    convsKeep = sortOn (.ixInp) [winner | (winner, _) <- Mp.elems chosen]
    repsDup = sortOn fst $ concatMap (\(winner, drops) ->
        fmap (\convIx -> (convIx.ixInp, dupReport convIx winner)) drops
      ) (Mp.elems chosen)
  in (convsKeep, repsDup)


groupByEid :: [ConvIx] -> Mp.Map Text [ConvIx]
groupByEid =
  foldl' (\acc convIx -> Mp.insertWith (<>) convIx.convInp.oaiIdCv [convIx] acc) Mp.empty


chooseWinner :: [ConvIx] -> (ConvIx, [ConvIx])
chooseWinner convsIx =
  let
    winner = maximumBy (comparing rankConv) convsIx
    drops = filter (\convIx -> convIx.ixInp /= winner.ixInp) convsIx
  in
  (winner, drops)


rankConv :: ConvIx -> (Scientific, Int)
rankConv convIx = (convIx.convInp.updateTimeCv, convIx.ixInp)


orderReports :: [(Int, Rpt.Report)] -> [Rpt.Report]
orderReports = fmap snd . sortOn fst


pushErr :: Hp.UsageError -> BatchAcc -> BatchAcc
pushErr err acc = acc { errsAcc = err : acc.errsAcc }


pushRep :: Int -> Rpt.Report -> BatchAcc -> BatchAcc
pushRep ix rpt acc = acc { repsAcc = (ix, rpt) : acc.repsAcc }


pushHalted :: Text -> [ConvIx] -> BatchAcc -> BatchAcc
pushHalted reason convsIx acc =
  acc { repsAcc = halted <> acc.repsAcc }
  where
  halted = fmap (\convIx -> (convIx.ixInp, haltedReport convIx.convInp reason)) convsIx


isHardFail :: Rpt.Report -> Bool
isHardFail rpt = rpt.action == Typ.FailA


dupReport :: ConvIx -> ConvIx -> Rpt.Report
dupReport convDrop convKeep =
  Rpt.Report
    { eidConv = convDrop.convInp.oaiIdCv
    , uidConv = Nothing
    , uidDisc = Nothing
    , action = Typ.SkipSameA
    , count = Rpt.emptyCount { Rpt.skipCnt = 1 }
    , notes =
        [ Rpt.WarnN $
            dupMsg convDrop convKeep
        ]
    }


dbErrReport :: Jd.Conversation -> Hp.UsageError -> Rpt.Report
dbErrReport conv dbErr =
  Rpt.Report
    { eidConv = conv.oaiIdCv
    , uidConv = Nothing
    , uidDisc = Nothing
    , action = Typ.FailA
    , count = Rpt.emptyCount { Rpt.failCnt = 1 }
    , notes =
        [ Rpt.ErrorN $
            "database error during batch import: " <> tshow dbErr
        ]
    }


haltedReport :: Jd.Conversation -> Text -> Rpt.Report
haltedReport conv reason =
  Rpt.Report
    { eidConv = conv.oaiIdCv
    , uidConv = Nothing
    , uidDisc = Nothing
    , action = Typ.SkipSameA
    , count = Rpt.emptyCount { Rpt.skipCnt = 1 }
    , notes = [Rpt.WarnN ("not executed: " <> reason)]
    }


dupMsg :: ConvIx -> ConvIx -> Text
dupMsg convDrop convKeep =
  let
    eid = convDrop.convInp.oaiIdCv
    updDrop = convDrop.convInp.updateTimeCv
    updKeep = convKeep.convInp.updateTimeCv
    whyKeep = if updDrop < updKeep then
        "kept newer occurrence"
      else
        "kept later occurrence with same update_time"
  in
  "duplicate conversation eid in input: " <> eid <> "; " <> whyKeep <> " at input index " <> tshow convKeep.ixInp
  <> " with update_time=" <> tshow updKeep <> "; skipped input index " <> tshow convDrop.ixInp
  <> " with update_time=" <> tshow updDrop


tshow :: Show a => a -> Text
tshow = T.pack . show