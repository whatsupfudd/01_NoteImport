{-# LANGUAGE DerivingStrategies #-}

module OpenAI.Import.Report
  ( Report(..)
  , BatchReport(..)
  , Count(..)
  , Note(..)
  , emptyCount
  , countReport
  , batchReport
  , renderReport
  , renderBatch
  ) where

import Data.Int (Int64)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import qualified OpenAI.Import.Types as It

data Count = Count
  { convAddedCnt :: Int
  , convUpdatedCnt :: Int
  , discAddedCnt :: Int
  , discUpdatedCnt :: Int
  , sumAddedCnt :: Int
  , skipCnt :: Int
  , failCnt :: Int
  }
  deriving stock (Eq, Show)

instance Semigroup Count where
  (<>) = addCount

instance Monoid Count where
  mempty = emptyCount

data Note
  = InfoN Text
  | WarnN Text
  | ErrorN Text
  deriving stock (Eq, Show)

data Report = Report
  { eidConv :: Text
  , uidConv :: Maybe Int64
  , uidDisc :: Maybe Int64
  , action :: It.Action
  , count :: Count
  , notes :: [Note]
  }
  deriving stock (Eq, Show)

data BatchReport = BatchReport
  { source :: It.Source
  , count :: Count
  , reports :: [Report]
  , failedEids :: [Text]
  , skippedEids :: [Text]
  }
  deriving stock (Eq, Show)

emptyCount :: Count
emptyCount =
  Count
    { convAddedCnt = 0
    , convUpdatedCnt = 0
    , discAddedCnt = 0
    , discUpdatedCnt = 0
    , sumAddedCnt = 0
    , skipCnt = 0
    , failCnt = 0
    }

countReport :: Report -> Count
countReport report = report.count

batchReport :: It.Source -> [Report] -> BatchReport
batchReport source reports =
  let count = foldl' (\acc report -> acc <> report.count) emptyCount reports
      failedEids = [report.eidConv | report <- reports, isFailed report]
      skippedEids = [report.eidConv | report <- reports, isSkipped report]
   in BatchReport
        { source = source
        , count = count
        , reports = reports
        , failedEids = failedEids
        , skippedEids = skippedEids
        }

renderReport :: Report -> Text
renderReport report =
  let status = "[" <> statusText report.action <> "]"
      eidTxt = shortEid report.eidConv
      uidParts = renderUidParts report
      countTxt = renderCount report.count
      noteTxt = renderNotes report.notes
      pieces = filter (not . T.null) [status, eidTxt, uidParts, countTxt, noteTxt]
   in T.intercalate " " pieces

renderBatch :: BatchReport -> Text
renderBatch batch =
  let header =
        [ "source: " <> renderSource batch.source
        , "total: " <> renderCount batch.count
        ]
      extra =
        concat
          [ [ "failed: " <> T.intercalate ", " batch.failedEids | not (null batch.failedEids) ]
          , [ "skipped: " <> T.intercalate ", " batch.skippedEids | not (null batch.skippedEids) ]
          ]
      body = map renderReport batch.reports
      sections = header <> extra <> [""] <> body
   in T.intercalate "\n" sections

addCount :: Count -> Count -> Count
addCount a b =
  Count
    { convAddedCnt = a.convAddedCnt + b.convAddedCnt
    , convUpdatedCnt = a.convUpdatedCnt + b.convUpdatedCnt
    , discAddedCnt = a.discAddedCnt + b.discAddedCnt
    , discUpdatedCnt = a.discUpdatedCnt + b.discUpdatedCnt
    , sumAddedCnt = a.sumAddedCnt + b.sumAddedCnt
    , skipCnt = a.skipCnt + b.skipCnt
    , failCnt = a.failCnt + b.failCnt
    }

isFailed :: Report -> Bool
isFailed report =
  case report.action of
    It.FailA -> True
    _ -> report.count.failCnt > 0 || any isErrorNote report.notes

isSkipped :: Report -> Bool
isSkipped report =
  case report.action of
    It.SkipSameA -> True
    It.SkipOlderA -> True
    _ -> report.count.skipCnt > 0

isErrorNote :: Note -> Bool
isErrorNote note =
  case note of
    ErrorN _ -> True
    _ -> False

statusText :: It.Action -> Text
statusText action =
  case action of
    It.AddFreshA -> "added"
    It.UpdateKnownA -> "updated"
    It.SkipSameA -> "skipped"
    It.SkipOlderA -> "skipped"
    It.FailA -> "failed"

shortEid :: Text -> Text
shortEid eid
  | T.length eid <= 12 = eid
  | otherwise = T.take 12 eid <> "..."

renderUidParts :: Report -> Text
renderUidParts report =
  let pieces =
        concat
          [ [ "uidConv=" <> tshow uid | uid <- maybeToList report.uidConv ]
          , [ "uidDisc=" <> tshow uid | uid <- maybeToList report.uidDisc ]
          ]
   in T.intercalate " " pieces

renderCount :: Count -> Text
renderCount count =
  let pieces =
        concat
          [ [ "conv +" <> tshow count.convAddedCnt | count.convAddedCnt > 0 ]
          , [ "conv ~" <> tshow count.convUpdatedCnt | count.convUpdatedCnt > 0 ]
          , [ "disc +" <> tshow count.discAddedCnt | count.discAddedCnt > 0 ]
          , [ "disc ~" <> tshow count.discUpdatedCnt | count.discUpdatedCnt > 0 ]
          , [ "sum +" <> tshow count.sumAddedCnt | count.sumAddedCnt > 0 ]
          , [ "skip " <> tshow count.skipCnt | count.skipCnt > 0 ]
          , [ "fail " <> tshow count.failCnt | count.failCnt > 0 ]
          ]
   in if null pieces then "no-op" else T.intercalate ", " pieces

renderNotes :: [Note] -> Text
renderNotes notes =
  let texts = map renderNote notes
   in T.intercalate "; " texts

renderNote :: Note -> Text
renderNote note =
  case note of
    InfoN txt -> txt
    WarnN txt -> "warn: " <> txt
    ErrorN txt -> "error: " <> txt

renderSource :: It.Source -> Text
renderSource source =
  let pathTxt =
        case source.pathSrc of
          Just path -> T.pack path
          Nothing -> "<unknown>"
      exportTxt = if source.exportSrc then "export" else "json"
      labelTxt =
        case source.labelSrc of
          Just label -> " label=" <> label
          Nothing -> ""
   in pathTxt <> " (" <> exportTxt <> ")" <> labelTxt

tshow :: Show a => a -> Text
tshow = T.pack . show

maybeToList :: Maybe a -> [a]
maybeToList mx =
  case mx of
    Nothing -> []
    Just x -> [x]