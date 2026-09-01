{-# LANGUAGE DerivingStrategies #-}

module OpenAI.Delta.Report (ReportRaw(..), fromDelta, fromApply, isSameRaw) where

import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Tx

import qualified OpenAI.Delta.Apply as Da
import qualified OpenAI.Delta.Types as Dt


data ReportRaw = ReportRaw {
    uidConv :: Int64
    , nodeAddedCnt :: Int
    , nodeMovedCnt :: Int
    , nodeRewrittenCnt :: Int
    , msgAddedCnt :: Int
    , msgRewrittenCnt :: Int
    , titleChanged :: Bool
    , sameRaw :: Bool
    , notesRaw :: [Text]
  }
  deriving stock (Eq, Show)


fromDelta :: Dt.Delta -> ReportRaw
fromDelta delta =
  let
    statD = delta.stat
  in
  ReportRaw {
      uidConv = delta.uidConv
      , nodeAddedCnt = statD.nodeAddCnt
      , nodeMovedCnt = statD.nodeMoveCnt
      , nodeRewrittenCnt = statD.nodeRewriteCnt
      , msgAddedCnt = statD.msgAddCnt
      , msgRewrittenCnt = statD.msgRewriteCnt
      , titleChanged = titleChangedD delta.metaAct
      , sameRaw = isSameRaw delta
      , notesRaw = notesDelta delta
    }


fromApply :: Dt.Delta -> Da.ApplyResult -> ReportRaw
fromApply delta applyResult =
  ReportRaw {
      uidConv = applyResult.uidConv
      , nodeAddedCnt = applyResult.nodeAddedCnt
      , nodeMovedCnt = applyResult.nodeMovedCnt
      , nodeRewrittenCnt = applyResult.nodeRewriteCnt
      , msgAddedCnt = applyResult.msgAddedCnt
      , msgRewrittenCnt = applyResult.msgRewriteCnt
      , titleChanged = applyResult.metaUpdated && titleChangedD delta.metaAct
      , sameRaw = isSameRaw delta && noSemanticWrites applyResult
      , notesRaw = notesApply delta applyResult
    }


isSameRaw :: Dt.Delta -> Bool
isSameRaw delta = metaSame delta.metaAct && all nodeSame delta.nodeActs && statSame delta.stat


metaSame :: Dt.MetaAct -> Bool
metaSame metaAct =
  case metaAct of
    Dt.KeepMeta -> True
    Dt.UpdateMeta {} -> False
    Dt.RejectOlderMeta {} -> False


nodeSame :: Dt.NodeAct -> Bool
nodeSame nodeAct =
  case nodeAct of
    Dt.KeepNA {} -> True
    Dt.AddNA {} -> False
    Dt.MoveNA {} -> False
    Dt.RewriteNA {} -> False
    Dt.ConflictNA {} -> False


statSame :: Dt.Stat -> Bool
statSame statD =
  statD.nodeAddCnt == 0
    && statD.nodeMoveCnt == 0
    && statD.nodeRewriteCnt == 0
    && statD.msgAddCnt == 0
    && statD.msgRewriteCnt == 0
    && statD.conflictCnt == 0


titleChangedD :: Dt.MetaAct -> Bool
titleChangedD metaAct =
  case metaAct of
    Dt.UpdateMeta titleOld titleNew _ _ -> titleOld /= titleNew
    Dt.KeepMeta -> False
    Dt.RejectOlderMeta {} -> False


noSemanticWrites :: Da.ApplyResult -> Bool
noSemanticWrites applyResult =
  not applyResult.metaUpdated
    && applyResult.nodeAddedCnt == 0
    && applyResult.nodeMovedCnt == 0
    && applyResult.nodeRewriteCnt == 0
    && applyResult.msgAddedCnt == 0
    && applyResult.msgRewriteCnt == 0


notesDelta :: Dt.Delta -> [Text]
notesDelta delta =
  delta.notes <> notesMeta delta.metaAct <> notesConflict delta.stat.conflictCnt


notesApply :: Dt.Delta -> Da.ApplyResult -> [Text]
notesApply delta applyResult =
  notesDelta delta <> notesIngest applyResult


notesMeta :: Dt.MetaAct -> [Text]
notesMeta metaAct =
  case metaAct of
    Dt.KeepMeta ->
      []

    Dt.UpdateMeta titleOld titleNew timeOld timeNew
      | titleOld /= titleNew && timeOld /= timeNew ->
          ["conversation title and update timestamp changed"]
      | titleOld /= titleNew ->
          ["conversation title changed"]
      | timeOld /= timeNew ->
          ["conversation update timestamp changed"]
      | otherwise ->
          []

    Dt.RejectOlderMeta timeDb timeJs ->
      [ "older JSON conversation rejected: db update_time="
          <> showText timeDb
          <> ", json update_time="
          <> showText timeJs
      ]


notesConflict :: Int -> [Text]
notesConflict conflictCnt
  | conflictCnt <= 0 = []
  | otherwise = ["conversation delta contains " <> showText conflictCnt <> " conflict(s)"]


notesIngest :: Da.ApplyResult -> [Text]
notesIngest applyResult
  | noSemanticWrites applyResult = []
  | applyResult.ingestRecorded = []
  | otherwise = ["conversation update applied without an ingest trace"]


showText :: Show a => a -> Text
showText = Tx.pack . show