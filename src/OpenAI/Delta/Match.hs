{-# LANGUAGE DerivingStrategies #-}

module OpenAI.Delta.Match (
  NodeDiff(..), MsgDiff(..), matchNodes, matchMsg, matchMeta
) where

import Control.Applicative ((<|>))

import Data.Int (Int32)
import qualified Data.List as L
import qualified Data.Map.Strict as Mp
import qualified Data.Set as St
import Data.Text (Text)
import qualified Data.Text as T
import qualified OpenAI.Delta.Index as Ix
import qualified OpenAI.Delta.Snap as Sn
import OpenAI.Delta.Types (Conflict(..), MetaAct(..), Reason(..), Verdict(..))


data NodeDiff = NodeDiff {
    eidNode :: Text
    , dbNode :: Maybe Sn.NodeSnap
    , jsNode :: Maybe Sn.NodeSnap
    , verdict :: Verdict
  }
  deriving stock (Eq, Show)


data MsgDiff = MsgDiff {
    eidMsg :: Text
    , dbMsg :: Maybe Sn.MsgSnap
    , jsMsg :: Maybe Sn.MsgSnap
    , verdict :: Verdict
  }
  deriving stock (Eq, Show)


matchMeta :: Sn.ConvSnap -> Sn.ConvSnap -> MetaAct
matchMeta dbSnap jsSnap
  | jsSnap.timeUpdateCv < dbSnap.timeUpdateCv =
      RejectOlderMeta {
        dbTimeUpd = dbSnap.timeUpdateCv
        , jsTimeUpd = jsSnap.timeUpdateCv
      }
  | jsSnap.titleConv /= dbSnap.titleConv || jsSnap.timeUpdateCv /= dbSnap.timeUpdateCv =
      UpdateMeta {
        oldTitle = dbSnap.titleConv
        , newTitle = jsSnap.titleConv
        , oldTimeUpd = dbSnap.timeUpdateCv
        , newTimeUpd = jsSnap.timeUpdateCv
      }
  | otherwise = KeepMeta


matchNodes :: Ix.Index -> Ix.Index -> [NodeDiff]
matchNodes dbIx jsIx =
  fmap (matchNodeOne dbIx jsIx) $ orderedNodeEids dbIx jsIx


matchMsg :: Maybe Sn.MsgSnap -> Maybe Sn.MsgSnap -> MsgDiff
matchMsg dbMsg jsMsg =
  let
    updEidMsg = msgEid dbMsg jsMsg
  in
  case (dbMsg, jsMsg) of
    (Nothing, Nothing) ->
      MsgDiff {eidMsg = updEidMsg, dbMsg = Nothing, jsMsg = Nothing, verdict = SameV}

    (Nothing, Just jsOnly) ->
      MsgDiff {eidMsg = updEidMsg, dbMsg = Nothing, jsMsg = Just jsOnly, verdict = AddedV}

    (Just dbOnly, Nothing) ->
      MsgDiff {eidMsg = updEidMsg, dbMsg = Just dbOnly, jsMsg = Nothing, verdict = MissingV}

    (Just dbOne, Just jsOne)
      | dbOne.eidMsg /= jsOne.eidMsg ->
          MsgDiff {eidMsg = updEidMsg, dbMsg = Just dbOne, jsMsg = Just jsOne, verdict = ChangedV ShapeChangedR}
      | dbOne.hashMsg == jsOne.hashMsg ->
          MsgDiff {eidMsg = updEidMsg, dbMsg = Just dbOne, jsMsg = Just jsOne, verdict = SameV}
      | Just LT <- cmpUpdateTime dbOne jsOne ->
          MsgDiff {eidMsg = updEidMsg, dbMsg = Just dbOne, jsMsg = Just jsOne, verdict = ConflictV OlderJsonC}
      | Just GT <- cmpUpdateTime dbOne jsOne ->
          MsgDiff {eidMsg = updEidMsg, dbMsg = Just dbOne, jsMsg = Just jsOne, verdict = ChangedV TimeNewerR}
      | otherwise ->
          MsgDiff {eidMsg = updEidMsg, dbMsg = Just dbOne, jsMsg = Just jsOne, verdict = ChangedV HashChangedR}


matchNodeOne :: Ix.Index -> Ix.Index -> Text -> NodeDiff
matchNodeOne dbIx jsIx updEidNode =
  let
    updDbNode = Mp.lookup updEidNode dbIx.nodesByEid
    updJsNode = Mp.lookup updEidNode jsIx.nodesByEid
    updVerdict = matchNodeVerdict updDbNode updJsNode
  in
  NodeDiff {eidNode = updEidNode, dbNode = updDbNode, jsNode = updJsNode, verdict = updVerdict}


matchNodeVerdict :: Maybe Sn.NodeSnap -> Maybe Sn.NodeSnap -> Verdict
matchNodeVerdict dbNode jsNode =
  case (dbNode, jsNode) of
    (Nothing, Nothing) ->
      SameV

    (Nothing, Just _) ->
      AddedV

    (Just _, Nothing) ->
      MissingV

    (Just dbOne, Just jsOne)
      | dbOne.hashNode == jsOne.hashNode ->
          SameV

      | otherwise ->
          let
            msgDiff = matchMsg dbOne.msg jsOne.msg
          in
          case msgDiff.verdict of
            ConflictV conflict ->
              ConflictV conflict

            ChangedV reason ->
              ChangedV reason

            AddedV ->
              ChangedV ShapeChangedR

            MissingV ->
              ChangedV ShapeChangedR

            SameV
              | dbOne.eidParent /= jsOne.eidParent ->
                  ChangedV ParentChangedR
              | orderChanged dbOne jsOne ->
                  ChangedV OrderChangedR
              | otherwise ->
                  ChangedV ShapeChangedR


orderedNodeEids :: Ix.Index -> Ix.Index -> [Text]
orderedNodeEids dbIx jsIx =
  let
    jsEids = (.eidNode) <$> orderedNodes jsIx
    jsSet = St.fromList jsEids
    dbOnlyEids = fmap (.eidNode) $ filter (\node -> St.notMember node.eidNode jsSet) $ orderedNodes dbIx
  in
  jsEids <> dbOnlyEids


orderedNodes :: Ix.Index -> [Sn.NodeSnap]
orderedNodes ix =
  L.sortOn orderKeyNd $ Mp.elems ix.nodesByEid


orderKeyNd :: Sn.NodeSnap -> (Int, Int, Int, Text)
orderKeyNd node =
  (fromIntegral node.seqPre, fromIntegral node.seqNode, fromIntegral node.seqChild, node.eidNode)


orderChanged :: Sn.NodeSnap -> Sn.NodeSnap -> Bool
orderChanged dbNode jsNode =
  dbNode.seqNode /= jsNode.seqNode
    || dbNode.seqChild /= jsNode.seqChild
    || dbNode.seqPre /= jsNode.seqPre


cmpUpdateTime :: Sn.MsgSnap -> Sn.MsgSnap -> Maybe Ordering
cmpUpdateTime dbMsg jsMsg =
  compare <$> jsMsg.timeUpdate <*> dbMsg.timeUpdate


msgEid :: Maybe Sn.MsgSnap -> Maybe Sn.MsgSnap -> Text
msgEid dbMsg jsMsg =
  maybe T.empty (.eidMsg) $ jsMsg <|> dbMsg