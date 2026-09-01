module OpenAI.Delta.Validate (
    check
  ) where

import Data.Graph (SCC(..), stronglyConnComp)
import Data.List (nub, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Mp
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as St
import Data.Text (Text)
import qualified Data.Text as Tx

import qualified OpenAI.Delta.Types as Dt


-- This validator is intentionally strict. A policy-aware planner must resolve
-- any permitted conflict before passing its Delta to check.
check :: Dt.Delta -> Either [Dt.Conflict] Dt.Delta
check delta =
  case validationConflicts delta of
    [] -> Right delta
    conflicts -> Left conflicts


validationConflicts :: Dt.Delta -> [Dt.Conflict]
validationConflicts delta =
  nub $
    duplicateAddConflicts actions
      <> addExistingConflicts actions
      <> parentConflicts knownEids actions
      <> nodeUidConflicts actions
      <> messageConflicts knownEids actions
      <> embeddedConflicts actions
      <> cycleConflicts knownEids actions
  where
    actions = delta.nodeActs
    knownEids = nodeEids actions


nodeEids :: [Dt.NodeAct] -> Set Text
nodeEids actions =
  St.union addedEids existingEids
  where
    addedEids = St.fromList $ mapMaybe addedEid actions
    existingEids = St.fromList $ mapMaybe existingEid actions


addedEid :: Dt.NodeAct -> Maybe Text
addedEid action =
  case action of
    Dt.AddNA eid _ _ _ _ -> Just eid
    _ -> Nothing


existingEid :: Dt.NodeAct -> Maybe Text
existingEid action =
  case action of
    Dt.KeepNA ref -> Just $ nodeEid ref
    Dt.MoveNA ref _ _ _ _ _ _ -> Just $ nodeEid ref
    Dt.RewriteNA ref _ -> Just $ nodeEid ref
    Dt.ConflictNA ref _ -> Just $ nodeEid ref
    Dt.AddNA {} -> Nothing


nodeEid :: Dt.RefNode -> Text
nodeEid ref =
  case ref of
    Dt.RefNode eid _ -> eid


duplicateAddConflicts :: [Dt.NodeAct] -> [Dt.Conflict]
duplicateAddConflicts actions =
  [Dt.DuplicateEidC eid | (eid, count) <- Mp.toAscList counts, count > 1]
  where
    counts = Mp.fromListWith (+) [(eid, 1 :: Int) | eid <- mapMaybe addedEid actions]


addExistingConflicts :: [Dt.NodeAct] -> [Dt.Conflict]
addExistingConflicts actions =
  map Dt.DuplicateEidC $ St.toAscList $ St.intersection addedEids existingEids
  where
    addedEids = St.fromList $ mapMaybe addedEid actions
    existingEids = St.fromList $ mapMaybe existingEid actions


parentConflicts :: Set Text -> [Dt.NodeAct] -> [Dt.Conflict]
parentConflicts knownEids =
  concatMap $ parentConflict knownEids


parentConflict :: Set Text -> Dt.NodeAct -> [Dt.Conflict]
parentConflict knownEids action =
  case action of
    Dt.AddNA _ (Just eidParent) _ _ _
      | St.notMember eidParent knownEids -> [Dt.MissingDbNodeC eidParent]

    Dt.MoveNA _ _ (Just eidParent) _ _ _ _
      | St.notMember eidParent knownEids -> [Dt.MissingDbNodeC eidParent]

    _ -> []


nodeUidConflicts :: [Dt.NodeAct] -> [Dt.Conflict]
nodeUidConflicts =
  concatMap nodeUidConflict


nodeUidConflict :: Dt.NodeAct -> [Dt.Conflict]
nodeUidConflict action =
  case action of
    Dt.MoveNA (Dt.RefNode eid Nothing) _ _ _ _ _ _ ->
      [Dt.BrokenShapeC $ "node move has no DB uid: " <> eid]

    Dt.RewriteNA (Dt.RefNode eid Nothing) _ ->
      [Dt.BrokenShapeC $ "node rewrite has no DB uid: " <> eid]

    _ -> []


messageConflicts :: Set Text -> [Dt.NodeAct] -> [Dt.Conflict]
messageConflicts knownEids =
  concatMap $ messageConflict knownEids


messageConflict :: Set Text -> Dt.NodeAct -> [Dt.Conflict]
messageConflict knownEids action =
  case action of
    Dt.RewriteNA ref (Just msgAct) ->
      validateMsgAct knownEids (nodeEid ref) msgAct

    _ -> []


validateMsgAct :: Set Text -> Text -> Dt.MsgAct -> [Dt.Conflict]
validateMsgAct knownEids eidOwner msgAct =
  case msgAct of
    Dt.RewriteMA (Dt.RefMsg eidMsg Nothing) _ _ _ ->
      [Dt.BrokenShapeC $ "message rewrite has no DB uid: " <> eidMsg]

    Dt.AddMA eidTarget _
      | St.notMember eidTarget knownEids ->
          [Dt.MissingDbNodeC eidTarget]
      | eidTarget /= eidOwner ->
          [Dt.BrokenShapeC $
            "message add targets node " <> eidTarget
              <> " from node action " <> eidOwner]

    _ -> []


embeddedConflicts :: [Dt.NodeAct] -> [Dt.Conflict]
embeddedConflicts =
  concatMap embeddedConflict


embeddedConflict :: Dt.NodeAct -> [Dt.Conflict]
embeddedConflict action =
  case action of
    Dt.ConflictNA _ conflict ->
      [conflict]

    Dt.RewriteNA _ (Just (Dt.ConflictMA _ conflict)) ->
      [conflict]

    _ -> []


cycleConflicts :: Set Text -> [Dt.NodeAct] -> [Dt.Conflict]
cycleConflicts knownEids actions =
  map cycleConflict cyclicComponents
  where
    parents = finalParents actions
    graphNodes =
      [ (eid, eid, parentEdges eid parents)
      | eid <- St.toAscList knownEids
      ]
    cyclicComponents =
      [ sort eids
      | DtCycle eids <- map classifyScc $ stronglyConnComp graphNodes
      ]


data CycleClass =
    DtAcyclic
  | DtCycle [Text]


classifyScc :: SCC Text -> CycleClass
classifyScc component =
  case component of
    AcyclicSCC _ -> DtAcyclic
    CyclicSCC eids -> DtCycle eids


cycleConflict :: [Text] -> Dt.Conflict
cycleConflict eids =
  Dt.BrokenShapeC $
    "node parent cycle: " <> Tx.intercalate " -> " closedPath
  where
    closedPath =
      case eids of
        [] -> []
        firstEid : _ -> eids <> [firstEid]


finalParents :: [Dt.NodeAct] -> Map Text (Maybe Text)
finalParents =
  Mp.fromList . mapMaybe finalParent


finalParent :: Dt.NodeAct -> Maybe (Text, Maybe Text)
finalParent action =
  case action of
    Dt.AddNA eid eidParent _ _ _ ->
      Just (eid, eidParent)

    Dt.MoveNA ref _ eidParent _ _ _ _ ->
      Just (nodeEid ref, eidParent)

    _ -> Nothing


parentEdges :: Text -> Map Text (Maybe Text) -> [Text]
parentEdges eid parents =
  case Mp.lookup eid parents of
    Just (Just eidParent) -> [eidParent]
    _ -> []