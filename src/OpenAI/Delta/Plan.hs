{- HLINT ignore "Use list comprehension" -}
{-# LANGUAGE DerivingStrategies #-}

module OpenAI.Delta.Plan (
    compute
    , policyDefault
    , PolicyD(..)
  ) where

import Data.Int (Int32)
import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as Mp
import Data.Maybe (isJust)
import qualified Data.Set as St
import Data.Text (Text)
import qualified Data.Text as T

import qualified OpenAI.Delta.Index as Ix
import qualified OpenAI.Delta.Match as Mt
import qualified OpenAI.Delta.Snap as Sn
import qualified OpenAI.Delta.Types as Ty


data PolicyD = PolicyD {
    allowOlderD :: Bool
    , allowMoveD :: Bool
    , allowRewriteD :: Bool
    , allowDbOnlyD :: Bool
    , rejectConflictD :: Bool
  }
  deriving stock (Eq, Show)


data MetaPlan = MetaPlan {
    actionMP :: Ty.MetaAct
    , conflictsMP :: [Ty.Conflict]
    , notesMP :: [Text]
  }


data NodePlan = NodePlan {
    actionsNP :: [Ty.NodeAct]
    , statNP :: Ty.Stat
    , conflictsNP :: [Ty.Conflict]
    , notesNP :: [Text]
  }


data PositionPlan = PositionPlan {
    actionsPP :: [Ty.NodeAct]
    , statPP :: Ty.Stat
    , conflictsPP :: [Ty.Conflict]
  }


data MessagePlan = MessagePlan {
    actionMsgP :: Maybe Ty.MsgAct
    , statMsgP :: Ty.Stat
    , conflictsMsgP :: [Ty.Conflict]
    , notesMsgP :: [Text]
  }


policyDefault :: PolicyD
policyDefault =
  PolicyD {
      allowOlderD = False
      , allowMoveD = True
      , allowRewriteD = True
      , allowDbOnlyD = True
      , rejectConflictD = True
    }


compute :: PolicyD -> Sn.ConvSnap -> Sn.ConvSnap -> Either [Ty.Conflict] Ty.Delta
compute policy dbSnap jsSnap
  | dbSnap.eidConv /= jsSnap.eidConv =
      Left [Ty.BrokenShapeC $ "conversation EID mismatch: DB=" <> dbSnap.eidConv <> ", JSON=" <> jsSnap.eidConv]
  | otherwise =
      case dbSnap.uidConv of
        Nothing -> Left [Ty.BrokenShapeC $ "DB conversation has no UID: " <> dbSnap.eidConv]
        Just uidConv ->
          let
            dbIx = Ix.build dbSnap
            jsIx = Ix.build jsSnap
            structuralConflicts = dbIx.issues <> jsIx.issues <> validateDbRefs dbSnap
          in
          if not $ null structuralConflicts then
            Left structuralConflicts
          else
            let
              metaPlan = planMeta policy dbSnap jsSnap
              plans = planNodes policy dbIx.nodesByEid jsIx.nodesByEid dbSnap.nodes jsSnap.nodes
              nodeStat = foldl' addStat zeroStat $ map (.statNP) plans
              metaConflictStat = conflictStat $ length metaPlan.conflictsMP
              stat = addStat nodeStat metaConflictStat
              conflicts = metaPlan.conflictsMP <> concatMap (.conflictsNP) plans
              actions = concatMap (.actionsNP) plans
              conflictNotes = if null conflicts then
                  []
                else
                  [T.pack (show $ length conflicts) <> " conflict(s) retained in the delta"]
              notes = metaPlan.notesMP <> concatMap (.notesNP) plans <> conflictNotes
              delta = Ty.Delta dbSnap.eidConv uidConv metaPlan.actionMP actions stat notes
            in
            if policy.rejectConflictD && not (null conflicts) then
              Left conflicts
            else
              Right delta


planMeta :: PolicyD -> Sn.ConvSnap -> Sn.ConvSnap -> MetaPlan
planMeta policy dbSnap jsSnap =
  case Mt.matchMeta dbSnap jsSnap of
    Ty.KeepMeta -> MetaPlan Ty.KeepMeta [] []
    action@Ty.UpdateMeta{} -> MetaPlan action [] []
    Ty.RejectOlderMeta timeDb timeJs
      | policy.allowOlderD -> MetaPlan
            (Ty.UpdateMeta dbSnap.titleConv jsSnap.titleConv timeDb timeJs) []
            ["older conversation metadata accepted by delta policy"]
      | otherwise -> MetaPlan
            (Ty.RejectOlderMeta timeDb timeJs) [Ty.OlderJsonC]
            ["JSON conversation update_time is older than the stored conversation"]


planNodes :: PolicyD -> Mp.Map Text Sn.NodeSnap -> Mp.Map Text Sn.NodeSnap -> [Sn.NodeSnap] -> [Sn.NodeSnap]
      -> [NodePlan]
planNodes policy dbNodes jsNodes dbOrdered jsOrdered =
  let
    jsAsc = sortNodes jsOrdered
    dbAsc = sortNodes dbOrdered
    jsEids = St.fromList $ map (.eidNode) jsAsc
    planJsonNode jsNode =
      case Mp.lookup jsNode.eidNode dbNodes of
        Nothing -> planAdded dbNodes jsNodes jsNode
        Just dbNode -> planShared policy dbNodes jsNodes dbNode jsNode
    dbOnly = filter (\node -> not $ St.member node.eidNode jsEids) dbAsc
  in
  map planJsonNode jsAsc <> map (planDbOnly policy) dbOnly


sortNodes :: [Sn.NodeSnap] -> [Sn.NodeSnap]
sortNodes = sortOn (\node -> (fromIntegral node.seqPre, node.seqNode, node.eidNode))


planAdded :: Mp.Map Text Sn.NodeSnap -> Mp.Map Text Sn.NodeSnap -> Sn.NodeSnap -> NodePlan
planAdded dbNodes jsNodes jsNode =
  case validateAddedParent dbNodes jsNodes jsNode of
    Just conflict ->
      NodePlan
        [Ty.ConflictNA (refNodeJs jsNode) conflict]
        (conflictStat 1)
        [conflict]
        ["new JSON node cannot be inserted safely: " <> jsNode.eidNode]

    Nothing ->
      let
        messageStat =
          case jsNode.msg of
            Nothing -> zeroStat
            Just _ -> msgAddStat
        stat = addStat nodeAddStat messageStat
        action = Ty.AddNA jsNode.eidNode jsNode.eidParent jsNode.seqNode jsNode.seqChild jsNode.seqPre
      in
      NodePlan [action] stat [] []


validateAddedParent :: Mp.Map Text Sn.NodeSnap -> Mp.Map Text Sn.NodeSnap
      -> Sn.NodeSnap -> Maybe Ty.Conflict
validateAddedParent dbNodes jsNodes jsNode =
  case jsNode.eidParent of
    Nothing
      | Mp.null dbNodes -> Nothing
      | otherwise -> Just $ Ty.BrokenShapeC ("new node would introduce another root: " <> jsNode.eidNode)
    Just eidParent
      | eidParent == jsNode.eidNode ->
          Just $ Ty.BrokenShapeC ("node is its own parent: " <> jsNode.eidNode)
      | not (Mp.member eidParent jsNodes) ->
          Just $ Ty.BrokenShapeC ("JSON node references a missing parent: " <> jsNode.eidNode <> " -> " <> eidParent)
      | Mp.member eidParent dbNodes -> Nothing
      | otherwise ->
          case Mp.lookup eidParent jsNodes of
            Just parent
              | parent.seqPre < jsNode.seqPre -> Nothing
              | otherwise -> Just $ Ty.BrokenShapeC ("new parent is not ordered before its child: " <> eidParent
                    <> " -> " <> jsNode.eidNode)
            Nothing -> Just $ Ty.MissingDbNodeC eidParent


planShared :: PolicyD -> Mp.Map Text Sn.NodeSnap -> Mp.Map Text Sn.NodeSnap
      -> Sn.NodeSnap -> Sn.NodeSnap -> NodePlan
planShared policy dbNodes jsNodes dbNode jsNode =
  case validateSharedParent dbNodes jsNodes dbNode jsNode of
    Just conflict ->
      NodePlan
        [Ty.ConflictNA (refNodeDb dbNode) conflict]
        (conflictStat 1)
        [conflict]
        ["existing JSON node has an unsafe parent relationship: " <> jsNode.eidNode]

    Nothing ->
      let
        positionPlan = planPosition policy dbNode jsNode
        messagePlan = planMessage policy dbNode jsNode
        messageActions =
          case messagePlan.actionMsgP of
            Nothing -> []
            Just action -> [Ty.RewriteNA (refNodeDb dbNode) (Just action)]
        actions0 = positionPlan.actionsPP <> messageActions
        actions =
          if null actions0
            then [Ty.KeepNA $ refNodeDb dbNode]
            else actions0
        baseStat = addStat positionPlan.statPP messagePlan.statMsgP
        stat =
          if null actions0
            then addStat nodeKeepStat baseStat
            else baseStat
      in
      NodePlan {
          actionsNP = actions
          , statNP = stat
          , conflictsNP = positionPlan.conflictsPP <> messagePlan.conflictsMsgP
          , notesNP = messagePlan.notesMsgP
        }


validateSharedParent :: Mp.Map Text Sn.NodeSnap -> Mp.Map Text Sn.NodeSnap
      -> Sn.NodeSnap -> Sn.NodeSnap -> Maybe Ty.Conflict
validateSharedParent dbNodes jsNodes dbNode jsNode =
  case jsNode.eidParent of
    Nothing -> Nothing
    Just eidParent
      | eidParent == jsNode.eidNode ->
          Just $ Ty.BrokenShapeC ("node is its own parent: " <> jsNode.eidNode)

      | not (Mp.member eidParent jsNodes) ->
          Just $ Ty.BrokenShapeC
            ("JSON node references a missing parent: " <> jsNode.eidNode <> " -> " <> eidParent)

      | dbNode.eidParent /= jsNode.eidParent && not (Mp.member eidParent dbNodes) ->
          Just $ Ty.MissingDbNodeC eidParent

      | otherwise ->
          Nothing


planPosition :: PolicyD -> Sn.NodeSnap -> Sn.NodeSnap -> PositionPlan
planPosition policy dbNode jsNode
  | samePosition dbNode jsNode =
      PositionPlan [] zeroStat []

  | policy.allowMoveD =
      let
        action = Ty.MoveNA
          (refNodeDb dbNode)
          dbNode.eidParent
          jsNode.eidParent
          dbNode.seqChild
          jsNode.seqChild
          dbNode.seqPre
          jsNode.seqPre
      in
      PositionPlan [action] nodeMoveStat []

  | otherwise =
      let
        conflict =
          if dbNode.eidParent /= jsNode.eidParent
            then Ty.ParentMismatchC dbNode.eidNode
            else Ty.BrokenShapeC $ "node order changed while moves are disabled: " <> dbNode.eidNode
      in
      PositionPlan
        [Ty.ConflictNA (refNodeDb dbNode) conflict]
        (conflictStat 1)
        [conflict]


samePosition :: Sn.NodeSnap -> Sn.NodeSnap -> Bool
samePosition dbNode jsNode =
  dbNode.eidParent == jsNode.eidParent
    && dbNode.seqNode == jsNode.seqNode
    && dbNode.seqChild == jsNode.seqChild
    && dbNode.seqPre == jsNode.seqPre


planMessage :: PolicyD -> Sn.NodeSnap -> Sn.NodeSnap -> MessagePlan
planMessage policy dbNode jsNode =
  case (dbNode.msg, jsNode.msg) of
    (Nothing, Nothing) ->
      MessagePlan Nothing zeroStat [] []

    (Nothing, Just jsMsg) ->
      MessagePlan
        (Just $ Ty.AddMA jsNode.eidNode jsMsg.eidMsg)
        msgAddRewriteNodeStat
        []
        []

    (Just dbMsg, Nothing)
      | policy.allowDbOnlyD ->
          MessagePlan
            Nothing
            msgKeepStat
            []
            ["DB-only message retained for node: " <> dbNode.eidNode]

      | otherwise ->
          let
            conflict = Ty.MissingJsonNodeC dbNode.eidNode
          in
          MessagePlan
            (Just $ Ty.ConflictMA (refMsgDb dbMsg) conflict)
            (conflictStat 1)
            [conflict]
            []

    (Just dbMsg, Just jsMsg)
      | dbMsg.eidMsg /= jsMsg.eidMsg ->
          let
            conflict = Ty.BrokenShapeC $ "message EID changed on node " <> dbNode.eidNode <> ": DB="
                <> dbMsg.eidMsg <> ", JSON=" <> jsMsg.eidMsg <> "| dbMsg: " <> (T.pack . show) dbMsg <> ", jsMsg: " <> (T.pack . show) jsMsg
          in
          MessagePlan
            (Just $ Ty.ConflictMA (refMsgDb dbMsg) conflict)
            (conflictStat 1)
            [conflict]
            []

      | dbMsg.hashMsg == jsMsg.hashMsg ->
          MessagePlan Nothing msgKeepStat [] []

      | messageOlder dbMsg jsMsg && not policy.allowOlderD ->
          MessagePlan
            (Just $ Ty.ConflictMA (refMsgDb dbMsg) Ty.OlderJsonC)
            (conflictStat 1)
            [Ty.OlderJsonC]
            ["older JSON message retained without rewriting DB state: " <> dbMsg.eidMsg]

      | not policy.allowRewriteD ->
          MessagePlan
            (Just $ Ty.ConflictMA (refMsgDb dbMsg) Ty.HashMismatchC)
            (conflictStat 1)
            [Ty.HashMismatchC]
            ["message hash differs while rewrites are disabled: " <> dbMsg.eidMsg]

      | otherwise ->
          let
            reason = messageReason policy dbMsg jsMsg
            action = Ty.RewriteMA
              (refMsgDb dbMsg)
              (Just dbMsg.hashMsg)
              jsMsg.hashMsg
              reason
          in
          MessagePlan
            (Just action)
            msgRewriteNodeStat
            []
            [rewriteNote reason dbMsg.eidMsg]


messageOlder :: Sn.MsgSnap -> Sn.MsgSnap -> Bool
messageOlder dbMsg jsMsg =
  case (dbMsg.timeUpdate, jsMsg.timeUpdate) of
    (Just timeDb, Just timeJs) -> timeJs < timeDb
    _ ->
      case (dbMsg.timeCreate, jsMsg.timeCreate) of
        (Just timeDb, Just timeJs) -> timeJs < timeDb
        _ -> False


messageReason :: PolicyD -> Sn.MsgSnap -> Sn.MsgSnap -> Ty.Reason
messageReason policy dbMsg jsMsg
  | messageOlder dbMsg jsMsg && policy.allowOlderD =
      Ty.PolicyR "older JSON message rewrite accepted by delta policy"

  | messageNewer dbMsg jsMsg =
      Ty.TimeNewerR

  | otherwise =
      Ty.HashChangedR


messageNewer :: Sn.MsgSnap -> Sn.MsgSnap -> Bool
messageNewer dbMsg jsMsg =
  case (dbMsg.timeUpdate, jsMsg.timeUpdate) of
    (Just timeDb, Just timeJs) -> timeJs > timeDb
    _ ->
      case (dbMsg.timeCreate, jsMsg.timeCreate) of
        (Just timeDb, Just timeJs) -> timeJs > timeDb
        _ -> False


rewriteNote :: Ty.Reason -> Text -> Text
rewriteNote reason eidMsg =
  case reason of
    Ty.TimeNewerR -> "newer JSON message will replace stored message payload: " <> eidMsg
    Ty.HashChangedR -> "message hash changed without a newer timestamp: " <> eidMsg
    Ty.PolicyR note -> note <> ": " <> eidMsg
    _ -> "message will be rewritten: " <> eidMsg


planDbOnly :: PolicyD -> Sn.NodeSnap -> NodePlan
planDbOnly policy dbNode
  | policy.allowDbOnlyD =
      let
        messageStat =
          case dbNode.msg of
            Nothing -> zeroStat
            Just _ -> msgKeepStat
      in
      NodePlan
        [Ty.KeepNA $ refNodeDb dbNode]
        (addStat nodeKeepStat messageStat)
        []
        ["DB-only node retained: " <> dbNode.eidNode]

  | otherwise =
      let
        conflict = Ty.MissingJsonNodeC dbNode.eidNode
      in
      NodePlan
        [Ty.ConflictNA (refNodeDb dbNode) conflict]
        (conflictStat 1)
        [conflict]
        ["DB-only node rejected by delta policy: " <> dbNode.eidNode]


validateDbRefs :: Sn.ConvSnap -> [Ty.Conflict]
validateDbRefs snap =
  concatMap validateNode snap.nodes
  where
  validateNode :: Sn.NodeSnap -> [Ty.Conflict]
  validateNode node =
    let
      nodeIssues =
        if isJust node.uidNode then
          []
        else
          [Ty.BrokenShapeC $ "DB node has no UID: " <> node.eidNode]
      messageIssues =
        case node.msg of
          Nothing -> []
          Just msg
            | isJust msg.uidMsg -> []
            | otherwise -> [Ty.BrokenShapeC $ "DB message has no UID: " <> msg.eidMsg]
    in
    nodeIssues <> messageIssues


refNodeDb :: Sn.NodeSnap -> Ty.RefNode
refNodeDb node = Ty.RefNode node.eidNode node.uidNode


refNodeJs :: Sn.NodeSnap -> Ty.RefNode
refNodeJs node = Ty.RefNode node.eidNode Nothing


refMsgDb :: Sn.MsgSnap -> Ty.RefMsg
refMsgDb msg = Ty.RefMsg msg.eidMsg msg.uidMsg


zeroStat :: Ty.Stat
zeroStat = Ty.Stat 0 0 0 0 0 0 0 0


nodeAddStat :: Ty.Stat
nodeAddStat = Ty.Stat 1 0 0 0 0 0 0 0


nodeKeepStat :: Ty.Stat
nodeKeepStat = Ty.Stat 0 1 0 0 0 0 0 0


nodeMoveStat :: Ty.Stat
nodeMoveStat = Ty.Stat 0 0 1 0 0 0 0 0


msgAddStat :: Ty.Stat
msgAddStat = Ty.Stat 0 0 0 0 1 0 0 0


msgKeepStat :: Ty.Stat
msgKeepStat = Ty.Stat 0 0 0 0 0 1 0 0


msgAddRewriteNodeStat :: Ty.Stat
msgAddRewriteNodeStat = Ty.Stat 0 0 0 1 1 0 0 0


msgRewriteNodeStat :: Ty.Stat
msgRewriteNodeStat = Ty.Stat 0 0 0 1 0 0 1 0


conflictStat :: Int -> Ty.Stat
conflictStat count = Ty.Stat 0 0 0 0 0 0 0 count


addStat :: Ty.Stat -> Ty.Stat -> Ty.Stat
addStat
    (Ty.Stat nodeAddA nodeKeepA nodeMoveA nodeRewriteA msgAddA msgKeepA msgRewriteA conflictA)
    (Ty.Stat nodeAddB nodeKeepB nodeMoveB nodeRewriteB msgAddB msgKeepB msgRewriteB conflictB) =
  Ty.Stat
    (nodeAddA + nodeAddB)
    (nodeKeepA + nodeKeepB)
    (nodeMoveA + nodeMoveB)
    (nodeRewriteA + nodeRewriteB)
    (msgAddA + msgAddB)
    (msgKeepA + msgKeepB)
    (msgRewriteA + msgRewriteB)
    (conflictA + conflictB)