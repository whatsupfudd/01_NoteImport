{-# LANGUAGE DerivingStrategies #-}

module OpenAI.Delta.Index (Index(..), build, lookupNode, lookupMsg, nodeEids, msgEids) where

import Data.Int (Int32)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Mp
import Data.Set (Set)
import qualified Data.Set as St
import Data.Text (Text)
import qualified Data.Text as T

import qualified OpenAI.Delta.Snap as Sn
import OpenAI.Delta.Types (Conflict(..))


data Index = Index {
    nodesByEid :: Map Text Sn.NodeSnap
    , msgsByEid :: Map Text Sn.MsgSnap
    , nodesBySeq :: Map Int32 Sn.NodeSnap
    , roots :: [Sn.NodeSnap]
    , issues :: [Conflict]
  }
  deriving stock (Eq, Show)


data BuildSt = BuildSt {
    nodesByEidBs :: Map Text Sn.NodeSnap
    , msgsByEidBs :: Map Text Sn.MsgSnap
    , nodesBySeqBs :: Map Int32 Sn.NodeSnap
    , rootsRevBs :: [Sn.NodeSnap]
    , issuesRevBs :: [Conflict]
  }


build :: Sn.ConvSnap -> Index
build conv =
  let
    st0 = emptyBuildSt
    st1 = foldl' addNode st0 conv.nodes
    st2 = addParentIssues st1
    st3 = addRootIssues conv st2
  in
  Index {
      nodesByEid = st3.nodesByEidBs
      , msgsByEid = st3.msgsByEidBs
      , nodesBySeq = st3.nodesBySeqBs
      , roots = reverse st3.rootsRevBs
      , issues = reverse st3.issuesRevBs
    }


lookupNode :: Text -> Index -> Maybe Sn.NodeSnap
lookupNode eid index = Mp.lookup eid index.nodesByEid


lookupMsg :: Text -> Index -> Maybe Sn.MsgSnap
lookupMsg eid index = Mp.lookup eid index.msgsByEid


nodeEids :: Index -> Set Text
nodeEids index = St.fromList $ Mp.keys index.nodesByEid


msgEids :: Index -> Set Text
msgEids index = St.fromList $ Mp.keys index.msgsByEid


emptyBuildSt :: BuildSt
emptyBuildSt =
  BuildSt {
      nodesByEidBs = Mp.empty
      , msgsByEidBs = Mp.empty
      , nodesBySeqBs = Mp.empty
      , rootsRevBs = []
      , issuesRevBs = []
    }


addNode :: BuildSt -> Sn.NodeSnap -> BuildSt
addNode st node =
  let
    st1 = addRoot st node
    st2 = addNodeEid st1 node
    st3 = addNodeSeq st2 node
  in
  case node.msg of
    Nothing -> st3
    Just msg -> addMsg st3 msg


addRoot :: BuildSt -> Sn.NodeSnap -> BuildSt
addRoot st node =
  case node.eidParent of
    Nothing -> st {rootsRevBs = node : st.rootsRevBs}
    Just _ -> st


addNodeEid :: BuildSt -> Sn.NodeSnap -> BuildSt
addNodeEid st node =
  case keepFirst node.eidNode node st.nodesByEidBs of
    (updNodesByEidBs, True) -> st { nodesByEidBs = updNodesByEidBs }
    (_, False) -> addIssue (DuplicateEidC node.eidNode) st


addNodeSeq :: BuildSt -> Sn.NodeSnap -> BuildSt
addNodeSeq st node =
  case keepFirst node.seqNode node st.nodesBySeqBs of
    (updNodesBySeqBs, True) -> st {nodesBySeqBs = updNodesBySeqBs}
    (_, False) ->
      addIssue (BrokenShapeC $ "duplicate node seq " <> renderI32 node.seqNode <> " at node " <> node.eidNode) st


addMsg :: BuildSt -> Sn.MsgSnap -> BuildSt
addMsg st msg =
  case keepFirst msg.eidMsg msg st.msgsByEidBs of
    (updMsgsByEidBs, True) -> st {msgsByEidBs = updMsgsByEidBs}
    (_, False) -> addIssue (DuplicateEidC msg.eidMsg) st


addParentIssues :: BuildSt -> BuildSt
addParentIssues st =
  foldl' checkParent st $ Mp.elems st.nodesByEidBs


checkParent :: BuildSt -> Sn.NodeSnap -> BuildSt
checkParent st node =
  case node.eidParent of
    Nothing -> st
    Just eidParent
      | eidParent == node.eidNode ->
          addIssue (BrokenShapeC $ "node " <> node.eidNode <> " is its own parent") st
      | Mp.member eidParent st.nodesByEidBs -> st
      | otherwise ->
          addIssue (BrokenShapeC $ "node " <> node.eidNode <> " refers to missing parent " <> eidParent) st


addRootIssues :: Sn.ConvSnap -> BuildSt -> BuildSt
addRootIssues conv st =
  case reverse st.rootsRevBs of
    [] -> addIssue (BrokenShapeC $ "conversation " <> conv.eidConv <> " has no root node") st
    [_] -> st
    roots ->
      addIssue (BrokenShapeC $ "conversation " <> conv.eidConv <> " has multiple roots: "
          <> T.intercalate ", " (map (.eidNode) roots)
        ) st


addIssue :: Conflict -> BuildSt -> BuildSt
addIssue issue st = st {issuesRevBs = issue : st.issuesRevBs}


keepFirst :: Ord key => key -> val -> Map key val -> (Map key val, Bool)
keepFirst key value mapping =
  case Mp.lookup key mapping of
    Just _ -> (mapping, False)
    Nothing -> (Mp.insert key value mapping, True)


renderI32 :: Int32 -> Text
renderI32 = T.pack . show