{-# LANGUAGE DerivingStrategies #-}
{- HLINT ignore "Use list comprehension" -}
module OpenAI.Conversation.Json.Node.Order
  ( NodeOrd(..)
  , OrdIssue(..)
  , buildNodeOrd
  , specialRootEid
  , renderOrdIssues
  , renderOrdIssue
  ) where

import Data.Int (Int32)
import Data.List (foldl', nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Mp
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as St
import Data.Text (Text)
import qualified Data.Text as T

import qualified OpenAI.Conversation.Json.V1.Schema as Jv1
import qualified OpenAI.Conversation.Json.Node as Nd
import Notion.Fetch (Block(children))


data NodeOrd = NodeOrd {
    eidNode :: Text
  , eidParent :: Maybe Text
  , seqNode :: Int32
  , seqChild :: Int32
  , seqPre :: Int32
  }
  deriving stock (Eq, Show)

data OrdIssue =
    MissingRootOI
  | MultipleRootOI
  | MissingNodeOI Text
  | MissingParentOI Text Text
  | CycleOI Text
  | DuplicateChildOI Text Text
  | BranchOI Text [Text]
  | DebugInfo Text Text
  deriving stock (Eq, Show)

specialRootEid :: Text
specialRootEid = T.pack "client-created-root"

data WalkSt = WalkSt {
    nextSeqWs :: Int32
  , ordsRevWs :: [NodeOrd]
  , seenWs :: Set Text
  , activeWs :: Set Text
  , issuesWs :: [OrdIssue]
  }
  deriving (Show)


buildNodeOrd :: Map Text Nd.Node -> Either [OrdIssue] [NodeOrd]
buildNodeOrd mapping =
  case selectRoot mapping of
    Left issues -> Left issues
    Right eidRoot ->
      let
        issuesBase = validateMapping mapping
        issuesBranch = branchIssues mapping
        initState = WalkSt { nextSeqWs = 0, ordsRevWs = [], seenWs = St.empty, activeWs = St.empty, issuesWs = [] }
        walkRez = scanNodes mapping Nothing 0 eidRoot initState
        dbgWalkRez = walkRez -- { issuesWs = [DebugInfo "root" eidRoot] <> walkRez.issuesWs } -- , DebugInfo "state" (T.pack $ show walkRez)
        issuesRest = disconnectedIssues mapping dbgWalkRez.seenWs
        issuesAll = nub (issuesBase <> dbgWalkRez.issuesWs <> issuesRest)
      in
      if null issuesAll then
        Right $ reverse walkRez.ordsRevWs
      else
        Left . nub $ issuesAll <> issuesBranch


selectRoot :: Map Text Nd.Node -> Either [OrdIssue] Text
selectRoot mapping
  | Mp.member specialRootEid mapping = Right specialRootEid
  | otherwise =
      case [eid | (eid, node) <- Mp.toAscList mapping, isNothing node.parentNd] of
        [eid] -> Right eid
        [] -> Left [MissingRootOI]
        _ -> Left [MultipleRootOI]


validateMapping :: Map Text Nd.Node -> [OrdIssue]
validateMapping mapping =
  concatMap validateNode (Mp.toAscList mapping)
  where
  validateNode :: (Text, Nd.Node) -> [OrdIssue]
  validateNode (eid, node) = missingParent node <> missingChildren node <> dupChildren eid node

  missingParent :: Nd.Node -> [OrdIssue]
  missingParent node =
    case node.parentNd of
      Just parentEid | Mp.notMember parentEid mapping -> [MissingParentOI node.idNd parentEid]
      _ -> []

  missingChildren :: Nd.Node -> [OrdIssue]
  missingChildren node = [ MissingNodeOI eidChild | eidChild <- node.childrenNd, Mp.notMember eidChild mapping ]

  dupChildren :: Text -> Nd.Node -> [OrdIssue]
  dupChildren eid node = [ DuplicateChildOI eid eidChild | eidChild <- duplicateItems node.childrenNd ]


branchIssues :: Map Text Nd.Node -> [OrdIssue]
branchIssues mapping =
  [ BranchOI eid eidsChild
  | (eid, node) <- Mp.toAscList mapping
  , let eidsChild = node.childrenNd
  , length eidsChild > 1
  ]


disconnectedIssues :: Map Text Nd.Node -> Set Text -> [OrdIssue]
disconnectedIssues mapping seen =
  let
    rest = [(eid, node) | (eid, node) <- Mp.toAscList mapping, St.notMember eid seen]
    hasExtraRoot = any (isNothing . (.parentNd) . snd) rest
    rootIssues = if hasExtraRoot then [MissingRootOI] else []
    parentIssues =
      [ MissingParentOI eid ("(disc) " <> eidParent) | (eid, node) <- rest, Just eidParent <- [node.parentNd] ]
  in
  rootIssues <> parentIssues


scanNodes :: Map Text Nd.Node -> Maybe Text -> Int32 -> Text -> WalkSt -> WalkSt
scanNodes mapping mbParent childIx eid st
  | St.member eid st.activeWs = addIssue (CycleOI eid) st
  | St.member eid st.seenWs = case mbParent of
      Just eidParent -> addIssue (DuplicateChildOI eidParent eid) st
      Nothing -> addIssue (CycleOI eid) st
  | otherwise = case Mp.lookup eid mapping of
      Nothing -> addIssue (MissingNodeOI eid) st
      Just node ->
        let
          curSeq = st.nextSeqWs
          ord = NodeOrd { eidNode = eid, eidParent = mbParent, seqNode = curSeq
                  , seqChild = childIx, seqPre = curSeq
                }
          st1 = st { nextSeqWs = curSeq + 1, ordsRevWs = ord : st.ordsRevWs
                  , seenWs = St.insert eid st.seenWs, activeWs = St.insert eid st.activeWs
                  -- , issuesWs = [DebugInfo ("eid: " <> eid <> ", children") (T.pack $ show node.childrenNd)] <> st.issuesWs
                }
          st2 = foldl' (\acc (ix, eidChild) -> scanNodes mapping (Just eid) ix eidChild acc) st1 (zip [0 ..] node.childrenNd)
        in
        st2 { activeWs = St.delete eid st2.activeWs }


addIssue :: OrdIssue -> WalkSt -> WalkSt
addIssue issue st = st { issuesWs = issue : st.issuesWs }


duplicateItems :: Ord a => [a] -> [a]
duplicateItems xs = reverse (go xs St.empty St.empty [])
  where
    go :: Ord a => [a] -> Set a -> Set a -> [a] -> [a]
    go [] _ _ acc = acc
    go (y : ys) seen dup acc
      | St.member y dup = go ys seen dup acc
      | St.member y seen = go ys seen (St.insert y dup) (y : acc)
      | otherwise = go ys (St.insert y seen) dup acc


renderOrdIssues :: Jv1.Conversation -> [OrdIssue] -> String
renderOrdIssues conversation issues =
  T.unpack $ "@[renderOrdIssues] node issues.\ntitle: " <> conversation.titleCv
      <> "\neid: " <> conversation.convIdCv <> " => \n" <> T.intercalate "\n" (map renderOrdIssue issues)


renderOrdIssue :: OrdIssue -> Text
renderOrdIssue issue = "- " <> case issue of
  MissingRootOI -> "missing root node"
  MissingNodeOI eidNode -> "missing node in mapping: " <> eidNode
  MissingParentOI nodeEid parentEid -> "missing parent, child: " <> nodeEid <> ", parent: " <> parentEid
  CycleOI eidNode -> "cycle detected at node: " <> eidNode
  DuplicateChildOI eidParent eidChild ->
    "duplicate child reference: parent = " <> eidParent <> ", child = " <> eidChild
  BranchOI eidParent eidsChild ->
    "branch detected under parent = " <> eidParent <> " children = [" <> T.intercalate ", " eidsChild <> "]"
  DebugInfo label text -> "dbg " <> label <> ": " <> text