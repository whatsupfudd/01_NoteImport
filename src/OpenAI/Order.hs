{-# LANGUAGE DerivingStrategies #-}
module OpenAI.Order
  ( NodeOrd(..)
  , OrdIssue(..)
  , buildNodeOrd
  , rootEidNode
  , renderOrdIssues
  , renderOrdIssue
  ) where

import Data.Int (Int32)
import Data.List (foldl', nub)
import Data.Map.Strict (Map)
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Mp
import qualified Data.Set as St
import qualified Data.Text as Tx

import qualified OpenAI.Json.Reader as Jd


data NodeOrd = NodeOrd
  { eidNode :: Text
  , eidParent :: Maybe Text
  , seqNode :: Int32
  , seqChild :: Int32
  , seqPre :: Int32
  }
  deriving stock (Eq, Show)

data OrdIssue = MissingRootOI
  | MissingNodeOI Text
  | MissingParentOI Text Text
  | CycleOI Text
  | DuplicateChildOI Text Text
  | BranchOI Text [Text]
  deriving stock (Eq, Show)

rootEidNode :: Text
rootEidNode = Tx.pack "client-created-root"

data WalkSt = WalkSt
  { nextSeqWs :: Int32
  , ordsRevWs :: [NodeOrd]
  , seenWs :: Set Text
  , activeWs :: Set Text
  , issuesWs :: [OrdIssue]
  }

buildNodeOrd :: Map Text Jd.Node -> Either [OrdIssue] [NodeOrd]
buildNodeOrd mapping = do
  eidRoot <- selectRoot mapping
  let issuesBase = validateMapping mapping
      issuesBranch = branchIssues mapping
      walk0 = WalkSt { nextSeqWs = 0, ordsRevWs = [], seenWs = St.empty, activeWs = St.empty, issuesWs = [] }
      walk1 = walk mapping Nothing 0 eidRoot walk0
      issuesRest = disconnectedIssues mapping walk1.seenWs
      issuesAll = nub (issuesBase <> walk1.issuesWs <> issuesRest)

  if null issuesAll
    then Right (reverse walk1.ordsRevWs)
    else Left (nub (issuesAll <> issuesBranch))

selectRoot :: Map Text Jd.Node -> Either [OrdIssue] Text
selectRoot mapping
  | Mp.member rootEidNode mapping = Right rootEidNode
  | otherwise =
      case [eid | (eid, node) <- Mp.toAscList mapping, isNothing (parentOf node)] of
        eid : _ -> Right eid
        [] -> Left [MissingRootOI]

validateMapping :: Map Text Jd.Node -> [OrdIssue]
validateMapping mapping =
  concatMap validateOne (Mp.toAscList mapping)
  where
    validateOne :: (Text, Jd.Node) -> [OrdIssue]
    validateOne (eid, node) =
      missingParent eid node <> missingChildren node <> dupChildren eid node

    missingParent :: Text -> Jd.Node -> [OrdIssue]
    missingParent eid node =
      case parentOf node of
        Just eidParent | Mp.notMember eidParent mapping -> [MissingParentOI eid eidParent]
        _ -> []

    missingChildren :: Jd.Node -> [OrdIssue]
    missingChildren node =
      [ MissingNodeOI eidChild
      | eidChild <- childrenOf node
      , Mp.notMember eidChild mapping
      ]

    dupChildren :: Text -> Jd.Node -> [OrdIssue]
    dupChildren eid node =
      [ DuplicateChildOI eid eidChild
      | eidChild <- duplicateItems (childrenOf node)
      ]

branchIssues :: Map Text Jd.Node -> [OrdIssue]
branchIssues mapping =
  [ BranchOI eid eidsChild
  | (eid, node) <- Mp.toAscList mapping
  , let eidsChild = childrenOf node
  , length eidsChild > 1
  ]

disconnectedIssues :: Map Text Jd.Node -> Set Text -> [OrdIssue]
disconnectedIssues mapping seen =
  let rest = [(eid, node) | (eid, node) <- Mp.toAscList mapping, St.notMember eid seen]
      hasExtraRoot = any (isNothing . parentOf . snd) rest
      rootIssues = [MissingRootOI | hasExtraRoot]
      parentIssues =
        [ MissingParentOI eid eidParent
        | (eid, node) <- rest
        , Just eidParent <- [parentOf node]
        ]
  in rootIssues <> parentIssues

walk :: Map Text Jd.Node -> Maybe Text -> Int32 -> Text -> WalkSt -> WalkSt
walk mapping mbParent childIx eid st
  | St.member eid st.activeWs = addIssue (CycleOI eid) st
  | St.member eid st.seenWs =
      case mbParent of
        Just eidParent -> addIssue (DuplicateChildOI eidParent eid) st
        Nothing -> addIssue (CycleOI eid) st
  | otherwise =
      case Mp.lookup eid mapping of
        Nothing -> addIssue (MissingNodeOI eid) st
        Just node ->
          let seqPre' = st.nextSeqWs
              ord =
                NodeOrd
                  { eidNode = eid
                  , eidParent = mbParent
                  , seqNode = seqPre'
                  , seqChild = childIx
                  , seqPre = seqPre'
                  }
              st1 =
                st
                  { nextSeqWs = seqPre' + 1
                  , ordsRevWs = ord : st.ordsRevWs
                  , seenWs = St.insert eid st.seenWs
                  , activeWs = St.insert eid st.activeWs
                  }
              st2 =
                foldl'
                  (\acc (ix, eidChild) -> walk mapping (Just eid) ix eidChild acc)
                  st1
                  (zip [0 ..] (childrenOf node))
          in st2 { activeWs = St.delete eid st2.activeWs }

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

parentOf :: Jd.Node -> Maybe Text
parentOf node = node.parentNd

childrenOf :: Jd.Node -> [Text]
childrenOf node = node.childrenNd


renderOrdIssues :: Jd.Conversation -> [OrdIssue] -> String
renderOrdIssues conversation issues =
  T.unpack . T.unlines $
    [ "@[renderOrdIssues] invalid node ordering"
    , "title: " <> conversation.titleCv
    , "eid: " <> conversation.convIdCv
    ]
      <> map (("- " <>) . renderOrdIssue) issues


renderOrdIssue :: OrdIssue -> Text
renderOrdIssue issue =
  case issue of
    MissingRootOI -> "missing root node"
    MissingNodeOI eidNode -> "missing node in mapping: " <> eidNode
    MissingParentOI eidNode eidParent -> "missing parent: child = " <> eidNode <> ", parent=" <> eidParent
    CycleOI eidNode -> "cycle detected at node: " <> eidNode
    DuplicateChildOI eidParent eidChild ->
      "duplicate child reference: parent = " <> eidParent <> ", child = " <> eidChild
    BranchOI eidParent eidsChild ->
      "branch detected under parent = " <> eidParent <> " children = [" <> T.intercalate ", " eidsChild <> "]"