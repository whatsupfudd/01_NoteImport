module OpenAI.Conversation.Json.Node.Build
  ( IssueNB(..)
  , buildNodeMapCv
  , buildNodeMapCvWith
  , buildNodeMapMsg
  , buildNodeMapMsgWith
  , renderIssuesNB
  ) where

import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Mp
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Aeson as Ae

import qualified OpenAI.Conversation.Json.MsgSchema as Jm
import qualified OpenAI.Conversation.Json.Node as Nd
import qualified OpenAI.Conversation.Json.Node.Order as Oor
import qualified OpenAI.Conversation.Json.Schema as J
import OpenAI.Conversation.Json.Types (VersionJson(..))


data IssueNB
  = InvalidMessageEidNB Text
  | ConflictingCopiesNB Text
  | InvalidParentValueNB Text Ae.Value
  | UnknownParentNB Jm.Message Text
  | ConflictingParentNB Text
  | InvalidParentEidNB Text Text
  | MissingParentNB Jm.Message Text
  | SelfParentNB Text
  | InvalidOrderNB Oor.OrdIssue
  deriving (Eq, Show)


-- | Finalize only after every available page has been collected.
buildNodeMapCv :: J.Conversation -> Either [IssueNB] J.Conversation
buildNodeMapCv = buildNodeMapCvWith Mp.empty


-- | The additional map contains independently verified parent links.
-- A present key with Nothing explicitly identifies a root message.
-- Such links may fill absent metadata but cannot contradict it.
buildNodeMapCvWith :: Mp.Map Text (Maybe Text) -> J.Conversation -> Either [IssueNB] J.Conversation
buildNodeMapCvWith parentsKnown conversation =
  case conversation.versionJsonCv of
    V1vj -> Right conversation
    V2vj -> do
      (mapping, orders) <- assembleNodes parentsKnown conversation.messagesCv
      let
        messageFor :: Oor.NodeOrd -> Maybe Jm.Message
        messageFor order = Mp.lookup order.eidNode mapping >>= (.messageNd)
      pure $ conversation {
        J.nodeMapCv = mapping
      , J.messagesCv = mapMaybe messageFor orders
      }


buildNodeMapMsg :: [Jm.Message] -> Either [IssueNB] (Mp.Map Text Nd.Node)
buildNodeMapMsg = buildNodeMapMsgWith Mp.empty


buildNodeMapMsgWith :: Mp.Map Text (Maybe Text) -> [Jm.Message] -> Either [IssueNB] (Mp.Map Text Nd.Node)
buildNodeMapMsgWith parentsKnown messages =
  fst <$> assembleNodes parentsKnown messages


assembleNodes :: Mp.Map Text (Maybe Text) -> [Jm.Message] -> Either [IssueNB] (Mp.Map Text Nd.Node, [Oor.NodeOrd])
assembleNodes parentsKnown messages = do
  indexed <- indexMessages messages
  nodes <- collectE $ map (makeNode parentsKnown indexed) (Mp.elems indexed)
  let
    initial = Mp.insert Oor.specialRootEid rootNode $ Mp.fromList [(node.idNd, node) | node <- nodes]
    linked = Nd.buildChildrenNd initial
    childKey eid = (Jm.createTimeMsg <$> Mp.lookup eid indexed, eid)
    orderChildren :: Nd.Node -> Nd.Node
    orderChildren node = node { Nd.childrenNd = sortOn childKey node.childrenNd }
    mapping = Mp.map orderChildren linked
  orders <- first (map InvalidOrderNB) $ Oor.buildNodeOrd mapping
  pure (mapping, orders)


-- | Exact overlap is harmless. Conflicting snapshots require reconciliation.
-- The choice of input page order never selects a winner.
indexMessages :: [Jm.Message] -> Either [IssueNB] (Mp.Map Text Jm.Message)
indexMessages messages =
  Mp.fromList <$> collectE (map selectCopy $ Mp.toAscList grouped)
  where
  grouped = Mp.fromListWith (<>) [(message.idMsg, message :| []) | message <- messages]

  selectCopy (eid, message :| rest)
    | T.null (T.strip eid) || eid == Oor.specialRootEid = Left [InvalidMessageEidNB eid]
    | all (== message) rest = Right (eid, message)
    | otherwise = Left [ConflictingCopiesNB eid]


makeNode :: Mp.Map Text (Maybe Text) -> Mp.Map Text Jm.Message -> Jm.Message -> Either [IssueNB] Nd.Node
makeNode parentsKnown indexed message = do
  source <- readParent message
  chosen <- chooseParent message message.idMsg source $ Mp.lookup message.idMsg parentsKnown

  let
    eid = message.idMsg
    eidParent = fromMaybe Oor.specialRootEid chosen

  if T.null (T.strip eidParent) then
    Left [InvalidParentEidNB eid eidParent]
  else if eid == eidParent then
    Left [SelfParentNB eid]
  else if eidParent /= Oor.specialRootEid && Mp.notMember eidParent indexed then
    Left [MissingParentNB message eidParent]
  else
    Right $ Nd.Node {
      Nd.idNd = eid
    , Nd.messageNd = Just message
    , Nd.parentNd = Just eidParent
    , Nd.childrenNd = []
    }


-- Outer Nothing: field absent; Just Nothing: explicit JSON null.
readParent :: Jm.Message -> Either [IssueNB] (Maybe (Maybe Text))
readParent message =
  case Mp.lookup "parent_id" message.metadataMsg of
    Nothing -> Right Nothing
    Just Ae.Null -> Right $ Just Nothing
    Just (Ae.String eid) | not $ T.null (T.strip eid) -> Right $ Just $ Just eid
    Just value -> Left [InvalidParentValueNB message.idMsg value]


chooseParent :: Jm.Message -> Text -> Maybe (Maybe Text) -> Maybe (Maybe Text) -> Either [IssueNB] (Maybe Text)
chooseParent message eid source supplied =
  case (source, supplied) of
    (Nothing, Nothing) -> Left [UnknownParentNB message eid]
    (Just parent, Nothing) -> Right parent
    (Nothing, Just parent) -> Right parent
    (Just parent, Just other)
      | defToRoot parent == defToRoot other -> Right parent
      | otherwise -> Left [ConflictingParentNB eid]
  where
  defToRoot = fromMaybe Oor.specialRootEid


rootNode :: Nd.Node
rootNode =
  Nd.Node {
    Nd.idNd = Oor.specialRootEid
  , Nd.messageNd = Nothing
  , Nd.parentNd = Nothing
  , Nd.childrenNd = []
  }


collectE :: [Either [IssueNB] a] -> Either [IssueNB] [a]
collectE results =
  case partitionEithers results of
    ([], values) -> Right values
    (issues, _) -> Left $ concat issues


renderIssuesNB :: [IssueNB] -> String
renderIssuesNB = unlines . map show