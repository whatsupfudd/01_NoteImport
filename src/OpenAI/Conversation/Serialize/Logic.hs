{-# LANGUAGE DerivingStrategies #-}

module OpenAI.Conversation.Serialize.Logic where

import Data.Int (Int32, Int64)
import qualified Data.List as L
import qualified Data.Map.Strict as Mp
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Scientific (Scientific, toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Hasql.Pool as Hp
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as Txs

import OpenAI.Conversation.Json.Schema (Conversation (..), Message (..))
import qualified OpenAI.Conversation.Json.V1.Schema as Jv1
import qualified OpenAI.Conversation.Json.Node.Order as Oor
import qualified OpenAI.Conversation.Serialize.Content as Cw
import qualified OpenAI.Conversation.Serialize.ConversationStmt as St
import qualified OpenAI.Conversation.Json.Node as Nd


data ReportRawAdd = ReportRawAdd {
    uidConv :: Int64
    , nodeAddedCnt :: Int
    , msgAddedCnt :: Int
  }
  deriving stock (Eq, Show)


data StatRawAdd = StatRawAdd {
    nodeAddedSra :: Int
    , msgAddedSra :: Int
  }
  deriving stock (Eq, Show)


instance Semigroup StatRawAdd where
  left <> right =
    StatRawAdd {
        nodeAddedSra = left.nodeAddedSra + right.nodeAddedSra
        , msgAddedSra = left.msgAddedSra + right.msgAddedSra
      }


instance Monoid StatRawAdd where
  mempty =
    StatRawAdd {
        nodeAddedSra = 0
        , msgAddedSra = 0
      }


statNodeOne :: StatRawAdd
statNodeOne =
  StatRawAdd {
      nodeAddedSra = 1
      , msgAddedSra = 0
    }


statMsgOne :: StatRawAdd
statMsgOne =
  StatRawAdd {
      nodeAddedSra = 0
      , msgAddedSra = 1
    }


useTx :: Hp.Pool -> Tx.Transaction result -> IO (Either Hp.UsageError result)
useTx pool transaction =
  Hp.use pool $ Txs.transaction Txs.ReadCommitted Txs.Write transaction


addConversation :: Hp.Pool -> Jv1.Conversation -> IO (Either Hp.UsageError (Either String Int64))
addConversation pool conversation =
  fmap (fmap (fmap (.uidConv))) $ addConversationR pool conversation


addConversationR :: Hp.Pool -> Jv1.Conversation -> IO (Either Hp.UsageError (Either String ReportRawAdd))
addConversationR pool conversation = do
  putStrLn . T.unpack $ "@[addConversationR] node map: " 
    <> T.intercalate "\n, " (map (\(eid, node) -> "eid: " <> eid <> ", parentEid: " <> fromMaybe "<none>" node.parentNd) (Mp.toList conversation.nodeMapCv))

  case Oor.buildNodeOrd conversation.nodeMapCv of
    Left issues ->
      pure . Right . Left $ Oor.renderOrdIssues conversation issues
    Right nodeOrds ->
      let
        ordsAsc = sortNodeOrds nodeOrds
        ordByEid = Mp.fromList [(nodeOrd.eidNode, nodeOrd) | nodeOrd <- ordsAsc]
      in
      useTx pool $ do
        convUid <- addConversationRoot conversation
        statRez <- addOrderedNodesReportSession convUid conversation.nodeMapCv ordByEid ordsAsc
        case statRez of
          Left err -> do
            Tx.condemn
            pure . Left $ renderConversationErr conversation err
          Right stat ->
            pure . Right $ reportRawAdd convUid stat


reportRawAdd :: Int64 -> StatRawAdd -> ReportRawAdd
reportRawAdd convUid stat =
  ReportRawAdd {
      uidConv = convUid
      , nodeAddedCnt = stat.nodeAddedSra
      , msgAddedCnt = stat.msgAddedSra
    }


addConversationRoot :: Jv1.Conversation -> Tx.Transaction Int64
addConversationRoot conversation =
  Tx.statement
    ( conversation.titleCv
    , conversation.convIdCv
    , toRealFloat conversation.createTimeCv
    , toRealFloat conversation.updateTimeCv
    )
    St.insertConversation


addOrderedNodesSession :: Int64 -> Mp.Map Text Nd.Node -> Mp.Map Text Oor.NodeOrd -> [Oor.NodeOrd]
      -> Tx.Transaction (Either String ())
addOrderedNodesSession convUid mapping ordByEid ordsAsc =
  fmap (fmap (const ())) $ addOrderedNodesReportSession convUid mapping ordByEid ordsAsc


addOrderedNodesReportSession :: Int64 -> Mp.Map Text Nd.Node -> Mp.Map Text Oor.NodeOrd -> [Oor.NodeOrd]
      -> Tx.Transaction (Either String StatRawAdd)
addOrderedNodesReportSession convUid mapping ordByEid ordsAsc
  | Mp.size ordByEid /= length ordsAsc =
      pure . Left $ "@[addOrderedNodesReportSession] duplicate node order entries detected"
  | otherwise =
      iterNode Mp.empty mempty ordsAsc
  where
    iterNode :: Mp.Map Text Int64 -> StatRawAdd -> [Oor.NodeOrd]
          -> Tx.Transaction (Either String StatRawAdd)
    iterNode _ stat [] =
      pure $ Right stat
    iterNode uidByEid stat (nodeOrd : rest) =
      case Mp.lookup nodeOrd.eidNode mapping of
        Nothing ->
          pure . Left $ "@[addOrderedNodesReportSession] node missing in mapping: "
            <> T.unpack nodeOrd.eidNode
        Just node ->
          case parentUidFor uidByEid nodeOrd of
            Left err ->
              pure $ Left err
            Right parentUid -> do
              nodeRez <- addNodeR convUid parentUid nodeOrd.eidNode node
                nodeOrd.seqNode nodeOrd.seqChild nodeOrd.seqPre
              case nodeRez of
                Left err ->
                  pure $ Left err
                Right (nodeUid, nodeStat) ->
                  iterNode
                    (Mp.insert nodeOrd.eidNode nodeUid uidByEid)
                    (stat <> nodeStat)
                    rest

    parentUidFor :: Mp.Map Text Int64 -> Oor.NodeOrd -> Either String (Maybe Int64)
    parentUidFor uidByEid nodeOrd =
      case nodeOrd.eidParent of
        Nothing ->
          Right Nothing
        Just parentEid
          | not $ Mp.member parentEid ordByEid ->
              Left $ "@[addOrderedNodesReportSession] ordered node references unknown parent: child = "
                <> T.unpack nodeOrd.eidNode <> ", parent = " <> T.unpack parentEid
          | otherwise ->
              case Mp.lookup parentEid uidByEid of
                Nothing ->
                  Left $ "@[addOrderedNodesReportSession] parent not inserted before child: child = "
                    <> T.unpack nodeOrd.eidNode <> ", parent = " <> T.unpack parentEid
                Just parentUid ->
                  Right $ Just parentUid


sortNodeOrds :: [Oor.NodeOrd] -> [Oor.NodeOrd]
sortNodeOrds =
  L.sortOn $ \nodeOrd -> (nodeOrd.seqPre, nodeOrd.seqNode, nodeOrd.seqChild, nodeOrd.eidNode)


addNode :: Int64 -> Maybe Int64 -> Text -> Nd.Node -> Int32 -> Int32 -> Int32
      -> Tx.Transaction (Either String Int64)
addNode convUid parentUid eidNode node seqNode seqChild seqPre =
  fmap (fmap fst) $ addNodeR convUid parentUid eidNode node seqNode seqChild seqPre


addNodeR :: Int64 -> Maybe Int64 -> Text -> Nd.Node -> Int32 -> Int32 -> Int32
      -> Tx.Transaction (Either String (Int64, StatRawAdd))
addNodeR convUid parentUid eidNode node seqNode seqChild seqPre = do
  nodeUid <- Tx.statement
    (convUid, eidNode, parentUid, seqNode, seqChild, seqPre)
    St.insertNode

  case node.messageNd of
    Nothing ->
      pure . Right $ (nodeUid, statNodeOne)
    Just msg -> do
      msgRez <- Cw.insertMsgTree nodeUid msg
      case msgRez of
        Left issue ->
          pure . Left $ renderNodeIssue eidNode msg.idMsg issue
        Right _ ->
          pure . Right $ (nodeUid, statNodeOne <> statMsgOne)


renderIssueC :: Text -> String
renderIssueC = T.unpack


renderNodeIssue :: Text -> Text -> Text -> String
renderNodeIssue eidNode eidMsg issue =
  T.unpack $
    "@[addNodeR] message/content insertion failed, node eid: " <> eidNode
      <> ", message eid: " <> eidMsg
      <> ", error: " <> T.pack (renderIssueC issue)


renderConversationErr :: Jv1.Conversation -> String -> String
renderConversationErr conversation err =
  T.unpack $
    "@[addConversation] insert failed, title: " <> conversation.titleCv
      <> "\neid: " <> conversation.convIdCv
      <> ", error: " <> T.pack err