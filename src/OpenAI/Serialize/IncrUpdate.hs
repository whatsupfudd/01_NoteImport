{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}

module OpenAI.Serialize.IncrUpdate
  ( ReportRaw(..)
  , updateConversation
  ) where

import Control.Monad (forM_, when)
import Data.Aeson (Value)
import qualified Data.Aeson as Ae
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import Data.Int (Int32, Int64)
import Data.List (sortOn)
import qualified Data.Map.Strict as Mp
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Crypto.Hash as CH
import qualified Hasql.Pool as Hp
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as TxS

import qualified OpenAI.Deserialize.ConversationStmt as Dcv
import qualified OpenAI.Json.Reader as Jd
import qualified OpenAI.Order as Oor
import qualified OpenAI.Serialize.ConversationStmt as Scv

data ReportRaw = ReportRaw
  { uidConv :: Int64
  , nodeAddedCnt :: Int
  , msgAddedCnt :: Int
  , titleChanged :: Bool
  , sameRaw :: Bool
  , notesRaw :: [Text]
  }
  deriving stock (Eq, Show)

data AddStat = AddStat
  { nodeAddedCntAS :: Int
  , msgAddedCntAS :: Int
  }
  deriving stock (Eq, Show)

emptyAddStat :: AddStat
emptyAddStat = AddStat { nodeAddedCntAS = 0, msgAddedCntAS = 0 }

appendAddStat :: AddStat -> AddStat -> AddStat
appendAddStat left right =
  AddStat
    { nodeAddedCntAS = left.nodeAddedCntAS + right.nodeAddedCntAS
    , msgAddedCntAS = left.msgAddedCntAS + right.msgAddedCntAS
    }

type NodeIdT = Text

updateConversation :: Hp.Pool -> Jd.Conversation -> IO (Either Hp.UsageError (Either String ReportRaw))
updateConversation pool conv =
  Hp.use pool $
    TxS.transaction TxS.ReadCommitted TxS.Write $
      updateTx conv

updateTx :: Jd.Conversation -> Tx.Transaction (Either String ReportRaw)
updateTx conv = do
  mbConv <- Tx.statement conv.convIdCv Dcv.selectConversationForUpdate
  case mbConv of
    Nothing ->
      pure $
        Left $
          "@[OpenAI.Serialize.IncrUpdate.updateConversation] conversation not found: "
            <> T.unpack conv.convIdCv

    Just (uidConv, titleDb, timeUpdateDb) -> do
      case Oor.buildNodeOrd conv.mappingCv of
        Left issues ->
          pure $
            Left $
              "@[OpenAI.Serialize.IncrUpdate.updateConversation] invalid node order: "
                <> renderOrdIssues issues

        Right nodeOrds -> do
          maxSeq <- Tx.statement uidConv Dcv.selectMaxNodeSeq
          if maxSeq < 0
            then do
              Tx.condemn
              pure $
                Left $
                  "@[OpenAI.Serialize.IncrUpdate.updateConversation] existing conversation has no stored nodes: "
                    <> show uidConv
            else do
              let orderedNodeIds = orderedNodeEids nodeOrds
              let startIdx = fromIntegral maxSeq + 1
              let nodeCountJs = length orderedNodeIds
              if startIdx > nodeCountJs
                then do
                  Tx.condemn
                  pure $
                    Left $
                      "@[OpenAI.Serialize.IncrUpdate.updateConversation] DB node sequence exceeds JSON node order; append-only updater cannot reconcile"
                else do
                  let newTail = drop startIdx orderedNodeIds
                  let titleChanged = conv.titleCv /= titleDb
                  let updateChanged = conv.updateTimeCv /= timeUpdateDb
                  let sameRaw = not titleChanged && not updateChanged && null newTail

                  if sameRaw
                    then
                      pure $
                        Right $
                          ReportRaw
                            { uidConv = uidConv
                            , nodeAddedCnt = 0
                            , msgAddedCnt = 0
                            , titleChanged = False
                            , sameRaw = True
                            , notesRaw = ["no raw changes detected"]
                            }
                    else do
                      when (titleChanged || updateChanged) $ do
                        Tx.statement (uidConv, timeUpdateDb, titleDb) Scv.insertConversationPrevious
                        Tx.statement (conv.titleCv, conv.updateTimeCv, uidConv) Scv.updateConversation

                      addStatE <-
                        if null newTail
                          then pure (Right emptyAddStat)
                          else do
                            let ordByEid = Mp.fromList [(nodeOrd.eidNode, nodeOrd) | nodeOrd <- nodeOrds]
                            insertTail uidConv (fromIntegral startIdx) conv.mappingCv ordByEid newTail

                      case addStatE of
                        Left err -> do
                          Tx.condemn
                          pure (Left err)

                        Right addStat -> do
                          let hashConv = sha256 (Ae.encode conv)
                          Tx.statement
                            (uidConv, Just "phase2-incrupdate" :: Maybe Text, Just hashConv, "incremental-append" :: Text)
                            Scv.insertConversationIngest

                          pure $
                            Right $
                              ReportRaw
                                { uidConv = uidConv
                                , nodeAddedCnt = addStat.nodeAddedCntAS
                                , msgAddedCnt = addStat.msgAddedCntAS
                                , titleChanged = titleChanged
                                , sameRaw = False
                                , notesRaw = buildNotes titleChanged updateChanged addStat
                                }

insertTail
  :: Int64
  -> Int32
  -> Mp.Map NodeIdT Jd.Node
  -> Mp.Map NodeIdT Oor.NodeOrd
  -> [NodeIdT]
  -> Tx.Transaction (Either String AddStat)
insertTail conversationUid startSeq mapping ordByEid newNodeIds =
  go Mp.empty startSeq newNodeIds
  where
    go :: Mp.Map NodeIdT Int64 -> Int32 -> [NodeIdT] -> Tx.Transaction (Either String AddStat)
    go _ _ [] = pure (Right emptyAddStat)
    go uidByEid seqNbr (eidNode : rest) =
      case Mp.lookup eidNode mapping of
        Nothing ->
          pure $
            Left $
              "@[OpenAI.Serialize.IncrUpdate.insertTail] node not found in mapping: "
                <> T.unpack eidNode

        Just node ->
          case Mp.lookup eidNode ordByEid of
            Nothing ->
              pure $
                Left $
                  "@[OpenAI.Serialize.IncrUpdate.insertTail] node order missing for node: "
                    <> T.unpack eidNode

            Just nodeOrd -> do
              parentFkE <- resolveParentUid conversationUid uidByEid nodeOrd.eidParent
              case parentFkE of
                Left err ->
                  pure (Left err)

                Right parentFk -> do
                  uidNode <-
                    Tx.statement
                      (conversationUid, node.idNd, parentFk, seqNbr, nodeOrd.seqChild, nodeOrd.seqPre)
                      Scv.insertNode

                  msgCntE <-
                    case node.messageNd of
                      Nothing -> pure (Right 0)
                      Just msg -> insertMessageTree uidNode msg

                  case msgCntE of
                    Left err ->
                      pure (Left err)

                    Right msgCnt -> do
                      restE <- go (Mp.insert eidNode uidNode uidByEid) (seqNbr + 1) rest
                      pure $
                        fmap
                          (\restStat -> appendAddStat (AddStat 1 msgCnt) restStat)
                          restE

resolveParentUid
  :: Int64
  -> Mp.Map NodeIdT Int64
  -> Maybe NodeIdT
  -> Tx.Transaction (Either String (Maybe Int64))
resolveParentUid _ _ Nothing =
  pure (Right Nothing)
resolveParentUid conversationUid uidByEid (Just eidParent) =
  case Mp.lookup eidParent uidByEid of
    Just uidParent ->
      pure (Right (Just uidParent))

    Nothing -> do
      mbParent <- Tx.statement (conversationUid, eidParent) Dcv.selectNodeByEid
      case mbParent of
        Nothing ->
          pure $
            Left $
              "@[OpenAI.Serialize.IncrUpdate.resolveParentUid] parent node not found in DB: "
                <> T.unpack eidParent

        Just (uidParent, _, _, _) ->
          pure (Right (Just uidParent))

insertMessageTree :: Int64 -> Jd.Message -> Tx.Transaction (Either String Int)
insertMessageTree uidNode msg = do
  uidMsg <-
    Tx.statement
      ( uidNode
      , msg.idMsg
      , msg.createTimeMsg
      , msg.updateTimeMsg
      , msg.statusMsg
      , msg.endTurnMsg
      , msg.weightMsg
      , mapJson msg.metadataMsg
      , msg.recipientMsg
      , msg.channelMsg
      , 0 :: Int32
      )
      Scv.insertMessageRetUid

  let author = msg.authorMsg
  Tx.statement
    (uidMsg, author.roleAu, author.nameAu, mapJson author.metadataAu)
    Scv.insertAuthor

  contentE <- insertContent uidMsg 0 msg.contentMsg
  pure (fmap (const 1) contentE)

insertContent :: Int64 -> Int32 -> Jd.Content -> Tx.Transaction (Either String ())
insertContent uidMsg seqContent content = do
  let (kindC, payload) = contentTypeAndPayload content
  uidContent <- Tx.statement (uidMsg, kindC, seqContent) Scv.insertContentRetUid

  case payload of
    CodePL langCode formatRef textCode -> do
      Tx.statement (uidContent, langCode, formatRef, textCode) Scv.insertCodeContent
      pure (Right ())

    ExecOutPL textOut -> do
      Tx.statement (uidContent, textOut) Scv.insertExecutionOutputContent
      pure (Right ())

    ModelCtxPL modelSlug repoJson rsJson scJson -> do
      Tx.statement (uidContent, modelSlug, repoJson, rsJson, scJson) Scv.insertModelEditableContextContent
      pure (Right ())

    ReasoningPL textReasoning -> do
      Tx.statement (uidContent, textReasoning) Scv.insertReasoningRecapContent
      pure (Right ())

    SystemErrPL nameErr textErr -> do
      Tx.statement (uidContent, nameErr, textErr) Scv.insertSystemErrorContent
      pure (Right ())

    TetherBrowsePL resultsJson summaryJson assetsJson tetherId -> do
      Tx.statement (uidContent, resultsJson, summaryJson, assetsJson, tetherId) Scv.insertTetherBrowsingDisplayContent
      pure (Right ())

    TetherQuotePL urlQuote domainQuote textQuote titleQuote tetherId -> do
      Tx.statement (uidContent, urlQuote, domainQuote, textQuote, titleQuote, tetherId) Scv.insertTetherQuoteContent
      pure (Right ())

    TextPL partsText -> do
      Tx.statement (uidContent, partsText) Scv.insertTextContent
      pure (Right ())

    ThoughtsPL sourceId thoughts -> do
      Tx.statement (uidContent, sourceId) Scv.insertThoughtsContent
      forM_ (zip [0 :: Int32 ..] (V.toList thoughts)) $ \(seqThought, thought) ->
        Tx.statement
          ( uidContent
          , thought.summaryTh
          , thought.contentTh
          , Ae.toJSON thought.chunksTh
          , fromMaybe False thought.finishedTh
          , seqThought
          )
          Scv.insertThought
      pure (Right ())

    UnknownPL rawJson -> do
      Tx.statement (uidContent, rawJson) Scv.insertUnknownContent
      pure (Right ())

data Payload
  = CodePL !Text !(Maybe Text) !Text
  | ExecOutPL !Text
  | ModelCtxPL !Text !(Maybe Value) !(Maybe Value) !(Maybe Value)
  | ReasoningPL !Text
  | SystemErrPL !Text !Text
  | TetherBrowsePL !Text !(Maybe Value) !(Maybe Value) !(Maybe Text)
  | TetherQuotePL !Text !Text !Text !Text !(Maybe Text)
  | TextPL !(V.Vector Text)
  | ThoughtsPL !Text !(V.Vector Jd.Thought)
  | UnknownPL !Value

contentTypeAndPayload :: Jd.Content -> (Text, Payload)
contentTypeAndPayload = \case
  Jd.CodeCT langCode formatRef textCode ->
    ("code", CodePL langCode formatRef textCode)

  Jd.ExecutionOutputCT textOut ->
    ("execution_output", ExecOutPL textOut)

  Jd.ModelEditableContextCT modelSlug repoJson rsJson scJson ->
    ("model_editable_context", ModelCtxPL modelSlug repoJson rsJson scJson)

  Jd.ReasoningRecapCT textReasoning ->
    ("reasoning_recap", ReasoningPL textReasoning)

  Jd.SystemErrorCT nameErr textErr ->
    ("system_error", SystemErrPL nameErr textErr)

  Jd.TetherBrowsingDisplayCT resultsJson summaryJson assetsJson tetherId ->
    ( "tether_browsing_display"
    , TetherBrowsePL resultsJson (Ae.toJSON <$> summaryJson) (Ae.toJSON <$> assetsJson) tetherId
    )

  Jd.TetherQuoteCT urlQuote domainQuote textQuote titleQuote tetherId ->
    ("tether_quote", TetherQuotePL urlQuote domainQuote textQuote titleQuote tetherId)

  Jd.TextCT partsText ->
    ("text", TextPL (V.fromList partsText))

  Jd.ThoughtsCT thoughts sourceId ->
    ("thoughts", ThoughtsPL sourceId (V.fromList thoughts))

  Jd.MultimodalTextCT parts ->
    ("multimodal_text", UnknownPL (Ae.toJSON parts))

  Jd.OtherCT kindOther rawOther ->
    (kindOther, UnknownPL (mapJson rawOther))

  other ->
    ("unknown", UnknownPL (Ae.toJSON other))

orderedNodeEids :: [Oor.NodeOrd] -> [NodeIdT]
orderedNodeEids nodeOrds =
  map (.eidNode) $
    sortOn
      (\nodeOrd -> (nodeOrd.seqPre, nodeOrd.seqNode, nodeOrd.seqChild, nodeOrd.eidNode))
      nodeOrds

buildNotes :: Bool -> Bool -> AddStat -> [Text]
buildNotes titleChanged updateChanged addStat =
  catMaybes
    [ if titleChanged then Just "title updated" else Nothing
    , if updateChanged then Just "update_time updated" else Nothing
    , if addStat.nodeAddedCntAS > 0
        then Just ("tail nodes inserted: " <> tshow addStat.nodeAddedCntAS)
        else Nothing
    , if addStat.msgAddedCntAS > 0
        then Just ("message trees inserted: " <> tshow addStat.msgAddedCntAS)
        else Nothing
    , if addStat.nodeAddedCntAS > 0
        then Just "append-only raw updater applied tail insertion"
        else Nothing
    ]

renderOrdIssues :: [Oor.OrdIssue] -> String
renderOrdIssues issues =
  T.unpack $
    T.intercalate "; " $
      map (T.pack . show) issues

mapJson :: Mp.Map Text Value -> Value
mapJson mp =
  Ae.toJSON (HM.fromList (Mp.toList mp))

tshow :: Show a => a -> Text
tshow =
  T.pack . show

sha256 :: BL.ByteString -> ByteString
sha256 bytesLazy =
  BA.convert (CH.hashlazy bytesLazy :: CH.Digest CH.SHA256)