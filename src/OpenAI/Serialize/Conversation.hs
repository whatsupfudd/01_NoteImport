module OpenAI.Serialize.Conversation where

import Control.Monad (forM_)
import Data.Int (Int32, Int64)
import qualified Data.Aeson as Ae
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Map.Strict as Mp
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Hasql.Pool as Hp
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as Txs
import OpenAI.Json.Reader
import qualified OpenAI.Order as Oor
import qualified OpenAI.Serialize.ConversationStmt as St

data ReportRawAdd = ReportRawAdd
  { uidConv :: Int64
  , nodeAddedCnt :: Int
  , msgAddedCnt :: Int
  }
  deriving (Eq, Show)

data StatRawAdd = StatRawAdd
  { nodeAddedSra :: Int
  , msgAddedSra :: Int
  }
  deriving (Eq, Show)

instance Semigroup StatRawAdd where
  a <> b =
    StatRawAdd
      { nodeAddedSra = a.nodeAddedSra + b.nodeAddedSra
      , msgAddedSra = a.msgAddedSra + b.msgAddedSra
      }

instance Monoid StatRawAdd where
  mempty = StatRawAdd {nodeAddedSra = 0, msgAddedSra = 0}

statNodeOne :: StatRawAdd
statNodeOne = StatRawAdd {nodeAddedSra = 1, msgAddedSra = 0}

statMsgOne :: StatRawAdd
statMsgOne = StatRawAdd {nodeAddedSra = 0, msgAddedSra = 1}

useTx :: Hp.Pool -> Tx.Transaction a -> IO (Either Hp.UsageError a)
useTx pool tx = Hp.use pool (Txs.transaction Txs.ReadCommitted Txs.Write tx)

addConversation :: Hp.Pool -> Conversation -> IO (Either Hp.UsageError (Either String Int64))
addConversation pool conversation =
  fmap (fmap (fmap uidConv)) (addConversationR pool conversation)

addConversationR :: Hp.Pool -> Conversation -> IO (Either Hp.UsageError (Either String ReportRawAdd))
addConversationR pool conversation =
  case Oor.buildNodeOrd conversation.mappingCv of
    Left issues ->
      pure . Right . Left $ Oor.renderOrdIssues conversation issues
    Right ords -> do
      let ordsAsc = sortNodeOrds ords
          ordByEid = Mp.fromList [(nodeOrd.eidNode, nodeOrd) | nodeOrd <- ordsAsc]
      useTx pool $ do
        convUid <- addConversationRoot conversation
        eiStat <- addOrderedNodesReportSession convUid conversation.mappingCv ordByEid ordsAsc
        case eiStat of
          Left err -> do
            Tx.condemn
            pure . Left $ renderConversationErr conversation err
          Right stat ->
            pure . Right $ reportRawAdd convUid stat

reportRawAdd :: Int64 -> StatRawAdd -> ReportRawAdd
reportRawAdd convUid stat =
  ReportRawAdd
    { uidConv = convUid
    , nodeAddedCnt = stat.nodeAddedSra
    , msgAddedCnt = stat.msgAddedSra
    }

addConversationRoot :: Conversation -> Tx.Transaction Int64
addConversationRoot conversation =
  Tx.statement
    ( conversation.titleCv
    , conversation.convIdCv
    , conversation.createTimeCv
    , conversation.updateTimeCv
    )
    St.insertConversation

addOrderedNodesSession
  :: Int64
  -> Mp.Map Text Node
  -> Mp.Map Text Oor.NodeOrd
  -> [Oor.NodeOrd]
  -> Tx.Transaction (Either String ())
addOrderedNodesSession convUid mapping ordByEid ordsAsc =
  fmap (fmap (const ())) (addOrderedNodesReportSession convUid mapping ordByEid ordsAsc)

addOrderedNodesReportSession
  :: Int64
  -> Mp.Map Text Node
  -> Mp.Map Text Oor.NodeOrd
  -> [Oor.NodeOrd]
  -> Tx.Transaction (Either String StatRawAdd)
addOrderedNodesReportSession convUid mapping ordByEid ordsAsc
  | Mp.size ordByEid /= length ordsAsc =
      pure . Left $ "@[addOrderedNodesReportSession] duplicate node order entries detected"
  | otherwise =
      iterNode Mp.empty mempty ordsAsc
  where
    iterNode :: Mp.Map Text Int64 -> StatRawAdd -> [Oor.NodeOrd] -> Tx.Transaction (Either String StatRawAdd)
    iterNode _ acc [] = pure (Right acc)
    iterNode uidByEid acc (nodeOrd : rest) =
      case Mp.lookup nodeOrd.eidNode mapping of
        Nothing ->
          pure . Left $
            "@[addOrderedNodesReportSession] node missing in mapping: " <> T.unpack nodeOrd.eidNode
        Just node ->
          case parentUidFor uidByEid nodeOrd of
            Left err ->
              pure (Left err)
            Right parentUid -> do
              (nodeUid, statNode) <-
                addNodeR convUid parentUid nodeOrd.eidNode node nodeOrd.seqNode nodeOrd.seqChild nodeOrd.seqPre
              iterNode (Mp.insert nodeOrd.eidNode nodeUid uidByEid) (acc <> statNode) rest

    parentUidFor :: Mp.Map Text Int64 -> Oor.NodeOrd -> Either String (Maybe Int64)
    parentUidFor uidByEid nodeOrd =
      case nodeOrd.eidParent of
        Nothing ->
          Right Nothing
        Just parentEid ->
          if not (Mp.member parentEid ordByEid) then
            Left $
              "@[addOrderedNodesReportSession] ordered node references unknown parent: child = "
                <> T.unpack nodeOrd.eidNode
                <> ", parent = "
                <> T.unpack parentEid
          else
            case Mp.lookup parentEid uidByEid of
              Nothing ->
                Left $
                  "@[addOrderedNodesReportSession] parent not inserted before child: child = "
                    <> T.unpack nodeOrd.eidNode
                    <> ", parent = "
                    <> T.unpack parentEid
              Just parentUid ->
                Right (Just parentUid)

sortNodeOrds :: [Oor.NodeOrd] -> [Oor.NodeOrd]
sortNodeOrds =
  L.sortOn (\nodeOrd -> (nodeOrd.seqPre, nodeOrd.seqNode, nodeOrd.seqChild, nodeOrd.eidNode))

addNode :: Int64 -> Maybe Int64 -> Text -> Node -> Int32 -> Int32 -> Int32 -> Tx.Transaction Int64
addNode convUid parentUid eidNode node seqNode seqChild seqPre =
  fmap fst (addNodeR convUid parentUid eidNode node seqNode seqChild seqPre)

addNodeR
  :: Int64
  -> Maybe Int64
  -> Text
  -> Node
  -> Int32
  -> Int32
  -> Int32
  -> Tx.Transaction (Int64, StatRawAdd)
addNodeR convUid parentUid eidNode node seqNode seqChild seqPre = do
  nodeUid <- Tx.statement (convUid, eidNode, parentUid, seqNode, seqChild, seqPre) St.insertNodeStmt
  msgStat <-
    case node.messageNd of
      Nothing ->
        pure mempty
      Just msg ->
        addMessageR nodeUid msg 0
  pure (nodeUid, statNodeOne <> msgStat)

addMessage :: Int64 -> Message -> Int32 -> Tx.Transaction ()
addMessage nodeUid msg seqMsg = do
  _ <- addMessageR nodeUid msg seqMsg
  pure ()

addMessageR :: Int64 -> Message -> Int32 -> Tx.Transaction StatRawAdd
addMessageR nodeUid msg seqMsg = do
  msgUid <-
    Tx.statement
      ( nodeUid
      , msg.idMsg
      , msg.createTimeMsg
      , msg.updateTimeMsg
      , msg.statusMsg
      , msg.endTurnMsg
      , msg.weightMsg
      , jsonMetaMap msg.metadataMsg
      , msg.recipientMsg
      , msg.channelMsg
      , seqMsg
      )
      St.insertMessageStmt
  addAuthor msgUid msg.authorMsg
  addContent msgUid msg.contentMsg 0
  pure statMsgOne

addAuthor :: Int64 -> Author -> Tx.Transaction ()
addAuthor msgUid author =
  Tx.statement
    (msgUid, author.roleAu, author.nameAu, jsonMetaMap author.metadataAu)
    St.insertAuthorStmt

addContent :: Int64 -> Content -> Int32 -> Tx.Transaction ()
addContent msgUid content seqContent = do
  contentUid <- Tx.statement (msgUid, contentType content, seqContent) St.insertContentStmt
  case content of
    CodeCT lang formatRef textCode ->
      addCodeContentSession contentUid lang formatRef textCode
    ExecutionOutputCT textOut ->
      addExecutionOutputContentSession contentUid textOut
    ModelEditableContextCT modelSlug repoJson rsJson scJson ->
      addModelEditableContextSession contentUid modelSlug repoJson rsJson scJson
    MultimodalTextCT parts ->
      forM_ (zip parts [0 :: Int32 ..]) $ \(part, seqPart) ->
        addMultiModalPartSession contentUid part seqPart
    ReasoningRecapCT textReasoning ->
      addReasoningRecapContentSession contentUid textReasoning
    SystemErrorCT nameErr textErr ->
      addSystemErrorContentSession contentUid nameErr textErr
    TetherBrowsingDisplayCT results summary assets tetherId ->
      addTetherBrowsingDisplayContentSession contentUid results summary assets tetherId
    TetherQuoteCT urlQuote domainQuote textQuote titleQuote tetherId ->
      addTetherQuoteContentSession contentUid urlQuote domainQuote textQuote titleQuote tetherId
    TextCT partsText ->
      addTextContentSession contentUid partsText
    ThoughtsCT thoughts sourceId ->
      addThoughtsContentSession contentUid thoughts sourceId
    OtherCT _ _ ->
      pure ()

contentType :: Content -> Text
contentType content =
  case content of
    CodeCT {} -> "code"
    ExecutionOutputCT {} -> "execution_output"
    ModelEditableContextCT {} -> "model_editable_context"
    MultimodalTextCT {} -> "multimodal_text"
    ReasoningRecapCT {} -> "reasoning_recap"
    SystemErrorCT {} -> "system_error"
    TetherBrowsingDisplayCT {} -> "tether_browsing_display"
    TetherQuoteCT {} -> "tether_quote"
    TextCT {} -> "text"
    ThoughtsCT {} -> "thoughts"
    OtherCT kind _ -> kind

addCodeContentSession :: Int64 -> Text -> Maybe Text -> Text -> Tx.Transaction ()
addCodeContentSession contentUid langCode formatRef textCode =
  Tx.statement (contentUid, langCode, formatRef, textCode) St.insertCodeContentStmt

addExecutionOutputContentSession :: Int64 -> Text -> Tx.Transaction ()
addExecutionOutputContentSession contentUid textOut =
  Tx.statement (contentUid, textOut) St.insertExecutionOutputContentStmt

addModelEditableContextSession :: Int64 -> Text -> Maybe Ae.Value -> Maybe Ae.Value -> Maybe Ae.Value -> Tx.Transaction ()
addModelEditableContextSession contentUid modelSlug repoJson rsJson scJson =
  Tx.statement
    (contentUid, modelSlug, fmap Ae.toJSON repoJson, fmap Ae.toJSON rsJson, fmap Ae.toJSON scJson)
    St.insertModelEditableContextStmt

addReasoningRecapContentSession :: Int64 -> Text -> Tx.Transaction ()
addReasoningRecapContentSession contentUid textReasoning =
  Tx.statement (contentUid, textReasoning) St.insertReasoningRecapContentStmt

addSystemErrorContentSession :: Int64 -> Text -> Text -> Tx.Transaction ()
addSystemErrorContentSession contentUid nameErr textErr =
  Tx.statement (contentUid, nameErr, textErr) St.insertSystemErrorContentStmt

addTetherBrowsingDisplayContentSession :: Int64 -> Text -> Maybe Text -> Maybe [Ae.Value] -> Maybe Text -> Tx.Transaction ()
addTetherBrowsingDisplayContentSession contentUid results summary assets tetherId =
  Tx.statement
    (contentUid, results, Ae.toJSON <$> summary, Ae.toJSON <$> assets, tetherId)
    St.insertTetherBrowsingDisplayContentStmt

addTetherQuoteContentSession :: Int64 -> Text -> Text -> Text -> Text -> Maybe Text -> Tx.Transaction ()
addTetherQuoteContentSession contentUid urlQuote domainQuote textQuote titleQuote tetherId =
  Tx.statement
    (contentUid, urlQuote, domainQuote, textQuote, titleQuote, tetherId)
    St.insertTetherQuoteContentStmt

addTextContentSession :: Int64 -> [Text] -> Tx.Transaction ()
addTextContentSession contentUid partsText =
  Tx.statement (contentUid, V.fromList partsText) St.insertTextContentStmt

addThoughtsContentSession :: Int64 -> [Thought] -> Text -> Tx.Transaction ()
addThoughtsContentSession contentUid thoughts sourceId = do
  Tx.statement (contentUid, sourceId) St.insertThoughtsContentStmt
  forM_ (zip thoughts [0 :: Int32 ..]) $ \(thought, seqThought) ->
    addThought contentUid thought seqThought

addThought :: Int64 -> Thought -> Int32 -> Tx.Transaction ()
addThought thoughtsUid thought seqThought =
  Tx.statement
    ( thoughtsUid
    , thought.summaryTh
    , thought.contentTh
    , Ae.toJSON thought.chunksTh
    , maybe False id thought.finishedTh
    , seqThought
    )
    St.insertThoughtStmt

addNodeTreeSession :: Int64 -> Mp.Map Text Node -> Text -> Maybe Int64 -> Int32 -> Tx.Transaction (Either String ())
addNodeTreeSession convUid mapping rootEid parentUid seqStart = do
  ei <- addNodeSubtreeSession convUid mapping rootEid parentUid 0 seqStart
  pure (fmap (const ()) ei)

addNodeSubtreeSession
  :: Int64
  -> Mp.Map Text Node
  -> Text
  -> Maybe Int64
  -> Int32
  -> Int32
  -> Tx.Transaction (Either String Int32)
addNodeSubtreeSession convUid mapping eidNode parentUid seqChild seqPre =
  case Mp.lookup eidNode mapping of
    Nothing ->
      pure . Left $ "@[addNodeSubtreeSession] node not found in mapping: " <> T.unpack eidNode
    Just node -> do
      nodeUid <- addNode convUid parentUid eidNode node seqPre seqChild seqPre
      goChildren nodeUid (seqPre + 1) 0 node.childrenNd
  where
    goChildren :: Int64 -> Int32 -> Int32 -> [Text] -> Tx.Transaction (Either String Int32)
    goChildren _ nextPre _ [] =
      pure (Right nextPre)
    goChildren nodeUid nextPre childSeq (childEid : rest) = do
      childRez <- addNodeSubtreeSession convUid mapping childEid (Just nodeUid) childSeq nextPre
      case childRez of
        Left err ->
          pure (Left err)
        Right nextPre' ->
          goChildren nodeUid nextPre' (childSeq + 1) rest

addMultiModalPartSession :: Int64 -> MultiModalPart -> Int32 -> Tx.Transaction ()
addMultiModalPartSession contentUid part seqPart = do
  partUid <- Tx.statement (contentUid, mmPartContentType part, seqPart) St.insertMultiModalPartStmt
  case part of
    AudioTranscriptionPT textAudio direction decodingId ->
      Tx.statement (partUid, textAudio, direction, decodingId) St.insertAudioTranscriptionMMPartStmt
    AudioAssetPointerPT ptrAudio -> do
      audioPtrUid <-
        Tx.statement
          ( partUid
          , ptrAudio.expiryDatetimeAap
          , ptrAudio.assetPointerAap
          , fromIntegral ptrAudio.sizeBytesAap
          , ptrAudio.formatAap
          , ptrAudio.toolAudioDirectionAap
          )
          St.insertAudioAssetPointerMMPartStmt
      case ptrAudio.metadataAap of
        Nothing ->
          pure ()
        Just audioMeta ->
          addAudioMetadata 1 audioPtrUid audioMeta
    TextPT textPart ->
      Tx.statement (partUid, textPart) St.insertTextMMPartStmt
    ImageAssetPointerPT assetPtr sizeBytes widthPx heightPx fovea metadata -> do
      imagePtrUid <-
        Tx.statement
          ( partUid
          , assetPtr
          , fromIntegral sizeBytes
          , fromIntegral widthPx
          , fromIntegral heightPx
          , Ae.toJSON <$> fovea
          )
          St.insertImageAssetPointerMMPartStmt
      case metadata of
        Nothing ->
          pure ()
        Just imageMeta ->
          addImageMetadataSession imagePtrUid imageMeta
    RealTimeUserAVPT expiryDatetime framePtrs videoContainerPtr audioPtr audioStartTs -> do
      let framePtrsJson = if null framePtrs then Nothing else Just (Ae.toJSON framePtrs)
      avUid <-
        Tx.statement
          (partUid, expiryDatetime, framePtrsJson, videoContainerPtr, audioStartTs)
          St.insertRealTimeUserAVMMPartStmt
      case audioPtr.metadataAap of
        Nothing ->
          pure ()
        Just audioMeta ->
          addAudioMetadata 2 avUid audioMeta

mmPartContentType :: MultiModalPart -> Text
mmPartContentType part =
  case part of
    AudioTranscriptionPT {} -> "audio_transcription"
    AudioAssetPointerPT {} -> "audio_asset_pointer"
    ImageAssetPointerPT {} -> "image_asset_pointer"
    RealTimeUserAVPT {} -> "real_time_user_audio_video_asset_pointer"
    TextPT {} -> "text"

addImageMetadataSession :: Int64 -> ImageMetadata -> Tx.Transaction ()
addImageMetadataSession imagePtrUid meta = do
  metaUid <-
    Tx.statement
      ( imagePtrUid
      , Ae.toJSON <$> meta.gizmoMd
      , fromIntegral <$> meta.containerPixelHeightMd
      , fromIntegral <$> meta.containerPixelWidthMd
      , Ae.toJSON <$> meta.emuOmitGlimpseImageMd
      , Ae.toJSON <$> meta.emuPatchesOverrideMd
      , Ae.toJSON <$> meta.lpeKeepPatchIjhwMd
      , Ae.toJSON <$> meta.lpeDeltaEncodingChannelMd
      , meta.sanitizedMd
      , Ae.toJSON <$> meta.assetPointerLinkMd
      , Ae.toJSON <$> meta.watermarkedAssetPointerMd
      , Ae.toJSON <$> meta.isNoAuthPlaceholderMd
      )
      St.insertImageMetadataStmt
  case meta.dalleMd of
    Nothing ->
      pure ()
    Just dalle ->
      addDalle metaUid dalle
  case meta.generationMd of
    Nothing ->
      pure ()
    Just generation ->
      addGeneration metaUid generation

addDalle :: Int64 -> Dalle -> Tx.Transaction ()
addDalle metaUid dalle =
  Tx.statement
    ( metaUid
    , dalle.genIdDa
    , dalle.promptDa
    , fromIntegral <$> dalle.seedDa
    , dalle.parentGenIdDa
    , dalle.editOpDa
    , dalle.serializationTitleDa
    )
    St.insertDalleStmt

addGeneration :: Int64 -> Generation -> Tx.Transaction ()
addGeneration metaUid generation =
  Tx.statement
    ( metaUid
    , generation.genIdGe
    , generation.genSizeGe
    , fromIntegral <$> generation.seedGe
    , generation.parentGenIdGe
    , fromIntegral generation.heightGe
    , fromIntegral generation.widthGe
    , generation.transparentBackgroundGe
    , generation.serializationTitleGe
    , generation.orientationGe
    )
    St.insertGenerationStmt

addAudioMetadata :: Int32 -> Int64 -> AudioMetadata -> Tx.Transaction ()
addAudioMetadata kindItem itemUid meta =
  Tx.statement
    ( itemUid
    , kindItem
    , Ae.toJSON <$> meta.startTimestampAm
    , Ae.toJSON <$> meta.endTimestampAm
    , Ae.toJSON <$> meta.pretokenizedVqAm
    , Ae.toJSON <$> meta.interruptionsAm
    , Ae.toJSON <$> meta.originalAudioSourceAm
    , Ae.toJSON <$> meta.transcriptionAm
    , Ae.toJSON <$> meta.wordTranscriptionAm
    , meta.startAm
    , meta.endAm
    )
    St.insertAudioMetadataStmt

jsonMetaMap :: Mp.Map Text Ae.Value -> Ae.Value
jsonMetaMap = Ae.toJSON . HM.fromList . Mp.toList

renderConversationErr :: Conversation -> String -> String
renderConversationErr conversation err =
  T.unpack $
    T.unlines
      [ "@[addConversation] insert failed"
      , "title: " <> conversation.titleCv
      , "eid: " <> conversation.convIdCv
      , "error: " <> T.pack err
      ]