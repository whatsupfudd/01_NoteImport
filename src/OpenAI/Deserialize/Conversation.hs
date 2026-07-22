module OpenAI.Deserialize.Conversation
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)

import Data.Char (toLower)
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Mp
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as St
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import qualified Data.Aeson as Ae
import Data.Aeson (Value)

import Hasql.Statement (Statement)
import qualified Hasql.Pool as Hp
import qualified Hasql.Session as Ses

import qualified OpenAI.Conversation as Cv
import qualified OpenAI.Deserialize.ConversationStmt as Dst


-- -----------------------------
-- Public API
-- -----------------------------

fetchAllConversations :: Hp.Pool -> IO (Either Hp.UsageError (Map Text Int64))
fetchAllConversations pool = do
  dbRez <- Hp.use pool (Ses.statement () Dst.fetchAllConversationsRows)
  pure $
    fmap
      ( Mp.fromList
          . map (\(uid, title) -> (title, uid))
          . V.toList
      )
      dbRez

fetchAllConversationEids :: Hp.Pool -> IO (Either Hp.UsageError (Set Text))
fetchAllConversationEids pool = do
  dbRez <- Hp.use pool (Ses.statement () Dst.fetchAllConversationEids)
  pure $ fmap (St.fromList . V.toList) dbRez

-- | Load a conversation by external id (conversation_id / oai.conversations.eid).
--
-- Returns:
--   * Right Nothing  => not found
--   * Right (Just x) => loaded
--   * Left err       => DB error or unexpected schema inconsistency
getConversationByEid :: Hp.Pool -> Text -> IO (Either Hp.UsageError (Either String (Maybe Cv.ConversationDb)))
getConversationByEid pool targetEid = do
  eiMbConv <- Hp.use pool $ Ses.statement targetEid Dst.selectConversationByEid
  case eiMbConv of
    Left err -> pure (Left err)
    Right Nothing -> pure (Right $ Right Nothing)
    Right (Just convRow) -> getConversationBody pool convRow

getConversationByUid :: Hp.Pool -> Int64 -> IO (Either Hp.UsageError (Either String (Maybe Cv.ConversationDb)))
getConversationByUid pool uid = do
  eiMbConv <- Hp.use pool $ Ses.statement uid Dst.selectConversationByUid
  case eiMbConv of
    Left err -> pure (Left err)
    Right Nothing -> pure (Right $ Right Nothing)
    Right (Just convRow) -> getConversationBody pool convRow

getConversationBody
  :: Hp.Pool
  -> Dst.ConversationRow
  -> IO (Either Hp.UsageError (Either String (Maybe Cv.ConversationDb)))
getConversationBody pool convRow@(convUid, _, _, _, _) =
  runExceptT $ do
    nodeRows <- runStmt pool Dst.selectNodes convUid
    msgRows <- runStmt pool Dst.selectMessagesWithAuthor convUid

    contentRows <- runStmt pool Dst.selectContents convUid

    codeRows <- runStmt pool Dst.selectCodeContents convUid
    execRows <- runStmt pool Dst.selectExecutionOutputContents convUid
    mecRows <- runStmt pool Dst.selectModelEditableContextContents convUid
    rrcRows <- runStmt pool Dst.selectReasoningRecapContents convUid
    sesRows <- runStmt pool Dst.selectSystemErrorContents convUid
    tbdRows <- runStmt pool Dst.selectTetherBrowsingDisplayContents convUid
    tqRows <- runStmt pool Dst.selectTetherQuoteContents convUid
    txtRows <- runStmt pool Dst.selectTextContents convUid
    thHdrRows <- runStmt pool Dst.selectThoughtsContents convUid
    thRows <- runStmt pool Dst.selectThoughts convUid
    unkRows <- runStmt pool Dst.selectUnknownContents convUid

    mmPartRows <- runStmt pool Dst.selectMultiModalParts convUid
    mmTextRows <- runStmt pool Dst.selectTextMmParts convUid
    mmAtRows <- runStmt pool Dst.selectAudioTranscriptionMmParts convUid

    mmImgPtrRows <- runStmt pool Dst.selectImageAssetPointerMmParts convUid
    mmImgMdRows <- runStmt pool Dst.selectImageAssetMetadatas convUid
    mmDalleRows <- runStmt pool Dst.selectDalles convUid
    mmGenRows <- runStmt pool Dst.selectGenerations convUid

    mmAapRows <- runStmt pool Dst.selectAudioAssetPointerMmParts convUid
    mmAapMetaRows <- runStmt pool Dst.selectAudioMetadataForAap convUid

    mmRtuavRows <- runStmt pool Dst.selectRealTimeUserAVMmParts convUid
    mmRtuavMetaRows <- runStmt pool Dst.selectAudioMetadataForRtuav convUid

    pure $
      Just <$> buildConversationDb
        convRow
        nodeRows
        msgRows
        contentRows
        codeRows execRows mecRows rrcRows sesRows tbdRows tqRows txtRows thHdrRows thRows unkRows
        mmPartRows mmTextRows mmAtRows
        mmImgPtrRows mmImgMdRows mmDalleRows mmGenRows
        mmAapRows mmAapMetaRows
        mmRtuavRows mmRtuavMetaRows


-- -----------------------------
-- Orchestration helpers
-- -----------------------------

runStmt :: Hp.Pool -> Statement a b -> a -> ExceptT Hp.UsageError IO b
runStmt pool stmt a = do
  rez <- lift $ Hp.use pool (Ses.statement a stmt)
  case rez of
    Left err -> throwE err
    Right val -> pure val


-- -----------------------------
-- Assembly
-- -----------------------------

buildConversationDb
  :: Dst.ConversationRow
  -> Vector Dst.NodeRow
  -> Vector Dst.MessageRow
  -> Vector Dst.ContentRow
  -> Vector Dst.CodeRow
  -> Vector Dst.ExecRow
  -> Vector Dst.MecRow
  -> Vector Dst.RrcRow
  -> Vector Dst.SesRow
  -> Vector Dst.TbdRow
  -> Vector Dst.TqRow
  -> Vector Dst.TextRow
  -> Vector Dst.ThoughtsHdrRow
  -> Vector Dst.ThoughtRow
  -> Vector Dst.UnknownRow
  -> Vector Dst.MmPartRow
  -> Vector Dst.MmTextRow
  -> Vector Dst.MmAtRow
  -> Vector Dst.MmImgPtrRow
  -> Vector Dst.MmImgMdRow
  -> Vector Dst.DalleRow
  -> Vector Dst.GenerationRow
  -> Vector Dst.AapRow
  -> Vector Dst.AudioMetaRow
  -> Vector Dst.RtuavRow
  -> Vector Dst.AudioMetaRow
  -> Either String Cv.ConversationDb
buildConversationDb
  (convUid, title, eid, cTime, uTime)
  nodeRows
  msgRows
  contentRows
  codeRows execRows mecRows rrcRows sesRows tbdRows tqRows txtRows thHdrRows thRows unkRows
  mmPartRows mmTextRows mmAtRows
  mmImgPtrRows mmImgMdRows dalleRows genRows
  aapRows aapMetaRows
  rtuavRows rtuavMetaRows =
    do
      let codeMap = Mp.fromList [ (k, (l, f, t)) | (k, l, f, t) <- V.toList codeRows ]
          execMap = Mp.fromList [ (k, t) | (k, t) <- V.toList execRows ]
          mecMap = Mp.fromList [ (k, (msc, r, rs, sc)) | (k, msc, r, rs, sc) <- V.toList mecRows ]
          rrcMap = Mp.fromList [ (k, t) | (k, t) <- V.toList rrcRows ]
          sesMap = Mp.fromList [ (k, (nm, tx)) | (k, nm, tx) <- V.toList sesRows ]
          tbdMap = Mp.fromList [ (k, (res, sm, as, tid)) | (k, res, sm, as, tid) <- V.toList tbdRows ]
          tqMap = Mp.fromList [ (k, (u, d, tx, ti, tid)) | (k, u, d, tx, ti, tid) <- V.toList tqRows ]
          txtMap = Mp.fromList [ (k, ps) | (k, ps) <- V.toList txtRows ]
          thHdrMap = Mp.fromList [ (k, sid) | (k, sid) <- V.toList thHdrRows ]
          unkMap = Mp.fromList [ (k, v) | (k, v) <- V.toList unkRows ]

          thoughtsMap :: Map Int64 (Vector Cv.ThoughtDb)
          thoughtsMap =
            groupVecByKey thRows $ \(fk, summ, cont, chunks, fin) ->
              ( fk
              , Cv.ThoughtDb
                  { Cv.summaryTh = summ
                  , Cv.contentTh = cont
                  , Cv.chunksTh = chunks
                  , Cv.finishedTh = fin
                  }
              )

      let mmTextMap = Mp.fromList [ (k, t) | (k, t) <- V.toList mmTextRows ]
          mmAtMap = Mp.fromList [ (k, (t, dir, dec)) | (k, t, dir, dec) <- V.toList mmAtRows ]

          mmImgPtrMap :: Map Int64 (Int64, Text, Int64, Int32, Int32, Maybe Value)
          mmImgPtrMap =
            Mp.fromList
              [ (mpUid, (imgUid, ap, sb, w, h, fv))
              | (mpUid, imgUid, ap, sb, w, h, fv) <- V.toList mmImgPtrRows
              ]

          mmImgMdMap :: Map Int64 Cv.ImageMetadataDb
          mmImgMdMap =
            let mdByImgPtr =
                  Mp.fromList
                    [ (imgPtrFk, (mdUid, giz, cph, cpw, og, po, kp, de, san, apl, wml, nap))
                    | (imgPtrFk, mdUid, giz, cph, cpw, og, po, kp, de, san, apl, wml, nap) <- V.toList mmImgMdRows
                    ]

                dalleByMd =
                  Mp.fromList
                    [ (mdFk, (gid, prm, sd, pgid, eop, st))
                    | (mdFk, gid, prm, sd, pgid, eop, st) <- V.toList dalleRows
                    ]

                genByMd =
                  Mp.fromList
                    [ (mdFk, (gid, gs, sd, pgid, ht, wd, tb, st, ori))
                    | (mdFk, gid, gs, sd, pgid, ht, wd, tb, st, ori) <- V.toList genRows
                    ]
            in
              Mp.map
                ( \(mdUid, giz, cph, cpw, og, po, kp, de, san, apl, wml, nap) ->
                    Cv.ImageMetadataDb
                      { Cv.dalleMd =
                          case Mp.lookup mdUid dalleByMd of
                            Nothing -> Nothing
                            Just (gid, prm, sd, pgid, eop, st) ->
                              Just Cv.DalleDb
                                { Cv.genIdDa = gid
                                , Cv.promptDa = prm
                                , Cv.seedDa = fmap fromIntegral sd
                                , Cv.parentGenIdDa = pgid
                                , Cv.editOpDa = eop
                                , Cv.serializationTitleDa = st
                                }
                      , Cv.gizmoMd = giz
                      , Cv.generationMd =
                          case Mp.lookup mdUid genByMd of
                            Nothing -> Nothing
                            Just (gid, gs, sd, pgid, ht, wd, tb, st, ori) ->
                              Just Cv.GenerationDb
                                { Cv.genIdGe = gid
                                , Cv.genSizeGe = gs
                                , Cv.seedGe = fmap fromIntegral sd
                                , Cv.parentGenIdGe = pgid
                                , Cv.heightGe = fromIntegral ht
                                , Cv.widthGe = fromIntegral wd
                                , Cv.transparentBackgroundGe = tb
                                , Cv.serializationTitleGe = st
                                , Cv.orientationGe = ori
                                }
                      , Cv.containerPixelHeightMd = fmap fromIntegral cph
                      , Cv.containerPixelWidthMd = fmap fromIntegral cpw
                      , Cv.emuOmitGlimpseImageMd = og
                      , Cv.emuPatchesOverrideMd = po
                      , Cv.lpeKeepPatchIjhwMd = kp
                      , Cv.lpeDeltaEncodingChannelMd = de
                      , Cv.sanitizedMd = san
                      , Cv.assetPointerLinkMd = apl
                      , Cv.watermarkedAssetPointerMd = wml
                      , Cv.isNoAuthPlaceholderMd = nap
                      }
                )
                mdByImgPtr

          aapMetaMap :: Map Int64 Cv.AudioMetadataDb
          aapMetaMap =
            Mp.fromList [ (fk, mkAudioMeta row) | row@(fk, _, _, _, _, _, _, _, _, _) <- V.toList aapMetaRows ]

          _rtuavMetaMap :: Map Int64 Cv.AudioMetadataDb
          _rtuavMetaMap =
            Mp.fromList [ (fk, mkAudioMeta row) | row@(fk, _, _, _, _, _, _, _, _, _) <- V.toList rtuavMetaRows ]

          aapMap :: Map Int64 Cv.AudioAssetPointerDb
          aapMap =
            Mp.fromList
              [ ( mpUid
                , Cv.AudioAssetPointerDb
                    { Cv.expiryDatetimeAap = expd
                    , Cv.assetPointerAap = ap
                    , Cv.sizeBytesAap = sb
                    , Cv.formatAap = fmt
                    , Cv.toolAudioDirectionAap = toolDir
                    , Cv.metadataAap = Mp.lookup aapUid aapMetaMap
                    }
                )
              | (mpUid, aapUid, expd, ap, sb, fmt, toolDir) <- V.toList aapRows
              ]

          rtuavMap :: Map Int64 Cv.RealTimeUserAVDb
          rtuavMap =
            Mp.fromList
              [ ( mpUid
                , Cv.RealTimeUserAVDb
                    { Cv.expiryDatetimeRtuav = expd
                    , Cv.framesAssetPointersRtuav = frames
                    , Cv.videoContainerAssetPointerRtuav = vcap
                    , Cv.audioStartTimestampRtuav = ast
                    }
                )
              | (mpUid, _rUid, expd, frames, vcap, ast) <- V.toList rtuavRows
              ]

      let mmPartsByContent :: Map Int64 (Vector Cv.MultiModalPartDb)
          mmPartsByContent =
            groupVecByKey mmPartRows $ \(mpUid, cFk, cTyp) ->
              ( cFk
              , mkMultiModalPart mpUid cTyp mmTextMap mmAtMap mmImgPtrMap mmImgMdMap aapMap rtuavMap
              )

      let contentsByMessage :: Map Int64 (Vector Cv.ContentDb)
          contentsByMessage =
            groupVecByKey contentRows $ \(cUid, mUid, cTyp) ->
              ( mUid
              , mkContent cUid cTyp
                  codeMap execMap mecMap rrcMap sesMap tbdMap tqMap txtMap thHdrMap thoughtsMap unkMap mmPartsByContent
              )

      let msgMapByNode :: Map Int64 Cv.MessageDb
          msgMapByNode =
            Mp.fromList
              [ (nodeUid, msg)
              | (nodeUid, mUid, mEid, mc, mu, st, et, wt, md, rcpt, ch, auUid, auRole, auName, auMeta) <- V.toList msgRows
              , let author =
                      Cv.AuthorDb
                        { Cv.uidAu = auUid
                        , Cv.roleAu = auRole
                        , Cv.nameAu = auName
                        , Cv.metadataAu = auMeta
                        }
                    msg =
                      Cv.MessageDb
                        { Cv.uidMsg = mUid
                        , Cv.eidMsg = mEid
                        , Cv.createTimeMsg = mc
                        , Cv.updateTimeMsg = mu
                        , Cv.statusMsg = st
                        , Cv.endTurnMsg = et
                        , Cv.weightMsg = wt
                        , Cv.metadataMsg = md
                        , Cv.recipientMsg = rcpt
                        , Cv.channelMsg = ch
                        , Cv.authorMsg = author
                        , Cv.contentsMsg = Mp.findWithDefault V.empty mUid contentsByMessage
                        }
              ]

      let nodesMap :: Map Text Cv.NodeDb
          nodesMap =
            Mp.fromList
              [ (node.eidNd, attachMessageNode msgMapByNode node)
              | row <- V.toList nodeRows
              , let node = nodeFromRow row
              ]

      pure $
        Cv.ConversationDb
          { Cv.uidCv = convUid
          , Cv.titleCv = title
          , Cv.eidCv = eid
          , Cv.createTimeCv = cTime
          , Cv.updateTimeCv = uTime
          , Cv.nodesCv = nodesMap
          }


-- -----------------------------
-- Node builders
-- -----------------------------

nodeFromRow :: Dst.NodeRow -> Cv.NodeDb
nodeFromRow (uid, eid, parentFk, seqNode, seqChild, seqPre) =
  Cv.NodeDb
    { Cv.uidNd = uid
    , Cv.eidNd = eid
    , Cv.parentFkNd = parentFk
    , Cv.seqNodeNd = seqNode
    , Cv.seqChildNd = seqChild
    , Cv.seqPreNd = seqPre
    , Cv.messageNd = Nothing
    }

attachMessageNode :: Map Int64 Cv.MessageDb -> Cv.NodeDb -> Cv.NodeDb
attachMessageNode msgMap node =
  Cv.NodeDb
    { Cv.uidNd = node.uidNd
    , Cv.eidNd = node.eidNd
    , Cv.parentFkNd = node.parentFkNd
    , Cv.seqNodeNd = node.seqNodeNd
    , Cv.seqChildNd = node.seqChildNd
    , Cv.seqPreNd = node.seqPreNd
    , Cv.messageNd = Mp.lookup node.uidNd msgMap
    }


-- -----------------------------
-- Content builders
-- -----------------------------

mkContent
  :: Int64
  -> Text
  -> Map Int64 (Text, Maybe Text, Text)
  -> Map Int64 Text
  -> Map Int64 (Text, Maybe Value, Maybe Value, Maybe Value)
  -> Map Int64 Text
  -> Map Int64 (Text, Text)
  -> Map Int64 (Text, Maybe Value, Maybe Value, Maybe Text)
  -> Map Int64 (Text, Text, Text, Text, Maybe Text)
  -> Map Int64 (Vector Text)
  -> Map Int64 Text
  -> Map Int64 (Vector Cv.ThoughtDb)
  -> Map Int64 Value
  -> Map Int64 (Vector Cv.MultiModalPartDb)
  -> Cv.ContentDb
mkContent cUid cTyp codeMap execMap mecMap rrcMap sesMap tbdMap tqMap txtMap thHdrMap thoughtsMap unkMap mmPartsByContent =
  case normalizeTyp cTyp of
    "code" ->
      case Mp.lookup cUid codeMap of
        Just (lang, fmt, txt) -> Cv.CodeCT_Db lang fmt txt
        Nothing -> Cv.UnknownCT_Db cTyp Ae.Null

    "execution_output" ->
      case Mp.lookup cUid execMap of
        Just t -> Cv.ExecutionOutputCT_Db t
        Nothing -> Cv.UnknownCT_Db cTyp Ae.Null

    "multimodal_text" ->
      let parts = Mp.findWithDefault V.empty cUid mmPartsByContent
      in Cv.MultimodalTextCT_Db parts

    "model_editable_context" ->
      case Mp.lookup cUid mecMap of
        Just (msc, r, rs, sc) -> Cv.ModelEditableContextCT_Db msc r rs sc
        Nothing -> Cv.UnknownCT_Db cTyp Ae.Null

    "reasoning_recap" ->
      case Mp.lookup cUid rrcMap of
        Just t -> Cv.ReasoningRecapCT_Db t
        Nothing -> Cv.UnknownCT_Db cTyp Ae.Null

    "system_error" ->
      case Mp.lookup cUid sesMap of
        Just (nm, tx) -> Cv.SystemErrorCT_Db nm tx
        Nothing -> Cv.UnknownCT_Db cTyp Ae.Null

    "tether_browsing_display" ->
      case Mp.lookup cUid tbdMap of
        Just (res, sm, as, tid) -> Cv.TetherBrowsingDisplayCT_Db res sm as tid
        Nothing -> Cv.UnknownCT_Db cTyp Ae.Null

    "tether_quote" ->
      case Mp.lookup cUid tqMap of
        Just (u, d, tx, ti, tid) -> Cv.TetherQuoteCT_Db u d tx ti tid
        Nothing -> Cv.UnknownCT_Db cTyp Ae.Null

    "text" ->
      case Mp.lookup cUid txtMap of
        Just parts -> Cv.TextCT_Db parts
        Nothing -> Cv.UnknownCT_Db cTyp Ae.Null

    "thoughts" ->
      let sid = fromMaybe "" (Mp.lookup cUid thHdrMap)
          ths = Mp.findWithDefault V.empty cUid thoughtsMap
      in Cv.ThoughtsCT_Db sid ths

    "unknown" ->
      Cv.UnknownCT_Db cTyp (Mp.findWithDefault Ae.Null cUid unkMap)

    _ ->
      Cv.UnknownCT_Db cTyp (Mp.findWithDefault Ae.Null cUid unkMap)

mkMultiModalPart
  :: Int64
  -> Text
  -> Map Int64 Text
  -> Map Int64 (Text, Text, Maybe Text)
  -> Map Int64 (Int64, Text, Int64, Int32, Int32, Maybe Value)
  -> Map Int64 Cv.ImageMetadataDb
  -> Map Int64 Cv.AudioAssetPointerDb
  -> Map Int64 Cv.RealTimeUserAVDb
  -> Cv.MultiModalPartDb
mkMultiModalPart mpUid cTyp mmTextMap mmAtMap mmImgPtrMap mmImgMdMap aapMap rtuavMap =
  case normalizeTyp cTyp of
    "text" ->
      Cv.TextPT_Db (Mp.findWithDefault "" mpUid mmTextMap)

    "audio_transcription" ->
      case Mp.lookup mpUid mmAtMap of
        Nothing -> Cv.TextPT_Db "(missing audio transcription)"
        Just (t, dir, dec) ->
          Cv.AudioTranscriptionPT_Db Cv.AudioTranscriptionDb
            { Cv.textAtp = t
            , Cv.directionAtp = dir
            , Cv.decodingIdAtp = dec
            }

    "image_asset_pointer" ->
      case Mp.lookup mpUid mmImgPtrMap of
        Nothing -> Cv.TextPT_Db "(missing image asset pointer)"
        Just (imgPtrUid, ap, sb, w, h, fv) ->
          let md = Mp.lookup imgPtrUid mmImgMdMap
          in
            Cv.ImageAssetPointerPT_Db Cv.ImageAssetPointerDb
              { Cv.assetPointerIap = ap
              , Cv.sizeBytesIap = sb
              , Cv.widthIap = w
              , Cv.heightIap = h
              , Cv.foveaIap = fv
              , Cv.metadataIap = md
              }

    "audio_asset_pointer" ->
      case Mp.lookup mpUid aapMap of
        Nothing -> Cv.TextPT_Db "(missing audio asset pointer)"
        Just aap -> Cv.AudioAssetPointerPT_Db aap

    "real_time_user_av" ->
      case Mp.lookup mpUid rtuavMap of
        Nothing -> Cv.TextPT_Db "(missing real time user AV)"
        Just rtuav -> Cv.RealTimeUserAVPT_Db rtuav

    other ->
      Cv.TextPT_Db ("(unsupported multimodal part type: " <> other <> ")")

mkAudioMeta :: Dst.AudioMetaRow -> Cv.AudioMetadataDb
mkAudioMeta (_fk, st, et, pvq, ints, oas, tr, wtr, ss, es) =
  Cv.AudioMetadataDb
    { Cv.startTimestampAm = st
    , Cv.endTimestampAm = et
    , Cv.pretokenizedVqAm = pvq
    , Cv.interruptionsAm = ints
    , Cv.originalAudioSourceAm = oas
    , Cv.transcriptionAm = tr
    , Cv.wordTranscriptionAm = wtr
    , Cv.startStampAm = ss
    , Cv.endStampAm = es
    }


-- -----------------------------
-- Helpers
-- -----------------------------

normalizeTyp :: Text -> Text
normalizeTyp = T.pack . map toLower . T.unpack

-- | Group rows (already ordered by SQL) into Map k (Vector v).
--
-- We prepend during the fold and reverse once at the end so the final Vector
-- preserves SQL ordering without repeated right-appends.
groupVecByKey :: Ord k => Vector a -> (a -> (k, v)) -> Map k (Vector v)
groupVecByKey xs extractor =
  let grouped =
        V.foldl'
          (\m a ->
            let (k, v) = extractor a
            in Mp.insertWith (V.++) k (V.singleton v) m
          )
          Mp.empty
          xs
  in
    Mp.map V.reverse grouped