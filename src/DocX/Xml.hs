

-- | DOCX XML importer and enrichment layer.
--
-- This module provides:
--   • importDocxStructure  — structural fallback via direct XML parsing
--   • enrichDocxWithXml    — DOCX-specific enrichment (tracked changes, comments,
--                            styles/numbering details) layered on top of an
--                            already-built 'HBDoc.Types.Doc' (usually from Pandoc).
--
-- It intentionally keeps structure simple and conservative; the Pandoc importer
-- is the canonical structural path, while this XML layer supplies fidelity where
-- Pandoc does not (revisions, comments, fine list/style meta).
module DocX.Xml
  ( -- * Types
    EnrichmentLevel(..)
    -- * API
  , importDocxStructure
  , enrichDocxWithXml
  ) where

import Control.Applicative ((<|>))
import Control.Monad (foldM, join)

import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as Tl
import qualified Data.Text.Read as TR

import GHC.IO.StdHandles (withFile)

import qualified Data.Aeson as A
import qualified Data.Aeson.Text as At

import qualified Codec.Archive.Zip as Zp
import qualified Text.XML as X
import qualified Text.XML.Cursor as C

import HBDoc.Types (Doc(..), Block(..), BlockKind(..), ObjectAttrs(..), EnrichmentLevel(..))

-- ---------------------------------------------------------------------
-- X.Names & helpers
-- ---------------------------------------------------------------------

wns :: Text
wns = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"

nm :: Text -> Text -> X.Name
nm ns local = X.Name local (Just ns) Nothing

txtAttr :: X.Name -> C.Cursor -> Maybe Text
txtAttr n cur = listToMaybe (C.attribute n cur)

attrInt :: X.Name -> C.Cursor -> Maybe Int32
attrInt n cur = do
  t <- txtAttr n cur
  case TR.decimal t of
    Right (v,_) -> Just (fromIntegral (v :: Integer))
    _           -> Nothing

-- Gather text from runs under a paragraph-like container
paraTextHere :: C.Cursor -> Text
paraTextHere here =
  let
    ts = (here C.$/ C.element (nm wns "r") C.&// C.element (nm wns "t"))
      <> (here C.$/ C.element (nm wns "hyperlink") C.&/ C.element (nm wns "r") C.&// C.element (nm wns "t"))
  in T.concat [ T.concat (C.content t) | t <- ts, keep t ]
  where
  keep tCur = not (any (isDel . C.node) (C.ancestor tCur))
  isDel anc =
    case anc of
      X.NodeElement e -> X.elementName e == nm wns "del"
      _ -> False


-- Paragraph classification helpers
paraStyleIdHere :: C.Cursor -> Maybe Text
paraStyleIdHere here =
  join (listToMaybe (here
      C.$/ C.element (nm wns "pPr")
    C.&/ C.element (nm wns "pStyle")
    C.&| (listToMaybe . C.attribute (nm wns "val"))))

paraOutlineLvlHere :: C.Cursor -> Maybe Int
paraOutlineLvlHere here = do
  mbVv <- listToMaybe $ here
        C.$/ C.element (nm wns "pPr")
       C.&/ C.element (nm wns "outlineLvl")
       C.&| (listToMaybe . C.attribute (nm wns "val"))
  case TR.decimal <$> mbVv of
    Just (Right (n, _)) -> Just n
    _ -> Nothing

classifyHeading :: (Text -> Maybe Int) -> C.Cursor -> Maybe Int
classifyHeading styleToLvl c =
  case paraOutlineLvlHere c of
    Just lvl0 -> Just (lvl0 + 1)
    Nothing   -> styleToLvl =<< paraStyleIdHere c

-- List meta (numId, ilvl)
listMetaHere :: C.Cursor -> (Maybe Int32, Maybe Int32)
listMetaHere here =
  let numId = listToMaybe $ here
                C.$/ C.element (nm wns "pPr")
               C.&/ C.element (nm wns "numPr")
               C.&/ C.element (nm wns "numId")
               C.&| (listToMaybe . C.attribute (nm wns "val"))
      ilvlT = listToMaybe $ here
                C.$/ C.element (nm wns "pPr")
               C.&/ C.element (nm wns "numPr")
               C.&/ C.element (nm wns "ilvl")
               C.&| (listToMaybe . C.attribute (nm wns "val"))
      
  in (numId >>= asMbInt32, ilvlT >>= asMbInt32)
  where
  asMbInt32 :: Maybe Text -> Maybe Int32
  asMbInt32 Nothing = Nothing
  asMbInt32 (Just t) =
    case TR.decimal t of
      Right (v,_) -> Just (fromIntegral (v :: Integer))
      _ -> Nothing


-- ---------------------------------------------------------------------
-- Structure fallback: importDocxStructure
-- ---------------------------------------------------------------------

-- | Parse 'word/document.xml' into a simple HBDoc 'Doc'.
-- Use Pandoc path as the canonical importer; this is a conservative fallback.
importDocxStructure :: BL.ByteString -> IO (Either Text Doc)
importDocxStructure lbs =
  case Zp.toArchiveOrFail lbs of
    Left e   -> pure (Left (T.pack e))
    Right ar ->
      case Zp.findEntryByPath "word/document.xml" ar of
        Nothing   -> pure (Left "Missing word/document.xml")
        Just ent  -> do
          let doc = X.parseLBS_ X.def (Zp.fromEntry ent)
              root = C.fromDocument doc
              -- Walk only direct children of the root so we see 'w:p'/'w:tbl'
              top  = root C.$/ C.child
          (blocks, _) <- foldM step ([], 0 :: Int32) top
          let title = "" -- no reliable title at structure-only stage
          pure $ Right Doc
            { uidDoc       = Nothing
            , stableIdDoc  = Nothing
            , titleDoc     = title
            , formatDoc    = Just "docx"
            , metaDoc      = mempty
            , blocksDoc    = blocks
            }
  where
    step :: ([Block], Int32) -> C.Cursor -> IO ([Block], Int32)
    step (acc, n) c =
      case c.node of
        X.NodeElement e ->
          case X.nameLocalName (X.elementName e) of
            "p"   -> do
              let txt  = T.strip (paraTextHere c)
                  (mNum, mLvl) = listMetaHere c
                  kind = case (mNum, mLvl) of
                           (Just _, Just il) -> ListItemBK il
                           _                 -> ParagraphBK
                  blk  = Block n Nothing kind  mLvl
                          (if T.null txt then Nothing else Just txt)
                          (Just ObjectAttrs
                            { styleNameOA   = Nothing
                            , styleIdOA     = paraStyleIdHere c
                            , numIdOA       = mNum
                            , ilvlOA        = mLvl
                            , numFmtOA      = Nothing
                            , lvlTextOA     = Nothing
                            , startAtOA     = Nothing
                            , captionOA     = Nothing
                            , imageRidOA    = Nothing
                            , imageNameOA   = Nothing
                            , imageKeyOA    = Nothing
                            , widthEmuOA    = Nothing
                            , heightEmuOA   = Nothing
                            , noteIdOA      = Nothing
                            , noteTypeOA    = Nothing
                            , htmlIdOA      = Nothing
                            , htmlClassesOA = []
                            , htmlAttrsOA   = mempty
                            })
                          []
              pure (acc <> [blk], n + 1)

            "tbl" -> do
              (tblBlk, n') <- parseTbl n c
              pure (acc <> [tblBlk], n')

            _     -> pure (acc, n)
        _ -> pure (acc, n)

    parseTbl :: Int32 -> C.Cursor -> IO (Block, Int32)
    parseTbl n tbl = do
      let rows = tbl C.$/ C.element (nm wns "tr")
      (rBlocks, n1) <- foldM stepRow ([], n+1) rows
      let tblBlk = Block n Nothing TableBK Nothing (Just "[table]") Nothing rBlocks
      pure (tblBlk, n1)
      where
        stepRow (acc, n0) r = do
          let cells = r C.$/ C.element (nm wns "tc")
          (cBlocks, n1) <- foldM stepCell ([], n0+1) cells
          let rowBlk = Block n0 Nothing TableRowBK Nothing Nothing Nothing cBlocks
          pure (acc <> [rowBlk], n1)
        stepCell (acc, n0) tc = do
          let ps = tc C.$/ C.element (nm wns "p")
          (pBlocks, n1) <- foldM stepPara ([], n0+1) ps
          let cBlk = Block n0 Nothing TableCellBK Nothing Nothing Nothing pBlocks
          pure (acc <> [cBlk], n1)
        stepPara (acc, n0) pc = do
          let t = T.strip (paraTextHere pc)
              p = Block n0 Nothing ParagraphBK Nothing (if T.null t then Nothing else Just t) Nothing []
          pure (acc <> [p], n0 + 1)

-- ---------------------------------------------------------------------
-- Enrichment: comments, tracked changes, styles/numbering
-- ---------------------------------------------------------------------

-- | Enrich an existing 'Doc' with DOCX-specific info extracted from XML parts.
--   Minimal:   tracked changes (ins/del) + comments
--   Full:      minimal + style/numbering fill-in on paragraph-like blocks
enrichDocxWithXml :: EnrichmentLevel -> BL.ByteString -> Doc -> IO (Either Text Doc)
enrichDocxWithXml lvl lbs docIn =
  case Zp.toArchiveOrFail lbs of
    Left e   -> pure (Left (T.pack e))
    Right ar -> do
      let mDoc = Zp.findEntryByPath "word/document.xml" ar
      case mDoc of
        Nothing  -> pure (Left "Missing word/document.xml")
        Just ent -> do
          let root = C.fromDocument (X.parseLBS_ X.def (Zp.fromEntry ent))
          paraInfos <- collectParas root ar (lvl /= EnrichNone)
          let blocks' = applyParaInfoToBlocks lvl paraInfos (blocksDoc docIn)
          pure $ Right docIn { blocksDoc = blocks' }

-- Per-paragraph metadata we collect from XML
data ParaInfo = ParaInfo
  { idxPI      :: !Int                 -- sequential paragraph index
  , textPI     :: !Text                -- plain text
  , styleIdPI  :: !(Maybe Text)        -- w:pStyle/@w:val
  , styleNamePI:: !(Maybe Text)        -- resolved human-readable name
  , numIdPI    :: !(Maybe Int32)
  , ilvlPI     :: !(Maybe Int32)
  , numFmtPI   :: !(Maybe Text)
  , lvlTextPI  :: !(Maybe Text)
  , insPI      :: ![Text]              -- tracked insert texts
  , delPI      :: ![Text]              -- tracked delete texts
  , commentsPI :: ![A.Value]           -- comment JSON objects (author/date/text)
  } deriving (Eq, Show)

-- Collect per-paragraph info by walking document.xml, and referencing
-- styles.xml, numbering.xml, comments.xml if present.
collectParas :: C.Cursor -> Zp.Archive -> Bool -> IO [ParaInfo]
collectParas root ar withMin =
  let ps = root C.$/ C.child C.&| id
  in do
    stylesMap <- loadStylesMap ar
    numMaps   <- loadNumberingMaps ar
    cmts      <- loadCommentsMap ar
    pure $ catMaybes $ zipWith (onePara stylesMap numMaps cmts) [0..] ps
  where
    onePara styles numMaps cmts i cur =
      case cur.node of
        X.NodeElement e | X.nameLocalName (X.elementName e) == "p" ->
          let t      = T.strip (paraTextHere cur)
              stId   = paraStyleIdHere cur
              stName = stId >>= (`M.lookup` styles)
              (mNum, mLvl) = listMetaHere cur
              (fmtT, lvlT) = resolveNum (mNum, mLvl) numMaps
              insTxt = tracked cur "ins"
              delTxt = tracked cur "del"
              cmJs   = commentsFor cur cmts
          in Just ParaInfo
                { idxPI       = i
                , textPI      = t
                , styleIdPI   = stId
                , styleNamePI = stName
                , numIdPI     = mNum
                , ilvlPI      = mLvl
                , numFmtPI    = fmtT
                , lvlTextPI   = lvlT
                , insPI       = insTxt
                , delPI       = delTxt
                , commentsPI  = cmJs
                }
        _ -> Nothing

    tracked here tag =
      let els = here C.$// C.element (nm wns (T.pack tag)) C.&// C.element (nm wns "t")
      in map (T.concat . C.content) els

    commentsFor here cmtMap =
      -- find commentRangeStart/End and commentReference ids under this paragraph
      let ids1 = here C.$// C.element (nm wns "commentRangeStart") C.&| (fromMaybe "" . listToMaybe . C.attribute (nm wns "id"))
          ids2 = here C.$// C.element (nm wns "commentReference")   C.&| (fromMaybe "" . listToMaybe . C.attribute (nm wns "id"))
          ids  = ids1 ++ ids2
      in mapMaybe (\t -> A.toJSON <$> M.lookup t cmtMap) ids

-- Styles: map styleId -> styleName
loadStylesMap :: Zp.Archive -> IO (M.Map Text Text)
loadStylesMap ar =
  case Zp.findEntryByPath "word/styles.xml" ar of
    Nothing  -> pure mempty
    Just ent -> do
      let doc = X.parseLBS_ X.def (Zp.fromEntry ent)
          cur = C.fromDocument doc
          pairs = cur C.$// C.element (nm wns "style")
             C.&| (\c -> ( listToMaybe (c C.$| C.attribute (nm wns "styleId"))
                      , join (listToMaybe (c C.$/ C.element (nm wns "name") C.&| (listToMaybe . C.attribute (nm wns "val"))))))
      pure $ M.fromList [ (sid, sname) | (Just sid, Just sname) <- pairs ]

-- Numbering: resolve numId+ilvl → (fmt, lvlText)
data NumMaps = NumMaps
  { numIdToAbs :: M.Map Int32 Int32
  , absLvlMap  :: M.Map (Int32, Int32) (Maybe Text, Maybe Text) -- (fmt, lvlText)
  }

loadNumberingMaps :: Zp.Archive -> IO NumMaps
loadNumberingMaps ar =
  case Zp.findEntryByPath "word/numbering.xml" ar of
    Nothing  -> pure (NumMaps mempty mempty)
    Just ent -> do
      let doc = X.parseLBS_ X.def (Zp.fromEntry ent)
          cur = C.fromDocument doc
          -- numId -> abstractNumId
          nums = cur C.$/ C.element (nm wns "num")
          numPairs = [(nid, aid) |
                        n <- nums,
                        let a = listToMaybe (n C.$/ C.element (nm wns "abstractNumId")),
                        Just nid <- [attrInt (nm wns "numId") n],
                        Just aid <- [a >>= attrInt (nm wns "val")]]
          abstrs = cur C.$/ C.element (nm wns "abstractNum")
          lvlPairs =
            [((aid, lvl),
              (fmtNode >>= listToMaybe . C.attribute (nm wns "val"),
               txtNode >>= listToMaybe . C.attribute (nm wns "val"))) |
               a <- abstrs,
               Just aid <- [attrInt (nm wns "abstractNumId") a],
               l <- a C.$/ C.element (nm wns "lvl"),
               let fmtNode = listToMaybe (l C.$/ C.element (nm wns "numFmt")),
               let txtNode = listToMaybe (l C.$/ C.element (nm wns "lvlText")),
               Just lvl <- [attrInt (nm wns "ilvl") l]]
      pure $ NumMaps (M.fromList numPairs) (M.fromList lvlPairs)

resolveNum :: (Maybe Int32, Maybe Int32) -> NumMaps -> (Maybe Text, Maybe Text)
resolveNum (Nothing, _) _ = (Nothing, Nothing)
resolveNum (Just _, Nothing) _ = (Nothing, Nothing)
resolveNum (Just numId, Just ilvl) nmaps =
  case M.lookup numId (numIdToAbs nmaps) of
    Nothing  -> (Nothing, Nothing)
    Just aid -> fromMaybe (Nothing, Nothing) (M.lookup (aid, ilvl) (absLvlMap nmaps))

-- Comments: id -> (author,date,text) as JSON object
loadCommentsMap :: Zp.Archive -> IO (M.Map Text A.Value)
loadCommentsMap ar =
  case Zp.findEntryByPath "word/comments.xml" ar of
    Nothing  -> pure mempty
    Just ent -> do
      let doc = X.parseLBS_ X.def (Zp.fromEntry ent)
          cur = C.fromDocument doc
          cmts = cur C.$/ C.element (nm wns "comments") C.&/ C.element (nm wns "comment")
      pure $ M.fromList $ mapMaybe oneComment cmts
  where
    oneComment :: C.Cursor -> Maybe (Text, A.Value)
    oneComment comment = do
      let
        mbCid = listToMaybe (comment C.$| C.attribute (nm wns "id"))
        auth = listToMaybe (comment C.$| C.attribute (nm wns "author"))
        date = listToMaybe (comment C.$| C.attribute (nm wns "date"))
        textT = T.concat (comment C.$// C.element (nm wns "t") C.&// C.content)

      case mbCid of
        Nothing -> Nothing
        Just cid -> Just (cid, A.object [ "author" A..= auth
                          , "date"   A..= date
                          , "text"   A..= textT ])

-- ---------------------------------------------------------------------
-- Apply ParaInfo back to the imported Doc (sequential mapping)
-- ---------------------------------------------------------------------

applyParaInfoToBlocks :: EnrichmentLevel -> [ParaInfo] -> [Block] -> [Block]
applyParaInfoToBlocks lvl pis0 = fst . go 0 pis0
  where
    go :: Int -> [ParaInfo] -> [Block] -> ([Block], [ParaInfo])
    go _ pis []     = ([], pis)
    go i pis (b:bs) =
      let (b', pis1) = enhance i pis b
          (bs', pis2)= go i pis1 bs
      in (b' : bs', pis2)

    enhance :: Int -> [ParaInfo] -> Block -> (Block, [ParaInfo])
    enhance i pis blk =
      let (kids', pisAfterKids) = go i pis (kidsBlk blk)
      in case kindBlk blk of
           ParagraphBK ->
             case pisAfterKids of
               []     -> (blk { kidsBlk = kids' }, [])
               (p:ps) ->
                 let oa' = mergeOA (attrsBlk blk) (mkOA p)
                     html' = mergeHtml (attrsBlk blk) (mkHtml p)
                     blk' = blk
                       { attrsBlk = Just (oa' { htmlAttrsOA = html' })
                       , kidsBlk  = kids'
                       }
                 in (blk', ps)
           HeadingBK _ ->
             case pisAfterKids of
               []     -> (blk { kidsBlk = kids' }, [])
               (p:ps) ->
                 let oa'   = mergeOA (attrsBlk blk) (mkOA p)
                     html' = mergeHtml (attrsBlk blk) (mkHtml p)
                 in ( blk { attrsBlk = Just (oa' { htmlAttrsOA = html' })
                          , kidsBlk  = kids' }
                    , ps)
           _ ->
             (blk { kidsBlk = kids' }, pisAfterKids)

    mkOA :: ParaInfo -> ObjectAttrs
    mkOA p =
      ObjectAttrs
        { styleNameOA   = styleNamePI p
        , styleIdOA     = styleIdPI p
        , numIdOA       = numIdPI p
        , ilvlOA        = ilvlPI p
        , numFmtOA      = case lvl of
                            EnrichDocxFull -> numFmtPI p
                            _              -> Nothing
        , lvlTextOA     = case lvl of
                            EnrichDocxFull -> lvlTextPI p
                            _              -> Nothing
        , startAtOA     = Nothing
        , captionOA     = Nothing
        , imageRidOA    = Nothing
        , imageNameOA   = Nothing
        , imageKeyOA    = Nothing
        , widthEmuOA    = Nothing
        , heightEmuOA   = Nothing
        , noteIdOA      = Nothing
        , noteTypeOA    = Nothing
        , htmlIdOA      = Nothing
        , htmlClassesOA = []
        , htmlAttrsOA   = mempty
        }

    mkHtml :: ParaInfo -> M.Map Text Text
    mkHtml p =
      let add k v m = if T.null v then m else M.insert k v m
          insJ = if null (insPI p) then Nothing else Just (At.encodeToLazyText (A.toJSON (insPI p)))
          delJ = if null (delPI p) then Nothing else Just (At.encodeToLazyText (A.toJSON (delPI p)))
          cJ   = if null (commentsPI p) then Nothing else Just (At.encodeToLazyText (A.toJSON (commentsPI p)))
      in M.fromList $ catMaybes
           [ fmap (\j -> ("docx:ins", Tl.toStrict j)) insJ
           , fmap (\j -> ("docx:del",  Tl.toStrict j)) delJ
           , fmap (\j -> ("docx:comments", Tl.toStrict j)) cJ
           ]

    mergeOA :: Maybe ObjectAttrs -> ObjectAttrs -> ObjectAttrs
    mergeOA Nothing  b = b
    mergeOA (Just a) b =
      ObjectAttrs
        { styleNameOA   = styleNameOA a   <|> styleNameOA b
        , styleIdOA     = styleIdOA a     <|> styleIdOA b
        , numIdOA       = numIdOA a       <|> numIdOA b
        , ilvlOA        = ilvlOA a        <|> ilvlOA b
        , numFmtOA      = numFmtOA a      <|> numFmtOA b
        , lvlTextOA     = lvlTextOA a     <|> lvlTextOA b
        , startAtOA     = startAtOA a     <|> startAtOA b
        , captionOA     = captionOA a     <|> captionOA b
        , imageRidOA    = imageRidOA a    <|> imageRidOA b
        , imageNameOA   = imageNameOA a   <|> imageNameOA b
        , imageKeyOA    = imageKeyOA a    <|> imageKeyOA b
        , widthEmuOA    = widthEmuOA a    <|> widthEmuOA b
        , heightEmuOA   = heightEmuOA a   <|> heightEmuOA b
        , noteIdOA      = noteIdOA a      <|> noteIdOA b
        , noteTypeOA    = noteTypeOA a    <|> noteTypeOA b
        , htmlIdOA      = htmlIdOA a      <|> htmlIdOA b
        , htmlClassesOA = if null (htmlClassesOA a) then htmlClassesOA b else htmlClassesOA a
        , htmlAttrsOA   = htmlAttrsOA a <> htmlAttrsOA b
        }

    mergeHtml :: Maybe ObjectAttrs -> M.Map Text Text -> M.Map Text Text
    mergeHtml Nothing  b = b
    mergeHtml (Just a) b = htmlAttrsOA a <> b

-- ---------------------------------------------------------------------
-- Notes
-- ---------------------------------------------------------------------
-- • The structural importer here keeps things intentionally conservative.
--   It does not attempt deep list/table shaping – Pandoc remains the canonical
--   structural path. This function is a fallback and returns a flat-ish tree
--   that is still valid for your Block model.
--
-- • The enrichment pass maps paragraph metadata *sequentially* onto
--   ParagraphBK/HeadingBK blocks in pre-order. For most legal docs, this
--   provides stable-enough attachment points. If you want stronger anchoring,
--   extend it to perform fuzzy matching by text hashes on a per-paragraph basis.
--
-- • Comments and tracked changes are attached into 'htmlAttrsOA' as compact JSON
--   strings under keys "docx:comments", "docx:ins", "docx:del". Your DB writers
--   can parse and persist them into dedicated tables if needed.
