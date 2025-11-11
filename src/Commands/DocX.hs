{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

{-
Docx→Blocks — convert .docx to a hierarchical block tree for our HBDS (Hierarchical Block Document System)
---------------------------------------------------------------------------------
First draft v0.1 (for CTO/dev team review)

Build notes (recommended):
  • GHC ≥ 9.6
  • cabal.project uses pandoc 3.x; if you’re pinned to 2.x, see the API compatibility notes below.

Quick start (local):
  cabal run docx-to-blocks -- --in From\ age_grade\ to\ mastery-driven\ learning\ journeys.docx --out out.json

Executable target name: docx-to-blocks

API compatibility (Pandoc 2.x vs 3.x):
  • This file targets Pandoc 3.x (Text.Pandoc qualified API). If you need 2.x, replace the imports of
    Text.Pandoc (runIO, runIOorExplode, def, ReaderOptions, readDocx) accordingly and adjust 'Table' matches.

Design choices (high level):
  • Parse .docx → Pandoc AST → normalize → assemble a clean Section tree using Header levels.
  • Paragraphs, lists, list items, tables, quotes, code blocks, figures become children under the nearest header.
  • Optional promotion: numbered lines like "1) Foo" / "1.2 Bar" can be heuristically promoted to headers (Megaparsec).
  • Stable IDs: SHA-256 of (path + logical path + text preview) → base16 prefix (8–12 chars) for deterministic IDs.
  • Records use short postfixes (D,B,I) and OverloadedRecordDot. No RecordWildCards.

Output: JSON — a single root Doc with nested Blocks, suitable for direct ingestion by our HBDS service/UI.

-}

module Commands.DocX where

import Control.Monad (foldM)
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum, isSpace)
import Data.Foldable (toList)
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as S
import Data.String (fromString)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.Time (getCurrentTime)
import GHC.Generics (Generic)
import Options.Applicative

import qualified Data.Aeson as Ae
import qualified Data.Aeson.Encode.Pretty as AeP
import Data.Aeson ((.=))

import Data.Yaml (encodeFile)
import qualified Data.Yaml as Y
import qualified Data.Yaml.Pretty as YP

import Crypto.Hash (hashWith, SHA256(..))

import qualified Text.Megaparsec as MP

import Text.Megaparsec (Parsec, (<|>))
import qualified Text.Megaparsec.Char as MPC

import qualified Text.Pandoc as P
import qualified Text.Pandoc.Definition as PT
import qualified Text.Pandoc.Walk as PW

import qualified Options.Runtime as Rt
import qualified Options.Cli as Opt
import Control.Exception (catch)


loadDoc :: Opt.DocXOpts -> Rt.RunOptions -> IO ()
loadDoc opts rtOpts = do
  eDoc <- readDocxPandoc opts.inPath
  case eDoc of
    Left err -> fail ("Failed to read DOCX: " <> err)
    Right pandocDoc -> do
      let d = buildDoc opts.promote opts.inPath pandocDoc
      (if opts.asYaml then emitYaml opts.outPath d else emitJson opts.outPath d)


emitJson :: Maybe FilePath -> Doc -> IO ()
emitJson mb d = case mb of
  Nothing -> BL.putStr (AeP.encodePretty' cfg d)
  Just fp -> BL.writeFile fp (AeP.encodePretty' cfg d)
  where
    cfg = AeP.defConfig { AeP.confCompare = AeP.compare }

emitYaml :: Maybe FilePath -> Doc -> IO ()
emitYaml mb doc = do
  let val = Ae.toJSON doc
      cfg = YP.setConfDropNull True YP.defConfig
      bs  = YP.encodePretty cfg val   -- strict ByteString
  case mb of
    Nothing -> BS.putStr bs
    Just fp -> BS.writeFile fp bs


--------------------------------------------------------------------------------
-- Core model (JSON schema)
--------------------------------------------------------------------------------

data Doc = Doc
  { idD      :: Text
  , titleD   :: Text
  , metaD    :: M.Map Text Text
  , blocksD  :: [Block]
  } deriving (Show, Generic, Ae.ToJSON)

-- BlockKind captures the semantics/UI affordances downstream

data BlockKind
  = BkRoot | BkTitle | BkSection | BkParagraph | BkList | BkListItem
  | BkTable | BkFigure | BkCode | BkQuote | BkRule
  deriving (Show, Read, Eq, Ord, Generic, Ae.ToJSON)

-- Block stores tree shape and lightweight content/attrs

data Block = Block
  { idB       :: Text
  , kindB     :: BlockKind
  , levelB    :: Maybe Int         -- section level if applicable
  , textB     :: Maybe Text        -- plain/markdown preview
  , attrsB    :: M.Map Text Text   -- style classes, ids, key/vals
  , childrenB :: [Block]
  } deriving (Show, Generic, Ae.ToJSON)

rootBlock :: Text -> Block
rootBlock did = Block { idB = did, kindB = BkRoot, levelB = Just 0, textB = Nothing, attrsB = mempty, childrenB = [] }

mkSection :: Int -> Text -> M.Map Text Text -> Text -> Block
mkSection lvl txt attrs bid = Block { idB = bid, kindB = BkSection, levelB = Just lvl, textB = Just txt, attrsB = attrs, childrenB = [] }

mkPara :: Text -> Text -> Block
mkPara bid t = Block { idB = bid, kindB = BkParagraph, levelB = Nothing, textB = Just t, attrsB = mempty, childrenB = [] }

mkList :: Text -> [Block] -> Block
mkList bid items = Block { idB = bid, kindB = BkList, levelB = Nothing, textB = Nothing, attrsB = mempty, childrenB = items }

mkListItem :: Text -> [Block] -> Block
mkListItem bid kids = Block { idB = bid, kindB = BkListItem, levelB = Nothing, textB = Nothing, attrsB = mempty, childrenB = kids }

mkCode :: Text -> Text -> Block
mkCode bid t = Block { idB = bid, kindB = BkCode, levelB = Nothing, textB = Just t, attrsB = mempty, childrenB = [] }

mkQuote :: Text -> [Block] -> Block
mkQuote bid kids = Block { idB = bid, kindB = BkQuote, levelB = Nothing, textB = Nothing, attrsB = mempty, childrenB = kids }

mkRule :: Text -> Block
mkRule bid = Block { idB = bid, kindB = BkRule, levelB = Nothing, textB = Nothing, attrsB = mempty, childrenB = [] }

mkTable :: Text -> Text -> Block
mkTable bid preview = Block { idB = bid, kindB = BkTable, levelB = Nothing, textB = Just preview, attrsB = mempty, childrenB = [] }

mkFigure :: Text -> Text -> Block
mkFigure bid caption = Block { idB = bid, kindB = BkFigure, levelB = Nothing, textB = Just caption, attrsB = mempty, childrenB = [] }

--------------------------------------------------------------------------------
-- Pandoc → Blocks transformation
--------------------------------------------------------------------------------
-- Build-time stack invariant:
--   • stack !! 0 is the CURRENT TOP (deepest) section
--   • last stack item is the ROOT
--   • path is the current section ordinal path (top's path)
data Build = Build
  { stack :: [Block]
  , path  :: [Int]
  , src   :: FilePath
  }


emptyBuild :: FilePath -> Build
emptyBuild fp = Build { stack = [rootBlock (stableId fp "root" "")], path = [], src = fp }


-- Treat stack as [currentTop, parent, ..., root]; root is last.
levelOf :: Block -> Int
levelOf blk = maybe 0 id blk.levelB

push :: Block -> Build -> Build
push blk b = b { stack = blk : b.stack }

-- Attach current top as a child of its parent, then pop it.
bubbleOnce :: Build -> Build
bubbleOnce b = case b.stack of
  (child:parent:rest) ->
    let parent' = parent { childrenB = parent.childrenB <> [child] }
    in b { stack = parent' : rest }
  _ -> b

-- Close all open levels down to target (keep top.levelB < target)
closeToLevel :: Int -> Build -> Build
closeToLevel target b = case b.stack of
  (t:_:_) | levelOf t >= target -> closeToLevel target (bubbleOnce b)
  _                             -> b

-- At end of document, bubble everything into the root
bubbleAll :: Build -> Build
bubbleAll b = case b.stack of
  (_:_:_) -> bubbleAll (bubbleOnce b)
  _       -> b

-- Run a sub-fold in an isolated container; returns the children it produced,
-- leaving the original parent stack untouched.
pushIsolated :: Build -> (Build -> Build) -> ([Block], Build)
pushIsolated b k =
  let tmp = Block { idB       = stableId b.src (ordinalToString b.path) "tmp"
                  , kindB     = BkSection
                  , levelB    = Nothing
                  , textB     = Nothing
                  , attrsB    = mempty
                  , childrenB = [] }
      b1 = push tmp b
      b2 = k b1
  in case b2.stack of
       (cur:parents) -> (cur.childrenB, b2 { stack = parents })
       _             -> ([], b) -- shouldn't happen


-- Push child under the current top of the stack
pushChild :: Block -> Build -> Build
pushChild child b = case b.stack of
  []      -> b { stack = [child] }
  (t:ts)  -> b { stack = (t { childrenB = t.childrenB <> [child] }) : ts }

-- Replace top of stack
replaceTop :: Block -> Build -> Build
replaceTop newTop b = case b.stack of
  []     -> b { stack = [newTop] }
  (_:ts) -> b { stack = newTop : ts }

-- Append child to the top-most block (section), returning updated structure
appendToTop :: Block -> Build -> Build
appendToTop child b = case b.stack of
  []     -> b { stack = [child] }
  (t:ts) -> b { stack = (t { childrenB = t.childrenB <> [child] }) : ts }


getRootFromStack :: Build -> Block
getRootFromStack b = case b.stack of
  [] -> rootBlock (stableId b.src "root" "")
  xs -> last xs


-- Open a section at level n => pop to level n-1, append, push
openSection :: Int -> Text -> M.Map Text Text -> Build -> Build
openSection lvl title attrs b0 =
  let parentLvl = max 0 (lvl - 1)
      b1        = closeToLevel parentLvl b0              -- bubble down to the right parent
      bid       = stableId b1.src (ordinalToString b1.path) (T.unpack title)
      sec       = mkSection lvl title attrs bid
      newPath   = take parentLvl b1.path <> [nextOrdinal lvl b1]
  in push sec b1 { path = newPath }                       -- push, do NOT append to parent yet


nextOrdinal :: Int -> Build -> Int
nextOrdinal lvl b =
  case compare lvl (length b.path) of
    GT -> 1
    EQ -> case b.path of
            [] -> 1
            xs -> last xs + 1
    LT -> fromMaybe 1 (safeIndex (lvl-1) b.path) + 1

safeIndex :: Int -> [a] -> Maybe a
safeIndex i xs = if i < 0 || i >= length xs then Nothing else Just (xs !! i)

ordinalToText :: [Int] -> Text
ordinalToText = T.pack . intercalate "." . map show

ordinalToString :: [Int] -> String
ordinalToString = intercalate "." . map show

-- Stringify inlines (minimal, no formatting markers)
inlineText :: [PT.Inline] -> Text
inlineText = T.strip . T.concat . map go
  where
    go = \case
      PT.Str t        -> t
      PT.Space        -> " "
      PT.SoftBreak    -> " "
      PT.LineBreak    -> " "
      PT.Code _ t     -> t
      PT.Emph xs      -> inlineText xs
      PT.Strong xs    -> inlineText xs
      PT.Underline xs -> inlineText xs
      PT.SmallCaps xs -> inlineText xs
      PT.Quoted _ xs  -> inlineText xs
      PT.Span _ xs    -> inlineText xs
      PT.Link _ xs _  -> inlineText xs
      PT.Image _ xs _ -> inlineText xs
      PT.Math _ t     -> t
      PT.RawInline _ t-> t
      PT.Note _       -> ""

attrMap :: PT.Attr -> M.Map Text Text
attrMap (ident, classes, kvs) =
  let m1 = if T.null ident then mempty else M.singleton "id" ident
      m2 = if null classes then m1 else M.insert "class" (T.intercalate " " classes) m1
  in foldr (\(k,v) acc -> M.insert k v acc) m2 kvs

-- Heuristic: treat numbered prefix as heading when styles are lost (optional)
-- Examples: "1) Foo" → level 1; "2.3  Bar" → level 2

type P = Parsec () Text

pNumHead :: P (Int, Text)
pNumHead = do
  ns <- MP.some MPC.digitChar `MP.sepBy1` MPC.char '.'
  _  <- MP.optional (MPC.char ')')
  MPC.space1
  rest <- T.pack <$> MP.manyTill MP.anySingle MP.eof
  pure (length ns, rest)

maybePromotePara :: Bool -> [PT.Inline] -> Maybe (Int, Text)
maybePromotePara allow xs
  | not allow = Nothing
  | otherwise = case MP.parse pNumHead "para" (inlineText xs) of
      Left _  -> Nothing
      Right r -> Just r

-- Fold Pandoc blocks into our tree
foldPandoc :: Bool -> FilePath -> [PT.Block] -> Build
foldPandoc promoteHdrsFlag fp = loadBlocks (emptyBuild fp)

promoteHdrs :: Bool
promoteHdrs = True

loadBlocks :: Build -> [PT.Block] -> Build
loadBlocks b [] = b
loadBlocks b (blk:rest) = case blk of
  PT.Header lvl attr inl ->
    let title = inlineText inl
        attrs = attrMap attr
        b' = openSection lvl title attrs b
    in loadBlocks b' rest

  PT.Para inl -> case maybePromotePara promoteHdrs inl of
    Just (lvl, t) -> loadBlocks (openSection lvl t mempty b) rest
    Nothing       ->
      let bid = stableId b.src (ordinalToString b.path) (T.unpack (T.take 64 (inlineText inl)))
          p   = mkPara bid (inlineText inl)
      in loadBlocks (appendToTop p b) rest

  PT.Plain inl ->
    let bid = stableId b.src (ordinalToString b.path) (T.unpack (T.take 64 (inlineText inl)))
        p   = mkPara bid (inlineText inl)
    in loadBlocks (appendToTop p b) rest

  PT.BulletList items ->
    let (b', liBlocks) = foldList b items
        bid            = stableId b'.src (ordinalToString b'.path) "list"
        lst            = mkList bid liBlocks
    in loadBlocks (appendToTop lst b') rest

  PT.OrderedList _ items ->
    let (b', liBlocks) = foldList b items
        bid            = stableId b'.src (ordinalToString b'.path) "olist"
        lst            = mkList bid liBlocks
    in loadBlocks (appendToTop lst b') rest

  PT.BlockQuote inner ->
    let (kids, b1) = pushIsolated b (\b0 -> loadBlocks b0 inner)
        bid        = stableId b1.src (ordinalToString b1.path) "quote"
        q          = mkQuote bid kids
    in loadBlocks (appendToTop q b1) rest

  PT.CodeBlock _ txt ->
    let bid = stableId b.src (ordinalToString b.path) (T.unpack (T.take 48 txt))
    in loadBlocks (appendToTop (mkCode bid txt) b) rest

  PT.HorizontalRule -> loadBlocks (appendToTop (mkRule (stableId b.src (ordinalToString b.path) "hr")) b) rest

  PT.Table {} ->
    let preview = "[table]"
        bid = stableId b.src (ordinalToString b.path) "table"
    in loadBlocks (appendToTop (mkTable bid preview) b) rest

  PT.Div _ inner -> loadBlocks (loadBlocks b inner) rest
  PT.RawBlock _ _ -> loadBlocks b rest

  -- PT.Null -> go b rest -- No Null Block defined in Pandoc

-- Convert each list item ([[Block]]) into a BkListItem subtree
foldList :: Build -> [[PT.Block]] -> (Build, [Block])
foldList b0 []     = (b0, [])
foldList b0 (x:xs) =
  let (kids, b1)     = pushIsolated b0 (\b -> loadBlocks b x)
      liBid          = stableId b1.src (ordinalToString b1.path) "li"
      li             = mkListItem liBid kids
      (b2, restLis)  = foldList b1 xs
  in (b2, li : restLis)


top :: Build -> Block
top b = case b.stack of
  []    -> rootBlock (stableId b.src "root" "")
  (t:_) -> t

popChildrenFromTop :: Build -> ([Block], Build)
popChildrenFromTop b = case b.stack of
  []     -> ([], b)
  (t:ts) -> (t.childrenB, Build { stack = (t { childrenB = [] }) : ts, path = b.path, src = b.src })

--------------------------------------------------------------------------------
-- DOCX reading (Pandoc)
--------------------------------------------------------------------------------

readDocxPandoc :: FilePath -> IO (Either String PT.Pandoc)
readDocxPandoc fp = do
  lbs <- BL.readFile fp
  let ro = P.def { P.readerExtensions = P.pandocExtensions }
  res <- P.runIO $ P.readDocx ro lbs
  P.handleError res >>= \doc -> pure (Right doc)
    `catchIO` (\e -> pure (Left (show e)))
  where
    catchIO :: IO a -> (IOError -> IO a) -> IO a
    catchIO = catch

--------------------------------------------------------------------------------
-- Stable IDs
--------------------------------------------------------------------------------

stableId :: FilePath -> String -> String -> Text
stableId src key hint =
  let base = TE.encodeUtf8 (T.pack (src <> "|" <> key <> "|" <> take 80 hint))
      digest = hashWith SHA256 base
      hex = BAE.convertToBase BAE.Base16 digest :: BS.ByteString
  in T.take 12 (TE.decodeUtf8 hex)

--------------------------------------------------------------------------------
-- Top-level pipeline
--------------------------------------------------------------------------------

buildDoc :: Bool -> FilePath -> PT.Pandoc -> Doc
buildDoc promoteHdrs fp pandocDoc =
  let titleTxt = extractTitle pandocDoc
      meta = extractMeta pandocDoc
      PT.Pandoc _ blocks = pandocDoc
      built = bubbleAll (foldPandoc promoteHdrs fp blocks)  -- important
      root = case built.stack of
        []    -> rootBlock (stableId fp "root" (T.unpack titleTxt))
        (r:_) -> last built.stack
      docId = stableId fp "doc" (T.unpack titleTxt)
  in Doc { idD = docId, titleD = titleTxt, metaD = meta, blocksD = root.childrenB }

extractTitle :: PT.Pandoc -> Text
extractTitle (PT.Pandoc meta _) =
  case P.lookupMeta "title" meta of
    Just (PT.MetaString t) -> t
    Just (PT.MetaInlines ils) -> inlineText ils
    _ -> ""

extractMeta :: PT.Pandoc -> M.Map Text Text
extractMeta (PT.Pandoc meta _) =
  let pairs =
        [ ("title", metaToText (P.lookupMeta "title" meta))
        , ("author", metaToText (P.lookupMeta "author" meta))
        , ("date", metaToText (P.lookupMeta "date" meta))
        ]
  in M.fromList (mapMaybe toKV pairs)
  where
    toKV (k, Just v) | not (T.null v) = Just (k, v)
    toKV _ = Nothing
    metaToText = \case
      Nothing -> Nothing
      Just (PT.MetaString s) -> Just s
      Just (PT.MetaInlines ils) -> Just (inlineText ils)
      Just (PT.MetaList xs) -> Just (T.intercalate ", " (map metaVal xs))
      _ -> Nothing
    metaVal = \case
      PT.MetaString s -> s
      PT.MetaInlines ils -> inlineText ils
      _ -> ""

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Notes for reviewers / planned next steps
--------------------------------------------------------------------------------
{-
• Tables & figures: we currently emit lightweight previews. Next step is to expand Pandoc Table into a structured representation (headers, rows, captions) and attach as children or attrsB.

• Images: Pandoc keeps images as references; for HBDS we likely want a media import hook (copy asset + assign media id, then emit a BkFigure with that id in attrsB). We can thread an AssetSink in Build to accumulate.

• Header promotion: 'promote-numbered' turns paragraphs like "4.1 Forward planning" into proper sections if Word styles weren’t applied.

• Reordering affordances: the JSON tree (Block.childrenB) is directly compatible with our UI drag/drop. Server can persist with our CoW snapshot model.

• IDs & pathing: stableId uses (file|ordinal path|text preview). Moving a subtree changes downstream ids; we can alternatively store separate stable path keys in attrsB if we want ids to remain invariant across reorders.

• Pandoc 2.x support: swap imports to Text.Pandoc (readDocx :: ReaderOptions -> ByteString -> Pandoc) and adjust 'Table' pattern if needed.

• Validation tests: add golden tests using small synthetic docx files (H1/Para/List/Table) to ensure tree shape matches expectations.
-}
