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
emitYaml mb d = do
  -- keep JSON by default; plug in Data.Yaml if the team wants native YAML
  emitJson mb d


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

data Build = Build
  { stack :: [Block]     -- root .. current section (last is top)
  , path  :: [Int]       -- section ordinal path (1-based per level)
  , src   :: FilePath
  }

emptyBuild :: FilePath -> Build
emptyBuild fp = Build { stack = [rootBlock (stableId fp "root" "")], path = [], src = fp }

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

-- Open a section at level n => pop to level n-1, append, push
openSection :: Int -> Text -> M.Map Text Text -> Build -> Build
openSection lvl title attrs b =
  let (kept, _) = span (\blk -> fromMaybe 0 blk.levelB < lvl) (reverse b.stack)
      base = case reverse kept of
        []     -> [rootBlock (stableId b.src "root" (T.unpack title))]
        xs     -> xs
      -- compute new ordinal path
      newPath = take (lvl - 1) b.path <> [nextOrdinal lvl b]
      bid = stableId b.src (ordinalToString newPath) (T.unpack title)
      sec = mkSection lvl title attrs bid
  in Build { stack = sec : base, path = newPath, src = b.src }

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
        bid = stableId b'.src (ordinalToString b'.path) "list"
        lst = mkList bid liBlocks
    in loadBlocks (appendToTop lst b') rest

  PT.OrderedList _ items ->
    let (b', liBlocks) = foldList b items
        bid = stableId b'.src (ordinalToString b'.path) "olist"
        lst = mkList bid liBlocks
    in loadBlocks (appendToTop lst b') rest

  PT.BlockQuote inner ->
    let b' = loadBlocks b inner
        bid = stableId b'.src (ordinalToString b'.path) "quote"
        -- gather nodes appended during 'inner' and wrap them
        (children, parent') = popChildrenFromTop b'
        q = mkQuote bid children
    in loadBlocks (appendToTop q parent') rest

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
foldList b0 [] = (b0, [])
foldList b0 (x:xs) =
  let bid = stableId b0.src (ordinalToString b0.path) "li"
      -- build in an isolated scope: push a synthetic container
      container = Block { idB = bid, kindB = BkListItem, levelB = Nothing, textB = Nothing, attrsB = mempty, childrenB = [] }
      b1 = replaceTop (top b0) b0
      b2 = appendToTop container b1
      b3 = loadBlocks b2 x
      (kids, parentAfter) = popChildrenFromTop b3
      li = mkListItem bid kids
      (bNext, restLis) = foldList parentAfter xs
  in (bNext, li : restLis)

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
      built = foldPandoc promoteHdrs fp blocks
      root = case built.stack of
        []    -> rootBlock (stableId fp "root" (T.unpack titleTxt))
        (r:_) -> r
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
