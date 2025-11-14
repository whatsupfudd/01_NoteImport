{-# LANGUAGE LambdaCase #-}

-- | Pandoc-based DOCX importer producing the unified HBDoc AST.
--
--   .docx bytes
--     → Pandoc AST
--     → normalized section / paragraph / list / table tree
--     → 'HBDoc.Types.Doc' with hierarchical '[Block]'
--
-- This module is the canonical structural importer for .docx files.
-- It does *not* deal with DOCX-specific tracked changes or comments;
-- those are handled in the XML-based enrichment layer.

module DocX.Pandoc where

import Data.Int                    (Int32)
import qualified Data.ByteArray.Encoding     as BAE
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL
import Data.List                   (intercalate)
import qualified Data.Map.Strict             as M
import Data.Maybe                  (mapMaybe)
import Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE

import Crypto.Hash                 (SHA256, hashWith)

import qualified Text.Megaparsec             as MP
import Text.Megaparsec             (Parsec)
import qualified Text.Megaparsec.Char        as MPC

import qualified Text.Pandoc                 as P
import qualified Text.Pandoc.Definition      as PT

import HBDoc.Types ( Doc(..), Block(..), BlockKind(..), ObjectAttrs(..))

--------------------------------------------------------------------------------
-- Public entry point
--------------------------------------------------------------------------------

-- | Import a DOCX document from lazy bytes into the unified HBDoc 'Doc'.
--
-- Uses Pandoc as the structural spine and does not perform any XML
-- enrichment (tracked changes, comments, etc.).
importDocxPandoc :: BL.ByteString -> IO (Either Text Doc)
importDocxPandoc lbs
  | BL.null lbs = pure (Left "importDocxPandoc: empty ByteString")
  | otherwise = do
      let ro = P.def { P.readerExtensions = P.pandocExtensions }
      ePandoc <- P.runIO (P.readDocx ro lbs)
      case ePandoc of
        Left err ->
          pure (Left (T.pack (show err)))
        Right pandocDoc -> do
          let src  = "<docx-bytes>"
              doc0 = buildDoc True src pandocDoc
              doc1 = assignBlockIds doc0
          pure (Right doc1)

--------------------------------------------------------------------------------
-- Build an HBDoc.Doc from Pandoc
--------------------------------------------------------------------------------

-- | Build an HBDoc 'Doc' from a Pandoc document.
--
-- 'promoteHdrsFlag' controls whether numbered paragraphs like \"1. Foo\"
-- are heuristically promoted to headings when styles are not present.
buildDoc :: Bool -> FilePath -> PT.Pandoc -> Doc
buildDoc promoteHdrsFlag src pandocDoc =
  let
    titleTxt      = extractTitle pandocDoc
    meta          = extractMeta pandocDoc
    PT.Pandoc _ bs = pandocDoc
    built         = bubbleAll (foldPandoc promoteHdrsFlag src bs)
    rootBlk       = getRootFromStack built
    blocksFlat      = kidsBlk rootBlk
    blocksHier      = nestListsUnderPreviousText blocksFlat
    docStableId   = Just (stableId src "doc" (T.unpack titleTxt))
  in
  Doc {
    uidDoc       = Nothing
  , stableIdDoc  = docStableId
  , titleDoc     = titleTxt
  , formatDoc    = Nothing   -- filled by higher-level orchestrator
  , metaDoc      = meta
  , blocksDoc    = blocksHier
  }

-- | Assign simple in-memory uids to blocks in document (1..N).
-- This is purely for convenience; persistent identifiers are handled by DB.
assignBlockIds :: Doc -> Doc
assignBlockIds doc0 =
  let (_, blocks') = assignMany 1 (blocksDoc doc0)
  in doc0 { blocksDoc = blocks' }
  where
    assignMany :: Int32 -> [Block] -> (Int32, [Block])
    assignMany n []     = (n, [])
    assignMany n (b:bs) =
      let (n1, b')  = assignOne n b
          (n2, bs') = assignMany n1 bs
      in (n2, b' : bs')

    assignOne :: Int32 -> Block -> (Int32, Block)
    assignOne n b =
      let (n', kids') = assignMany (n + 1) (kidsBlk b)
          b'          = b { uidBlk = n, kidsBlk = kids' }
      in (n', b')

--------------------------------------------------------------------------------
-- Build state and tree utilities
--------------------------------------------------------------------------------

data Build = Build
  { stackB :: [Block]   -- ^ open blocks; head = current, last = root
  , pathB  :: [Int]     -- ^ ordinal path of current heading (1.2.3)
  , srcB   :: FilePath
  }

emptyBuild :: FilePath -> Build
emptyBuild src =
  let rootId = stableId src "root" ""
      root   = Block
        { uidBlk      = 0
        , stableIdBlk = Just rootId
        , kindBlk     = DocumentBK
        , levelBlk    = Just 0
        , textBlk     = Nothing
        , attrsBlk    = Nothing
        , kidsBlk     = []
        }
  in Build { stackB = [root], pathB = [], srcB = src }

-- | Heading level of a block; non-heading = 0.
levelOf :: Block -> Int
levelOf blk = maybe 0 fromIntegral (levelBlk blk)

-- | Push a block on the stack (without yet attaching as child).
push :: Block -> Build -> Build
push blk b = b { stackB = blk : stackB b }

-- | Attach current top as a child of its parent, then pop it.
bubbleOnce :: Build -> Build
bubbleOnce b =
  case stackB b of
    (child:parent:rest) ->
      let parent' = parent { kidsBlk = kidsBlk parent <> [child] }
      in b { stackB = parent' : rest }
    _ -> b

-- | Close all open levels down to 'target' (keeping top.level < target).
closeToLevel :: Int -> Build -> Build
closeToLevel target b =
  case stackB b of
    (t:_:_) | levelOf t >= target -> closeToLevel target (bubbleOnce b)
    _                             -> b

-- | At end of document, bubble everything into the root.
bubbleAll :: Build -> Build
bubbleAll b =
  case stackB b of
    (_:_:_) -> bubbleAll (bubbleOnce b)
    _       -> b

-- | Run a sub-fold in an isolated container; returns the children it produced,
-- leaving the original parent stack untouched.
pushIsolated :: Build -> (Build -> Build) -> ([Block], Build)
pushIsolated b k =
  let tmpId = stableId (srcB b) (ordinalToString (pathB b)) "tmp"
      tmp   = Block
        { uidBlk      = 0
        , stableIdBlk = Just tmpId
        , kindBlk     = HeadingBK 0
        , levelBlk    = Nothing
        , textBlk     = Nothing
        , attrsBlk    = Nothing
        , kidsBlk     = []
        }
      b1   = push tmp b
      b2   = k b1
  in case stackB b2 of
       (cur:parents) -> (kidsBlk cur, b2 { stackB = parents })
       _             -> ([], b)  -- shouldn't happen

-- | Append child to the top-most block.
appendToTop :: Block -> Build -> Build
appendToTop child b =
  case stackB b of
    []     -> b { stackB = [child] }
    (t:ts) -> b { stackB = (t { kidsBlk = kidsBlk t <> [child] }) : ts }

-- | Retrieve the root block from the stack.
getRootFromStack :: Build -> Block
getRootFromStack b =
  case stackB b of
    [] ->
      let rId = stableId (srcB b) "root" ""
      in Block
           { uidBlk      = 0
           , stableIdBlk = Just rId
           , kindBlk     = DocumentBK
           , levelBlk    = Just 0
           , textBlk     = Nothing
           , attrsBlk    = Nothing
           , kidsBlk     = []
           }
    xs -> last xs

--------------------------------------------------------------------------------
-- Headings
--------------------------------------------------------------------------------

-- | Open a heading/section at level n.
--
-- Implementation:
--   • pop to level n-1 (bubble children into parent),
--   • create a 'HeadingBK' block for this level,
--   • push it on the stack with updated ordinal path.
openSection :: Int -> Text -> PT.Attr -> Build -> Build
openSection lvl title attr b0 =
  let parentLvl = max 0 (lvl - 1)
      b1        = closeToLevel parentLvl b0
      bid       = stableId (srcB b1) (ordinalToString (pathB b1)) (T.unpack title)
      oa        = Just (attrsFromAttr attr)
      sec       = Block
        { uidBlk      = 0
        , stableIdBlk = Just bid
        , kindBlk     = HeadingBK (fromIntegral lvl)
        , levelBlk    = Just (fromIntegral lvl)
        , textBlk     = Just title
        , attrsBlk    = oa
        , kidsBlk     = []
        }
      newPath   = take parentLvl (pathB b1) <> [nextOrdinal lvl b1]
  in b1 { stackB = sec : stackB b1, pathB = newPath }

-- | Determine the next ordinal component for a heading at the given level.
nextOrdinal :: Int -> Build -> Int
nextOrdinal lvl b =
  case compare lvl (length (pathB b)) of
    GT -> 1
    EQ -> case pathB b of
            [] -> 1
            xs -> last xs + 1
    LT -> case safeIndex (lvl - 1) (pathB b) of
            Nothing -> 1
            Just v  -> v + 1

safeIndex :: Int -> [a] -> Maybe a
safeIndex i xs =
  if i < 0 || i >= length xs then Nothing else Just (xs !! i)

ordinalToText :: [Int] -> Text
ordinalToText = T.pack . intercalate "." . map show

ordinalToString :: [Int] -> String
ordinalToString = intercalate "." . map show

--------------------------------------------------------------------------------
-- Text and attributes
--------------------------------------------------------------------------------

-- | Stringify Pandoc inlines (minimal, no formatting markers).
inlineText :: [PT.Inline] -> Text
inlineText = T.strip . T.concat . map go
  where
    go = \case
      PT.Str t         -> t
      PT.Space         -> " "
      PT.SoftBreak     -> " "
      PT.LineBreak     -> " "
      PT.Code _ t      -> t
      PT.Emph xs       -> inlineText xs
      PT.Strong xs     -> inlineText xs
      PT.Underline xs  -> inlineText xs
      PT.SmallCaps xs  -> inlineText xs
      PT.Quoted _ xs   -> inlineText xs
      PT.Span _ xs     -> inlineText xs
      PT.Link _ xs _   -> inlineText xs
      PT.Image _ xs _  -> inlineText xs
      PT.Math _ t      -> t
      PT.RawInline _ t -> t
      PT.Note _        -> ""

-- | Convert Pandoc attributes to 'ObjectAttrs' for HTML-like metadata.
attrsFromAttr :: PT.Attr -> ObjectAttrs
attrsFromAttr (ident, classes, kvs) =
  let htmlId    = if T.null ident then Nothing else Just ident
      htmlAttrs = M.fromList kvs
  in ObjectAttrs
       { styleNameOA    = Nothing
       , styleIdOA      = Nothing
       , numIdOA        = Nothing
       , ilvlOA         = Nothing
       , numFmtOA       = Nothing
       , lvlTextOA      = Nothing
       , startAtOA      = Nothing
       , captionOA      = Nothing
       , imageRidOA     = Nothing
       , imageNameOA    = Nothing
       , imageKeyOA     = Nothing
       , widthEmuOA     = Nothing
       , heightEmuOA    = Nothing
       , noteIdOA       = Nothing
       , noteTypeOA     = Nothing
       , htmlIdOA       = htmlId
       , htmlClassesOA  = classes
       , htmlAttrsOA    = htmlAttrs
       }

nullAttr :: PT.Attr
nullAttr = ("", [], [])

--------------------------------------------------------------------------------
-- Heuristic paragraph promotion (numbered headings)
--------------------------------------------------------------------------------

type PNum = Parsec () Text

pNumHead :: PNum (Int, Text)
pNumHead = do
  _ns <- MP.some MPC.digitChar `MP.sepBy1` MPC.char '.'
  _   <- MP.optional (MPC.char ')')
  MPC.space1
  rest <- T.pack <$> MP.manyTill MP.anySingle MP.eof
  pure (1, rest)  -- we only care that "some numbering" exists; treat as level 1

maybePromotePara :: Bool -> [PT.Inline] -> Maybe (Int, Text)
maybePromotePara allow xs
  | not allow = Nothing
  | otherwise =
      case MP.parse pNumHead "para" (inlineText xs) of
        Left _  -> Nothing
        Right r -> Just r

--------------------------------------------------------------------------------
-- Pandoc block folding (shared)
--------------------------------------------------------------------------------

-- | Fold Pandoc blocks into our 'Build' tree.
foldPandoc :: Bool -> FilePath -> [PT.Block] -> Build
foldPandoc promoteHdrsFlag src =
  loadBlocks promoteHdrsFlag (emptyBuild src)

-- | Process a list of blocks, threading the 'Build' state.
loadBlocks :: Bool -> Build -> [PT.Block] -> Build
loadBlocks _       b []       = b
loadBlocks promote b (x : xs) =
  let b' = loadBlock promote b x
  in loadBlocks promote b' xs

-- | Process a single Pandoc block.
loadBlock :: Bool -> Build -> PT.Block -> Build
loadBlock promote b blk =
  case blk of
    PT.Header lvl attr inl ->
      let title = inlineText inl
      in openSection lvl title attr b

    PT.Para inl ->
      case maybePromotePara promote inl of
        Just (lvl, t) ->
          openSection lvl t nullAttr b
        Nothing ->
          let txt = inlineText inl
              bid = stableId (srcB b) (ordinalToString (pathB b)) (T.unpack (T.take 64 txt))
              p   = Block
                { uidBlk      = 0
                , stableIdBlk = Just bid
                , kindBlk     = ParagraphBK
                , levelBlk    = Nothing
                , textBlk     = Just txt
                , attrsBlk    = Nothing
                , kidsBlk     = []
                }
          in appendToTop p b

    PT.Plain inl ->
      let txt = inlineText inl
          bid = stableId (srcB b) (ordinalToString (pathB b)) (T.unpack (T.take 64 txt))
          p   = Block
            { uidBlk      = 0
            , stableIdBlk = Just bid
            , kindBlk     = ParagraphBK
            , levelBlk    = Nothing
            , textBlk     = Just txt
            , attrsBlk    = Nothing
            , kidsBlk     = []
            }
      in appendToTop p b

    PT.BulletList items ->
      let (b1, lis) = foldList promote b items
          bid       = stableId (srcB b1) (ordinalToString (pathB b1)) "list"
          lst       = Block
            { uidBlk      = 0
            , stableIdBlk = Just bid
            , kindBlk     = ListBK
            , levelBlk    = Nothing
            , textBlk     = Nothing
            , attrsBlk    = Nothing
            , kidsBlk     = lis
            }
      in appendToTop lst b1

    PT.OrderedList _attrs items ->
      let (b1, lis) = foldList promote b items
          bid       = stableId (srcB b1) (ordinalToString (pathB b1)) "olist"
          lst       = Block
            { uidBlk      = 0
            , stableIdBlk = Just bid
            , kindBlk     = ListBK
            , levelBlk    = Nothing
            , textBlk     = Nothing
            , attrsBlk    = Nothing
            , kidsBlk     = lis
            }
      in appendToTop lst b1

    PT.BlockQuote inner ->
      let (kids, b1) = pushIsolated b (\b0 -> loadBlocks promote b0 inner)
          bid        = stableId (srcB b1) (ordinalToString (pathB b1)) "quote"
          q          = Block
            { uidBlk      = 0
            , stableIdBlk = Just bid
            , kindBlk     = QuoteBK
            , levelBlk    = Nothing
            , textBlk     = Nothing
            , attrsBlk    = Nothing
            , kidsBlk     = kids
            }
      in appendToTop q b1

    PT.CodeBlock attr txt ->
      let bid = stableId (srcB b) (ordinalToString (pathB b)) (T.unpack (T.take 48 txt))
          oa  = Just (attrsFromAttr attr)
          cb  = Block
            { uidBlk      = 0
            , stableIdBlk = Just bid
            , kindBlk     = CodeBK
            , levelBlk    = Nothing
            , textBlk     = Just txt
            , attrsBlk    = oa
            , kidsBlk     = []
            }
      in appendToTop cb b

    PT.HorizontalRule ->
      let bid = stableId (srcB b) (ordinalToString (pathB b)) "hr"
          hr  = Block
            { uidBlk      = 0
            , stableIdBlk = Just bid
            , kindBlk     = RuleBK
            , levelBlk    = Nothing
            , textBlk     = Nothing
            , attrsBlk    = Nothing
            , kidsBlk     = []
            }
      in appendToTop hr b

    PT.Table {} ->
      -- For now we treat tables as opaque blocks with a simple preview;
      -- detailed table-to-Block mapping can be added later if needed.
      let preview = "[table]"
          bid     = stableId (srcB b) (ordinalToString (pathB b)) "table"
          tbl     = Block
            { uidBlk      = 0
            , stableIdBlk = Just bid
            , kindBlk     = TableBK
            , levelBlk    = Nothing
            , textBlk     = Just preview
            , attrsBlk    = Nothing
            , kidsBlk     = []
            }
      in appendToTop tbl b

    PT.Div _ inner ->
      loadBlocks promote b inner

    PT.RawBlock _ _ ->
      b

--------------------------------------------------------------------------------
-- List folding
--------------------------------------------------------------------------------

-- | Convert each list item ([[Block]]) into a 'ListItemBK' subtree.
foldList :: Bool -> Build -> [[PT.Block]] -> (Build, [Block])
foldList _ b [] = (b, [])
foldList promote b (x:xs) =
  let
    (kids, b1) = pushIsolated b (\b0 -> loadBlocks promote b0 x)
    liBid = stableId (srcB b1) (ordinalToString (pathB b1)) "li"
    li = Block {
          uidBlk      = 0
        , stableIdBlk = Just liBid
        , kindBlk     = ListItemBK 0
        , levelBlk    = Nothing
        , textBlk     = Nothing
        , attrsBlk    = Nothing
        , kidsBlk     = kids
        }
    (b2, restLis) = foldList promote b1 xs
  in
  (b2, li : restLis)


--------------------------------------------------------------------------------
-- “List under preceding paragraph” post-pass
--------------------------------------------------------------------------------

-- | Post-pass that organizes lists as children of the preceding text block.
--
-- For each siblings list:
--   • If we see a 'ListBK' and the *immediately preceding* sibling is a
--     'ParagraphBK' or 'HeadingBK _', we move that 'ListBK' into the host’s
--     'kidsBlk'.
--   • Otherwise the list remains at the current level.
--
-- This is applied to the top-level blocks and recursively to all children,
-- so the same rule holds inside sections, tables, list items, etc.
nestListsUnderPreviousText :: [Block] -> [Block]
nestListsUnderPreviousText = nestFold []
  where
    nestFold :: [Block] -> [Block] -> [Block]
    nestFold acc []         = reverse acc
    nestFold acc (b:rest) =
      let b' = b { kidsBlk = nestListsUnderPreviousText (kidsBlk b) }
      in case kindBlk b' of
           ListBK ->
             case acc of
               host:accRest | isTextHost host ->
                 let host' = host { kidsBlk = kidsBlk host ++ [b'] }
                 in nestFold (host':accRest) rest
               _ ->
                 nestFold (b':acc) rest
           _ ->
             nestFold (b':acc) rest

    isTextHost :: Block -> Bool
    isTextHost blk =
      case kindBlk blk of
        ParagraphBK   -> True
        HeadingBK _   -> True
        _             -> False


--------------------------------------------------------------------------------
-- Metadata helpers
--------------------------------------------------------------------------------

extractTitle :: PT.Pandoc -> Text
extractTitle (PT.Pandoc meta _) =
  case P.lookupMeta "title" meta of
    Just (PT.MetaString t) -> t
    Just (PT.MetaInlines is) -> inlineText is
    _ -> ""

extractMeta :: PT.Pandoc -> M.Map Text Text
extractMeta (PT.Pandoc meta _) =
  let pairs =
        [ ("title",  metaToText (P.lookupMeta "title"  meta))
        , ("author", metaToText (P.lookupMeta "author" meta))
        , ("date",   metaToText (P.lookupMeta "date"   meta))
        ]
  in M.fromList (mapMaybe toKV pairs)
  where
    toKV (k, Just v) | not (T.null v) = Just (k, v)
    toKV _                            = Nothing

    metaToText :: Maybe PT.MetaValue -> Maybe Text
    metaToText = \case
      Nothing                   -> Nothing
      Just (PT.MetaString s)    -> Just s
      Just (PT.MetaInlines is)  -> Just (inlineText is)
      Just (PT.MetaList xs)     -> Just (T.intercalate ", " (map metaVal xs))
      _                         -> Nothing

    metaVal :: PT.MetaValue -> Text
    metaVal = \case
      PT.MetaString s   -> s
      PT.MetaInlines is -> inlineText is
      _                 -> ""

--------------------------------------------------------------------------------
-- Stable IDs
--------------------------------------------------------------------------------

stableId :: FilePath -> String -> String -> Text
stableId src key hint =
  let base   = TE.encodeUtf8 (T.pack (src <> "|" <> key <> "|" <> take 80 hint))
      digest = hashWith (undefined :: SHA256) base
      hex    = BAE.convertToBase BAE.Base16 digest :: BS.ByteString
  in T.take 12 (TE.decodeUtf8 hex)
