{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- | Unified in-memory document model for import pipelines.
--
-- This module is the canonical AST for “document as a tree of blocks”
-- used by:
--
--   • Pandoc-based importers (Docx/HTML/Markdown, etc.)
--   • DOCX-XML-based enrichment (tracked changes, comments, styles)
--
-- All ingestion flows should end up producing a 'Doc' value from here.
--
-- Mapping to DB:
--   • This is *import-time* structure; DB persistence remains in
--     KMS.Types / SQL (blocks_bd) with a separate mapping layer.

module HBDoc.Types where

import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Text (Text)

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)


-- | In-memory document produced by an import pipeline.
--
-- 'uidDoc' can be filled once the document is attached to a DB row;
-- 'stableIdDoc' is a deterministic textual id (e.g. SHA-256 prefix
-- as used in the Pandoc-based importer).
data Doc = Doc
  { uidDoc       :: !(Maybe Int32)        -- ^ DB document.uid, if known
  , stableIdDoc  :: !(Maybe Text)         -- ^ stable import id (e.g. hex prefix)
  , titleDoc     :: !Text                 -- ^ logical title (from Pandoc meta or DOCX styles)
  , formatDoc    :: !(Maybe Text)         -- ^ "docx" | "html" | "markdown" | ...
  , metaDoc      :: !(Map Text Text)      -- ^ arbitrary document-level metadata
  , blocksDoc    :: ![Block]              -- ^ top-level blocks (root children)
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Semantic kind of a block.
--
-- This is a superset of both:
--   • the Pandoc-based BlockKind (BkSection, BkParagraph, BkList, ...)
--   • the existing KMS.Types BlockKind (DocumentBK, HeadingBK, ParagraphBK, ...)
--
-- The idea is to keep the *core* kinds aligned with what we already
-- persist in 'blocks_bd', while allowing Pandoc-specific constructs
-- (list container, figure, quote, rule, code) to map cleanly.
data BlockKind =
    DocumentBK                -- ^ synthetic document root (optional)
  | HeadingBK Int32           -- ^ heading level (1..N)
  | ParagraphBK               -- ^ plain paragraph
  | RunBK                     -- ^ inline run (rarely used in final AST)
  | ListBK                    -- ^ list container (for Pandoc lists)
  | ListItemBK Int32          -- ^ list item, with nesting level (0..N)
  | TableBK                   -- ^ table node
  | TableRowBK                -- ^ table row
  | TableCellBK               -- ^ table cell
  | ImageBK                   -- ^ image (figure without caption)
  | FigureBK                  -- ^ figure/image with caption or richer attrs
  | CodeBK                    -- ^ code block
  | QuoteBK                   -- ^ block quote
  | RuleBK                    -- ^ horizontal rule
  | FootnoteBK                -- ^ footnote content (or reference wrapper)
  | EndnoteBK                 -- ^ endnote content (or reference wrapper)
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | A block in the hierarchical document tree.
--
-- This is the unified node type that both Pandoc-based and DOCX-XML-
-- based pipelines should produce.
data Block = Block
  { uidBlk      :: !Int32                 -- ^ simple incremental id within imported doc
  , stableIdBlk :: !(Maybe Text)          -- ^ optional stable textual id (e.g. hash)
  , kindBlk     :: !BlockKind             -- ^ semantic kind
  , levelBlk    :: !(Maybe Int32)         -- ^ heading level or list level (when useful)
  , textBlk     :: !(Maybe Text)          -- ^ normalized textual content / preview
  , attrsBlk    :: !(Maybe ObjectAttrs)   -- ^ style/numbering/media/HTML attrs
  , kidsBlk     :: ![Block]               -- ^ children (subsections, list items, cells, ...)
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Extra attributes for a block.
--
-- This merges:
--   • DOCX-specific attrs (style id/name, numbering, images, notes)
--   • HTML/Markdown/Pandoc attrs (id, classes, arbitrary key/vals)
--
-- Everything optional; importers fill whatever they know.
data ObjectAttrs = ObjectAttrs
  { styleNameOA    :: !(Maybe Text)         -- ^ human-readable style name (DOCX)
  , styleIdOA      :: !(Maybe Text)         -- ^ style id (DOCX w:pStyle/@w:val)
  , numIdOA        :: !(Maybe Int32)        -- ^ numbering id (w:numId)
  , ilvlOA         :: !(Maybe Int32)        -- ^ list level (w:ilvl)
  , numFmtOA       :: !(Maybe Text)         -- ^ numbering format (e.g. "decimal", "bullet")
  , lvlTextOA      :: !(Maybe Text)         -- ^ level text pattern (e.g. "%1.%2.")
  , startAtOA      :: !(Maybe Int32)        -- ^ list start at
  , captionOA      :: !(Maybe Text)         -- ^ caption text (figures/tables/images)
  , imageRidOA     :: !(Maybe Text)         -- ^ DOCX relationship id for image
  , imageNameOA    :: !(Maybe Text)         -- ^ image original filename
  , imageKeyOA     :: !(Maybe Text)         -- ^ object-store key for binary asset
  , widthEmuOA     :: !(Maybe Int32)        -- ^ width in EMUs (DOCX wp:extent/@cx)
  , heightEmuOA    :: !(Maybe Int32)        -- ^ height in EMUs (DOCX wp:extent/@cy)
  , noteIdOA       :: !(Maybe Int32)        -- ^ footnote/endnote id
  , noteTypeOA     :: !(Maybe Text)         -- ^ "footnote" | "endnote"
  , htmlIdOA       :: !(Maybe Text)         -- ^ HTML/Pandoc element id
  , htmlClassesOA  :: ![Text]               -- ^ HTML/Pandoc classes
  , htmlAttrsOA    :: !(Map Text Text)      -- ^ arbitrary key/value attrs (data-*)
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)


-- | Enrichment level for DOCX-XML-based enrichment.
data EnrichmentLevel
  = EnrichNone              -- ^ no XML pass; structure only
  | EnrichDocxMinimal       -- ^ comments + tracked changes mapped onto blocks
  | EnrichDocxFull          -- ^ minimal + extra style/numbering/meta if available
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


data StructureSource
  = StructureFromPandoc
  | StructureFromXml
  | StructureAuto
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
