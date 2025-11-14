{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase #-}

-- | High-level DOCX import orchestrator.
--
-- This module defines the public API for importing .docx documents into the
-- unified hierarchical block document model ('HBDoc.Types.Doc').
--
-- Responsibilities:
--   • Decide which structural pipeline to use (Pandoc vs XML vs Auto).
--   • Optionally enrich the imported Doc with DOCX-specific metadata
--     (tracked changes, comments, numbering, styles) via the XML layer.
--   • Optionally retain the original .docx bytes for later storage.
--
-- The actual parsing logic lives in:
--   • 'HBDoc.DocX.Pandoc' — Pandoc-based structural importer.
--   • 'HBDoc.DocX.Xml'    — DOCX-XML-based structural importer and enrichment.
--
-- Future parallel modules:
--   • 'HBDoc.Html.Import'
--   • 'HBDoc.Markdown.Import'
--
module DocX.Import
  ( -- * Options / modes
  DocXImportOptions(..)
    -- * Result & errors
  , DocXImportResult(..)
  , DocXImportError(..)

    -- * Entry points
  , importDocxBytes
  , importDocxFile
  ) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import qualified Crypto.Hash as CH

import HBDoc.Types (Doc(..), EnrichmentLevel(..), StructureSource(..))

import qualified DocX.Pandoc as PandocImport
import qualified DocX.Xml as Xi


--------------------------------------------------------------------------------
-- Options / modes
--------------------------------------------------------------------------------

-- | Where should we take the primary / structural view of the document from?
--
-- In normal operation you will use 'StructureFromPandoc' as the canonical
-- path. 'StructureAuto' tries Pandoc first and falls back to XML if Pandoc
-- fails. 'StructureFromXml' is mostly for debugging or special cases.

-- | How much DOCX-XML-specific enrichment should be applied *after* a structural
-- import has succeeded?
--
-- Enrichment is always DOCX-specific (it inspects WordprocessingML parts).

-- | Options controlling a DOCX import.
data DocXImportOptions = DocXImportOptions
  { structureSourceOpts :: !StructureSource  -- ^ Pandoc/XML/auto
  , enrichmentLevelOpts :: !EnrichmentLevel  -- ^ XML enrichment level
  , keepOriginalOpts    :: !Bool                 -- ^ retain original .docx bytes?
  , titleOverrideOpts   :: !(Maybe Text)         -- ^ override titleDoc if provided
  , formatLabelOpts     :: !(Maybe Text)         -- ^ override formatDoc (default \"docx\")
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)


--------------------------------------------------------------------------------
-- Result & errors
--------------------------------------------------------------------------------

-- | Successful import result.
--
-- 'docResDocX' is the unified hierarchical document ('HBDoc.Types.Doc').
-- 'originalBytesDocX' is populated when 'keepOriginalOpts' is True.
-- 'warningsDocX' is a place to surface non-fatal issues (e.g., enrichment
-- partial failures) while still returning a usable Doc.
data DocXImportResult = DocXImportResult
  { docResDocX :: !Doc
  , originalBytesDocX :: !(Maybe BL.ByteString)
  , warningsDocX :: ![Text]
  , sha256 :: !Text
  , keyName :: !Text
  } deriving (Show, Eq, Generic)


-- | Errors that can occur during DOCX import.
data DocXImportError
  = DocXInvalidInputError !Text          -- ^ e.g. empty bytes, not a ZIP, etc.
  | DocXPandocError       !Text          -- ^ failure in the Pandoc reader
  | DocXXmlError          !Text          -- ^ failure in the XML structural importer
  | DocXEnrichmentError   !Text          -- ^ failure while enriching via XML
  | DocXCombinedError     !Text          -- ^ both Pandoc + XML failed in 'StructureAuto'
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


--------------------------------------------------------------------------------
-- Public entry points
--------------------------------------------------------------------------------

-- | Import a DOCX document from lazy 'ByteString' into the unified HBDoc model.
--
-- This function:
--
--   1. Chooses a structural importer (Pandoc vs XML) according to
--      'structureSourceOpts'.
--   2. Optionally post-processes the 'Doc' with an XML enrichment pass
--      according to 'enrichmentLevelOpts'.
--   3. Applies small doc-level decorations:
--        • sets 'formatDoc' (default \"docx\" or 'formatLabelOpts')
--        • applies 'titleOverrideOpts' if provided
--   4. Optionally keeps the original .docx bytes if 'keepOriginalOpts' is True.
--
importDocxBytes
  :: DocXImportOptions
  -> BL.ByteString
  -> IO (Either DocXImportError DocXImportResult)
importDocxBytes opts lbs
  | BL.null lbs =
      pure (Left (DocXInvalidInputError "Empty input: DOCX ByteString is empty"))
  | otherwise = do
      eDoc <- case structureSourceOpts opts of
        StructureFromPandoc ->
          importStructureWithPandoc lbs
        StructureFromXml ->
          importStructureWithXml lbs
        StructureAuto ->
          importStructureAuto lbs

      case eDoc of
        Left err -> pure (Left err)
        Right doc0 -> do
          let doc1 = applyDocDecorations opts doc0
          runEnrichment opts lbs doc1

-- | Convenience wrapper: read a .docx from disk and call 'importDocxBytes'.
importDocxFile
  :: DocXImportOptions
  -> FilePath
  -> IO (Either DocXImportError DocXImportResult)
importDocxFile opts fp = do
  lbs <- BL.readFile fp
  importDocxBytes opts lbs


--------------------------------------------------------------------------------
-- Internal orchestration helpers
--------------------------------------------------------------------------------

-- Structural import via Pandoc
importStructureWithPandoc
  :: BL.ByteString
  -> IO (Either DocXImportError Doc)
importStructureWithPandoc lbs = do
  -- HBDoc.DocX.Pandoc.importDocxPandoc is expected to be:
  --   importDocxPandoc :: BL.ByteString -> IO (Either Text Doc)
  PandocImport.importDocxPandoc lbs >>= \case
    Left errTxt -> pure (Left (DocXPandocError errTxt))
    Right doc   -> pure (Right doc)

-- Structural import via XML
importStructureWithXml
  :: BL.ByteString
  -> IO (Either DocXImportError Doc)
importStructureWithXml lbs = do
  -- HBDoc.DocX.Xml.importDocxStructure is expected to be:
  --   importDocxStructure :: BL.ByteString -> IO (Either Text Doc)
  Xi.importDocxStructure lbs >>= \case
    Left errTxt -> pure (Left (DocXXmlError errTxt))
    Right doc   -> pure (Right doc)

-- Auto mode: try Pandoc first, fall back to XML if Pandoc fails.
importStructureAuto
  :: BL.ByteString
  -> IO (Either DocXImportError Doc)
importStructureAuto lbs = do
  ePandoc <- importStructureWithPandoc lbs
  case ePandoc of
    Right doc -> pure (Right doc)
    Left (DocXPandocError msgP) -> do
      eXml <- importStructureWithXml lbs
      case eXml of
        Right doc -> pure (Right doc)
        Left (DocXXmlError msgX) ->
          pure (Left (DocXCombinedError ("Pandoc failed: " <> msgP <> "; XML failed: " <> msgX)))
        Left otherErr -> pure (Left otherErr)
    Left otherErr -> pure (Left otherErr)

-- Apply doc-level metadata adjustments after structural import.
applyDocDecorations :: DocXImportOptions -> Doc -> Doc
applyDocDecorations opts doc0 =
  let fmt = case formatLabelOpts opts of
              Just f  -> Just f
              Nothing -> Just "docx"
      title' = case titleOverrideOpts opts of
                 Nothing  -> titleDoc doc0
                 Just tOv -> tOv
  in doc0 { formatDoc = fmt, titleDoc = title' }

-- Run XML enrichment if requested.
runEnrichment
  :: DocXImportOptions
  -> BL.ByteString
  -> Doc
  -> IO (Either DocXImportError DocXImportResult)
runEnrichment opts lbs doc0 =
  case enrichmentLevelOpts opts of
    EnrichNone ->
      pure (Right (mkResult doc0 []))
    lvl -> do
      -- HBDoc.DocX.Xml.enrichDocxWithXml is expected to be:
      --   enrichDocxWithXml :: EnrichmentLevel -> BL.ByteString -> Doc -> IO (Either Text Doc)
      Xi.enrichDocxWithXml lvl lbs doc0 >>= \case
        Left errTxt ->
          pure (Left (DocXEnrichmentError errTxt))
        Right doc1 ->
          pure (Right (mkResult doc1 []))
  where
    mkResult doc warns =
      DocXImportResult
        { docResDocX = doc
        , originalBytesDocX = if keepOriginalOpts opts then Just lbs else Nothing
        , warningsDocX = warns
        , sha256 = computeSha256 lbs
        , keyName = "1234-5678-9012-3456"
        }


computeSha256 :: BL.ByteString -> Text
computeSha256 content =
  let
    hasher = CH.hashInit :: CH.Context CH.SHA256
    ctxt = CH.hashUpdate hasher (BL.toStrict content)
    digest = CH.hashFinalize ctxt :: CH.Digest CH.SHA256
  in
  T.pack (show digest)
