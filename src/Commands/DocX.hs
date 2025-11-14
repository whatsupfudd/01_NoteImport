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

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import qualified Data.Aeson as Ae
import qualified Data.Aeson.Encode.Pretty as AeP

import qualified Data.Yaml.Pretty as YP

import qualified Options.Runtime as Rt
import qualified Options.Types as Opt

import qualified DocX.Logic_v1 as Dl

loadDoc :: Opt.DocXOpts -> Rt.RunOptions -> IO ()
loadDoc opts rtOpts = do
  eDoc <- Dl.readDocxPandoc opts.inPath
  case eDoc of
    Left err -> fail ("Failed to read DOCX: " <> err)
    Right pandocDoc -> do
      let
        d = Dl.buildDoc opts.promote opts.inPath pandocDoc
      (if opts.asYaml then emitYaml opts.outPath d else emitJson opts.outPath d)


emitJson :: Maybe FilePath -> Dl.Doc -> IO ()
emitJson mb d = case mb of
  Nothing -> BL.putStr (AeP.encodePretty' cfg d)
  Just fp -> BL.writeFile fp (AeP.encodePretty' cfg d)
  where
    cfg = AeP.defConfig { AeP.confCompare = AeP.compare }

emitYaml :: Maybe FilePath -> Dl.Doc -> IO ()
emitYaml mb doc = do
  let val = Ae.toJSON doc
      cfg = YP.setConfDropNull True YP.defConfig
      bs  = YP.encodePretty cfg val   -- strict ByteString
  case mb of
    Nothing -> BS.putStr bs
    Just fp -> BS.writeFile fp bs

