{-# LANGUAGE DuplicateRecordFields #-}

{-
Docx → HBDoc — convert .docx to a hierarchical block tree for our HBDoc (Hierarchical Block Document System)
-}

module Commands.DocX where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import qualified Data.Aeson as Ae
import qualified Data.Aeson.Encode.Pretty as AeP

import qualified Data.Yaml.Pretty as YP

import qualified Options.Runtime as Rt
import qualified Options.Types as Opt

import qualified HBDoc.Parse.Structured as Ps
import HBDoc.Core.Types (HBDoc)


type Doc0 = HBDoc () ()

loadDoc :: Opt.DocXOpts -> Rt.RunOptions -> IO ()
loadDoc opts rtOpts = do
  eDoc <- Ps.parseDocxFile opts.inPath
  case eDoc of
    Left err -> fail ("Failed to read DOCX: " <> err)
    Right hbDoc ->
      if opts.asYaml then
        emitYaml opts.outPath hbDoc
      else
        emitJson opts.outPath hbDoc


emitJson :: Maybe FilePath -> Doc0 -> IO ()
emitJson mb doc =
  let
      cfg = AeP.defConfig { AeP.confCompare = AeP.compare }
  in
  case mb of
    Nothing -> BL.putStr (AeP.encodePretty' cfg doc)
    Just fp -> BL.writeFile fp (AeP.encodePretty' cfg doc)


emitYaml :: Maybe FilePath -> Doc0 -> IO ()
emitYaml mb doc = do
  let
    val = Ae.toJSON doc
    cfg = YP.setConfDropNull True YP.defConfig
    bs  = YP.encodePretty cfg val   -- strict ByteString
  case mb of
    Nothing -> BS.putStr bs
    Just fp -> BS.writeFile fp bs

