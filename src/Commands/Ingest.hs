{-# LANGUAGE LambdaCase #-}

module Commands.Ingest (runIngest) where

import Control.Monad (when, foldM)
import Control.Monad.Cont (ContT (..))
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Cont as Mc
import qualified Control.Exception as Cex

import qualified Data.Aeson as Ae
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

import System.FilePath (takeFileName)

import Hasql.Pool (Pool, use, UsageError)
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as Txs
import qualified Hasql.Session as Ses
import qualified Hasql.Statement as S
import Hasql.TH


-- Unified HBDoc types
import HBDoc.Core.Types (HBDoc) -- Doc(..), Block(..), BlockKind(..), ObjectAttrs(..), EnrichmentLevel(..))
import qualified HBDoc.Core.Types as HbT
import HBDoc.Manage.Types (User(..))

import qualified DB.Connect as Dbc

import qualified Options.Runtime as Rt
import qualified Options.Types as Opt

import qualified HBDoc.Parse.Structured as Ps
import qualified HBDoc.Serialize.Write as Sw
import HBDoc.Serialize.Types (SerializeInfo (..))
import qualified HBDoc.Core.Build as Cb
import qualified HBDoc.Render.PrettyPrint as Pp

type Doc0 = HBDoc () ()

data ImportResult = ImportResult {
    originalBytes :: Maybe BL.ByteString
  , warnings :: [Text]
  , sha256 :: Text
  , keyName :: Text
  , document :: Doc0
}

runIngest :: Opt.IngestOpts -> Rt.RunOptions -> IO ()
runIngest opts rtOpts = do
  rezA <- case opts.format of
    Opt.FDocx -> runDocx opts rtOpts
    Opt.FHtml -> runHtml opts rtOpts
    Opt.FMarkdown -> runMarkdown opts rtOpts
  case rezA of
    Left err -> do
      putStrLn $ "@[runIngest] import failed: " <> show err
      pure ()
    Right importResult -> do
      putStrLn "@[runIngest] starting serialization."
      let
        pgPool = Dbc.startPg rtOpts.pgDbConf
        sInfo = SerializeInfo {
            userName = opts.userName
          , mbDocID = fromIntegral <$> opts.docId
          , contentType = case opts.format of
              Opt.FDocx -> "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
              Opt.FHtml -> "text/html"
              Opt.FMarkdown -> "text/markdown"
          , size = maybe 0 (fromIntegral . BL.length) importResult.originalBytes
          , key = importResult.keyName
          , shaHex = importResult.sha256
          , originalName = case opts.input of
                Opt.FromFile fp -> T.pack (takeFileName fp)
                Opt.FromStdin -> "<anonymous-stdin>"
          , document = importResult.document
          , debugFlag = 1
          }

      rezA <- Mc.runContT pgPool (mainAction sInfo)
      case rezA of
        Left err ->
          putStrLn $ "@[runIngest] serialization failed: " <> show err
        Right apiRez -> pure ()
  where
  mainAction :: SerializeInfo docT blkT -> Pool -> IO (Either String Int64)
  mainAction sInfo dbPool = do
    rezB <- Sw.serializeDocument dbPool sInfo
    putStrLn "@[runIngest] serialzeDocument: done."
    pure rezB


runDocx :: Opt.IngestOpts -> Rt.RunOptions -> IO (Either String ImportResult)
runDocx opts rtOpts = do
  eiRezA <- case opts.input of
    Opt.FromFile fp -> Ps.parseDocxFile fp
    Opt.FromStdin -> do
      lbs <- BL.getContents
      Ps.parseDocxBytes Nothing lbs
  case eiRezA of
    Left err -> do
      putStrLn $ "@[runDocx] DOCX import failed: " <> err
      pure $ Left err
    Right hbDoc  ->
      -- outputResult opts r
      let
        importResult = ImportResult {
            originalBytes = Nothing
          , warnings = []
          , sha256 = "<no sha256 signature>"
          , keyName = "1234-5678-9012-3456"
          , document = hbDoc
        }
      in
      pure $ Right importResult


runHtml :: Opt.IngestOpts -> Rt.RunOptions -> IO (Either String ImportResult)
runHtml opts rtOpts = do
  putStrLn "HTML import not yet wired. (Stub) Returning empty document."
  -- Stub: create minimal Doc; replace with HBDoc.Html.Import once available
  lbs <- case opts.input of
           Opt.FromFile fp -> BL.readFile fp
           Opt.FromStdin   -> BL.getContents
  let
    fakeRoot = Cb.mkContainerBk () []
    doc = Cb.mkHBDocSimple () (fromMaybe "" opts.titleOverride) (Just "html") fakeRoot
  outputDocOnly opts doc
  let
    docResult = ImportResult {
        originalBytes = Nothing
      , warnings = []
      , sha256 = "<no sha256 signature>"
      , keyName = "1234-5678-9012-3456"
      , document = doc
    }

  pure $ Right docResult

runMarkdown :: Opt.IngestOpts -> Rt.RunOptions -> IO (Either String ImportResult)
runMarkdown opts rtOpts = do
  eiRez <- case opts.input of
    Opt.FromFile fp -> Ps.parseMarkdownFile fp
    Opt.FromStdin -> do
      lbs <- BL.getContents
      Ps.parseMarkdownBytes Nothing lbs

  case eiRez of
    Left err -> do
      putStrLn $ "@[runMarkdown] parseMarkdownBytes err: " <> err
      pure $ Left err
    Right hbDoc -> do
      outputDocOnly opts hbDoc
      let
        docResult = ImportResult {
            originalBytes = Nothing
          , warnings = []
          , sha256 = "<no sha256 signature>"
          , keyName = "1234-5678-9012-3456"
          , document = hbDoc
        }
      pure $ Right docResult

-- ----------------------------------------------------------------------------
-- Output helpers
-- ----------------------------------------------------------------------------

outputResult :: Opt.IngestOpts -> ImportResult -> IO ()
outputResult opts res = do
  mapM_ (TIO.putStrLn . ("[warn] " <>)) res.warnings
  case Opt.outMode opts of
    Opt.OutJson -> do
      let bs = Ae.encode res.document
      maybe (BL.putStr bs) (`BL.writeFile` bs) opts.writeJson
    Opt.OutPretty ->
      let
        result = Pp.prettyHBDocText res.document
      in
      TIO.putStrLn result


outputDocOnly :: Opt.IngestOpts -> Doc0 -> IO ()
outputDocOnly opts doc =
  case opts.outMode of
    Opt.OutJson -> do
      let bs = Ae.encode doc
      maybe (BL.putStr bs) (`BL.writeFile` bs) opts.writeJson
    Opt.OutPretty ->
      let
        result = Pp.prettyHBDocText doc
      in
      TIO.putStrLn result

