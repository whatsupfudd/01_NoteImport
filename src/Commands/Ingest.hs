{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Commands.Ingest (runIngest) where

import Control.Monad (when, foldM)
import Control.Monad.Cont (ContT (..))
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Cont as Mc
import qualified Control.Exception as Cex

import qualified Data.Aeson as Ae
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32)
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
import HBDoc.Types (Doc(..), Block(..), BlockKind(..), ObjectAttrs(..), EnrichmentLevel(..))
import HBDoc.Structure (User(..))

import qualified DB.Connect as Dbc
import qualified DB.Statements as Dbs
import qualified DB.Operations as Dbo

import qualified Options.Runtime as Rt
import qualified Options.Types as Opt

-- DOCX orchestrator (structural import + optional enrichment)
import DocX.Import ( DocXImportOptions(..), DocXImportResult(..), DocXImportError(..)
      , importDocxBytes, importDocxFile)


data ApiResult a = ApiResult {
    okAR :: !Bool
    , resultAR :: !(Maybe a)
    , errAR :: !(Maybe Text)
  }
  deriving (Show, Eq)


data SerializeInfo = SerializeInfo {
      userName :: Text
    , mbDocID :: Maybe Int32
    , contentType :: Text
    , size :: Int32
    , key :: Text
    , shaHex :: Text
    , originalName :: Text
    , document :: Doc
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
          , size = maybe 0 (fromIntegral . BL.length) importResult.originalBytesDocX
          , key = importResult.keyName
          , shaHex = importResult.sha256
          , originalName = case opts.input of
              Opt.FromFile fp -> T.pack (takeFileName fp)
              Opt.FromStdin -> "<anonymous-stdin>"
          , document = importResult.docResDocX
          }

      rezA <- Mc.runContT pgPool (mainAction sInfo)
      case rezA of
        Left err -> do
          putStrLn $ "@[runIngest] serialization failed: " <> show err
          pure ()
        Right apiRez -> do
          pure ()
  where
  mainAction :: SerializeInfo -> Pool -> IO (Either String (ApiResult Int32))
  mainAction sInfo dbPool = do
    rezB <- serializeDocument dbPool sInfo
    putStrLn "@[runIngest] serialization done."
    pure rezB


runDocx :: Opt.IngestOpts -> Rt.RunOptions -> IO (Either DocXImportError DocXImportResult)
runDocx opts rtOpts = do
  let iopts = DocXImportOptions
                { structureSourceOpts =  opts.structure
                , enrichmentLevelOpts = opts.enrich
                , keepOriginalOpts    = opts.keepOriginal
                , titleOverrideOpts   = opts.titleOverride
                , formatLabelOpts     = Just "docx"
                }
  rezA <- case opts.input of
    Opt.FromFile fp -> importDocxFile iopts fp
    Opt.FromStdin -> do
      lbs <- BL.getContents
      importDocxBytes iopts lbs
  case rezA of
    Left err -> do
      putStrLn ("DOCX import failed: " <> showDocxErr err)
      pure (Left err)
    Right importResult  ->
      -- outputResult opts r
      pure $ Right importResult

runHtml :: Opt.IngestOpts -> Rt.RunOptions -> IO (Either DocXImportError DocXImportResult)
runHtml opts rtOpts = do
  putStrLn "HTML import not yet wired. (Stub) Returning empty document."
  -- Stub: create minimal Doc; replace with HBDoc.Html.Import once available
  lbs <- case opts.input of
           Opt.FromFile fp -> BL.readFile fp
           Opt.FromStdin   -> BL.getContents
  let doc = Doc { uidDoc = Nothing
                , stableIdDoc = Nothing
                , titleDoc = fromMaybe "" opts.titleOverride
                , formatDoc = Just "html"
                , metaDoc = M.empty
                , blocksDoc = []
                }
  outputDocOnly opts doc
  let
    docResult = DocXImportResult {
      docResDocX = doc
      , originalBytesDocX = Nothing
      , warningsDocX = []
      , sha256 = "<no sha256 signature>"
      , keyName = "1234-5678-9012-3456"
    }

  pure $ Right docResult

runMarkdown :: Opt.IngestOpts -> Rt.RunOptions -> IO (Either DocXImportError DocXImportResult)
runMarkdown opts rtOpts = do
  putStrLn "Markdown import not yet wired. (Stub) Returning empty document."
  inFile <- case opts.input of
            Opt.FromFile fp -> BL.readFile fp
            Opt.FromStdin -> BL.getContents
  let
    doc = Doc { uidDoc = Nothing
                , stableIdDoc = Nothing
                , titleDoc = fromMaybe "" opts.titleOverride
                , formatDoc = Just "markdown"
                , metaDoc = M.empty
                , blocksDoc = []
                }
  outputDocOnly opts doc
  let
    docResult = DocXImportResult {
      docResDocX = doc
      , originalBytesDocX = Nothing
      , warningsDocX = []
      , sha256 = "<no sha256 signature>"
      , keyName = "1234-5678-9012-3456"
    }
  pure (Right docResult)

-- ----------------------------------------------------------------------------
-- Output helpers
-- ----------------------------------------------------------------------------

outputResult :: Opt.IngestOpts -> DocXImportResult -> IO ()
outputResult opts res = do
  mapM_ (TIO.putStrLn . ("[warn] " <>)) (warningsDocX res)
  case Opt.outMode opts of
    Opt.OutJson   -> do
      let bs = Ae.encode (docResDocX res)
      maybe (BL.putStr bs) (`BL.writeFile` bs) opts.writeJson
    Opt.OutPretty -> prettyPrintDoc (docResDocX res)


outputDocOnly :: Opt.IngestOpts -> Doc -> IO ()
outputDocOnly opts doc =
  case opts.outMode of
    Opt.OutJson   -> do
      let bs = Ae.encode doc
      maybe (BL.putStr bs) (`BL.writeFile` bs) opts.writeJson
    Opt.OutPretty -> prettyPrintDoc doc

showDocxErr :: DocXImportError -> String
showDocxErr = \case
  DocXInvalidInputError t -> T.unpack t
  DocXPandocError t       -> "Pandoc: " <> T.unpack t
  DocXXmlError t          -> "XML: " <> T.unpack t
  DocXEnrichmentError t   -> "Enrichment: " <> T.unpack t
  DocXCombinedError t     -> T.unpack t

-- Simple, readable tree printer for Doc/Block
prettyPrintDoc :: Doc -> IO ()
prettyPrintDoc d = do
  TIO.putStrLn ("Title: " <> titleDoc d)
  TIO.putStrLn ("Format: " <> fromMaybe "" (formatDoc d))
  TIO.putStrLn "Blocks:"
  mapM_ (ppBlock 0) (blocksDoc d)


ppBlock :: Int -> Block -> IO ()
ppBlock depth b = do
  let
    indent = T.replicate depth "  "
    kTxt =
      case kindBlk b of
        DocumentBK     -> "Document"
        HeadingBK n    -> "Heading " <> T.pack (show n)
        ParagraphBK    -> "Paragraph"
        RunBK          -> "Run"
        ListBK         -> "List"
        ListItemBK _   -> "ListItem"
        TableBK        -> "Table"
        TableRowBK     -> "TableRow"
        TableCellBK    -> "TableCell"
        ImageBK        -> "Image"
        FigureBK       -> "Figure"
        CodeBK         -> "Code"
        QuoteBK        -> "Quote"
        RuleBK         -> "Rule"
        FootnoteBK     -> "Footnote"
        EndnoteBK      -> "Endnote"
    preview = maybe "" (truncateText 120) (textBlk b)
  TIO.putStrLn (indent <> "• " <> kTxt <> if T.null preview then "" else ": " <> preview)
  mapM_ (ppBlock (depth + 1)) (kidsBlk b)

truncateText :: Int -> Text -> Text
truncateText n t = if T.length t <= n then t else T.take n t <> "…"


-- DB Storage -----------------------------------------------------------------

useTx :: Pool -> Tx.Transaction tr -> IO (Either UsageError tr)
useTx pool stmts = use pool (Txs.transaction Txs.Serializable Txs.Write stmts)

blockCount :: [Block] -> Int
blockCount blkList = 
  length blkList + sum (map (\blk -> blockCount blk.kidsBlk) blkList)


serializeDocument :: Pool -> SerializeInfo -> IO (Either String (ApiResult Int32))
serializeDocument pool sInfo = do
  putStrLn $ "@[serializeDocument] starting."
  let blocks = blocksDoc sInfo.document
  putStrLn $ "@[serializeDocument] blocks nbr: " <> show (blockCount blocks)
  eiDocID <- case sInfo.mbDocID of
    Just docId -> do
      pure (Right docId)
    Nothing -> do
      putStrLn "@[serializeDocument] no docId provided, will create new document."
      {-
          createDoc :: Pool -> Int32
                -> Text
                -> Int32 -> Int32 -> Int32 -> Int32 -> Maybe Int32
                -> Maybe Text -> Bool -> Bool -> Maybe Day
                -> IO (ApiResult Int32)
          createDoc pool actor title domainID typeID tierID statusID ownerID residency aiAllowed legalHold due
      -}
      pure $ Left "no docId provided"
  case eiDocID of
    Left err -> pure (Left err)
    Right docId -> do
      eiMbUser <- Dbo.resolveUser pool sInfo.userName
      case eiMbUser of
        Left err -> pure $ Left err
        Right Nothing -> pure . Left $ "@[serializeDocument] user not found: " <> T.unpack sInfo.userName
        Right (Just user) -> do
          putStrLn $ "@[serializeDocument] user found: " <> show user
          eiTrxRez <- useTx pool $ do
            -- Guard / policy
            g <- Tx.statement (user.uidUsr, "edit" :: Text, docId) Dbs.qCanUser
            if not g then pure (ApiResult False Nothing (Just "forbidden:edit")) else do
              (domFk, tierFk, stFk, mRes) <- Tx.statement docId Dbs.qDocMeta
              mBlock <- Tx.statement (domFk, tierFk, stFk, mRes) Dbs.qPolicyBlockImport
              case mBlock of
                Just True -> pure (ApiResult False Nothing (Just "blocked_by_policy:import"))
                _ -> do
                  -- Stream to temp + upload original
                  attUid <- Tx.statement (docId, sInfo.originalName, sInfo.contentType, sInfo.size, sInfo.key, sInfo.shaHex, user.uidUsr) Dbs.qInsertAttachment

                  -- Ensure root
                  rootUid <- Tx.statement (docId, user.uidUsr) Dbs.qEnsureRoot

                  -- Parse into blocks (capture media meta during upload)
                  -- Write blocks into blocks_bd using heading/list stacks
                  -- putStrLn $ "@[importDocxStreamToBlockTree] going to write blocks."
                  uidMap <- writeBlocks (user.uidUsr, docId) rootUid blocks
                  -- liftIO $ putStrLn $ "@[importDocxStreamToBlockTree] block written."

                  -- Link image blocks to attachments
                  -- For each image block, create tattachment and link
                  -- (If your store already recorded metas, insert precise size/sha here)
                  let imgBlocks = [ (bid, attrs) | (bid, attrs) <- collectImageBlocks blocks ]
                  mapM_ (linkImage (user.uidUsr, docId) uidMap) imgBlocks

                  Tx.statement (user.uidUsr, "doc.import.blocktree", Just docId, Just "document", Nothing, Nothing) Dbs.qAudit
                  pure (ApiResult True (Just rootUid) Nothing)
          case eiTrxRez of
            Left ue -> pure $ Left $ "@[serializeDocument] transaction error: " <> show ue
            Right apiRez ->
              if apiRez.okAR then
                pure $ Right apiRez
              else do
                putStrLn $ "@[importDocxStreamToBlockTree] got non-expected Right call, rez: " <> show apiRez
                pure $ Right apiRez
  -- pure (ApiResult True (Just 0) Nothing)

  where
  -- Insert all blocks recursively according to hierarchy produced by heading/list stacks
  {-
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
-}
  writeBlocks :: (Int32, Int32) -> Int32 -> [Block] -> Tx.Transaction (M.Map Int32 Int32)
  writeBlocks docCtxt@(userID, docID) parent = foldM step M.empty
    where
      step acc b = do
        let kindTxt = case b.kindBlk of
              DocumentBK -> "document"
              HeadingBK n -> T.pack ("heading:"<> show n)
              ParagraphBK -> "paragraph"
              RunBK -> "run"
              ListBK -> "list"
              ListItemBK il -> T.pack ("list_item:"<> show il)
              TableBK -> "table"
              TableRowBK -> "table_row"
              TableCellBK -> "table_cell"
              ImageBK -> "image"
              FigureBK -> "figure"
              CodeBK -> "code"
              QuoteBK -> "quote"
              RuleBK -> "rule"
              FootnoteBK -> "footnote_ref"
              EndnoteBK  -> "endnote_ref"
            attrsJson = TE.decodeUtf8 (BL.toStrict (Ae.encode b.attrsBlk))
            textVal = textBlk b
        newUid <- Tx.statement (docID, Just parent, kindTxt, Just (fromMaybe "<nil>" textVal), Just (Ae.toJSON attrsJson), userID) Dbs.qAppendChild
        -- recurse on children
        childMap <- writeBlocks docCtxt newUid b.kidsBlk
        pure (M.insert b.uidBlk newUid (M.union acc childMap))

  collectImageBlocks :: [Block] -> [(Int32, ObjectAttrs)]
  collectImageBlocks = go []
    where
      go :: [(Int32, ObjectAttrs)] -> [Block] -> [(Int32, ObjectAttrs)]
      go acc [] = acc
      go acc (x:xs) =
        let acc' = case (x.kindBlk, x.attrsBlk) of
                      (ImageBK, Just a) -> (x.uidBlk, a):acc
                      _                  -> acc
        in go (go acc' x.kidsBlk) xs

  linkImage :: (Int32, Int32) -> M.Map Int32 Int32 -> (Int32, ObjectAttrs) -> Tx.Transaction ()
  linkImage docCtxt@(docID, userID) uidMap (localId, a) = case (a.imageKeyOA, a.imageNameOA) of
    (Just key, Just fname) -> do
      -- Insert tattachment for the image and link
      let ct = guessContentType fname
      -- size/sha could be looked up if recorded during upload
      attUid <- Tx.statement (docID, fname, ct, 0, key, "", userID) Dbs.qInsertAttachment
      case M.lookup localId uidMap of
        Nothing    -> pure ()
        Just blkDb -> Tx.statement (blkDb, attUid, userID) Dbs.qLinkBlockAttachment
    _ -> pure ()

  guessContentType t
    | ".png" `T.isSuffixOf` T.toLower t = "image/png"
    | ".jpg" `T.isSuffixOf` T.toLower t = "image/jpeg"
    | ".jpeg"`T.isSuffixOf` T.toLower t = "image/jpeg"
    | ".gif" `T.isSuffixOf` T.toLower t = "image/gif"
    | otherwise = "application/octet-stream"

