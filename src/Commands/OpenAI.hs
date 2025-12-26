{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
module Commands.OpenAI where

import qualified Control.Monad.Cont as Mc

import qualified Data.ByteString.Lazy as BS
import Data.Int (Int64)
import qualified Data.List as L
import Data.Either (lefts, rights)
import Data.Text (Text, unpack)
import qualified Data.Map as Mp

import qualified Data.Aeson as Ae

import qualified Hasql.Pool as Hp

import qualified Options.Runtime as Rto
import qualified DB.Connect as Dbc
import qualified OpenAI.Json.Reader as Jd
import qualified OpenAI.Parse as Op
import qualified OpenAI.Operations as Dbo

parseCmd :: (FilePath, Bool) -> Rto.RunOptions -> IO ()
parseCmd (filePath, exportFlag) rtOpts = do
  jsonContent <- BS.readFile filePath
  let
    rezA = if exportFlag then
      Ae.eitherDecode jsonContent :: Either String [Jd.Discussion]
    else
      L.singleton <$> (Ae.eitherDecode jsonContent :: Either String Jd.Discussion)
  case rezA of
    Left err -> putStrLn $ "Parsing failed: " ++ err
    Right discussions ->
      let
        pgPool = Dbc.startPg rtOpts.pgDbConf
      in do
      putStrLn $ "@[parseCmd] loaded " <> show (length discussions) <> " discussions."
      -- showDiscussions discussions
      rezA <- Mc.runContT pgPool (saveDiscussions discussions)
      case rezA of
        Left err -> do
          putStrLn $ "@[parseCmd] saving discussions failed: " <> show err
        Right apiRez -> do
          case apiRez of
            Right successInResults ->
              putStrLn $ "@[parseCmd] saved " <> show (length successInResults) <> " discussions."
            Left errorInResults ->
              putStrLn $ "@[parseCmd] logic errors: " <> L.intercalate "\n" errorInResults 
    


saveDiscussions :: [Jd.Discussion] -> Hp.Pool -> IO (Either [Hp.UsageError] (Either [String] [Int64]))
saveDiscussions discussions pgPool = do
  discussionMap <- Dbo.fetchAllDiscussions pgPool
  case discussionMap of
    Left err -> pure . Left $ [err]
    Right discussionMap ->
      let
        newDiscussions = filter (\discussion -> not (Mp.member discussion.titleCv discussionMap)) discussions
      in
      case newDiscussions of
        [] -> do
          putStrLn $ "@[saveDiscussions] no new discussions to save."
          pure . Right . Right $ []
        _ -> do
          putStrLn $ "@[saveDiscussions] saving " <> show (length newDiscussions) <> " new discussions."
          results <- mapM (Dbo.addDiscussion pgPool) newDiscussions
          if null (lefts results) then
            let
              errorInResults = lefts $ rights results
              successInResults = rights $ rights results
            in
            if null errorInResults then
              pure . Right . Right $ successInResults
            else
              pure . Right . Left $ errorInResults
          else
            pure (Left (lefts results))


showDiscussions :: [Jd.Discussion] -> IO ()
showDiscussions =
  mapM_ (\discussion -> do
      -- putStrLn "Parsing successful. Loaded Discussion:"
      -- print discussion
      -- Example of using OverloadedRecordDot: access title
      putStrLn . unpack $ "Title: " <> discussion.titleCv <> ", id: " <> discussion.convIdCv
      putStrLn . unpack $ Op.analyzeDiscussion discussion
    ) 

