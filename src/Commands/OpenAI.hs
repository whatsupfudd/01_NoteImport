{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
module Commands.OpenAI where

import qualified Data.ByteString.Lazy as BS
import qualified Data.List as L
import Data.Either (lefts, rights)
import Data.Text (Text, unpack)
import qualified Data.Map as Mp

import qualified Data.Aeson as Ae

import qualified Options.Runtime as Rto
import qualified OpenAI.Json.Reader as Jd
import qualified OpenAI.Parse as Op

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
    Right discussions -> do
      mapM_ (\discussion -> do
          -- putStrLn "Parsing successful. Loaded Discussion:"
          -- print discussion
          -- Example of using OverloadedRecordDot: access title
          putStrLn . unpack $ "Title: " <> discussion.titleCv <> ", id: " <> discussion.convIdCv
          Op.analyzeDiscussion discussion
        ) discussions

