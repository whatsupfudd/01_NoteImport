{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Main where

import Data.Aeson (eitherDecodeFileStrict')
import OpenAI.Json.V2 (Conversation (..))
import System.Environment (getArgs)
import System.Exit (die)


main :: IO ()
main = do
  args <- getArgs

  case args of
    [filePath] -> do
      eiResult <- eitherDecodeFileStrict' filePath :: IO (Either String Conversation)
      case eiResult of
        Left err -> die ("JSON parsing failed: " <> err)
        Right conversation -> print conversation.messagesCv
    _ -> die "Usage: runghc JsonTest.hs <file.json>"