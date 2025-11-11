{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
module Commands.Notion where

import qualified Data.List as L
import Data.Either (lefts, rights)
import Data.Text (Text, unpack)
import qualified Data.Map as Mp

import qualified Notion.Fetch as No
import qualified Options.Runtime as Rto


notionCmd :: Text -> Rto.RunOptions -> IO ()
notionCmd aWorkspace rtOpts = do
  putStrLn $ "@[notionCmd] starting, workspace: " <> unpack aWorkspace
  case rtOpts.notion of
    Nothing -> putStrLn "@[notionCmd] no notion options found."
    Just notionOpts -> case Mp.lookup aWorkspace notionOpts.apiKeys of
      Nothing -> putStrLn $ "@[notionCmd] no api key found for workspace: " <> unpack aWorkspace
      Just apiKey -> do
        notion <- No.mkNotion apiKey
        pages <- No.fetchAllPages notion
        if null pages then
          putStrLn "@[notionCmd] no pages found."
        else
          -- 
          putStrLn $ "@[notionCmd] pages found: " <> L.intercalate "\n" (map showPage pages)
        pageBlocks <- mapM (\aPage -> do
            blocks <- No.fetchAllPageBlocks notion aPage
            pure (aPage, blocks)
          ) pages
        if null pageBlocks then
          putStrLn "@[notionCmd] no blocks found."
        else
          -- putStrLn $ "@[notionCmd] blocks found: " <> L.intercalate "\n" (map (\b -> "block: " <> show b) blocks)
          let
            blkCount = map length (snd <$> pageBlocks)
            blockTypes = foldl (\accum b -> Mp.insert b.ktype b.content accum) Mp.empty (concatMap snd pageBlocks)
            pageUpgrades = map (\(aPage, blocks) -> (aPage, No.upgradeToHBlocks blocks)) pageBlocks
          in do
            putStrLn $ "@[notionCmd] block counts: " <> L.intercalate "\n ," (map show blkCount)
            putStrLn $ "@[notionCmd] block types: " <> L.intercalate "\n ," (map unpack (Mp.keys blockTypes))
            mapM_ (\(aPage, pBlocks) -> case lefts pBlocks of
                [] -> do
                  putStrLn $ "@[notionCmd] page: " <> maybe "<nil>" unpack aPage.title_plain
                  putStrLn $ "@[notionCmd] blocks: " <> show (rights pBlocks)
                errs -> putStrLn $ "@[notionCmd] errors: " <> L.intercalate "\n ," errs
              ) pageUpgrades
        pure ()

showPage :: No.PageMeta -> String
showPage p = "id: " <> show p.id <> ", title: " <> maybe "<nil>" unpack p.title_plain <> ", last_edited: " <> show p.last_edited_time