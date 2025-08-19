module Commands.Notion where

import qualified Data.List as L
import Data.Text (Text, unpack)
import qualified Data.Map as Mp

import Notion.Fetch
import qualified Options.Runtime as Rto


notionCmd :: Text -> Rto.RunOptions -> IO ()
notionCmd aWordspace rtOpts = do
  putStrLn $ "@[notionCmd] starting, workspace: " <> unpack aWordspace
  case rtOpts.notion of
    Nothing -> putStrLn "@[notionCmd] no notion options found."
    Just notionOpts -> do
      notion <- mkNotion notionOpts.apiKey
      pages <- fetchAllPages notion
      if null pages then
        putStrLn "@[notionCmd] no pages found."
      else
        putStrLn $ "@[notionCmd] pages found: " <> L.intercalate "\n" (map (\p -> "id: " <> show p.id <> " url: " <> unpack p.url) pages)
      blocks <- mapM (fetchPageBlocksTop notion) [ p.id | p <- pages ]
      if null blocks then
        putStrLn "@[notionCmd] no blocks found."
      else
        -- putStrLn $ "@[notionCmd] blocks found: " <> L.intercalate "\n" (map (\b -> "block: " <> show b) blocks)
        let
          blockTypes = foldl (\accum b -> Mp.insert b.ktype b.content accum) Mp.empty (concat blocks)
        in
          putStrLn $ "@[notionCmd] block types: " <> L.intercalate "\n" (map unpack (Mp.keys blockTypes))
      pure ()
