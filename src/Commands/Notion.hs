module Commands.Notion where

import Data.Text (Text, unpack)

import Notion.Fetch
import qualified Options.Runtime as Rto

notionCmd :: Text -> Rto.RunOptions -> IO ()
notionCmd aWordspace rtOpts =
  putStrLn $ "@[notionCmd] starting, workspace: " <> unpack aWordspace