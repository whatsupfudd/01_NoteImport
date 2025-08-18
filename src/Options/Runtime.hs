module Options.Runtime (
  defaultRun, RunOptions (..), PgDbConfig (..), defaultPgDbConf, NotionOptions (..)
  ) where
-- import Data.Int (Int)

import Data.Text (Text)

import DB.Connect (PgDbConfig (..), defaultPgDbConf)


newtype NotionOptions = NotionOptions {
  apiKey :: Text
  }
  deriving (Show)

data RunOptions = RunOptions {
    debug :: Int
    , pgDbConf :: PgDbConfig
    , notion :: Maybe NotionOptions
  }
  deriving (Show)

defaultRun :: RunOptions
defaultRun =
  RunOptions {
    debug = 0
    , pgDbConf = defaultPgDbConf
    , notion = Nothing
  }
