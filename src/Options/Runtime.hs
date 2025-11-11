module Options.Runtime (
  defaultRun, RunOptions (..), PgDbConfig (..), defaultPgDbConf, NotionOptions (..)
  ) where
-- import Data.Int (Int)

import qualified Data.Map as Mp
import Data.Text (Text)

import DB.Connect (PgDbConfig (..), defaultPgDbConf)
import Options.ConfFile (PairReference)


newtype NotionOptions = NotionOptions {
  apiKeys :: Mp.Map Text Text
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
