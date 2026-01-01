{-# LANGUAGE DerivingStrategies #-}

module Options.Types where

import Data.Text (Text)
import HBDoc.Types (EnrichmentLevel (..), StructureSource (..))

data Format =
    FDocx
    | FHtml
    | FMarkdown
  deriving stock (Show, Eq)

data Input =
    FromFile FilePath
    | FromStdin
  deriving stock (Show, Eq)

data OutMode =
    OutJson
    | OutPretty
  deriving stock (Show, Eq)

-- v1 parser options:
data DocXOpts = DocXOpts {
  inPath :: FilePath
  , outPath :: Maybe FilePath
  , asYaml :: Bool
  , promote :: Bool
  } deriving stock (Show)


data IngestOpts = IngestOpts
  { format         :: !Format
  , input          :: !Input
  , structure      :: !StructureSource
  , enrich         :: !EnrichmentLevel
  , keepOriginal   :: !Bool
  , titleOverride  :: !(Maybe Text)
  , formatLabel    :: !(Maybe Text)
  , outMode        :: !OutMode
  , writeJson      :: !(Maybe FilePath)
  , userName :: !Text
  , docId          :: !(Maybe Int)
  } deriving stock (Show)


data OaiSubCommand =
  JsonSC OaiOpts
  | SummarySC OaiSummaryOpts
  | DocxSC OaiGenOpts
  | ElmSC OaiGenOpts
  | ProjFetchSC OaiProjFetchOpts
  deriving stock (Show)


data OaiOpts = OaiOpts {
  exportB :: !Bool
  , printB :: !Bool
  , jsonFile :: !FilePath
  } deriving stock (Show)

newtype OaiSummaryOpts = OaiSummaryOpts {
  targets :: [Text]
}
  deriving stock (Show)

data OaiGenOpts = OaiGenOpts {
  destPath :: FilePath
  , targets :: [Text]
}
  deriving stock (Show)

data OaiProjFetchOpts = OaiProjFetchOpts {
  label :: Text
  , sourcePath :: FilePath
}
  deriving stock (Show)

