{-# LANGUAGE DerivingStrategies #-}

module Options.Types where

import Data.Text (Text)
import HBDoc.Core.Types (HBDoc)

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


data IngestOpts = IngestOpts {
    format :: !Format
  , input :: !Input
  , keepOriginal :: !Bool
  , titleOverride :: !(Maybe Text)
  , formatLabel :: !(Maybe Text)
  , outMode :: !OutMode
  , writeJson :: !(Maybe FilePath)
  , userName :: !Text
  , docId :: !(Maybe Int)
  }
  deriving stock (Show)


data OaiSubCommand =
  JsonSC JsonSubCommand
  | SummarySC TargetsOpts
  | DocxSC OaiGenOpts
  | ElmSC OaiGenOpts
  | ProjFetchSC OaiProjFetchOpts
  | ConversationSC ConversationSubCommand
  deriving stock (Show)


data JsonSubCommand =
  PrintJS OaiPrintOpts TargetsOpts
  | StoreJS OaiStoreOpts TargetsOpts
  deriving stock (Show)

data OaiPrintOpts = OaiPrintOpts {
  exportPrB :: !Bool
  , jsonFilePR :: FilePath
  } deriving stock (Show)

data OaiStoreOpts = OaiStoreOpts {
  exportB :: !Bool
  , summariseB :: !Bool
  , dryRunB :: !Bool
  , jsonFileST :: !FilePath
  , followFilesST :: [FilePath]
  } deriving stock (Show)


data TargetsOpts = TargetsOpts {
    targetsTO :: [Text]
    , groupTO :: Maybe Text
  }
  deriving stock (Show)

data OaiGenOpts = OaiGenOpts {
  destPath :: FilePath
  , targets :: [Text]
  , group :: Maybe Text
}
  deriving stock (Show)

data OaiProjFetchOpts = OaiProjFetchOpts {
  label :: Text
  , sourcePath :: FilePath
}
  deriving stock (Show)


data ConversationSubCommand =
  DeserializeCS OaiGenOpts
  | ConvertCS TargetsOpts
  | DocxCS OaiGenOpts
  deriving stock (Show)


-- KMS options:
data KmsLocatorOpts = KmsLocatorOpts {
  title :: Maybe Text
  , key :: Maybe Text
} deriving stock (Show)


data KmsGetOpts = KmsGetOpts {
  key :: Text
  , filePath :: FilePath
} deriving stock (Show)

{-
The creation logic needs this:
 raw input: (Text, Int32, Int32, Int32, Int32, Maybe Text, Bool, Bool, Maybe Day, Int32)
SQL operation:
 insert into document (title, domain_fk, doc_type_fk, tier_fk, status_fk,
     owner_user_fk, residency, ai_allowed, legal_hold, due_date,
     created_by_user_fk)
  values
    ($1::text, $2::int4, $3::int4 ,$4::int4 ,$5::int4
    , $6::text?, $7::bool, $8::bool, $9::date?
    , $10::int4)

and:

AddAcl :: St.Statement (Int32, Text, Maybe Int32, Maybe Int32, Maybe Int32, Maybe Int32, Vector Text, Maybe Text, Maybe Text, Int32) Int32
qAddAcl =
  [singletonStatement|
    insert into kms.document_acl
      ( document_fk
      , principal
      , user_fk
      , group_fk
      , role_fk
      , org_fk
      , rights
      , scope
      , scope_value
      , created_by_user_fk
      )
    values
      ( $1::int4
      , $2::text
      , $3::int4?
      , $4::int4?
      , $5::int4?
      , $6::int4?
      , $7::text[]
      , $8::text?
      , $9::text?
      , $10::int4
      )
    returning uid::int4
  |]

On the CLI, we get the textual values for each required parameter, they will be converted by the Commands.Kms logic.
-}


data KmsCreateOpts = KmsCreateOpts {
  docTitle :: Text
  , code :: Text
  , domainDC :: Text
  , typeDC :: Text
  , tierDC :: Text
  , statusDC :: Text
  , ownerUserDC :: Text
  , emailDC :: Text
  , residencyDC :: Maybe Text
  , aiAllowedDC :: Bool
  , legalHoldDC :: Bool
  }
  deriving stock (Show)
