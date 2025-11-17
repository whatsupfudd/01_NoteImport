{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module Options.ConfFile where

import qualified Control.Exception as Cexc
import Data.Text (Text, pack)

import GHC.Generics

import qualified System.Directory as Sdir
import qualified System.Environment as Senv
import qualified System.FilePath.Posix as Spsx
import qualified System.IO.Error as Serr

import qualified Data.Aeson as Aes
import qualified Data.Aeson.KeyMap as Akm
import qualified Data.Aeson.Key as Ak

import qualified Data.Yaml as Yaml


data FileOptions = FileOptions {
  debug :: Maybe Int
  , db :: Maybe PgDbOpts
  , notion :: Maybe NotionDef
 }
 deriving stock (Show, Generic)

data PgDbOpts = PgDbOpts {
  port :: Maybe Int
  , host :: Maybe String
  , user :: Maybe String
  , passwd :: Maybe String
  , dbase :: Maybe String
  , poolSize :: Maybe Int
  , poolTimeOut :: Maybe Int
  }
  deriving stock (Show, Generic)

newtype NotionDef = NotionDef {
  apiKeys :: [ PairReference ]
  }
  deriving stock (Show, Generic)

data PairReference = PairReference {
  label :: Text
  , ident :: Text
  }
 deriving (Generic, Show)

instance Aes.FromJSON PairReference where
  parseJSON (Aes.Object o) =
    let
      (label, value) = head $ Akm.toList o
      ident = case value of
        Aes.String s -> s
        Aes.Null -> ""
        _ -> "[PairReference.parseJSON] Invalid fct value: " <> (pack . show) value
    in
      pure (PairReference (pack . Ak.toString $ label) ident)



defaultConfName :: FilePath
-- HERE: modify config file to .config/noteimp.yaml or .noteimp/config.yaml
defaultConfName = ".fudd/docimp/config.yaml"


defaultConfigFilePath :: IO (Either String FilePath)
defaultConfigFilePath = do
  eiHomeDir <- Cexc.try $ Sdir.getHomeDirectory :: IO (Either Serr.IOError FilePath)
  case eiHomeDir of
    Left err -> pure . Left $ "@[defaultConfigFilePath] err: " <> show err
    Right aPath -> pure . Right $ Spsx.joinPath [aPath, defaultConfName]


-- YAML support:
instance Aes.FromJSON FileOptions
instance Aes.FromJSON PgDbOpts
instance Aes.FromJSON NotionDef


parseFileOptions :: FilePath -> IO (Either String FileOptions)
parseFileOptions filePath =
  let
    fileExt = Spsx.takeExtension filePath
  in case fileExt of
    ".yaml" -> do
      eiRez <- Yaml.decodeFileEither filePath
      case eiRez of
        Left err -> pure . Left $ "@[parseYaml] err: " <> show err
        Right aContent ->
          pure $ Right aContent
    _ -> pure . Left $ "@[parseFileOptions] unknown conf-file extension: " <> fileExt
