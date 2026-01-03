{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Options.Cli where

import Data.Text (Text)
import Options.Applicative

import HBDoc.Types (EnrichmentLevel (..), StructureSource (..))
import Options.Types

newtype EnvOptions = EnvOptions {
    appHome :: Maybe Text
  }

data CliOptions = CliOptions {
  debug :: Maybe Int
  , configFile :: Maybe FilePath
  , job :: Maybe Command
 }
 deriving stock (Show)

data GlobalOptions = GlobalOptions {
  confPathGO :: String
  , debugGO :: String
  }


data Command =
  HelpCmd
  | VersionCmd
  | NotionCmd Text
  | DocXCmd DocXOpts    -- v1 parsing.
  | IngestCmd IngestOpts
  | OaiCmd OaiSubCommand
  deriving stock (Show)

{- HERE: Additional structures for holding new command parameters:
Eg:
data ImportOpts = ImportOpts {
    taxonomy :: Text
    , path :: Text
  }
-}

parseCliOptions :: IO (Either String CliOptions)
parseCliOptions =
  Right <$> execParser parser

parser :: ParserInfo CliOptions
parser =
  info (helper <*> argumentsP) $
    fullDesc <> progDesc "notionapp." <> header "notionapp - ."


argumentsP :: Parser CliOptions
argumentsP = do
  buildOptions <$> globConfFileDef <*> hsubparser commandDefs
  where
    buildOptions :: GlobalOptions -> Command -> CliOptions
    buildOptions globs cmd =
      let
        mbConfPath = case globs.confPathGO of
          "" -> Nothing
          aValue -> Just aValue
        mbDebug = case globs.debugGO of
          "" -> Nothing
          aValue -> Just (read aValue :: Int)
      in
      CliOptions {
        debug = mbDebug
        , configFile = mbConfPath
        , job = Just cmd
      }


globConfFileDef :: Parser GlobalOptions
globConfFileDef =
  GlobalOptions <$>
    strOption (
      long "config"
      <> short 'c'
      <> metavar "notionappCONF"
      <> value ""
      <> showDefault
      <> help "Global config file (default is ~/.notionapp/config.yaml)."
    )
    <*>
    strOption (
      long "debug"
      <> short 'd'
      <> metavar "DEBUGLVL"
      <> value ""
      <> showDefault
      <> help "Global debug state."
    )


commandDefs :: Mod CommandFields Command
commandDefs =
  let
    cmdArray = [
      ("help", pure HelpCmd, "Help about any command.")
      , ("version", pure VersionCmd, "Shows the version number of importer.")
      , ("notion", notionOpts, "Notion command.")
      , ("docx", DocXCmd <$> docxOpts, "DocX command.")
      , ("ingest", IngestCmd <$> ingestOpts, "Ingest command.")
      , ("oai", oaiSubCommands, "OpenAI JSON command.")
      ]
    headArray = head cmdArray
    tailArray = tail cmdArray
  in
    foldl (\accum aCmd -> cmdBuilder aCmd <> accum) (cmdBuilder headArray) tailArray
  where
    cmdBuilder (label, cmdDef, desc) =
      command label (info cmdDef (progDesc desc))

notionOpts :: Parser Command
notionOpts =
  NotionCmd <$> strArgument (metavar "WORDSPACE" <> help "Notion workspace to use.")

-- v1 parser options:
docxOpts :: Parser DocXOpts
docxOpts =
  DocXOpts <$>
      strOption (long "in" <> short 'i' <> help "Input file path" <> metavar "FILE")
    <*> optional (strOption (long "out" <> short 'o' <> help "Output file path" <> metavar "FILE"))
    <*> switch (long "yaml" <> help "Output as YAML" <> showDefault)
    <*> switch (long "promote" <> help "Promote numbered paragraphs to headers" <> showDefault)

ingestOpts :: Parser IngestOpts
ingestOpts =
  IngestOpts
    <$> formatP
    <*> inputP
    <*> structureP
    <*> enrichP
    <*> switch (long "keep-original" <> help "Retain original bytes in result")
    <*> optional (strOption (long "title" <> metavar "TEXT" <> help "Override title"))
    <*> optional (strOption (long "format-label" <> metavar "TEXT" <> help "Override format label (default: docx/html/markdown)"))
    <*> outModeP
    <*> optional (strOption (long "write-json" <> metavar "FILE" <> help "Write JSON to file (default: stdout)"))
    <*> strOption (long "user" <> metavar "USERNAME" <> help "User" <> value "nobody")
    <*> optional (option (eitherReader toDocId) (long "doc-id" <> metavar "DOCID" <> help "Document ID"))
  where
    toDocId aStr = case reads aStr of
      [(n, "")] -> Right n
      _ -> Left $ "Invalid doc id: " <> aStr

    formatP :: Parser Format
    formatP =
      option (eitherReader toFmt)
        ( long "format" <> short 'f' <> metavar "docx|html|markdown"
       <> value FDocx <> showDefaultWith (const "docx")
       <> help "Input format" )
      where
        toFmt s = case s of
          "docx"     -> Right FDocx
          "html"     -> Right FHtml
          "markdown" -> Right FMarkdown
          other      -> Left $ "Unknown format: " <> other

    inputP :: Parser Input
    inputP =
      (FromFile <$> strOption (long "file" <> short 'i' <> metavar "FILE" <> help "Input file"))
      <|> flag' FromStdin (long "stdin" <> help "Read from stdin")

    structureP :: Parser StructureSource
    structureP =
      option (eitherReader toStruct)
        ( long "structure" <> metavar "pandoc|xml|auto"
       <> value StructureFromPandoc
       <> showDefaultWith (const "pandoc")
       <> help "Choose structural importer for DOCX" )
      where
        toStruct = \case
          "pandoc" -> Right StructureFromPandoc
          "xml"    -> Right StructureFromXml
          "auto"   -> Right StructureAuto
          other    -> Left $ "Unknown structure: " <> other

    enrichP :: Parser EnrichmentLevel
    enrichP =
      option (eitherReader toEnrich)
        ( long "enrich" <> metavar "none|min|full"
       <> value EnrichDocxMinimal
       <> showDefaultWith (const "min")
       <> help "DOCX XML enrichment level" )
      where
        toEnrich = \case
          "none" -> Right EnrichNone
          "min"  -> Right EnrichDocxMinimal
          "full" -> Right EnrichDocxFull
          other  -> Left $ "Unknown enrich: " <> other

    outModeP :: Parser OutMode
    outModeP =
      option (eitherReader toOut)
        ( long "out" <> metavar "json|pretty"
       <> value OutPretty
       <> showDefaultWith (const "pretty")
       <> help "Output mode" )
      where
        toOut = \case
          "json"   -> Right OutJson
          "pretty" -> Right OutPretty
          other    -> Left $ "Unknown out mode: " <> other


oaiSubCommands :: Parser Command
oaiSubCommands =
  OaiCmd <$> subparser (
    -- TODO: json subcommands: print, save.
    command "json" (info (JsonSC <$> jsonSubCommands) (progDesc "Ingest OpenAI conversations as JSON files."))
    -- TODO: conversion: conversation to discussion, discussion to HBDoc.
    <> command "docx" (info (DocxSC <$> oaiGenOpts) (progDesc "Create DOCX documents."))
    <> command "summary" (info (SummarySC <$> oaiTargetsOpts) (progDesc "Summarise discussions."))
    <> command "elm" (info (ElmSC <$> oaiGenOpts) (progDesc "Create Elm documents."))
    <> command "project" (info (ProjFetchSC <$> oaiProjFetchOpts) (progDesc "Ingest OpenAI Project html files."))
    <> command "conversation" (info (ConversationSC <$> conversationSubCommands) (progDesc "Process conversations."))
  )

jsonSubCommands :: Parser JsonSubCommand
jsonSubCommands =
  subparser (
    command "print" (info (PrintJS <$> oaiJsonOpts <*> oaiTargetsOpts) (progDesc "Print the JSON to the console."))
    <> command "store" (info (StoreJS <$> oaiJsonOpts <*> oaiTargetsOpts) (progDesc "Store the JSON to the database."))
  )

oaiJsonOpts :: Parser OaiJsonOpts
oaiJsonOpts =
  OaiJsonOpts <$>
    switch (long "export" <> short 'e' <> help "Comes from the OpenAI export service." <> showDefault)
    <*> strArgument (metavar "JSONFILE" <> help "JSON file file path")

oaiTargetsOpts :: Parser TargetsOpts
oaiTargetsOpts =
  TargetsOpts <$>
    many (strOption (long "target" <> short 't' <> help "A target to summarize." <> metavar "TARGET"))
    <*> optional (strOption (long "group" <> short 'g' <> help "The group of discussions to select." <> metavar "GROUP"))

oaiGenOpts :: Parser OaiGenOpts
oaiGenOpts =
  OaiGenOpts <$>
    strArgument (help "Destination directory." <> metavar "DESTDIR")
    <*> some (strOption (long "target" <> short 't' <> help "A target to elmify." <> metavar "TARGET"))
    <*> optional (strOption (long "group" <> short 'g' <> help "The group of discussions to select." <> metavar "GROUP"))

oaiProjFetchOpts :: Parser OaiProjFetchOpts
oaiProjFetchOpts =
  OaiProjFetchOpts <$>
    strOption (long "label" <> short 'l' <> help "The label of the project to fetch." <> metavar "LABEL")
    <*> strArgument (help "Source file path." <> metavar "SOURCEPATH")


conversationSubCommands :: Parser ConversationSubCommand
conversationSubCommands =
  subparser (
    command "deserialize" (info (DeserializeCS <$> oaiGenOpts) (progDesc "Deserialize a conversation."))
    <> command "convert" (info (ConvertCS <$> oaiTargetsOpts) (progDesc "Convert a conversation to a discussion."))
    <> command "docx" (info (DocxCS <$> oaiGenOpts) (progDesc "Create DOCX documents."))
  )