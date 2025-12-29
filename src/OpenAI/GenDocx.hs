{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
module OpenAI.GenDocx where

import Control.Monad (forM, void)
import Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString.Lazy as Bl
import Data.Bifunctor (first)
import Data.Default (def)
import Data.Either (fromRight)
import Data.List (foldl')
import qualified Data.Map.Strict as Mp
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import GHC.Generics (Generic)

import qualified Data.Aeson as Ae

import qualified Text.Pandoc as P
import qualified Text.Pandoc.Options as Po
import Text.Pandoc.Walk (walk)

import Network.HTTP.Client
  ( Manager, Request(..), RequestBody(..), Response(..)
  , defaultManagerSettings, httpLbs, newManager, parseRequest
  , responseBody, responseTimeoutMicro, method, requestBody, requestHeaders
  )
import Network.HTTP.Types.Header (hContentType)


import OpenAI.Parse
import OpenAI.Types


newtype CodeQuerySearch = CodeQuerySearch {
  searchQueryQS :: [ QuestionsCQS ]
  }
  deriving (Show, Generic)

instance Ae.FromJSON CodeQuerySearch where
  parseJSON = Ae.withObject "CodeQuerySearch" $ \o -> CodeQuerySearch
    <$> o Ae..: "search_query"

newtype QuestionsCQS = QuestionsCQS {
  questionQS :: Text
  }
  deriving (Show, Generic)

instance Ae.FromJSON QuestionsCQS where
  parseJSON = Ae.withObject "QuestionsCQS" $ \o -> QuestionsCQS
    <$> o Ae..: "q"


-- | Generate a DOCX file from a 'Context'.
--
-- Returns:
--   * Right () on success
--   * Left <error> on failure
--
-- This is intentionally conservative: it prefers fidelity/clarity over
-- “trying too hard” to convert complex HTML into Word layout.
writeContextDocx :: Context -> Text -> FilePath -> IO (Either String ())
writeContextDocx context title outPath = do
  httpManager <- newOllamaManager
  e <- P.runIO $ do
    pd0 <- contextToPandoc httpManager title context
    let pd1 = normalizePandoc pd0
    bs <- P.writeDocx writerOpts pd1
    liftIO $ BL.writeFile outPath bs
  pure $ first show (void e)


-- -------------------------------
-- Pandoc construction
-- -------------------------------

contextToPandoc :: Manager -> Text -> Context -> P.PandocIO P.Pandoc
contextToPandoc httpManager title context = do
  msgBlocks <- concat <$> forM (zip [1 :: Int ..] (reverse context.messages)) (uncurry (messageToBlocks httpManager))

  let
    issueBlocks = if null context.issues then
        []
      else
        [ P.Header 1 (mkIdAttr "issues") [P.Str "Issues"]
        , P.BulletList (map (\t -> [P.Para [P.Str t]]) context.issues)
        ]

  let
    meta = P.Meta (Mp.fromList [
          ("title", P.MetaInlines [P.Str title])
          , ("authors", P.MetaList [])
          , ("date", P.MetaString "")
      ])

  pure $
    P.Pandoc meta
      ( [ P.Header 1 (mkIdAttr "conversation") [P.Str "Conversation"]
        , P.Para [P.Str "(presentation export)"]
        ]
          <> msgBlocks
          <> issueBlocks
      )


messageToBlocks :: Manager -> Int -> MessageFsm -> P.PandocIO [P.Block]
messageToBlocks httpManager idx msg = do
  let anchor = "msg-" <> T.pack (show idx)

  case msg of
    UserMF t um -> do
      body <- parseMarkdownBlocks um.textUM
      eiSummary <- liftIO $ summarizeOneLineTOC httpManager um.textUM
      let
        summary = fromRight (fallbackOneLine um.textUM) eiSummary
        header = messageHeaderWithSummary "Manager" t anchor summary
            -- messageHeader "Client" t anchor
        attachments = attachmentsBlocks um.attachmentsUM
      pure $
        [styledDiv "GF M Header" [header]]
          <> [styledDiv "GF Q Body" body]
          <> attachments

    AssistantMF t am -> do
      -- Primary assistant response
      let
        mainText = maybe "" (\rep -> rep.textRA) am.response
      mainBody <-
        case mainText of
          "" -> pure []
          _ -> parseMarkdownBlocks mainText
      eiSummary <- liftIO $ summarizeOneLineTOC httpManager mainText
      let
        summary = fromRight (fallbackOneLine mainText) eiSummary
        header = messageHeaderWithSummary "Analyst" t anchor summary
        attachments = attachmentsBlocks am.attachmentsAM

      -- SubActions: we render them as optional “appendix” blocks per message.
      sub <- subActionsBlocks am.subActions
      -- let header = messageHeader "Analyst" t anchor

      pure $ [styledDiv "GF A Header" [header]] <> sub <> [styledDiv "GF A Body" mainBody] <> attachments
    _ -> pure []
    {-
    SystemMF t sm ->
      case sm.textSM of
        "" -> pure []
        _ -> do
          body <- parseMarkdownBlocks sm.textSM
          let header = messageHeader "System" t anchor
          pure [header, styledDiv "GF System Body" body]

    ToolMF t tm -> do
      let header = messageHeader "Tool" t anchor
      pure
        [ header
        , styledDiv "GF Tool Body" [P.CodeBlock nullAttr tm.textTM]
        ]

    UnknownMF t um -> do
      body <- parseMarkdownBlocks um.textUM
      let header = messageHeader "Unknown" t anchor
      pure [header, styledDiv "GF Unknown Body" body]
    -}

messageHeader :: Text -> Timing -> Text -> P.Block
messageHeader role timing anchorId =
  P.Header 2 (mkIdAttr anchorId)
    [ P.Str role
    , P.Space
    {- , P.Str "—"
    , P.Space
    , P.Str (renderTiming timing)
    -}
    ]


attachmentsBlocks :: [Text] -> [P.Block]
attachmentsBlocks [] = []
attachmentsBlocks xs =
  [ P.Para [P.Strong [P.Str "Attachments:" ]]
  , P.BulletList (map (\t -> [P.Para [P.Str t]]) xs)
  ]


subActionsBlocks :: [SubAction] -> P.PandocIO [P.Block]
subActionsBlocks sas = fmap concat $ forM sas $ \case
    -- We keep reflections in the export, but clearly marked.
    ReflectionSA rf -> do
      body <- parseMarkdownBlocks rf.contentRF
      pure
        [ P.Header 3 P.nullAttr [P.Str "Reflection"]
        , styledDiv "GF Reflection Body" body
        ]

    -- Code actions become code blocks, with language captured.
    CodeSA cc ->
      case cc.languageCC of
        "json" ->
          case Ae.eitherDecode (Bl.fromStrict $ TE.encodeUtf8 cc.textCC) :: Either String OaiCodeJson of
            Left err -> pure [
              P.Header 3 P.nullAttr [P.Str "CodeSA"]
              , P.Para [P.Str (cc.languageCC <> " err: " <> sanitizeText (T.pack err))]
              ]
            Right jsonBlock -> do
              body <- parseMarkdownBlocks jsonBlock.contentOJ
              pure [
                  P.Header 3 P.nullAttr [P.Str "Document"]
                , styledDiv "Text Block" body
                ]
        "html" -> do
          body <- parseMarkdownBlocks cc.textCC
          pure [
              P.Header 3 P.nullAttr [P.Str "Reflection"]
            , styledDiv "HTML Block" body
            ]
        "python" -> do
          body <- parseMarkdownBlocks cc.textCC
          pure [
              P.Header 3 P.nullAttr [P.Str "Reflection"]
            , styledDiv "Python Block" body
            ]
        _ ->
          case Ae.eitherDecode (Bl.fromStrict $ TE.encodeUtf8 cc.textCC) :: Either String CodeQuerySearch of
            Left err -> pure [
                P.Header 3 nullAttr [P.Str ("Code (" <> languageCC cc <> ")")]
              , styledDiv "GF Code Block" [P.CodeBlock (codeAttr cc.languageCC) cc.textCC]
              ]
            Right query -> pure
              [
              P.Header 3 P.nullAttr [P.Str "Search"]
              , styledDiv "CodeQuerySearch Block" (map (\q -> P.Para [P.Str q.questionQS]) query.searchQueryQS)
              ]

    -- Tool calls: keep inputs for audit/repro.
    ToolCallSA tc ->
      pure
        [ P.Header 3 nullAttr [P.Str "Tool call"]
        , P.Para [P.Strong [P.Str "Tool:"], P.Space, P.Code nullAttr tc.toolNameTC]
        , styledDiv "GF Tool Input" [P.CodeBlock (codeAttr "json") tc.toolInputTC]
        ]

    -- Intermediate notes: plain paragraphs.
    IntermediateSA t -> do
      body <- parseMarkdownBlocks t
      pure
        [ P.Header 3 nullAttr [P.Str "Intermediate"]
        , styledDiv "GF Intermediate Body" body
        ]


-- -------------------------------
-- Parsing & normalization
-- -------------------------------

parseMarkdownBlocks :: Text -> P.PandocIO [P.Block]
parseMarkdownBlocks src = do
  let src' = normalizeSourceText src
  P.Pandoc _ bs <- P.readMarkdown readerOpts src'
  pure bs

-- | Light pre-normalization on the raw source string before Pandoc.
--
-- For v1 we do *not* attempt a true MathML → TeX conversion here.
-- We only ensure the reader sees coherent Markdown.
normalizeSourceText :: Text -> Text
normalizeSourceText = id

-- | Normalize the Pandoc AST so DOCX output remains readable.
--
-- Policies:
--   * Complex raw HTML blocks are converted into fenced code blocks (language=html).
--   * MathML inside raw HTML is also “literalized” as code for now (language=mathml).
normalizePandoc :: P.Pandoc -> P.Pandoc
normalizePandoc =
  walk normalizeHtmlBlocks
    . walk normalizeHtmlInlines

normalizeHtmlBlocks :: P.Block -> P.Block
normalizeHtmlBlocks b = case b of
  P.RawBlock (P.Format "html") h
    | containsMathML h ->
        styledDiv "GF Code Block" [P.CodeBlock (codeAttr "mathml") h]
    | isComplexHtml h ->
        styledDiv "GF Code Block" [P.CodeBlock (codeAttr "html") h]
    | otherwise -> P.RawBlock (P.Format "html") h
  _ -> b

normalizeHtmlInlines :: P.Inline -> P.Inline
normalizeHtmlInlines i = case i of
  P.RawInline (P.Format "html") h
    | containsMathML h -> P.Code nullAttr "<mathml>" -- inline MathML is rare; keep safe.
    | isComplexHtml h -> P.Code nullAttr (truncateInline h)
    | otherwise -> P.RawInline (P.Format "html") h
  _ -> i

truncateInline :: Text -> Text
truncateInline t =
  let t' = T.strip t
  in if T.length t' <= 80 then t' else T.take 77 t' <> "…"

containsMathML :: Text -> Bool
containsMathML t =
  let tl = T.toLower t
  in "<math" `T.isInfixOf` tl || "</math" `T.isInfixOf` tl

-- Heuristic: treat layout-heavy blocks as “complex HTML”.
isComplexHtml :: Text -> Bool
isComplexHtml t =
  let tl = T.toLower t
      has tag = tag `T.isInfixOf` tl
      longish = T.length (T.strip t) > 180
      layouty =
        or
          [ has "<div"
          , has "<section"
          , has "<article"
          , has "<table"
          , has "<style"
          , has "class="
          , has "tailwind"
          ]
  in longish || layouty


-- -------------------------------
-- Options
-- -------------------------------

readerOpts :: Po.ReaderOptions
readerOpts =
  def
    { Po.readerExtensions = mdExtensions
    }

-- Extensions tuned for LLM-ish mixed Markdown.
mdExtensions :: Po.Extensions
mdExtensions =
  foldl' (flip Po.enableExtension) Po.githubMarkdownExtensions
    [ Po.Ext_raw_html
    , Po.Ext_fenced_code_blocks
    , Po.Ext_backtick_code_blocks
    , Po.Ext_pipe_tables
    , Po.Ext_table_captions
    , Po.Ext_task_lists
    , Po.Ext_tex_math_dollars
    , Po.Ext_tex_math_single_backslash
    ]

writerOpts :: Po.WriterOptions
writerOpts =
  def
    { Po.writerTableOfContents = True
    , Po.writerTOCDepth = 2
    , Po.writerReferenceDoc = referenceDocPath
    }

-- Set to (Just "./assets/reference.docx") once you check in your style template.
referenceDocPath :: Maybe FilePath
referenceDocPath = Nothing


-- -------------------------------
-- Small helpers
-- -------------------------------

nullAttr :: P.Attr
nullAttr = ("", [], [])

mkIdAttr :: Text -> P.Attr
mkIdAttr ident = (ident, [], [])

styledDiv :: Text -> [P.Block] -> P.Block
styledDiv styleName =
  P.Div ("", [], [("custom-style", styleName)])

codeAttr :: Text -> P.Attr
codeAttr lang = ("", [lang], [])

renderTiming :: Timing -> Text
renderTiming (Timing c u) =
  case (c, u) of
    (Just ct, _) -> renderEpoch ct
    (Nothing, Just ut) -> "(upd) " <> renderEpoch ut
    _ -> "(no timestamp)"

renderEpoch :: Double -> Text
renderEpoch secs =
  let utc :: UTCTime
      utc = posixSecondsToUTCTime (realToFrac secs)
  in T.pack (formatTime defaultTimeLocale "%y-%m-%d %H:%M:%S" utc)

----- Summarisation service: ----
ollamaBaseUrl :: String
ollamaBaseUrl = "http://chodov:11434"

ollamaGeneratePath :: String
ollamaGeneratePath = "/api/generate"

-- Pick your model name (or thread it from config)
ollamaModel :: Text
ollamaModel = "llama3"  -- <-- change to whatever you run in Ollama

data OllamaGenerateReq = OllamaGenerateReq
  { ogrModel  :: !Text
  , ogrPrompt :: !Text
  , ogrStream :: !Bool
  }

instance Ae.ToJSON OllamaGenerateReq where
  toJSON (OllamaGenerateReq m p s) =
    Ae.object
      [ "model"  Ae..= m
      , "prompt" Ae..= p
      , "stream" Ae..= s
      ]

-- /api/generate (stream=false) typically returns { response: "...", done: true, ... }
newtype OllamaGenerateResp = OllamaGenerateResp { ogResponse :: Text }

instance Ae.FromJSON OllamaGenerateResp where
  parseJSON = Ae.withObject "OllamaGenerateResp" $ \o ->
    OllamaGenerateResp <$> o Ae..: "response"

newOllamaManager :: IO Manager
newOllamaManager = newManager defaultManagerSettings

-- Create a one-line TOC label from arbitrary content.
summarizeOneLineTOC :: Manager -> Text -> IO (Either String Text)
summarizeOneLineTOC mgr content = do
  req0 <- parseRequest (ollamaBaseUrl <> ollamaGeneratePath)

  let prefix =
        "As an expert in journalism, editorial reviews, legal affairs, dissertation analysis and information management, you are tasked by management with creating a short title for the following entry in a document that we'll integrate in the table of content:<entry>"
      postfix = "</entry>. Provide a short one-liner title for this entry, keep it in 20 words or less, be affirmative and use proper, concise and descriptive English. Don't go into details."
      prompt  = prefix <> content <> postfix

      body = Ae.encode (OllamaGenerateReq ollamaModel prompt False)

      req =
        req0
          { method = "POST"
          , requestHeaders = [(hContentType, "application/json")]
          , requestBody = RequestBodyLBS body
          , responseTimeout = responseTimeoutMicro (600 * 1000000)
          }

  resp <- httpLbs req mgr
  case Ae.eitherDecode (responseBody resp) of
    Left err -> pure (Left err)
    Right (OllamaGenerateResp t) ->
      pure (Right (cleanOneLine t))

cleanOneLine :: Text -> Text
cleanOneLine =
    truncateWords 14
  . truncateChars 120
  . collapseWS
  . T.strip

collapseWS :: Text -> Text
collapseWS =
  T.unwords . T.words . T.map (\c -> if c == '\n' || c == '\r' || c == '\t' then ' ' else c)

truncateChars :: Int -> Text -> Text
truncateChars n t
  | T.length t <= n = t
  | otherwise       = T.take (max 0 (n - 1)) t <> "…"

truncateWords :: Int -> Text -> Text
truncateWords n t =
  let ws = T.words t
  in T.unwords (take n ws)

-- Fallback if the model errors/timeouts/etc.
fallbackOneLine :: Text -> Text
fallbackOneLine =
    truncateWords 14
  . truncateChars 120
  . collapseWS
  . T.take 600
  . T.strip

messageHeaderWithSummary :: Text -> Timing -> Text -> Text -> P.Block
messageHeaderWithSummary role timing anchorId tocLine =
  P.Header 2 (mkIdAttr anchorId)
    [ P.Str tocLine
    , P.Space
    , P.Str ":"
    , P.Space
    , P.Str role
    , P.Space
    , P.Str "—"
    , P.Space
    , P.Str (renderTiming timing)
    ]
