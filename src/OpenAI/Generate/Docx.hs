{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
module OpenAI.Generate.Docx where

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
import OpenAI.Generate.DocxGeneral


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
      -- eiSummary <- liftIO $ summarizeOneLineTOC httpManager um.textUM
      let
        eiSummary = Left "(no summary)"
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
      -- eiSummary <- liftIO $ summarizeOneLineTOC httpManager mainText
      let
        eiSummary = Left "(no summary)"
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
