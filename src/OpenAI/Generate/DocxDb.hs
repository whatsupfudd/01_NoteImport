-- | DOCX generation from the DB-backed in-memory discourse model.
--
-- This mirrors 'OpenAI.GenDocx' but consumes 'ContextDb' (soon: DiscussionDb)
-- from 'OpenAI.InOperations'.
--
module OpenAI.Generate.DocxDb where

import Control.Monad (forM, void)
import Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString.Lazy as Bl
import Data.Bifunctor (first)
import Data.Default (def)
import Data.List (foldl')
import qualified Data.Map.Strict as Mp
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import qualified Data.Vector as V

import GHC.Generics (Generic)

import qualified System.Directory as Sdir

import qualified Data.Aeson as Ae

import qualified Text.Pandoc as P
import qualified Text.Pandoc.Options as Po
import Text.Pandoc.Walk (walk)

import OpenAI.Parse (sanitizeText)
import OpenAI.Types (OaiCodeJson(..))

import OpenAI.Deserialize.Discussion
  ( DiscussionDb(..)
  , MessageDb(..)
  , MessageKindDb(..)
  , MessageBodyDb(..)
  , AttachmentDb(..)
  , SubActionDb(..)
  , SubActionKindDb(..)
  , SubActionBodyDb(..)
  , IssueDb(..)
  )

import OpenAI.Generate.DocxGeneral

-- -------------------------------
-- Public API
-- -------------------------------

-- | Generate a DOCX file from a DB-backed 'DiscussionDb'/'ContextDb'.
--
-- Returns:
--   * Right () on success
--   * Left <error> on failure
--
writeDiscussionDbDocx :: DiscussionDb -> Text -> FilePath -> IO (Either String ())
writeDiscussionDbDocx discourse title outPath = do
  opWriterOpts <- case Po.writerReferenceDoc writerOpts of
    Just refPath -> do
      fileExistF <- Sdir.doesFileExist refPath
      liftIO $ putStrLn $ "@[writeDiscussionDbDocx] referenceDocPath: " <> show refPath <> " exists: " <> show fileExistF
      if fileExistF then
        pure writerOpts
      else
        pure writerOpts { Po.writerReferenceDoc = Nothing }
    Nothing -> pure writerOpts
  
  e <- P.runIO $ do
    pd0 <- discourseToPandoc title discourse
    let
      pd1 = normalizePandoc pd0
    bs <- P.writeDocx opWriterOpts pd1
    liftIO $ Bl.writeFile outPath bs
  pure $ first show (void e)


-- -------------------------------
-- Pandoc construction
-- -------------------------------

discourseToPandoc :: Text -> DiscussionDb -> P.PandocIO P.Pandoc
discourseToPandoc title discourse = do
  -- NOTE: DB model already carries stable seq ordering in 'messagesCo'.
  msgBlocks <- concat <$> forM (zip [1 :: Int ..] (V.toList discourse.messagesCo)) (uncurry messageDbToBlocks)

  let
    issueBlocks =
      if V.null discourse.issuesCo then
        []
      else
        [ P.Header 1 (mkIdAttr "issues") [P.Str "Issues"]
        , P.BulletList (map (\anIssue -> [P.Para [P.Str anIssue.textIs]]) (V.toList discourse.issuesCo))
        ]

  let
    meta = P.Meta (Mp.fromList
      [ ("title", P.MetaInlines [P.Str title])
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


messageDbToBlocks :: Int -> MessageDb -> P.PandocIO [P.Block]
messageDbToBlocks idx msg = do
  let anchor = "msg-" <> T.pack (show idx)

  case msg.kindMb of
    UserMK -> do
      let
        (txt, mbSummary) = case msg.bodyMb of
            UserBody t sm -> (t, sm)
            UnknownBody t -> (t, Nothing)
            _ -> ("", Nothing)

      body <- parseMarkdownBlocks txt
      let
        -- TODO: implement summary as part of DB-extracted info.
        summary = fromMaybe (fallbackOneLine txt) mbSummary
        header = messageHeaderWithSummaryDb "Manager" msg anchor summary
        attachments = attachmentsBlocksDb msg.attachmentsMb

      pure $
        [ styledDiv "GF M Header" [header]
        , styledDiv "GF Q Body" body
        ]
          <> attachments

    AssistantMK -> do
      -- Primary assistant response
      let (mainText, mbSummary) = case msg.bodyMb of
            AssistantBody rt rx sm -> (fromMaybe "" rx, sm)
            _ -> ("", Nothing)

      mainBody <-
        case mainText of
          "" -> pure []
          _ -> parseMarkdownBlocks mainText

      let
        summary = fromMaybe (fallbackOneLine mainText) mbSummary
        header = messageHeaderWithSummaryDb "Analyst" msg anchor summary
        attachments = attachmentsBlocksDb msg.attachmentsMb

      sub <- subActionsBlocksDb msg.subActionsMb

      pure $
        [ styledDiv "GF A Header" [header] ]
          <> sub
          <> [
              styledDiv "GF EndOfReflection" [ P.Para [ P.Str "end-of-reflection" ] ]
            , P.HorizontalRule
            ]
          <> [ styledDiv "GF A Body" mainBody ]
          <> attachments

    -- Keep these out of the default export (same behavior as GenDocx.hs).
    -- If you later want them, implement similarly to GenDocx's commented blocks.
    _ -> pure []


attachmentsBlocksDb :: V.Vector AttachmentDb -> [P.Block]
attachmentsBlocksDb v
  | V.null v = []
  | otherwise =
      let xs = map valueAt (V.toList v)
      in
      [ P.Para [P.Strong [P.Str "Attachments:" ]]
      , P.BulletList (map (\t -> [P.Para [P.Str t]]) xs)
      ]


subActionsBlocksDb :: V.Vector SubActionDb -> P.PandocIO [P.Block]
subActionsBlocksDb v = fmap concat $ forM (V.toList v) $ \sa ->
  case sa.kindSa of
    ReflectionSAK ->
      case sa.bodySa of
        ReflectionBody summ cont chunks fin -> do
          body <- parseMarkdownBlocks cont
          -- we keep reflections in export, clearly marked
          {-
          let
            -- styledDiv "GF Reflection Chunks"
            tailChunks = ([
                  P.BulletList (map (\c -> [P.Para [P.Str c]]) (V.toList chunks)) | not (V.null chunks)
                ])
              finLine = case fin of
                Nothing -> []
                Just b -> [P.Para [P.Strong [P.Str "Finished:"], P.Space, P.Str (if b then "true" else "false")]]
          -}
          pure [
            styledDiv "GF Reflection Summary" [ P.Para [ P.Strong [ P.Str summ ] ] ]
            , styledDiv "GF Reflection Body" body
            ]
              -- <> tailChunks
              -- <> finLine
        _ -> pure [P.Para [P.Str "(invalid reflection sub-action payload)"]]

    CodeSAK ->
      case sa.bodySa of
        CodeBody lang fmt txt ->
          case T.toLower lang of
            "json" ->
              case Ae.eitherDecode (Bl.fromStrict $ TE.encodeUtf8 txt) :: Either String OaiCodeJson of
                Left err ->
                  pure
                    [ P.Header 3 P.nullAttr [P.Str "Code"]
                    , P.Para [P.Str (lang <> " err: " <> sanitizeText (T.pack err))]
                    ]
                Right jsonBlock -> do
                  body <- parseMarkdownBlocks jsonBlock.contentOJ
                  pure
                    [ P.Header 3 P.nullAttr [P.Str "Document"]
                    , styledDiv "Text Block" body
                    ]

            -- Heuristic: treat markdown-ish language blocks as text
            "html" -> do
              body <- parseMarkdownBlocks txt
              pure
                [ P.Header 3 P.nullAttr [P.Str "HTML"]
                , styledDiv "HTML Block" body
                ]

            "python" -> do
              body <- parseMarkdownBlocks txt
              pure
                [ P.Header 3 P.nullAttr [P.Str "Python"]
                , styledDiv "Python Block" body
                ]

            _ ->
              -- Try to interpret as CodeQuerySearch JSON; otherwise render raw code.
              case Ae.eitherDecode (Bl.fromStrict $ TE.encodeUtf8 txt) :: Either String CodeQuerySearch of
                Left _ ->
                  pure
                    [ P.Header 3 P.nullAttr [P.Str ("Code (" <> lang <> ")")]
                    , styledDiv "GF Code Block" [P.CodeBlock (codeAttr lang) txt]
                    ]
                Right query ->
                  pure
                    [ P.Header 3 P.nullAttr [P.Str "Search"]
                    , styledDiv "CodeQuerySearch Block" [
                     P.BulletList (map (\q -> [P.Para [P.Str q.questionQS]]) query.searchQueryQS)
                    ]
                    ]
        _ -> pure [P.Para [P.Str "(invalid code sub-action payload)"]]

    ToolCallSAK ->
      case sa.bodySa of
        ToolCallBody nm inp ->
          pure
            [ P.Header 3 P.nullAttr [P.Str "Tool call"]
            , P.Para [P.Strong [P.Str "Tool:"], P.Space, P.Code nullAttr nm]
            , styledDiv "GF Tool Input" [P.CodeBlock (codeAttr "json") inp]
            ]
        _ -> pure [P.Para [P.Str "(invalid tool_call sub-action payload)"]]

    IntermediateSAK ->
      case sa.bodySa of
        IntermediateBody t -> do
          body <- parseMarkdownBlocks t
          pure
            [ P.Header 3 P.nullAttr [P.Str "Intermediate"]
            , styledDiv "GF Intermediate Body" body
            ]
        _ -> pure [P.Para [P.Str "(invalid intermediate sub-action payload)"]]


messageHeaderWithSummaryDb :: Text -> MessageDb -> Text -> Text -> P.Block
messageHeaderWithSummaryDb role msg anchorId tocLine =
  P.Div ("", [], [("custom-style", "GF MessageHeader")]) [
      styledDiv "GF MessageSummary" [ P.Para [
              P.Str role
          , P.Str ">"
          , P.Space
          , P.Str tocLine
          ]
        ]
      , styledDiv "GF MessageTimestamp" [ P.Para [ P.Str (renderDbTime msg.createdAtMb msg.updatedAtMb) ] ]
    ]
