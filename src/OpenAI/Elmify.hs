{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Generate Elm (Fuddle/EasyWordy) discussion modules from DB-backed discourses.
--
-- This is the DB-variant of the earlier JSON-based 'discussionToElm' logic
-- in 'OpenAI.Parse'.
--
-- Entry point:
--   elmifyDiscourseByEid <conversation_eid> <pool> <title>
--
-- The <conversation_eid> is the external ID stored on oai.discourse.conversation_eid
-- (NOT the UUID).
--
module OpenAI.Elmify ( elmifyDiscourseByEid, discourseToElmItems)
where

import Control.Monad (when)
import Data.Bifunctor (first)

import qualified Data.ByteString.Lazy as Bl
import Data.Char (isAlpha, isAlphaNum, toUpper)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import qualified Data.Aeson as Ae

import Data.UUID (UUID)

import Hasql.Statement (Statement)
import qualified Hasql.Pool as Hp
import qualified Hasql.TH as TH

import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as TxS


import OpenAI.InOperations
  ( ContextDb(..)
  , MessageDb(..)
  , MessageKindDb(..)
  , MessageBodyDb(..)
  , SubActionDb(..)
  , SubActionKindDb(..)
  , SubActionBodyDb(..)
  , RefDb(..)
  , IssueDb(..)
  )
import qualified OpenAI.InOperations as InO
import OpenAI.Types (OaiCodeJson(..))


-- ---------------------------------
-- Public API
-- ---------------------------------

-- | Generate an Elm module containing the message/thread list for a discourse.
--
-- Writes a deterministic output file under 'elmifyOutDir':
--   <elmifyOutDir>/Discourse_<sanitised_eid>.elm
--
-- You can move/rename the file as needed for your EasyWordy/Fuddle repo layout.
--
elmifyDiscourseByEid :: FilePath -> Text -> Hp.Pool -> Text -> IO (Either String FilePath)
elmifyDiscourseByEid destPath discourseEid pool fileTitle = do
  eUuid <- Hp.use pool $
    TxS.transaction TxS.ReadCommitted TxS.Read (Tx.statement discourseEid selectDiscourseUuidByEid)

  case first show eUuid of
    Left err -> pure (Left err)
    Right Nothing -> pure (Left ("discourse not found for conversation_eid=" <> T.unpack discourseEid))
    Right (Just uuid) -> do
      eCtx <- InO.loadDiscourse pool uuid
      case eCtx of
        Left err -> pure (Left err)
        Right Nothing -> pure (Left ("discourse UUID resolved but load returned Nothing: " <> show uuid))
        Right (Just ctx) -> do
          let
            moduleName = "LegalNodes.Docs." <> T.unpack fileTitle -- mkElmModuleName discourseEid
            elmText = renderElmModule moduleName discourseEid fileTitle ctx
            outFile = destPath </> (T.unpack fileTitle <> ".elm")

          putStrLn $ "@[elmifyDiscourseByEid] sending to: " <> outFile
          createDirectoryIfMissing True destPath
          TIO.writeFile outFile elmText
          pure (Right outFile)


-- ---------------------------------
-- DB lookups
-- ---------------------------------

-- | Resolve a discourse UUID by the external conversation_eid.
selectDiscourseUuidByEid :: Statement Text (Maybe UUID)
selectDiscourseUuidByEid =
  [TH.maybeStatement|
    select
      d.uuid :: uuid
    from oai.discourse d
    where d.conversation_eid = $1 :: text
  |]


-- ---------------------------------
-- Elm rendering
-- ---------------------------------

-- | Render the full Elm module.
renderElmModule :: FilePath -> Text -> Text -> ContextDb -> Text
renderElmModule moduleName discussionId fileTitle ctx =
  let items = discourseToElmItems ctx
      issues = issuesToElm ctx
  in T.unlines
      [ "module " <> T.pack moduleName <> " exposing (content)"
      , ""
      , "import Components.LegalNodes.ReferenceDoc.Types as T"  -- NOTE: adapt to your actual EasyWordy/Fuddle thread-types module
      , ""
      , "content = (" <> elmString fileTitle <> ", " <> elmString discussionId <> ", messages)"
      , ""
      , "messages = " <> items
      ]


-- | Equivalent of the earlier 'discussionToElm': produces the Elm list literal.
--
-- Output shape matches the previous generator:
--
--   [ { id = "msg_1", kind = T.Question, title = "...", body = [...] }
--   , ...
--   ]
--
-- Non user/assistant roles are skipped (same as old logic).
discourseToElmItems :: ContextDb -> Text
discourseToElmItems ctx =
  let
    msgs = V.toList (messagesCo ctx)
    elmStructs = zipWith messageDbToElm msgs [1..]
    kept = filter (not . T.null) elmStructs
  in "[\n  " <> T.intercalate "\n  , " kept <> "\n  ]"


issuesToElm :: ContextDb -> Text
issuesToElm ctx =
  let xs = map textIs (V.toList (issuesCo ctx))
  in "[ " <> T.intercalate ", " (map elmString xs) <> " ]"


messageDbToElm :: MessageDb -> Int -> Text
messageDbToElm msg index =
  let msgID = "msg_" <> T.pack (show index)
  in case kindMb msg of
      UserMK ->
        case bodyMb msg of
          UserBody { textUB = t, summaryUB = s } ->
            let titleTxt = fromMaybe msgID s
            in "{ id = " <> elmString msgID
              <> ", kind = T.Question, title = " <> elmString titleTxt
              <> ", body = [ T.Basic " <> elmTriple t <> " ] }"
          _ -> ""

      AssistantMK ->
        case bodyMb msg of
          AssistantBody { responseTextAB = mResp, summaryAB = s } ->
            let titleTxt = fromMaybe msgID s
                respTxt  = fromMaybe "No response" mResp
                content  = map subActionDbToElm (V.toList (subActionsMb msg))
                        <> [ "T.LineSep"
                           , "T.Basic " <> elmTriple respTxt
                           ]
            in "{ id = " <> elmString msgID
              <> ", kind = T.Answer, title = " <> elmString titleTxt
              <> ", body = [ " <> T.intercalate "\n  , " content <> "\n    ] }"
          _ -> ""

      _ -> ""


subActionDbToElm :: SubActionDb -> Text
subActionDbToElm sa =
  case kindSa sa of
    IntermediateSAK ->
      case bodySa sa of
        IntermediateBody { textIb = t } ->
          if T.null t
            then "T.Intermediate \"\""
            else "T.Intermediate " <> elmTriple t
        _ -> "T.Error \"Invalid Intermediate payload\""

    ReflectionSAK ->
      case bodySa sa of
        ReflectionBody { summarySab = summ, contentSab = cont } ->
          "T.Reflect " <> elmString summ <> " " <> elmTriple cont
        _ -> "T.Error \"Invalid Reflection payload\""

    CodeSAK ->
      case bodySa sa of
        CodeBody { languageCb = lang, formatNameCb = fmt, textCb = txt } ->
          case T.toLower lang of
            "json" ->
              case Ae.eitherDecode (Bl.fromStrict $ TE.encodeUtf8 txt) :: Either String OaiCodeJson of
                Left err ->
                  "T.Error " <> elmTriple ("CodeSA: json err: " <> T.pack err)
                Right oaiCodeJson ->
                  case oaiCodeJson.typeOJ of
                    "document" -> "T.Document " <> elmTriple oaiCodeJson.contentOJ
                    other -> "T.Error " <> elmTriple ("CodeSA: json unknown type: " <> other <> "\n" <> oaiCodeJson.contentOJ)
            _ ->
              "T.Error " <> elmTriple ("CodeSA: " <> lang <> "\n" <> fromMaybe "No response format name" fmt <> "\n" <> txt)
        _ -> "T.Error \"Invalid Code payload\""

    ToolCallSAK ->
      case bodySa sa of
        ToolCallBody { toolNameTc = nm, toolInputTc = inp } ->
          "T.ToolCall " <> elmTriple (nm <> "\n" <> inp)
        _ -> "T.Error \"Invalid ToolCall payload\""


-- ---------------------------------
-- Escaping helpers
-- ---------------------------------

-- | For Elm normal string literals (single-line).
-- Ensures no raw newlines leak into "...".
elmString :: Text -> Text
elmString t =
  let t' = escapeCommon t
        & T.replace "\n" "\n"
        & T.replace "\r" ""
        & T.replace "…" "..."
  in "\"" <> t' <> "\""

-- | For Elm multiline string literals """...""".
elmTriple :: Text -> Text
elmTriple t =
  let t' = escapeCommon t
        -- prevent accidental terminator
        & T.replace "\"\"\"" "\\\"\\\"\\\""
  in "\"\"\"" <> t' <> "\"\"\""

escapeCommon :: Text -> Text
escapeCommon =
  -- Escape backslashes first to keep idempotence.
  T.replace "\\" "\\\\" . T.replace "\"" "\\\""

infixl 1 &
(&) :: a -> (a -> b) -> b
x & f = f x


-- ---------------------------------
-- Naming / paths
-- ---------------------------------

-- | Produce a deterministic, Elm-valid module name from the conversation_eid.
--
-- Elm module segments must start with an uppercase letter; we keep everything in
-- a single segment (no dots) to avoid directory layout coupling.
mkElmModuleName :: Text -> FilePath
mkElmModuleName eid =
  let raw = T.filter isAlphaNum eid
      base = if T.null raw then "Discourse" else raw
      -- ensure leading char is alpha and uppercase
      fixed =
        case T.uncons base of
          Nothing -> "Discourse"
          Just (c, rest) ->
            let lead = if isAlpha c then toUpper c else 'D'
            in T.cons lead rest
  in "Discourse_" <> T.unpack fixed
