{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Background summarisation of user/assistant messages for TOC-style labels.
--
-- This module:
--   * iterates over a DB-backed conversation in memory (ContextDb)
--   * calls Ollama /api/generate to produce a one-line title/summary per message
--   * stores results in message_summary(message_fk, content)
--
module OpenAI.Summarisation
  ( summarizeDiscourseMessages
  , newOllamaManager
  , summarizeMessageOneLine
  )
where

import Control.Monad (forM_)
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import qualified Data.Aeson as Ae
import GHC.Generics (Generic)

import Hasql.Statement (Statement)
import qualified Hasql.Pool as Hp
import qualified Hasql.TH as TH

import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as TxS

import Network.HTTP.Client
  ( Manager, Request(..), RequestBody(..), Response(..)
  , defaultManagerSettings, httpLbs, newManager, parseRequest
  , httpLbs, responseBody, responseTimeoutMicro
  )
import Network.HTTP.Types.Header (hContentType)

import OpenAI.Deserialize.Discussion
  ( DiscussionDb(..)
  , MessageDb(..)
  , MessageKindDb(..)
  , MessageBodyDb(..)
  , RefDb(..)
  )


-- -------------------------------
-- Public API
-- -------------------------------

-- | Iterate over all UserMK and AssistantMK messages in the given ContextDb,
-- generate one-line summaries using Ollama, and store them in 'message_summary'.
--
-- Behavior:
--   * skips messages with empty content
--   * skips messages that already have a row in message_summary
--   * on successful summarisation, upserts the DB row
--   * continues on errors and returns a combined error string at the end
summarizeDiscourseMessages :: Hp.Pool -> Manager -> DiscussionDb -> IO (Either String ())
summarizeDiscourseMessages pool httpManager ctx = do
  let candidates :: [(Int64, Text)]
      candidates =
        [ (uidRd (refMb m), txt)
        | m <- V.toList (messagesCo ctx)
        , kindMb m == UserMK || kindMb m == AssistantMK
        , let txt = messageMainText m
        , not (T.null (T.strip txt))
        ]

  if null candidates
    then pure (Right ())
    else do
      -- skip already summarised messages
      let uidsVec = V.fromList (map fst candidates)
      existing <- Hp.use pool $
        TxS.transaction TxS.ReadCommitted TxS.Read (Tx.statement uidsVec selectExistingSummaryMessageFks)

      case first show existing of
        Left e -> pure (Left e)
        Right existingFks -> do
          let doneSet :: Set Int64
              doneSet = Set.fromList (V.toList existingFks)

          errs <- foldlMIO
            (\acc (mid, txt) ->
              if Set.member mid doneSet
                then pure acc
                else do
                  ei <- summarizeMessageOneLine httpManager txt
                  case ei of
                    Left err -> pure (acc <> ["[ollama] message_fk=" <> show mid <> ": " <> err])
                    Right summary -> do
                      ins <- Hp.use pool $
                        TxS.transaction TxS.ReadCommitted TxS.Write (Tx.statement (mid, summary) upsertMessageSummary)
                      case first show ins of
                        Left e -> pure (acc <> ["[db] message_fk=" <> show mid <> ": " <> e])
                        Right _ -> pure acc
            )
            []
            candidates

          pure $
            if null errs
              then Right ()
              else Left (unlines errs)


-- | Summarize a single message's content as a one-line TOC label.
--
-- This mirrors the prompt used previously in GenDocx.
summarizeMessageOneLine :: Manager -> Text -> IO (Either String Text)
summarizeMessageOneLine mgr content = do
  req0 <- parseRequest (ollamaBaseUrl <> ollamaGeneratePath)

  let prefix =
        "As an expert in journalism, editorial reviews, legal affairs, dissertation analysis and information management, you are tasked by management with creating a short title for the following entry in a document that we'll integrate in the table of content:<entry>"
      postfix =
        "</entry>. Provide a short one-liner title for this entry, keep it within 20 words, be affirmative and use proper, concise and descriptive English. Don't go into details, don't use markdown."
      prompt = prefix <> content <> postfix

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


-- -------------------------------
-- Message content selection
-- -------------------------------

-- | Extract the main user/assistant text that we want to summarise.
messageMainText :: MessageDb -> Text
messageMainText m =
  case kindMb m of
    UserMK ->
      case bodyMb m of
        UserBody t mbSummary -> t
        _ -> ""
    AssistantMK ->
      case bodyMb m of
        AssistantBody _ mTxt mbSummary -> fromMaybe "" mTxt
        _ -> ""
    _ -> ""


-- -------------------------------
-- DB statements (Hasql-TH)
-- -------------------------------

-- | Existing summaries for a set of message uids.
selectExistingSummaryMessageFks :: Statement (Vector Int64) (Vector Int64)
selectExistingSummaryMessageFks =
  [TH.vectorStatement|
    select
      message_fk :: int8
    from oai.message_summary
    where message_fk = any($1 :: int8[])
  |]

-- | Upsert a summary for a message.
upsertMessageSummary :: Statement (Int64, Text) ()
upsertMessageSummary =
  [TH.resultlessStatement|
    insert into oai.message_summary (message_fk, content)
    values ($1 :: int8, $2 :: text)
    on conflict (message_fk) do update
      set content = excluded.content
  |]


-- -------------------------------
-- Ollama protocol
-- -------------------------------

-- Environment: host "aiserver" port 11494, plain HTTP
ollamaBaseUrl :: String
ollamaBaseUrl = "http://chodov:11434"

ollamaGeneratePath :: String
ollamaGeneratePath = "/api/generate"

-- If you want this configurable, thread it from your app config.
ollamaModel :: Text
ollamaModel = "llama3"

newOllamaManager :: IO Manager
newOllamaManager = newManager defaultManagerSettings


-- /api/generate (stream=false) typically returns { response: "...", done: true, ... }
newtype OllamaGenerateResp = OllamaGenerateResp { ogResponse :: Text }
  deriving (Show, Generic)

instance Ae.FromJSON OllamaGenerateResp where
  parseJSON = Ae.withObject "OllamaGenerateResp" $ \o ->
    OllamaGenerateResp <$> o Ae..: "response"

data OllamaGenerateReq = OllamaGenerateReq
  { ogrModel  :: !Text
  , ogrPrompt :: !Text
  , ogrStream :: !Bool
  }
  deriving (Show, Generic)

instance Ae.ToJSON OllamaGenerateReq where
  toJSON (OllamaGenerateReq m p s) =
    Ae.object
      [ "model"  Ae..= m
      , "prompt" Ae..= p
      , "stream" Ae..= s
      ]


-- -------------------------------
-- Text cleanup (same semantics as GenDocx)
-- -------------------------------

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


-- -------------------------------
-- Small IO folds
-- -------------------------------

foldlMIO :: (b -> a -> IO b) -> b -> [a] -> IO b
foldlMIO f = iterFold
  where
    iterFold z [] = pure z
    iterFold z (y:ys) = f z y >>= \z' -> iterFold z' ys
