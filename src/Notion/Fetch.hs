{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Minimal Notion client for:
--   1) Fetching ALL pages visible to an integration (workspace-scoped via /v1/search with filter=page)
--   2) Fetching page content as a tree of Blocks (recursively follows children)
--
-- Usage (sketch):
--   import qualified Notion as N
--   main = do
--     n <- N.mkNotion "secret_..."  -- your integration secret
--     pages <- N.fetchAllPages n
--     putStrLn ("Found pages: " ++ show (length pages))
--     -- Pull one page's full block tree
--     case pages of
--       (p:_) -> do
--         blocks <- N.fetchPageBlocksDeep n p.id
--         print (length blocks)
--       _ -> pure ()
--
-- Notes:
--   • This module focuses on IDs/URLs for pages and generic block trees. You can enrich the
--     Page and Block types as needed (e.g., titles, rich text spans, etc.).
--   • Headers: Authorization bearer + Notion-Version are set automatically.
--   • Pagination is fully handled for search and blocks children endpoints.

module Notion.Fetch
  ( Notion(..)
  , mkNotion
  , Page(..)
  , Block(..)
  , fetchAllPages
  , fetchPageBlocksTop
  , fetchPageBlocksDeep
  , fetchBlocksDeep
  ) where

import Control.Monad (when)
import Data.Aeson
import qualified Data.Aeson.Key as Aek
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- Orphan instances for UUID <-> JSON (Text)
{-
instance FromJSON UUID where
  parseJSON = withText "UUID" $ \t -> maybe (fail "invalid UUID") pure (UUID.fromText t)
instance ToJSON UUID where
  toJSON = String . UUID.toText
-}

-- Environment ---------------------------------------------------------------

data Notion = Notion
  { token :: Text
  , version :: Text
  , manager :: Manager
  }

mkNotion :: Text -> IO Notion
mkNotion token = do
  manager <- newManager tlsManagerSettings
  let version = "2022-06-28"
  pure Notion{token, version, manager}

setNotionHeaders :: Notion -> Request -> Request
setNotionHeaders n req = req
  { requestHeaders =
      [ ("Authorization", B8.pack ("Bearer " ++ T.unpack n.token))
      , ("Notion-Version", B8.pack (T.unpack n.version))
      , ("Content-Type", "application/json")
      ]
  }

-- Pages (Search API) --------------------------------------------------------

data Filter = Filter { property :: Text, value :: Text } deriving (Show, Generic, ToJSON)

data SearchBody = SearchBody
  { filter :: Filter
  , page_size :: Int
  , start_cursor :: Maybe Text
  } deriving (Show, Generic, ToJSON)

data Page = Page
  { object :: Text
  , id :: UUID
  , url :: Text
  } deriving (Show, Generic, FromJSON, ToJSON)

-- Generic Notion list envelope
-- Used by both /v1/search and /v1/blocks/{id}/children

data ListEnvelope a = ListEnvelope
  { object :: Text
  , results :: [a]
  , next_cursor :: Maybe Text
  , has_more :: Bool
  } deriving (Show, Generic)
instance FromJSON a => FromJSON (ListEnvelope a) where
  parseJSON = withObject "ListEnvelope" $ \o -> do
    object <- o .: "object"
    results <- o .: "results"
    next_cursor <- o .:? "next_cursor"
    has_more <- o .: "has_more"
    pure ListEnvelope{object,results,next_cursor,has_more}

searchPagesPage :: Notion -> Maybe Text -> IO (ListEnvelope Page)
searchPagesPage n mCursor = do
  initReq <- parseRequest "https://api.notion.com/v1/search"
  let body = SearchBody
        { filter = Filter { property = "object", value = "page" }
        , page_size = 100
        , start_cursor = mCursor
        }
      req = setNotionHeaders n initReq
              { method = "POST"
              , requestBody = RequestBodyLBS (encode body)
              }
  withResponse req n.manager $ \res -> do
    bytes <- brConsume (responseBody res)
    let lbs = BL.fromChunks bytes
    case eitherDecode' lbs of
      Left e -> fail ("search decode error: " ++ e)
      Right env -> pure env

fetchAllPages :: Notion -> IO [Page]
fetchAllPages n = go Nothing []
  where
    go mCur acc = do
      env <- searchPagesPage n mCur
      let acc' = acc ++ env.results
      if env.has_more
        then go env.next_cursor acc'
        else pure acc'

-- Blocks --------------------------------------------------------------------

-- A generic Block that preserves the type-specific payload under `content`.
-- Example: for type_ == "paragraph", `content` holds the value of the
-- "paragraph" object as JSON.

data Block = Block
  { object :: Text
  , id :: UUID
  , ktype :: Text
  , archived :: Bool
  , has_children :: Bool
  , content :: Value
  , children :: [Block]
  } deriving (Show, Generic, ToJSON)

instance FromJSON Block where
  parseJSON = withObject "Block" $ \o -> do
    object <- o .: "object"
    id <- o .: "id"
    ktype <- o .: "type"
    archived <- o .: "archived"
    has_children <- o .: "has_children"
    content <- (o .:? (Aek.fromString . T.unpack) ktype) .!= Null
    pure $ Block object id ktype archived has_children content []

blocksChildrenPage :: Notion -> UUID -> Maybe Text -> IO (ListEnvelope Block)
blocksChildrenPage n bid mCursor = do
  let base = "https://api.notion.com/v1/blocks/"
      q = case mCursor of
            Nothing -> "?page_size=100"
            Just c  -> "?page_size=100&start_cursor=" <> T.unpack c
      url = base ++ T.unpack (UUID.toText bid) ++ "/children" ++ q
  initReq <- parseRequest url
  let req = setNotionHeaders n initReq { method = "GET" }
  withResponse req n.manager $ \res -> do
    bytes <- brConsume (responseBody res)
    let lbs = BL.fromChunks bytes
    case eitherDecode' lbs of
      Left e -> fail ("blocks decode error: " ++ e)
      Right env -> pure env

-- Fetch ONLY top-level blocks (paginated)
fetchPageBlocksTop :: Notion -> UUID -> IO [Block]
fetchPageBlocksTop n pid = go Nothing []
  where
    go mCur acc = do
      env <- blocksChildrenPage n pid mCur
      let acc' = acc ++ env.results
      if env.has_more then go env.next_cursor acc' else pure acc'

-- Recursively fetch children for any blocks that indicate has_children=true
fetchBlocksDeep :: Notion -> [Block] -> IO [Block]
fetchBlocksDeep n = mapM fill
  where
    fill b =
      if b.has_children
        then do
          bs <- fetchPageBlocksTop n b.id
          bs' <- fetchBlocksDeep n bs
          pure b{children = bs'}
        else pure b

-- Fetch a page's full block tree (top-level + recursive children)
fetchPageBlocksDeep :: Notion -> UUID -> IO [Block]
fetchPageBlocksDeep n pid = do
  top <- fetchPageBlocksTop n pid
  fetchBlocksDeep n top

