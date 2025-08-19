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

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID

import GHC.Generics (Generic)

import qualified Data.Aeson as Ae
import qualified Data.Aeson.Key as Aek
import Data.Aeson.Types (Parser)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)


-- Orphan instances for UUID <-> JSON (Text)
{-
instance FromJSON UUID where
  parseJSON = withText "UUID" $ \t -> maybe (fail "invalid UUID") pure (UUID.fromText t)
instance Ae.ToJSON UUID where
  Ae.ToJSON = String . UUID.toText
-}

-- Environment ---------------------------------------------------------------

data Notion = Notion
  { token :: Text
  , version :: Text
  , manager :: Manager
  }

mkNotion :: Text -> IO Notion
mkNotion token =
  let
    version = "2022-06-28"
  in do
  manager <- newManager tlsManagerSettings
  pure $ Notion token version manager

setNotionHeaders :: Notion -> Request -> Request
setNotionHeaders n req = req
  { requestHeaders =
      [ ("Authorization", B8.pack ("Bearer " ++ T.unpack n.token))
      , ("Notion-Version", B8.pack (T.unpack n.version))
      , ("Content-Type", "application/json")
      ]
  }

-- Pages (Search API) --------------------------------------------------------

data Filter = Filter { property :: Text, value :: Text } deriving (Show, Generic, Ae.ToJSON)

data SearchBody = SearchBody
  { filter :: Filter
  , page_size :: Int
  , start_cursor :: Maybe Text
  } deriving (Show, Generic)
instance Ae.ToJSON SearchBody where
  toJSON sb =
    Ae.object $ [
         ("filter", Ae.toJSON sb.filter)
      , ("page_size", Ae.toJSON sb.page_size)
      ]
      <> case sb.start_cursor of
        Nothing -> []
        Just c -> [("start_cursor", Ae.toJSON c)]

data Page = Page { 
  id :: UUID
  , url :: Text
  } deriving (Show, Generic, Ae.FromJSON, Ae.ToJSON)


-- Generic Notion list envelope
-- Used by both /v1/search and /v1/blocks/{id}/children

data ListEnvelope a = ListEnvelope
  { object :: Text
  , results :: [a]
  , next_cursor :: Maybe Text
  , has_more :: Bool
  } deriving (Show, Generic)
instance Ae.FromJSON a => Ae.FromJSON (ListEnvelope a) where
  parseJSON = Ae.withObject "ListEnvelope" $ \o -> do
    object <- o Ae..: "object"
    results <- o Ae..: "results"
    next_cursor <- o Ae..:? "next_cursor"
    has_more <- o Ae..: "has_more"
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
              , requestBody = RequestBodyLBS (Ae.encode body)
              }
  putStrLn $ "@[searchPagesPage] req: " <> show req <> " body: " <> (T.unpack . T.decodeUtf8 . BL.toStrict) (Ae.encode body)
  withResponse req n.manager $ \res -> do
    bytes <- brConsume (responseBody res)
    let lbs = BL.fromChunks bytes
    case Ae.eitherDecode' lbs of
      Left e -> do
        putStrLn $ "@[searchPagesPage] lbs: " <> (T.unpack . T.decodeUtf8 . BL.toStrict) lbs
        fail ("search decode error: " ++ e)
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

data Block = Block { 
  id :: UUID
  , ktype :: Text
  , archived :: Bool
  , has_children :: Bool
  , content :: Ae.Value
  , children :: [Block]
  } deriving (Show, Generic, Ae.ToJSON)


instance Ae.FromJSON Block where
  parseJSON = Ae.withObject "Block" $ \o -> do
    id <- o Ae..: "id"
    ktype <- o Ae..: "type"
    archived <- o Ae..: "archived"
    has_children <- o Ae..: "has_children"
    content <- (o Ae..:? (Aek.fromString . T.unpack) ktype) Ae..!= Ae.Null
    pure $ Block id ktype archived has_children content []


blocksChildrenPage :: Notion -> UUID -> Maybe Text -> IO (ListEnvelope Block)
blocksChildrenPage n bid mCursor = do
  let
    base = "https://api.notion.com/v1/blocks/"
    q = case mCursor of
          Nothing -> "?page_size=100"
          Just c  -> "?page_size=100&start_cursor=" <> T.unpack c
    url = base ++ T.unpack (UUID.toText bid) ++ "/children" ++ q
  initReq <- parseRequest url
  let req = setNotionHeaders n initReq { method = "GET" }
  withResponse req n.manager $ \res -> do
    bytes <- brConsume (responseBody res)
    let lbs = BL.fromChunks bytes
    case Ae.eitherDecode' lbs of
      Left e -> fail ("blocks decode error: " ++ e)
      Right env -> pure env


-- Fetch ONLY top-level blocks (paginated)
fetchPageBlocksTop :: Notion -> UUID -> IO [Block]
fetchPageBlocksTop n anID = go Nothing []
  where
    go mCur acc = do
      env <- blocksChildrenPage n anID mCur
      let acc' = acc ++ env.results
      if env.has_more then go env.next_cursor acc' else pure acc'


-- Fetch a page's full block tree (top-level + recursive children)
fetchPageBlocksDeep :: Notion -> UUID -> IO [Block]
fetchPageBlocksDeep n anID = do
  top <- fetchPageBlocksTop n anID
  fetchBlocksDeep n top


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



-- Block content types:
data BlockContent =
  ParagraphBC Paragraph
  | RichTextBC RichText
  | ImageBC Image
  | BulletedListItemBC BulletedListItem
  | NumberedListItemBC NumberedListItem
  | DividerBC Divider
  | Heading1BC Heading1
  | Heading2BC Heading2
  | Heading3BC Heading3
  | CodeBC Code
  | TableBC Table
  deriving (Show, Generic)

instance Ae.FromJSON BlockContent where

data Paragraph = Paragraph
  { color :: Text
  , rich_text :: [RichText]
  } deriving (Show, Generic)
instance Ae.FromJSON Paragraph

-- RichText (handles the common "text" variant; other variants will simply
-- leave `text = Nothing` but still parse annotations/plain_text/href/type_)

data RichText = RichText
  { type_ :: Text
  , text :: Maybe TextSpan
  , annotations :: Annotations
  , plain_text :: Text
  , href :: Maybe Text
  } deriving (Show, Generic)
instance Ae.FromJSON RichText where
  parseJSON = Ae.genericParseJSON Ae.defaultOptions{ Ae.fieldLabelModifier = flm }
    where flm "type_" = "type"; flm x = x

-- Text payload for the "text" rich_text variant

data TextSpan = TextSpan
  { content :: Text
  , link :: Maybe Url
  } deriving (Show, Generic, Ae.FromJSON)

newtype Url = Url { url :: Text } deriving (Show, Generic, Ae.FromJSON)

-- Style annotations on rich_text

data Annotations = Annotations
  { bold :: Bool
  , code :: Bool
  , color :: Text
  , italic :: Bool
  , strikethrough :: Bool
  , underline :: Bool
  } deriving (Show, Generic, Ae.FromJSON)

-- Helper to extract a Paragraph from a Block's `content` when type_ == "paragraph"
-- Returns Nothing if the block is not a paragraph or content is malformed.

asParagraph :: Block -> Maybe Paragraph
asParagraph b =
  if b.ktype == "paragraph"
    then case Ae.fromJSON b.content of
           Ae.Success p -> Just p
           _ -> Nothing
    else Nothing

-- Image content (for Block.ktype == "image")
data Image = Image {
     caption :: [RichText]
  , source :: ImageSource
  }
  deriving (Show, Generic)

data ImageSource
  = External { url :: Text }
  | File { url :: Text, expiry_time :: UTCTime }
  deriving (Show, Generic)

instance Ae.FromJSON Image where
  parseJSON = Ae.withObject "Image" $ \o -> do
    caption <- o Ae..:? "caption" Ae..!= []
    t <- o Ae..: "type" :: Parser Text
    case t of
      "external" -> do
        e <- o Ae..: "external"
        url <- e Ae..: "url"
        pure $ Image caption (External url)
      "file" -> do
        f <- o Ae..: "file"
        url <- f Ae..: "url"
        expiry_time <- f Ae..: "expiry_time"
        pure $ Image caption (File url expiry_time)
      _ -> fail ("unsupported image.type: " <> show t)

-- Helper to decode an Image from your existing Block
asImage :: Block -> Maybe Image
asImage b =
  if b.ktype == "image" then
    case Ae.fromJSON b.content of
      Ae.Success x -> Just x
      _ -> Nothing
  else
    Nothing

-- Bulleted list item content (for Block.ktype == "bulleted_list_item")
data BulletedListItem = BulletedListItem { 
    color :: Text
  , rich_text :: [RichText]
  }
  deriving (Show, Generic)

instance Ae.FromJSON BulletedListItem where
  parseJSON = Ae.withObject "BulletedListItem" $ \o -> do
    color <- o Ae..: "color"
    rich_text <- o Ae..:? "rich_text" Ae..!= []
    pure $ BulletedListItem color rich_text

-- Helper to decode a BulletedListItem from your Block.content
asBulletedListItem :: Block -> Maybe BulletedListItem
asBulletedListItem b =
  if b.ktype == "bulleted_list_item" then
    case Ae.fromJSON b.content of
      Ae.Success x -> Just x
      _ -> Nothing
  else
    Nothing

-- Heading_1 content (for Block.ktype == "heading_1")
-- JSON shape: {"color":"default","is_toggleable":Bool,"rich_text":[...]}
data Heading1 = Heading1 {
     color :: Text
  , is_toggleable :: Bool
  , rich_text :: [RichText]
  }
  deriving (Show, Generic)

instance Ae.FromJSON Heading1 where
  parseJSON = Ae.withObject "Heading1" $ \o -> do
    color <- o Ae..:  "color"
    is_toggleable <- o Ae..:? "is_toggleable" Ae..!= False
    rich_text <- o Ae..:? "rich_text"     Ae..!= []
    pure $ Heading1 color is_toggleable rich_text

-- Helper to decode a Heading1 from your existing Block.content
asHeading1 :: Block -> Maybe Heading1
asHeading1 b =
  if b.ktype == "heading_1" then
    case Ae.fromJSON b.content of
      Ae.Success x -> Just x
      _ -> Nothing
  else
    Nothing

-- Numbered list item content (for Block.ktype == "numbered_list_item")
data NumberedListItem = NumberedListItem
  { color :: Text
  , rich_text :: [RichText]
  } deriving (Show, Generic)

instance Ae.FromJSON NumberedListItem where
  parseJSON = Ae.withObject "NumberedListItem" $ \o -> do
    color <- o Ae..:  "color"
    rich_text <- o Ae..:? "rich_text" Ae..!= []
    pure $ NumberedListItem color rich_text

-- Helper to decode a NumberedListItem from your Block.content
asNumberedListItem :: Block -> Maybe NumberedListItem
asNumberedListItem b =
  if b.ktype == "numbered_list_item" then
    case Ae.fromJSON b.content of
        Ae.Success x -> Just x
        _ -> Nothing
  else
    Nothing

-- Divider (for Block.ktype == "divider")
data Divider = Divider deriving (Show)

instance Ae.FromJSON Divider where
  parseJSON = Ae.withObject "Divider" $ \_ -> pure Divider

asDivider :: Block -> Maybe Divider
asDivider b =
  if b.ktype == "divider"
    then case Ae.fromJSON b.content of Ae.Success x -> Just x; _ -> Nothing
    else Nothing


-- Heading_2 (for Block.ktype == "heading_2")
-- JSON: {"color":"...","is_toggleable":Bool,"rich_text":[RichText]}
data Heading2 = Heading2
  { color :: Text
  , is_toggleable :: Bool
  , rich_text :: [RichText]
  } deriving (Show, Generic)

instance Ae.FromJSON Heading2 where
  parseJSON = Ae.withObject "Heading2" $ \o -> do
    color <- o Ae..:  "color"
    is_toggleable <- o Ae..:? "is_toggleable" Ae..!= False
    rich_text <- o Ae..:? "rich_text" Ae..!= []
    pure (Heading2 color is_toggleable rich_text)

asHeading2 :: Block -> Maybe Heading2
asHeading2 b =
  if b.ktype == "heading_2"
    then case Ae.fromJSON b.content of Ae.Success x -> Just x; _ -> Nothing
    else Nothing


-- Heading_3 (for Block.ktype == "heading_3")
-- JSON: {"color":"...","is_toggleable":Bool,"rich_text":[RichText]}
data Heading3 = Heading3
  { color :: Text
  , is_toggleable :: Bool
  , rich_text :: [RichText]
  } deriving (Show, Generic)

instance Ae.FromJSON Heading3 where
  parseJSON = Ae.withObject "Heading3" $ \o -> do
    color <- o Ae..:  "color"
    is_toggleable <- o Ae..:? "is_toggleable" Ae..!= False
    rich_text <- o Ae..:? "rich_text" Ae..!= []
    pure (Heading3 color is_toggleable rich_text)

asHeading3 :: Block -> Maybe Heading3
asHeading3 b =
  if b.ktype == "heading_3"
    then case Ae.fromJSON b.content of Ae.Success x -> Just x; _ -> Nothing
    else Nothing

-- Code (for Block.ktype == "code")
-- JSON: {"caption":[RichText], "language":"elm", "rich_text":[RichText]}
data Code = Code
  { caption :: [RichText]
  , language :: Text
  , rich_text :: [RichText]
  } deriving (Show, Generic)

instance Ae.FromJSON Code where
  parseJSON = Ae.withObject "Code" $ \o -> do
    caption <- o Ae..:? "caption"   Ae..!= []
    language <- o Ae..:? "language" Ae..!= "plain text"
    rich_text <- o Ae..:? "rich_text" Ae..!= []
    pure (Code caption language rich_text)

asCode :: Block -> Maybe Code
asCode b =
  if b.ktype == "code"
    then case Ae.fromJSON b.content of Ae.Success x -> Just x; _ -> Nothing
    else Nothing

-- Table (for Block.ktype == "table")
-- JSON: {"has_column_header":Bool, "has_row_header":Bool, "table_width":Number}
-- Note: rows are separate child blocks (type "table_row"), not embedded here.
data Table = Table
  { has_column_header :: Bool
  , has_row_header :: Bool
  , table_width :: Int
  } deriving (Show, Generic)

instance Ae.FromJSON Table where
  parseJSON = Ae.withObject "Table" $ \o -> do
    has_column_header <- o Ae..:? "has_column_header" Ae..!= False
    has_row_header <- o Ae..:? "has_row_header" Ae..!= False
    -- robust width parsing (handles 2 or 2.0)
    n <- o Ae..: "table_width" :: Parser Scientific
    let table_width = floor n
    pure (Table has_column_header has_row_header table_width)

asTable :: Block -> Maybe Table
asTable b =
  if b.ktype == "table"
    then case Ae.fromJSON b.content of Ae.Success x -> Just x; _ -> Nothing
    else Nothing
