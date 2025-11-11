{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RankNTypes #-}

-- | Minimal Notion client for:
--   1) fetchAllPages: fetching ALL pages visible to an integration (workspace-scoped via /v1/search with filter=page)
--   2) fetchPageBlocksTop: fetching page content as a tree of Blocks (recursively follows children)
--   3) fetchPageBlocksDeep: fetching a page's full block tree (top-level + recursive children)
--   4) upgradeToHBlocks: upgrading a list of blocks to a list of block contents
--
-- Notes:
--   • This module focuses on IDs/URLs for pages and generic block trees. You can enrich the
--     Page and Block types as needed (e.g., titles, rich text spans, etc.).
--   • Headers: Authorization bearer + Notion-Version are set automatically.
--   • Pagination is fully handled for search and blocks children endpoints.

module Notion.Fetch where

import Control.Monad (when)

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as Bls
import qualified Data.ByteString as Bs
import Data.Maybe (fromMaybe, mapMaybe)
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
import qualified Data.Aeson.KeyMap as Akm
import Data.Aeson.Types (Parser)

import qualified Network.HTTP.Client as Ht
import Network.HTTP.Client.TLS (tlsManagerSettings)


data Notion = Notion { 
    token :: Text
  , version :: Text
  , manager :: Ht.Manager
  }


mkNotion :: Text -> IO Notion
mkNotion token =
  let
    version = "2022-06-28"
  in do
  manager <- Ht.newManager tlsManagerSettings
  pure $ Notion token version manager


setHeaders :: Notion -> Ht.Request -> Ht.Request
setHeaders notion request = request { 
    Ht.requestHeaders = [ 
        ("Authorization", B8.pack ("Bearer " ++ T.unpack notion.token))
      , ("Notion-Version", B8.pack (T.unpack notion.version))
      , ("Content-Type", "application/json")
      ]
  }


data Filter = Filter {
    property :: Text
  , value :: Text
 }
 deriving (Show, Generic, Ae.ToJSON)


data SearchBody = SearchBody {
    filter :: Filter
  , page_size :: Int
  , start_cursor :: Maybe Text
  }
  deriving (Show, Generic)

instance Ae.ToJSON SearchBody where
  toJSON sb =
    Ae.object $ [
        ("filter", Ae.toJSON sb.filter)
      , ("page_size", Ae.toJSON sb.page_size)
      ]
      <> case sb.start_cursor of
        Nothing -> []
        Just c -> [("start_cursor", Ae.toJSON c)]


data PageB = PageB { 
  id :: UUID
  , url :: Text
  }
  deriving (Show, Generic, Ae.FromJSON, Ae.ToJSON)


data UserRef = UserRef
  { object :: Text
  , id :: UUID
  } deriving (Show, Generic)
instance Ae.FromJSON UserRef

-- Parent union
data Parent
  = WorkspaceParent { workspace :: Bool }
  | DatabaseParent  { database_id :: UUID }
  | PageParent      { page_id :: UUID }
  deriving (Show, Generic)

instance Ae.FromJSON Parent where
  parseJSON = Ae.withObject "Parent" $ \o -> do
    t <- o Ae..: "type" :: Parser Text
    case t of
      "workspace"   -> WorkspaceParent <$> o Ae..: "workspace"
      "database_id" -> DatabaseParent  <$> o Ae..: "database_id"
      "page_id"     -> PageParent      <$> o Ae..: "page_id"
      _             -> fail ("unsupported parent.type: " <> show t)

-- File link (used by cover and file/external icons)
data FileLink
  = ExternalLink { url :: Text }
  | HostedLink   { url :: Text, expiry_time :: UTCTime }
  deriving (Show, Generic)

instance Ae.FromJSON FileLink where
  parseJSON = Ae.withObject "FileLink" $ \o -> do
    t <- o Ae..: "type" :: Parser Text
    case t of
      "external" -> do e <- o Ae..: "external"; ExternalLink <$> (e Ae..: "url")
      "file"     -> do f <- o Ae..: "file"; HostedLink <$> (f Ae..: "url") <*> (f Ae..: "expiry_time")
      _          -> fail ("unsupported filelink.type: " <> show t)

-- Icon union (emoji or a file link)
data Icon
  = IconEmoji { emoji :: Text }
  | IconLink  { file :: FileLink }
  deriving (Show, Generic)

instance Ae.FromJSON Icon where
  parseJSON = Ae.withObject "Icon" $ \o -> do
    t <- o Ae..: "type" :: Parser Text
    case t of
      "emoji"    -> IconEmoji <$> o Ae..: "emoji"
      "external" -> IconLink  <$> (Ae.parseJSON (Ae.Object o) :: Parser FileLink)
      "file"     -> IconLink  <$> (Ae.parseJSON (Ae.Object o) :: Parser FileLink)
      _          -> fail ("unsupported icon.type: " <> show t)

-- Full page payload (search result)
data PageMeta = PageMeta { 
    object :: Text
  , id :: UUID
  , created_time :: UTCTime
  , last_edited_time :: UTCTime
  , created_by :: UserRef
  , last_edited_by :: UserRef
  , cover :: Maybe FileLink
  , icon :: Maybe Icon
  , parent :: Parent
  , archived :: Bool
  , in_trash :: Bool
  , properties :: Ae.Value          -- keep raw for flexibility
  , title_rich :: [RichText]        -- extracted convenience
  , title_plain :: Maybe Text       -- extracted convenience
  , url :: Text
  , public_url :: Maybe Text
  } deriving (Show, Generic)

instance Ae.FromJSON PageMeta where
  parseJSON = Ae.withObject "Page" $ \o -> do
    object <- o Ae..: "object"
    id <- o Ae..: "id"
    created_time <- o Ae..: "created_time"
    last_edited_time <- o Ae..: "last_edited_time"
    created_by <- o Ae..: "created_by"
    last_edited_by <- o Ae..: "last_edited_by"
    cover <- o Ae..:? "cover"
    icon <- o Ae..:? "icon"
    parent <- o Ae..: "parent"
    archived <- o Ae..: "archived"
    in_trash <- o Ae..: "in_trash"
    properties <- o Ae..: "properties"

    -- Extract title property if present
    title_rich <-
      case properties of
        Ae.Object hm ->
          case Akm.lookup "title" hm of
            Just v  -> Ae.withObject "TitleProperty" (\tp -> tp Ae..:? "title" Ae..!= []) v
            Nothing -> pure []
        _ -> pure []
    let title_plain =
          case title_rich of
            [] -> Nothing
            xs -> Just (mconcat (map (.plain_text) xs))

    url <- o Ae..: "url"
    public_url <- o Ae..:? "public_url"
    pure (PageMeta object id created_time last_edited_time created_by last_edited_by cover icon parent archived in_trash properties title_rich title_plain url public_url)


-- Generic Notion list envelope
-- Used by both /v1/search and /v1/blocks/{id}/children

data ListEnvelope a = ListEnvelope { 
    object :: Text
  , results :: [a]
  , next_cursor :: Maybe Text
  , has_more :: Bool
  }
  deriving (Show, Generic)
instance Ae.FromJSON a => Ae.FromJSON (ListEnvelope a) where
  parseJSON = Ae.withObject "ListEnvelope" $ \o -> do
    object <- o Ae..: "object"
    results <- o Ae..: "results"
    next_cursor <- o Ae..:? "next_cursor"
    has_more <- o Ae..: "has_more"
    pure $ ListEnvelope object results next_cursor has_more


searchPagesPage :: Notion -> Maybe Text -> IO (ListEnvelope PageMeta)
searchPagesPage notion mCursor = do
  initReq <- Ht.parseRequest "https://api.notion.com/v1/search"
  let body = SearchBody { 
          filter = Filter { property = "object", value = "page" }
        , page_size = 100
        , start_cursor = mCursor
        }
      req = setHeaders notion initReq { 
                Ht.method = "POST"
              , Ht.requestBody = Ht.RequestBodyLBS (Ae.encode body)
              }
  -- putStrLn $ "@[searchPagesPage] req: " <> show req <> " body: " <> (T.unpack . T.decodeUtf8 . Bls.toStrict) (Ae.encode body)
  Ht.withResponse req notion.manager $ \res -> do
    bytes <- Ht.brConsume (Ht.responseBody res)
    let lbs = Bls.fromChunks bytes
    let fileName = "/tmp/search-pages-page-" <> maybe "0" T.unpack mCursor <> ".json"
    Bls.writeFile fileName lbs
    case Ae.eitherDecode' lbs of
      Left e -> do
        putStrLn $ "@[searchPagesPage] lbs: " <> (T.unpack . T.decodeUtf8 . Bls.toStrict) lbs
        fail ("search decode error: " ++ e)
      Right env -> pure env


fetchAllPages :: Notion -> IO [PageMeta]
fetchAllPages n = go Nothing []
  where
    go mCur acc = do
      env <- searchPagesPage n mCur
      let acc' = acc ++ env.results
      if env.has_more
        then go env.next_cursor acc'
        else pure acc'

-- Blocks --------------------------------------------------------------------

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


fetchPageBlocks :: Notion -> PageMeta -> IO [Block]
fetchPageBlocks n page = fetchPageBlocksTop n page.id

fetchAllPageBlocks :: Notion -> PageMeta -> IO [Block]
fetchAllPageBlocks n page = fetchPageBlocksDeep n page.id


-- Fetch ONLY top-level blocks (paginated)
fetchPageBlocksTop :: Notion -> UUID -> IO [Block]
fetchPageBlocksTop n anID =
  let
    pageName = "/tmp/blocks-" <> T.unpack (UUID.toText anID) <> ".out"
  in do
  blocks <- fetchPageIterator Nothing []
  Bs.writeFile pageName (Bs.intercalate "\n ," (map (T.encodeUtf8 . T.pack . show) blocks))
  pure blocks
  where
  fetchPageIterator mbCursor acc = do
    listEnv <- blocksChildrenPage n anID mbCursor
    let
      acc' = acc <> listEnv.results
    if listEnv.has_more then fetchPageIterator listEnv.next_cursor acc' else pure acc'


blocksChildrenPage :: Notion -> UUID -> Maybe Text -> IO (ListEnvelope Block)
blocksChildrenPage n bid mCursor = do
  let
    base = "https://api.notion.com/v1/blocks/"
    qParams = case mCursor of
          Nothing -> "?page_size=100"
          Just anID  -> "?page_size=100&start_cursor=" <> T.unpack anID
    url = base ++ T.unpack (UUID.toText bid) ++ "/children" ++ qParams
  initReq <- Ht.parseRequest url
  let request = setHeaders n initReq { Ht.method = "GET" }
  Ht.withResponse request n.manager $ \response -> do
    bStrings <- Ht.brConsume (Ht.responseBody response)
    let lbString = Bls.fromChunks bStrings
    case Ae.eitherDecode' lbString of
      Left err -> fail ("blocks decode error: " <> err)
      Right listEnv ->
        let
          fileName = "/tmp/blocks-" <> T.unpack (UUID.toText bid) <> "_" <> maybe "0" T.unpack mCursor <> ".json"
        in do
        Bls.writeFile fileName lbString
        pure listEnv


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
  | HeadingBC Int Heading
  | CodeBC Code
  | TableBC Table
  | QuoteBC Quote
  | ChildPageBC ChildPage
  deriving (Show, Generic)


-- Upgrade a list of blocks to a list of block contents
-- This is a helper function to convert the raw block data into a more structured format
-- that can be used to generate HTML or other output formats.
-- The input is a list of blocks, and the output is a list of block contents
upgradeToHBlocks :: [Block] -> [Either String BlockContent]
upgradeToHBlocks = map blockUpgrade
  where
  blockUpgrade :: Block -> Either String BlockContent
  blockUpgrade b = case b.ktype of
    "paragraph" -> blockToEither b.content ParagraphBC
    -- "rich_text" -> RichTextBC <$> asRichText b -- TODO: implement asRichText
    "image" -> blockToEither b.content ImageBC
    "bulleted_list_item" -> blockToEither b.content BulletedListItemBC
    "numbered_list_item" -> blockToEither b.content NumberedListItemBC
    "divider" -> blockToEither b.content DividerBC
    "heading_1" -> blockToEither b.content (HeadingBC 1)
    "heading_2" -> blockToEither b.content (HeadingBC 2)
    "heading_3" -> blockToEither b.content (HeadingBC 3)
    "code" -> blockToEither b.content CodeBC
    "table" -> blockToEither b.content TableBC
    "quote" -> blockToEither b.content QuoteBC
    "child_page" -> blockToEither b.content ChildPageBC
    _ -> Left $ "@[upgradeToHBlocks] unsupported block type: " <> show b.ktype <> " in " <> show b


blockToEither :: forall aBlockKind. Ae.FromJSON aBlockKind => Ae.Value -> (aBlockKind -> BlockContent) -> Either String BlockContent
blockToEither aContent upgradeFct = case Ae.fromJSON aContent of
  Ae.Success a -> Right $ upgradeFct a
  Ae.Error err -> Left $ "bad block content: " <> err <> " in " <> show aContent


data Paragraph = Paragraph { 
    color :: Text
  , rich_text :: [RichText]
  }
  deriving (Show, Generic, Ae.FromJSON)

-- RichText (handles the common "text" variant; other variants will simply
-- leave `text = Nothing` but still parse annotations/plain_text/href/type_)

data RichText = RichText { 
    type_ :: Text
  , text :: Maybe TextSpan
  , annotations :: Annotations
  , plain_text :: Text
  , href :: Maybe Text
  } 
  deriving (Show, Generic)

instance Ae.FromJSON RichText where
  parseJSON = Ae.genericParseJSON Ae.defaultOptions{ Ae.fieldLabelModifier = flm }
    where
    flm "type_" = "type"; flm x = x

-- Text payload for the "text" rich_text variant

data TextSpan = TextSpan { 
    content :: Text
  , link :: Maybe Url
  }
  deriving (Show, Generic, Ae.FromJSON)

newtype Url = Url { url :: Text } deriving (Show, Generic, Ae.FromJSON)

-- Style annotations on rich_text

data Annotations = Annotations {
    bold :: Bool
  , code :: Bool
  , color :: Text
  , italic :: Bool
  , strikethrough :: Bool
  , underline :: Bool
  }
  deriving (Show, Generic, Ae.FromJSON)


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


-- Heading_1 content (for Block.ktype == "heading_1")
-- JSON shape: {"color":"default","is_toggleable":Bool,"rich_text":[...]}
data Heading = Heading {
    color :: Text
  , is_toggleable :: Bool
  , rich_text :: [RichText]
  }
  deriving (Show, Generic)

instance Ae.FromJSON Heading where
  parseJSON = Ae.withObject "Heading" $ \o -> do
    color <- o Ae..:  "color"
    is_toggleable <- o Ae..:? "is_toggleable" Ae..!= False
    rich_text <- o Ae..:? "rich_text"     Ae..!= []
    pure $ Heading color is_toggleable rich_text



-- Numbered list item content (for Block.ktype == "numbered_list_item")
data NumberedListItem = NumberedListItem { 
    color :: Text
  , rich_text :: [RichText]
  }
  deriving (Show, Generic)

instance Ae.FromJSON NumberedListItem where
  parseJSON = Ae.withObject "NumberedListItem" $ \o -> do
    color <- o Ae..:  "color"
    rich_text <- o Ae..:? "rich_text" Ae..!= []
    pure $ NumberedListItem color rich_text


-- Divider (for Block.ktype == "divider")
data Divider = Divider deriving (Show)

instance Ae.FromJSON Divider where
  parseJSON = Ae.withObject "Divider" $ \_ -> pure Divider


-- Code (for Block.ktype == "code")
-- JSON: {"caption":[RichText], "language":"elm", "rich_text":[RichText]}
data Code = Code {
    caption :: [RichText]
  , language :: Text
  , rich_text :: [RichText]
  }
  deriving (Show, Generic)

instance Ae.FromJSON Code where
  parseJSON = Ae.withObject "Code" $ \o -> do
    caption <- o Ae..:? "caption"   Ae..!= []
    language <- o Ae..:? "language" Ae..!= "plain text"
    rich_text <- o Ae..:? "rich_text" Ae..!= []
    pure (Code caption language rich_text)


-- Table (for Block.ktype == "table")
-- JSON: {"has_column_header":Bool, "has_row_header":Bool, "table_width":Number}
-- Note: rows are separate child blocks (type "table_row"), not embedded here.
data Table = Table { 
    has_column_header :: Bool
  , has_row_header :: Bool
  , table_width :: Int
  }
  deriving (Show, Generic)

instance Ae.FromJSON Table where
  parseJSON = Ae.withObject "Table" $ \o -> do
    has_column_header <- o Ae..:? "has_column_header" Ae..!= False
    has_row_header <- o Ae..:? "has_row_header" Ae..!= False
    -- robust width parsing (handles 2 or 2.0)
    n <- o Ae..: "table_width" :: Parser Scientific
    let table_width = floor n
    pure (Table has_column_header has_row_header table_width)


-- Quote (for Block.ktype == "quote")
-- JSON: {"color":"...","rich_text":[RichText]}
data Quote = Quote { 
    color :: Text
  , rich_text :: [RichText]
  }
  deriving (Show, Generic)

instance Ae.FromJSON Quote where
  parseJSON = Ae.withObject "Quote" $ \o -> do
    color <- o Ae..:  "color"
    rich_text <- o Ae..:? "rich_text" Ae..!= []
    pure (Quote color rich_text)


-- Child page (for Block.ktype == "child_page")
-- JSON: {"title":"..."}
newtype ChildPage = ChildPage { 
    title :: Text
  }
  deriving (Show, Generic, Ae.FromJSON)
