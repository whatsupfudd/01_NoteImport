{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module OpenAI.Conversation.Json.Node where

import Control.Applicative ((<|>))

import qualified Data.Map.Strict as Mp
import qualified Data.Set as St
import Data.Text (Text)
import Data.Aeson (FromJSON (..), ToJSON, withObject, (.:), (.:?))

import GHC.Generics (Generic)

import OpenAI.Conversation.Json.MsgSchema (Message)

data Node = Node {
  idNd :: Text,
  messageNd :: Maybe Message,
  parentNd :: Maybe Text,
  childrenNd :: [Text]
} deriving (Show, Generic, ToJSON)

instance FromJSON Node where
  parseJSON = withObject "Node" $ \o -> Node
    <$> o .: "id"
    <*> o .:? "message"
    <*> o .:? "parent"
    <*> (o .: "children" <|> pure [])


-- | Derive child membership from parent links. Preserve the relative order
-- of valid existing children and append missing children by ascending EID.
-- Mapping keys are the EIDs used for subsequent traversal lookups.
buildChildrenNd :: Mp.Map Text Node -> Mp.Map Text Node
buildChildrenNd mapping =
  let
    childrenByParent = Mp.foldl' addChild Mp.empty mapping
  in
  Mp.map (setChildren childrenByParent) mapping
  where
  setChildren :: Mp.Map Text (St.Set Text) -> Node -> Node
  setChildren childrenMap node =
    node { childrenNd = orderChildrenNd node.childrenNd (Mp.findWithDefault St.empty node.idNd childrenMap) }

  addChild :: Mp.Map Text (St.Set Text) -> Node -> Mp.Map Text (St.Set Text)
  addChild accum child =
    case child.parentNd of
      Nothing -> accum
      Just eidParent -> Mp.insertWith St.union eidParent (St.singleton child.idNd) accum



-- | Use existing order only as a hint; membership comes from parent links.
-- Deleting emitted EIDs also removes duplicates from the resulting list.
{-
orderChildrenNd :: [Text] -> St.Set Text -> [Text]
orderChildrenNd [] remaining = St.toAscList remaining
orderChildrenNd (eid : rest) remaining
  | St.member eid remaining = eid : orderChildrenNd rest (St.delete eid remaining)
  | otherwise = orderChildrenNd rest remaining
-}

orderChildrenNd :: Ord a => [a] -> St.Set a -> [a]
orderChildrenNd accum remaining =
  case accum of
    [] -> St.toAscList remaining
    (eid : rest) ->
      if St.member eid remaining then
        orderChildrenNd rest (St.delete eid remaining)
      else
       orderChildrenNd rest remaining
