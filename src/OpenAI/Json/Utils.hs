module OpenAI.Json.Utils where

import Data.List (sortOn)
import qualified Data.Map.Strict as Mp
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Text.Read (readMaybe)

import Data.Aeson
import Data.Aeson.Types (Value, Object, Parser)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as Km


-- Helper to convert Object to Map String Value
objectToMapV1 :: Object -> Mp.Map Text Value
objectToMapV1 anObject =
  let
    keyMap = Km.toMap anObject
  in
    Mp.mapKeys (T.pack . show) keyMap


objectToMap :: Object -> Mp.Map Text Value
objectToMap anObject = Mp.fromList [ (K.toText key, value) | (key, value) <- Km.toList anObject ]


indexedObjectToVector :: FromJSON a => Object -> Parser (V.Vector a)
indexedObjectToVector anObject = do
  indexed <- traverse toIndexed (Km.toList anObject)
  let
    ordered = fmap snd (sortOn fst indexed)
  V.fromList <$> traverse parseJSON ordered
  where
  toIndexed :: (K.Key, Value) -> Parser (Integer, Value)
  toIndexed (key, value) =
    case readMaybe (T.unpack (K.toText key)) of
      Just n
        | n >= 0 -> pure (n, value)
      _ -> fail ("Invalid indexed object key: " <> T.unpack (K.toText key))
