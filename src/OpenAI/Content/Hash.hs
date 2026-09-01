module OpenAI.Content.Hash (hashPayload, hashPart, hashValue) where

import qualified Data.ByteArray as BA
import qualified Data.ByteString.Builder as Bb
import qualified Data.ByteString.Lazy as Bl
import Data.Foldable (toList)
import Data.List (sortOn)

import qualified Data.Aeson as Ae
import qualified Data.Aeson.Key as Ak
import qualified Data.Aeson.KeyMap as Km
import qualified Crypto.Hash as CH

import qualified OpenAI.Content.Codec as Codec
import OpenAI.Content.Types (PartPL, Payload)
import OpenAI.Delta.Types (Hash (..))


hashPayload :: Payload -> Hash
hashPayload = hashJson . Codec.valuePayload


hashPart :: PartPL -> Hash
hashPart = hashJson . Codec.valuePart

hashJson :: Ae.ToJSON a => a -> Hash
hashJson value = hashValue $ Ae.toJSON value

hashValue :: Ae.Value -> Hash
hashValue value =
  let
    digest = CH.hashlazy (encodeCanonical value) :: CH.Digest CH.SHA256
  in
  Hash $ BA.convert digest

encodeCanonical :: Ae.Value -> Bl.ByteString
encodeCanonical value = Bb.toLazyByteString $ buildValue value


buildValue :: Ae.Value -> Bb.Builder
buildValue value =
  case value of
    Ae.Object obj -> buildObject obj
    Ae.Array values -> buildArray $ toList values
    Ae.String text -> Bb.lazyByteString $ Ae.encode text
    Ae.Number number -> Bb.lazyByteString $ Ae.encode number
    Ae.Bool flag -> if flag then "true" else "false"
    Ae.Null -> "null"


buildObject :: Ae.Object -> Bb.Builder
buildObject obj =
  let
    entries = sortOn (Ak.toText . fst) $ Km.toList obj
    renderOne (key, value) = Bb.lazyByteString (Ae.encode $ Ak.toText key) <> ":" <> buildValue value
  in
  "{" <> joinComma (map renderOne entries) <> "}"


buildArray :: [Ae.Value] -> Bb.Builder
buildArray values = "[" <> joinComma (map buildValue values) <> "]"


joinComma :: [Bb.Builder] -> Bb.Builder
joinComma values =
  case values of
    [] -> mempty
    firstB : restB -> firstB <> foldMap ("," <>) restB
