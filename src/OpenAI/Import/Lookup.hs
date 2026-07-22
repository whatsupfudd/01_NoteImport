module OpenAI.Import.Lookup
  ( RowConv(..)
  , byEid
  , allKeys
  ) where


import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Map.Strict as Mp
import qualified Data.Vector as V

import qualified Hasql.Pool as Hp
import qualified Hasql.Session as Hs

import qualified OpenAI.Deserialize.ConversationStmt as Dcs


data RowConv = RowConv {
    uidConv :: Int64
  , eidConv :: Text
  , titleConv :: Text
  , timeCreateCv :: Double
  , timeUpdateCv :: Double
  }
  deriving (Eq, Show)


byEid :: Hp.Pool -> Text -> IO (Either Hp.UsageError (Maybe RowConv))
byEid pool eidA =
  fmap (fmap (fmap rowConvFromRow)) $ Hp.use pool $ Hs.statement eidA Dcs.selectConversationByEid


allKeys :: Hp.Pool -> IO (Either Hp.UsageError (Map Text RowConv))
allKeys pool =
  fmap (fmap rowsConvMapFromVec) $ Hp.use pool $ Hs.statement () Dcs.selectAllConversationsDetailedRows


rowConvFromRow :: (Int64, Text, Text, Double, Double) -> RowConv
rowConvFromRow (uidA, eidA, titleA, timeCreateA, timeUpdateA) =
  RowConv
    { uidConv = uidA
    , eidConv = eidA
    , titleConv = titleA
    , timeCreateCv = timeCreateA
    , timeUpdateCv = timeUpdateA
    }


rowsConvMapFromVec :: V.Vector (Int64, Text, Text, Double, Double) -> Map Text RowConv
rowsConvMapFromVec =
  V.foldl' iterRow Mp.empty
  where
  iterRow acc rowA =
    let
      conv = rowConvFromRow rowA
    in
    Mp.insert conv.eidConv conv acc