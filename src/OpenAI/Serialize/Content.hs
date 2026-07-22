module OpenAI.Serialize.Content ( 
    insertMessageTree
  , insertContentTree
  , insertMultiModalPartTree
  )
  where

import Data.Int (Int64)

import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as TxS

import qualified OpenAI.Json.Reader as Jd
import qualified OpenAI.Deserialize.Conversation as Dcv
import qualified OpenAI.Serialize.ConversationStmt as St

-- TODO:
insertMessageTree :: Int64 -> Jd.Message -> Tx.Transaction (Either String ())
insertMessageTree convUid msg = undefined


insertContentTree :: Int64 -> Jd.Content -> Tx.Transaction (Either String ())
insertContentTree convUid content = undefined


insertMultiModalPartTree :: Int64 -> Jd.MultiModalPart -> Tx.Transaction (Either String ())
insertMultiModalPartTree convUid multiModalPart = undefined