module OpenAI.Discussion.Serialize.DiscussionIncr
  ( upsertDiscussionFromConversation
  , computeDiscussionDelta
  , applyDiscussionDelta
  )
  where

import Data.Int (Int64)

import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as TxS

import qualified OpenAI.Discussion.Deserialize.Discussion as Dd
import qualified OpenAI.Discussion.Serialize.Discussion as Sd

-- TODO:
upsertDiscussionFromConversation :: Int64 -> Dd.DiscussionDb -> Tx.Transaction (Either String ())
upsertDiscussionFromConversation convUid discussion = undefined

computeDiscussionDelta :: Int64 -> Dd.DiscussionDb -> Tx.Transaction (Either String ())
computeDiscussionDelta convUid discussion = undefined

applyDiscussionDelta :: Int64 -> Dd.DiscussionDb -> Tx.Transaction (Either String ())
applyDiscussionDelta convUid discussion = undefined