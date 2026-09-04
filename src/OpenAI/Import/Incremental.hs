module OpenAI.Import.Incremental (
    importConversation, importConversations
  , ImportOptions(..), ImportReport(..), ConversationDelta(..)
  )
where

import Data.Text (Text)
import Data.Int (Int64)

import qualified Hasql.Transaction as Tx

import qualified OpenAI.Conversation as Cv
import qualified OpenAI.Conversation.Json.Schema as Jd


data ImportReport = ImportReport {
    conversationEid :: Text
  , conversationUid :: Int64
  , discussionUid :: Maybe Int64
  , insertedNodes :: Int
  , updatedMessages :: Int
  , insertedDiscussionMessages :: Int
  , updatedDiscussionMessages :: Int
  , insertedSummaries :: Int
  , skippedReason :: Maybe Text
  , warnings :: [Text]
  }


-- TODO:
data ImportOptions = ImportOptions
data ImportIssue = ImportIssue
data ConversationDelta = ConversationDelta
data ApplyReport = ApplyReport

importConversations :: [Cv.ConversationDb] -> [Jd.Conversation] -> Either ImportIssue [ConversationDelta]
importConversations convDbs convs = undefined

importConversation :: Cv.ConversationDb -> Jd.Conversation -> Either ImportIssue ConversationDelta
importConversation convDb conv = undefined


computeConversationDelta :: Cv.ConversationDb -> Jd.Conversation -> Either ImportIssue ConversationDelta
computeConversationDelta convDb conv = undefined

applyConversationDelta :: ConversationDelta -> Tx.Transaction ApplyReport
applyConversationDelta delta = undefined