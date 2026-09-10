module OpenAI.Conversation.Context where

import Data.Text (Text)

import qualified OpenAI.Conversation.Json.MsgSchema as Jm
import qualified OpenAI.Discussion.Types as Dt


data Context = Context {
  messages :: [Dt.MessageFsm]
  , currentMsg :: Maybe Dt.MessageFsm
  , issues :: [Text]
} deriving (Show)

initContext :: Context
initContext = Context {
  messages = []
  , currentMsg = Nothing
  , issues = []
}
