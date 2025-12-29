{-# LANGUAGE DeriveGeneric #-}
module OpenAI.Types where

import Data.Text (Text)

import GHC.Generics (Generic)
import qualified Data.Aeson as Ae


data Context = Context {
  messages :: [ MessageFsm ]
  , currentMsg :: Maybe MessageFsm
  , issues :: [ Text ]
} deriving (Show)


initContext :: Context
initContext = Context {
  messages = []
  , currentMsg = Nothing
  , issues = []
}


data MessageFsm =
  UserMF Timing UserMessage
  | AssistantMF Timing AssistantMessage
  | SystemMF Timing SystemMessage
  | ToolMF Timing ToolMessage
  | UnknownMF Timing UnknownMessage
  deriving (Show)

data Timing = Timing {
  createTime :: Maybe Double
  , updateTime :: Maybe Double
} deriving (Show)


data UserMessage = UserMessage {
  textUM :: Text
  , attachmentsUM :: [ Text ]
} deriving (Show)


data AssistantMessage = AssistantMessage {
  response :: Maybe ResponseAst
  , attachmentsAM :: [ Text ]
  , subActions :: [ SubAction ]
} deriving (Show)

newtype ResponseAst = ResponseAst {
  textRA :: Text
} deriving (Show)


data SubAction = SubAction
  | ReflectionSA Reflection
  | CodeSA Code
  | ToolCallSA ToolCall
  | IntermediateSA Text
  deriving (Show)


data Reflection = Reflection {
  summaryRF :: Text
  , contentRF :: Text
  , chunksRF :: [ Text ]
  , finishedRF :: Maybe Bool
} deriving (Show)


data Code = Code {
  languageCC :: Text
  , responseFormatNameCC :: Maybe Text
  , textCC :: Text
} deriving (Show)


data ToolCall = ToolCall {
  toolNameTC :: Text
  , toolInputTC :: Text
} deriving (Show)


newtype SystemMessage = SystemMessage {
  textSM :: Text
} deriving (Show)


newtype ToolMessage = ToolMessage {
  textTM :: Text
} deriving (Show)

newtype UnknownMessage = UnknownMessage {
  textUM :: Text
} deriving (Show)



data OaiCodeJson = OaiCodeJson {
  nameOJ :: Text
  , typeOJ :: Text
  , contentOJ :: Text
} deriving (Show, Generic)

instance Ae.FromJSON OaiCodeJson where
  parseJSON = Ae.withObject "OaiCodeJson" $ \o -> OaiCodeJson
    <$> o Ae..: "name"
    <*> o Ae..: "type"
    <*> o Ae..: "content"

