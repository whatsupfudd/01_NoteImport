{-# LANGUAGE DerivingStrategies #-}
module OpenAI.Id
  ( EidConv(EidConv)
  , EidNode(EidNode)
  , EidMsg(EidMsg)
  , RefConv(..)
  , textEidConv
  , textEidNode
  , textEidMsg
  , eidConvFromText
  , eidNodeFromText
  , eidMsgFromText
  , isRootEidNode
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Tx

newtype EidConv = EidConv { textEidConv :: Text }
  deriving stock (Eq, Ord, Show)

newtype EidNode = EidNode { textEidNode :: Text }
  deriving stock (Eq, Ord, Show)

newtype EidMsg = EidMsg { textEidMsg :: Text }
  deriving stock (Eq, Ord, Show)

data RefConv
  = UidRC Int64
  | EidRC EidConv
  deriving stock (Eq, Show)

eidConvFromText :: Text -> Either Text EidConv
eidConvFromText = mkEid "conversation" EidConv

eidNodeFromText :: Text -> Either Text EidNode
eidNodeFromText = mkEid "node" EidNode

eidMsgFromText :: Text -> Either Text EidMsg
eidMsgFromText = mkEid "message" EidMsg

isRootEidNode :: EidNode -> Bool
isRootEidNode eidNode = eidNode.textEidNode == rootNodeTxt

mkEid :: Text -> (Text -> a) -> Text -> Either Text a
mkEid kind wrap txt
  | Tx.null txt = Left ("empty " <> kind <> " eid")
  | otherwise = Right (wrap txt)

rootNodeTxt :: Text
rootNodeTxt = "client-created-root"