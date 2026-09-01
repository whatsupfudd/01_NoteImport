module OpenAI.Delta.Hash (
  hashNodeJs, hashMsgJs, hashContentJs
  , hashNodeDb, hashMsgDb, hashContentDb
) where

import qualified Data.ByteArray as BA
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Builder as Bb
import qualified Data.ByteString.Lazy as Bl
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Numeric (showHex)

import qualified Crypto.Hash as CH
import qualified Data.Aeson as Ae
import qualified Data.Aeson.Key as Ak
import qualified Data.Aeson.KeyMap as Km

import qualified OpenAI.Content.Codec as Cc
import qualified OpenAI.Content.Hash as Ch
import qualified OpenAI.Content.Types as Ct
import qualified OpenAI.Delta.Snap as Sn
import qualified OpenAI.Delta.Types as Dt
import qualified OpenAI.Json.Reader as Jd


hashNodeJs :: Jd.Node -> Dt.Hash
hashNodeJs node = Ch.hashValue $ valueNodeJs node


hashMsgJs :: Jd.Message -> Dt.Hash
hashMsgJs msg = Ch.hashValue $ valueMsgJs msg


hashContentJs :: Jd.Content -> Dt.Hash
hashContentJs = Ch.hashPayload . payloadContentJs


hashNodeDb :: Sn.NodeSnap -> Dt.Hash
hashNodeDb node = Ch.hashValue $ valueNodeDb node


hashMsgDb :: Sn.MsgSnap -> Dt.Hash
hashMsgDb msg = Ch.hashValue $ valueMsgDb msg


hashContentDb :: Sn.ContentSnap -> Dt.Hash
hashContentDb = hashContentSnap


hashContentSnap :: Sn.ContentSnap -> Dt.Hash
hashContentSnap content = Ch.hashValue content.payload


valueNodeJs :: Jd.Node -> Ae.Value
valueNodeJs node =
  Ae.object [
      "eid" Ae..= node.idNd
    , "parent_eid" Ae..= node.parentNd
    , "children" Ae..= node.childrenNd
    , "msg_hash" Ae..= maybe Ae.Null (valueHash . hashMsgJs) node.messageNd
    ]


valueMsgJs :: Jd.Message -> Ae.Value
valueMsgJs msg =
  Ae.object [
      "eid" Ae..= msg.idMsg
    , "author" Ae..= valueAuthorJs msg.authorMsg
    , "time_create" Ae..= msg.createTimeMsg
    , "time_update" Ae..= msg.updateTimeMsg
    , "status" Ae..= msg.statusMsg
    , "end_turn" Ae..= msg.endTurnMsg
    , "weight" Ae..= msg.weightMsg
    , "metadata" Ae..= msg.metadataMsg
    , "recipient" Ae..= msg.recipientMsg
    , "channel" Ae..= msg.channelMsg
    , "contents" Ae..= [valueHash $ hashContentJs msg.contentMsg]
    ]


valueAuthorJs :: Jd.Author -> Ae.Value
valueAuthorJs author =
  Ae.object [
      "role" Ae..= author.roleAu
    , "name" Ae..= author.nameAu
    , "metadata" Ae..= author.metadataAu
    ]


payloadContentJs :: Jd.Content -> Ct.Payload
payloadContentJs content =
  case Cc.fromJson content of
    Left issue ->
      error $ "OpenAI.Delta.Hash.hashContentJs: content codec invariant failed: " <> show issue
    Right payload ->
      payload


valueNodeDb :: Sn.NodeSnap -> Ae.Value
valueNodeDb node =
  Ae.object [
      "eid" Ae..= node.eidNode
    , "parent_eid" Ae..= node.eidParent
    , "seq_node" Ae..= node.seqNode
    , "seq_child" Ae..= node.seqChild
    , "seq_pre" Ae..= node.seqPre
    , "msg_hash" Ae..= maybe Ae.Null (valueHash . hashMsgDb) node.msg
    ]


valueMsgDb :: Sn.MsgSnap -> Ae.Value
valueMsgDb msg =
  Ae.object [
      "eid" Ae..= msg.eidMsg
    , "time_create" Ae..= msg.timeCreate
    , "time_update" Ae..= msg.timeUpdate
    , "status" Ae..= msg.status
    , "end_turn" Ae..= msg.endTurn
    , "weight" Ae..= msg.weight
    , "metadata" Ae..= msg.metadata
    , "recipient" Ae..= msg.recipient
    , "channel" Ae..= msg.channel
    , "contents" Ae..= map (valueHash . hashContentSnap) (toList msg.contents)
    ]


valueHash :: Dt.Hash -> Ae.Value
valueHash hash = Ae.String $ bytesHex hash.bytesHash


bytesHex :: Bs.ByteString -> Text
bytesHex bytes = T.pack $ concatMap byteHex $ Bs.unpack bytes


byteHex :: Word8 -> String
byteHex byte =
  let
    raw = showHex byte ""
  in
  case raw of
    [one] -> ['0', one]
    more -> more