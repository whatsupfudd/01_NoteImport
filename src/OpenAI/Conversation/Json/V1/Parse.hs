module OpenAI.Conversation.Json.V1.Parse where

import qualified Data.ByteString.Lazy as Bl
import qualified Data.List as L
import qualified Data.Map.Strict as Mp
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import GHC.Generics (Generic)

import qualified Data.Aeson as Ae

import qualified Options.Runtime as Rto
import qualified OpenAI.Conversation.Json.V1.Schema as Jv1
import qualified OpenAI.Conversation.Unify as Ju
import qualified OpenAI.Conversation.Json.MsgSchema as Jm
import OpenAI.Types


analyzeV1 :: Jv1.Conversation -> Either Text Context
analyzeV1 v1Conv =
  let
    -- rootChild = Mp.lookup "client-created-root" discussion.mappingCv
    mbRootNode = findRootNode v1Conv.mappingCv
  in
  case mbRootNode of
    Just rootNode ->
      Right $ runFSM initContext v1Conv.mappingCv rootNode.idNd
    Nothing -> Left $ "@[analyzeDiscussion] no root node found for discussion: " <> v1Conv.titleCv <> ", id: " <> v1Conv.convIdCv


runFSM :: Context -> Mp.Map Text Jv1.Node -> Text -> Context
runFSM context mapping nodeID =
  case Mp.lookup nodeID mapping of
    Nothing -> context { issues = "node not found: " <> nodeID : context.issues }
    Just node ->
      let
        updCtxt = case node.messageNd of
          Just message ->
            case message.authorMsg.roleAu of
              "user" -> Ju.handleUserMsg context message
              "assistant" -> Ju.handleAssistantMsg context message
              "system" -> Ju.handleSystemMsg context message
              "tool" -> Ju.handleToolMsg context message
              _ -> context { issues = "unknown role: " <> message.authorMsg.roleAu : context.issues }
          Nothing -> context
            -- context { issues = "no message found: " <> nodeID : context.issues }
      in
      foldr (\nodeID accum -> runFSM accum mapping nodeID) updCtxt node.childrenNd


findRootNode :: Mp.Map Text Jv1.Node -> Maybe Jv1.Node
findRootNode mapping =
  case Mp.lookup "client-created-root" mapping of
    Just node -> Just node
    Nothing ->
      L.find (\node -> isNothing node.parentNd) $ Mp.elems mapping