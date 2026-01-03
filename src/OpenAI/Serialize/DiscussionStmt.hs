{-# LANGUAGE QuasiQuotes #-}

module OpenAI.Serialize.DiscussionStmt where

import Data.Text (Text)
import Data.Int (Int32, Int64)
import Data.UUID (UUID)
import Data.Time.Clock (UTCTime)
import qualified Data.Vector as V
import Data.Vector (Vector)

import qualified Data.Aeson as Ae
import Hasql.Statement (Statement)
import qualified Hasql.TH as TH


--- Discussion Discussion serialisation:
insertDiscussion :: Statement (Text, UUID) (Int64, UUID)
insertDiscussion =
  [TH.singletonStatement|
    insert into oai.discussion
      (title, oaiid)
    values
      ($1 :: text, $2 :: uuid)
    returning uid :: int8, uuid :: uuid
  |]

insertDiscussionIssue :: Statement (Int64, Int32, Text) ()
insertDiscussionIssue =
  [TH.resultlessStatement|
    insert into oai.discussion_issue (context_fk, seq, text)
    values ($1 :: int8, $2 :: int4, $3 :: text)
  |]

insertMessage :: Statement (Int64, Int32, Text, Maybe UTCTime, Maybe UTCTime) Int64
insertMessage =
  [TH.singletonStatement|
    insert into oai.messagefsm (discussion_fk, seq, kind, created_at, updated_at)
    values (
      $1 :: int8,
      $2 :: int4,
      ($3 :: text)::oai.message_kind,
      $4 :: timestamptz?,
      $5 :: timestamptz?
    )
    returning uid :: int8
  |]


insertUserMessage :: Statement (Int64, Text) ()
insertUserMessage =
  [TH.resultlessStatement|
    insert into oai.user_message (message_fk, text)
    values ($1 :: int8, $2 :: text)
  |]

insertResponseAst :: Statement Text Int64
insertResponseAst =
  [TH.singletonStatement|
    insert into oai.response_ast (text)
    values ($1 :: text)
    returning uid :: int8
  |]

insertAssistantMessage :: Statement (Int64, Maybe Int64) ()
insertAssistantMessage =
  [TH.resultlessStatement|
    insert into oai.assistant_message (message_fk, response_fk)
    values ($1 :: int8, $2 :: int8?)
  |]

insertSystemMessage :: Statement (Int64, Text) ()
insertSystemMessage =
  [TH.resultlessStatement|
    insert into oai.system_message (message_fk, text)
    values ($1 :: int8, $2 :: text)
  |]

insertToolMessage :: Statement (Int64, Text) ()
insertToolMessage =
  [TH.resultlessStatement|
    insert into oai.tool_message (message_fk, text)
    values ($1 :: int8, $2 :: text)
  |]

insertUnknownMessage :: Statement (Int64, Text) ()
insertUnknownMessage =
  [TH.resultlessStatement|
    insert into oai.unknown_message (message_fk, text)
    values ($1 :: int8, $2 :: text)
  |]

insertAttachment :: Statement (Int64, Int32, Text) ()
insertAttachment =
  [TH.resultlessStatement|
    insert into oai.message_attachment (message_fk, seq, value)
    values ($1 :: int8, $2 :: int4, $3 :: text)
  |]

insertSubAction :: Statement (Int64, Int32, Text, Maybe Text) Int64
insertSubAction =
  [TH.singletonStatement|
    insert into oai.sub_action (message_fk, seq, kind, text)
    values (
      $1 :: int8,
      $2 :: int4,
      ($3 :: text)::oai.sub_action_kind,
      $4 :: text?
    )
    returning uid :: int8
  |]

insertReflection :: Statement (Int64, Text, Text, Maybe Bool) Int64
insertReflection =
  [TH.singletonStatement|
    insert into oai.reflection (sub_action_fk, summary, content, finished)
    values ($1 :: int8, $2 :: text, $3 :: text, $4 :: bool?)
    returning uid :: int8
  |]

insertReflectionChunk :: Statement (Int64, Int32, Text) ()
insertReflectionChunk =
  [TH.resultlessStatement|
    insert into oai.reflection_chunk (reflection_fk, seq, text)
    values ($1 :: int8, $2 :: int4, $3 :: text)
  |]

insertCode :: Statement (Int64, Text, Maybe Text, Text) ()
insertCode =
  [TH.resultlessStatement|
    insert into oai.code_block (sub_action_fk, language, format_name, text)
    values ($1 :: int8, $2 :: text, $3 :: text?, $4 :: text)
  |]

insertToolCall :: Statement (Int64, Text, Text) ()
insertToolCall =
  [TH.resultlessStatement|
    insert into oai.tool_call (sub_action_fk, tool_name, tool_input)
    values ($1 :: int8, $2 :: text, $3 :: text)
  |]
