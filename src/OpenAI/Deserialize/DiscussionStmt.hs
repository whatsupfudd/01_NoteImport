{-# LANGUAGE QuasiQuotes #-}

module OpenAI.Deserialize.DiscussionStmt where

import Data.Text (Text)
import Data.Int (Int32, Int64)
import Data.UUID (UUID)
import Data.Time.Clock (UTCTime)
import qualified Data.Vector as V
import Data.Vector (Vector)

import qualified Data.Aeson as Ae
import Hasql.Statement (Statement)
import qualified Hasql.TH as TH


fetchAllConversations :: Statement () (Vector (Int64, Text) )
fetchAllConversations =
  [TH.vectorStatement|
    select uid::int8, title::text from oai.conversations
  |]


-- Deserialisation of Discussion:
selectDiscussionByUuid :: Statement UUID (Maybe (Int64, UUID, Text, Text))
selectDiscussionByUuid =
  [TH.maybeStatement|
    select
      uid :: int8
      , uuid :: uuid
      , title :: text
      , conversation_eid :: text
    from oai.discussion
    where uuid = $1 :: uuid
  |]


selectDiscussionByConvId :: Statement Text (Maybe (Int64, UUID, Text, Text))
selectDiscussionByConvId =
  [TH.maybeStatement|
    select
      uid :: int8
      , uuid :: uuid
      , title :: text
      , conversation_eid :: text
    from oai.discussion
    where conversation_eid = $1 :: text
  |]


selectIssues :: Statement Int64 (Vector (Int32, Text))
selectIssues =
  [TH.vectorStatement|
    select
      seq :: int4,
      text :: text
    from oai.discussion_issue
    where discussion_fk = $1 :: int8
    order by seq
  |]

-- One row per message with all possible payloads left-joined.
type MessageRow =
  ( Int64 -- message uid
  , UUID  -- message uuid
  , Int32 -- seq
  , Text  -- kind as text
  , Maybe UTCTime -- created_at
  , Maybe UTCTime -- updated_at
  , Maybe Text    -- user_message.text
  , Maybe Int64   -- assistant_message.response_fk
  , Maybe Text    -- response_ast.text
  , Maybe Text    -- system_message.text
  , Maybe Text    -- tool_message.text
  , Maybe Text    -- unknown_message.text
  )

selectMessages :: Statement Int64 (Vector MessageRow)
selectMessages =
  [TH.vectorStatement|
    select
      m.uid :: int8,
      m.uuid :: uuid,
      m.seq :: int4,
      (m.kind::text) :: text,
      m.created_at :: timestamptz?,
      m.updated_at :: timestamptz?,
      um.text :: text?,
      am.response_fk :: int8?,
      ra.text :: text?,
      sm.text :: text?,
      tm.text :: text?,
      un.text :: text?
    from oai.messagefsm m
      left join oai.user_message um on um.message_fk = m.uid
      left join oai.assistant_message am on am.message_fk = m.uid
      left join oai.response_ast ra on ra.uid = am.response_fk
      left join oai.system_message sm on sm.message_fk = m.uid
      left join oai.tool_message tm on tm.message_fk = m.uid
      left join oai.unknown_message un on un.message_fk = m.uid
    where m.discussion_fk = $1 :: int8
    order by m.seq
  |]

-- message_fk, seq, value (ordered by parent message.seq)
type AttachmentRow = (Int64, Int32, Text)

selectAttachments :: Statement Int64 (Vector AttachmentRow)
selectAttachments =
  [TH.vectorStatement|
    select
      a.message_fk :: int8,
      a.seq :: int4,
      a.value :: text
    from oai.message_attachment a
      join oai.messagefsm m on m.uid = a.message_fk
    where m.discussion_fk = $1 :: int8
    order by m.seq, a.seq
  |]


type MessageSummaryRow = (Int64, Text)
-- (message_fk, content)

selectMessageSummaries :: Statement Int64 (Vector MessageSummaryRow)
selectMessageSummaries =
  [TH.vectorStatement|
    select
      ms.message_fk :: int8,
      ms.content :: text
    from oai.message_summary ms
      join oai.messagefsm m on m.uid = ms.message_fk
    where m.discussion_fk = $1 :: int8
    order by m.seq
  |]

-- uid, uuid, message_fk, seq, kind, text?
type SubActionRow = (Int64, UUID, Int64, Int32, Text, Maybe Text)

selectSubActions :: Statement Int64 (Vector SubActionRow)
selectSubActions =
  [TH.vectorStatement|
    select
      sa.uid :: int8,
      sa.uuid :: uuid,
      sa.message_fk :: int8,
      sa.seq :: int4,
      (sa.kind::text) :: text,
      sa.text :: text?
    from oai.sub_action sa
      join oai.messagefsm m on m.uid = sa.message_fk
    where m.discussion_fk = $1 :: int8
    order by m.seq, sa.seq
  |]

-- reflection uid, sub_action_fk, summary, content, finished?
type ReflectionRow = (Int64, Int64, Text, Text, Maybe Bool)

selectReflections :: Statement Int64 (Vector ReflectionRow)
selectReflections =
  [TH.vectorStatement|
    select
      r.uid :: int8,
      r.sub_action_fk :: int8,
      r.summary :: text,
      r.content :: text,
      r.finished :: bool?
    from oai.reflection r
      join oai.sub_action sa on sa.uid = r.sub_action_fk
      join oai.messagefsm m on m.uid = sa.message_fk
    where m.discussion_fk = $1 :: int8
    order by m.seq, sa.seq
  |]

-- reflection_fk, seq, text
type ReflectionChunkRow = (Int64, Int32, Text)

selectReflectionChunks :: Statement Int64 (Vector ReflectionChunkRow)
selectReflectionChunks =
  [TH.vectorStatement|
    select
      c.reflection_fk :: int8,
      c.seq :: int4,
      c.text :: text
    from oai.reflection_chunk c
      join oai.reflection r on r.uid = c.reflection_fk
      join oai.sub_action sa on sa.uid = r.sub_action_fk
      join oai.messagefsm m on m.uid = sa.message_fk
    where m.discussion_fk = $1 :: int8
    order by m.seq, sa.seq, c.seq
  |]

-- sub_action_fk, language, format_name?, text
type CodeRow = (Int64, Text, Maybe Text, Text)

selectCodes :: Statement Int64 (Vector CodeRow)
selectCodes =
  [TH.vectorStatement|
    select
      c.sub_action_fk :: int8,
      c.language :: text,
      c.format_name :: text?,
      c.text :: text
    from oai.code c
      join oai.sub_action sa on sa.uid = c.sub_action_fk
      join oai.messagefsm m on m.uid = sa.message_fk
    where m.discussion_fk = $1 :: int8
    order by m.seq, sa.seq
  |]

-- sub_action_fk, tool_name, tool_input
type ToolCallRow = (Int64, Text, Text)

selectToolCalls :: Statement Int64 (Vector ToolCallRow)
selectToolCalls =
  [TH.vectorStatement|
    select
      t.sub_action_fk :: int8,
      t.tool_name :: text,
      t.tool_input :: text
    from oai.tool_call t
      join oai.sub_action sa on sa.uid = t.sub_action_fk
      join oai.messagefsm m on m.uid = sa.message_fk
    where m.discussion_fk = $1 :: int8
    order by m.seq, sa.seq
  |]

