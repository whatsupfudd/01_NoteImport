create schema if not exists oai;

set schema 'oai';

-- For Discussion Context serialisation:
-- Postgres schema for serialising Context / MessageFsm / sub-structures
-- Conventions:
--   * Primary key: bigint identity, named "uid"
--   * External reference: "uuid" (default gen_random_uuid()) where meaningful
--   * Foreign keys: columns end with "_fk"
--   * Minimal redundancy in column names

CREATE EXTENSION IF NOT EXISTS pgcrypto;


-- -----------------------------
-- Enums
-- -----------------------------

DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'message_kind') THEN
    CREATE TYPE message_kind AS ENUM ('user', 'assistant', 'system', 'tool', 'unknown');
  END IF;

  IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'sub_action_kind') THEN
    CREATE TYPE sub_action_kind AS ENUM ('reflection', 'code', 'tool_call', 'intermediate');
  END IF;
END$$;

-- -----------------------------
-- Discussion
-- -----------------------------

CREATE TABLE IF NOT EXISTS discussion (
  uid BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY
  , uuid UUID NOT NULL DEFAULT gen_random_uuid()
  , oaiid uuid not null
  , created_at TIMESTAMPTZ NOT NULL DEFAULT now()
  , updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
  , CONSTRAINT discussion_uuid_uniq UNIQUE (uuid)
  , CONSTRAINT discussion_oaiid_uniq UNIQUE (oaiid)
);

-- Issues are a list: Context.issues :: [Text]
CREATE TABLE IF NOT EXISTS discussion_issue (
  uid        BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  uuid       UUID NOT NULL DEFAULT gen_random_uuid(),
  discussion_fk BIGINT NOT NULL,
  seq        INTEGER NOT NULL,
  text       TEXT NOT NULL,

  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),

  CONSTRAINT discussion_issue_uuid_uniq UNIQUE (uuid),
  CONSTRAINT discussion_issue_discussion_fk_fk
    FOREIGN KEY (discussion_fk) REFERENCES discussion(uid) ON DELETE CASCADE,
  CONSTRAINT discussion_issue_discussion_seq_uniq UNIQUE (discussion_fk, seq)
);

-- -----------------------------
-- Messages (MessageFsm)
-- -----------------------------

CREATE TABLE IF NOT EXISTS messagefsm (
  uid BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  uuid UUID NOT NULL DEFAULT gen_random_uuid(),
  discussion_fk BIGINT NOT NULL,
  seq INTEGER NOT NULL, -- order within Context.messages
  kind message_kind NOT NULL,
  -- Timing (converted from epoch seconds at ingestion)
  created_at TIMESTAMPTZ NULL,
  updated_at TIMESTAMPTZ NULL,
  CONSTRAINT message_uuid_uniq UNIQUE (uuid),
  CONSTRAINT message_discussion_fk_fk
    FOREIGN KEY (discussion_fk) REFERENCES discussion(uid) ON DELETE CASCADE,
  CONSTRAINT message_discussion_seq_uniq UNIQUE (discussion_fk, seq)
);

-- Attachments: UserMessage.attachmentsUM, AssistantMessage.attachmentsAM, etc.
CREATE TABLE IF NOT EXISTS message_attachment (
  uid        BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  uuid       UUID NOT NULL DEFAULT gen_random_uuid(),

  message_fk BIGINT NOT NULL,
  seq        INTEGER NOT NULL,
  value      TEXT NOT NULL,

  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),

  CONSTRAINT message_attachment_uuid_uniq UNIQUE (uuid),
  CONSTRAINT message_attachment_message_fk_fk
    FOREIGN KEY (message_fk) REFERENCES messagefsm(uid) ON DELETE CASCADE,
  CONSTRAINT message_attachment_message_seq_uniq UNIQUE (message_fk, seq)
);

create table if not exists message_summary (
  message_fk BIGINT PRIMARY KEY REFERENCES messagefsm(uid),
  content TEXT NOT NULL
);

-- -----------------------------
-- Per-kind payload tables
-- -----------------------------

-- UserMF Timing UserMessage
CREATE TABLE IF NOT EXISTS user_message (
  uid        BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  uuid       UUID NOT NULL DEFAULT gen_random_uuid(),

  message_fk BIGINT NOT NULL,
  text       TEXT NOT NULL,

  CONSTRAINT user_message_uuid_uniq UNIQUE (uuid),
  CONSTRAINT user_message_message_fk_fk
    FOREIGN KEY (message_fk) REFERENCES messagefsm(uid) ON DELETE CASCADE,
  CONSTRAINT user_message_message_fk_uniq UNIQUE (message_fk)
);

-- AssistantMF Timing AssistantMessage
-- AssistantMessage.response :: Maybe ResponseAst
CREATE TABLE IF NOT EXISTS response_ast (
  uid  BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  uuid UUID NOT NULL DEFAULT gen_random_uuid(),
  text TEXT NOT NULL,

  CONSTRAINT response_ast_uuid_uniq UNIQUE (uuid)
);

CREATE TABLE IF NOT EXISTS assistant_message (
  uid         BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  uuid        UUID NOT NULL DEFAULT gen_random_uuid(),

  message_fk  BIGINT NOT NULL,
  response_fk BIGINT NULL,

  CONSTRAINT assistant_message_uuid_uniq UNIQUE (uuid),
  CONSTRAINT assistant_message_message_fk_fk
    FOREIGN KEY (message_fk) REFERENCES messagefsm(uid) ON DELETE CASCADE,
  CONSTRAINT assistant_message_response_fk_fk
    FOREIGN KEY (response_fk) REFERENCES response_ast(uid) ON DELETE SET NULL,
  CONSTRAINT assistant_message_message_fk_uniq UNIQUE (message_fk)
);

-- SystemMF Timing SystemMessage
CREATE TABLE IF NOT EXISTS system_message (
  uid        BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  uuid       UUID NOT NULL DEFAULT gen_random_uuid(),

  message_fk BIGINT NOT NULL,
  text       TEXT NOT NULL,

  CONSTRAINT system_message_uuid_uniq UNIQUE (uuid),
  CONSTRAINT system_message_message_fk_fk
    FOREIGN KEY (message_fk) REFERENCES messagefsm(uid) ON DELETE CASCADE,
  CONSTRAINT system_message_message_fk_uniq UNIQUE (message_fk)
);

-- ToolMF Timing ToolMessage
CREATE TABLE IF NOT EXISTS tool_message (
  uid        BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  uuid       UUID NOT NULL DEFAULT gen_random_uuid(),

  message_fk BIGINT NOT NULL,
  text       TEXT NOT NULL,

  CONSTRAINT tool_message_uuid_uniq UNIQUE (uuid),
  CONSTRAINT tool_message_message_fk_fk
    FOREIGN KEY (message_fk) REFERENCES messagefsm(uid) ON DELETE CASCADE,
  CONSTRAINT tool_message_message_fk_uniq UNIQUE (message_fk)
);

-- UnknownMF Timing UnknownMessage
CREATE TABLE IF NOT EXISTS unknown_message (
  uid        BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  uuid       UUID NOT NULL DEFAULT gen_random_uuid(),

  message_fk BIGINT NOT NULL,
  text       TEXT NOT NULL,

  CONSTRAINT unknown_message_uuid_uniq UNIQUE (uuid),
  CONSTRAINT unknown_message_message_fk_fk
    FOREIGN KEY (message_fk) REFERENCES messagefsm(uid) ON DELETE CASCADE,
  CONSTRAINT unknown_message_message_fk_uniq UNIQUE (message_fk)
);

-- -----------------------------
-- SubActions (Assistant only)
-- -----------------------------

-- AssistantMessage.subActions :: [SubAction]
CREATE TABLE IF NOT EXISTS sub_action (
  uid        BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  uuid       UUID NOT NULL DEFAULT gen_random_uuid(),

  message_fk BIGINT NOT NULL, -- references message (assistant kind expected by app)
  seq        INTEGER NOT NULL,
  kind       sub_action_kind NOT NULL,

  -- Only used when kind = 'intermediate'
  text       TEXT NULL,

  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),

  CONSTRAINT sub_action_uuid_uniq UNIQUE (uuid),
  CONSTRAINT sub_action_message_fk_fk
    FOREIGN KEY (message_fk) REFERENCES messagefsm(uid) ON DELETE CASCADE,
  CONSTRAINT sub_action_message_seq_uniq UNIQUE (message_fk, seq)
);

-- ReflectionSA Reflection
CREATE TABLE IF NOT EXISTS reflection (
  uid           BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  uuid          UUID NOT NULL DEFAULT gen_random_uuid(),

  sub_action_fk BIGINT NOT NULL,

  summary       TEXT NOT NULL,
  content       TEXT NOT NULL,
  finished      BOOLEAN NULL,

  CONSTRAINT reflection_uuid_uniq UNIQUE (uuid),
  CONSTRAINT reflection_sub_action_fk_fk
    FOREIGN KEY (sub_action_fk) REFERENCES sub_action(uid) ON DELETE CASCADE,
  CONSTRAINT reflection_sub_action_fk_uniq UNIQUE (sub_action_fk)
);

-- Reflection.chunksRF :: [Text]
CREATE TABLE IF NOT EXISTS reflection_chunk (
  uid           BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  uuid          UUID NOT NULL DEFAULT gen_random_uuid(),

  reflection_fk BIGINT NOT NULL,
  seq           INTEGER NOT NULL,
  text          TEXT NOT NULL,

  CONSTRAINT reflection_chunk_uuid_uniq UNIQUE (uuid),
  CONSTRAINT reflection_chunk_reflection_fk_fk
    FOREIGN KEY (reflection_fk) REFERENCES reflection(uid) ON DELETE CASCADE,
  CONSTRAINT reflection_chunk_reflection_seq_uniq UNIQUE (reflection_fk, seq)
);

-- CodeSA Code
CREATE TABLE IF NOT EXISTS code_block (
  uid           BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  uuid          UUID NOT NULL DEFAULT gen_random_uuid(),

  sub_action_fk BIGINT NOT NULL,

  language      TEXT NOT NULL,
  format_name   TEXT NULL, -- responseFormatNameCC
  text          TEXT NOT NULL,

  CONSTRAINT code_uuid_uniq UNIQUE (uuid),
  CONSTRAINT code_sub_action_fk_fk
    FOREIGN KEY (sub_action_fk) REFERENCES sub_action(uid) ON DELETE CASCADE,
  CONSTRAINT code_sub_action_fk_uniq UNIQUE (sub_action_fk)
);

-- ToolCallSA ToolCall
CREATE TABLE IF NOT EXISTS tool_call (
  uid           BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  uuid          UUID NOT NULL DEFAULT gen_random_uuid(),

  sub_action_fk BIGINT NOT NULL,

  tool_name     TEXT NOT NULL,
  tool_input    TEXT NOT NULL,

  CONSTRAINT tool_call_uuid_uniq UNIQUE (uuid),
  CONSTRAINT tool_call_sub_action_fk_fk
    FOREIGN KEY (sub_action_fk) REFERENCES sub_action(uid) ON DELETE CASCADE,
  CONSTRAINT tool_call_sub_action_fk_uniq UNIQUE (sub_action_fk)
);

-- -----------------------------
-- Useful indexes
-- -----------------------------

CREATE INDEX IF NOT EXISTS message_discussion_fk_idx ON messagefsm (discussion_fk);
CREATE INDEX IF NOT EXISTS message_kind_idx       ON messagefsm (kind);
CREATE INDEX IF NOT EXISTS message_uuid_idx       ON messagefsm (uuid);

CREATE INDEX IF NOT EXISTS attachment_message_fk_idx ON message_attachment (message_fk);

CREATE INDEX IF NOT EXISTS sub_action_message_fk_idx ON sub_action (message_fk);
CREATE INDEX IF NOT EXISTS sub_action_kind_idx       ON sub_action (kind);

CREATE INDEX IF NOT EXISTS assistant_response_fk_idx ON assistant_message (response_fk);


--- 
-- Discussion Groups
-- -----------------------------

CREATE TABLE IF NOT EXISTS discussion_group (
  uid BIGSERIAL PRIMARY KEY
  , parent_fk BIGINT REFERENCES discussion_group(uid) ON DELETE CASCADE
  , uuid UUID NOT NULL DEFAULT gen_random_uuid()
  , label TEXT NOT NULL UNIQUE
);

CREATE INDEX IF NOT EXISTS discussion_group_parent_fk_idx ON discussion_group (parent_fk);
CREATE INDEX IF NOT EXISTS discussion_group_uuid_idx ON discussion_group (uuid);

create table if not exists discussion_group_member (
  discussion_group_fk BIGINT NOT NULL REFERENCES discussion_group(uid) ON DELETE CASCADE,
  conversation_fk BIGINT NOT NULL REFERENCES conversations(uid) ON DELETE CASCADE
  , lastUse date
  , PRIMARY KEY (discussion_group_fk, conversation_fk)
);

create index if not exists discussion_group_member_dgroup_fk_idx on discussion_group_member (discussion_group_fk);
create index if not exists discussion_group_member_discussion_fk_idx on discussion_group_member (conversation_fk);
