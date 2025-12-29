create schema if not exists oai;

set schema 'oai';

CREATE TABLE discussions (
  uid BIGSERIAL PRIMARY KEY,
  title TEXT NOT NULL,
  eid TEXT NOT NULL,
  create_time DOUBLE PRECISION NOT NULL,
  update_time DOUBLE PRECISION NOT NULL
);

CREATE TABLE nodes (
  uid BIGSERIAL PRIMARY KEY,
  discussion_fk BIGINT REFERENCES discussions(uid),
  eid TEXT NOT NULL,
  parent_fk BIGINT REFERENCES nodes(uid)
);

CREATE TABLE messages (
  uid BIGSERIAL PRIMARY KEY,
  node_fk BIGINT REFERENCES nodes(uid),
  eid TEXT NOT NULL,
  create_time DOUBLE PRECISION,
  update_time DOUBLE PRECISION,
  status TEXT NOT NULL,
  end_turn BOOLEAN,
  weight DOUBLE PRECISION NOT NULL,
  metadata JSONB NOT NULL DEFAULT '{}',
  recipient TEXT NOT NULL,
  channel TEXT
);

CREATE TABLE authors (
  uid BIGSERIAL PRIMARY KEY,
  message_fk BIGINT REFERENCES messages(uid),
  role TEXT NOT NULL,
  name TEXT,
  metadata JSONB NOT NULL DEFAULT '{}'
);

CREATE TABLE contents (
  uid BIGSERIAL PRIMARY KEY,
  message_fk BIGINT REFERENCES messages(uid),
  content_type TEXT NOT NULL
);


CREATE TABLE code_contents (
  content_fk BIGINT PRIMARY KEY REFERENCES contents(uid),
  language TEXT NOT NULL,
  response_format_name TEXT,
  text TEXT NOT NULL
);


CREATE TABLE execution_output_contents (
  content_fk BIGINT PRIMARY KEY REFERENCES contents(uid),
  text TEXT NOT NULL
);


--  multimodal_text_contents: it's simply a list of parts -> so multimodal_parts is directly connected to contents.


CREATE TABLE model_editable_context_contents (
  content_fk BIGINT PRIMARY KEY REFERENCES contents(uid),
  model_set_context TEXT NOT NULL,
  repository JSONB,
  repo_summary JSONB,
  structured_context JSONB
);

CREATE TABLE reasoning_recap_contents (
  content_fk BIGINT PRIMARY KEY REFERENCES contents(uid),
  content TEXT NOT NULL
);


CREATE TABLE system_error_contents (
  content_fk BIGINT PRIMARY KEY REFERENCES contents(uid),
  name TEXT NOT NULL,
  text TEXT NOT NULL
);


CREATE TABLE tether_browsing_display_contents (
  content_fk BIGINT PRIMARY KEY REFERENCES contents(uid),
  results TEXT NOT NULL,
  summary JSONB,
  assets JSONB,
  tether_id TEXT
);

CREATE TABLE tether_quote_contents (
  content_fk BIGINT PRIMARY KEY REFERENCES contents(uid),
  url TEXT NOT NULL,
  domain TEXT NOT NULL,
  text TEXT NOT NULL,
  title TEXT NOT NULL,
  tether_id TEXT
);

CREATE TABLE text_contents (
  content_fk BIGINT PRIMARY KEY REFERENCES contents(uid),
  parts TEXT[] NOT NULL
);

CREATE TABLE thoughts_contents (
  content_fk BIGINT PRIMARY KEY REFERENCES contents(uid),
  source_analysis_msg_id TEXT NOT NULL
);

CREATE TABLE thoughts (
  uid BIGSERIAL PRIMARY KEY,
  thoughts_content_fk BIGINT REFERENCES thoughts_contents(content_fk),
  summary TEXT NOT NULL,
  content TEXT NOT NULL,
  chunks JSONB NOT NULL DEFAULT '[]',
  finished BOOLEAN NOT NULL
);



CREATE TABLE multimodal_parts (
  uid BIGSERIAL PRIMARY KEY
, content_fk BIGINT REFERENCES contents(uid)
, content_type TEXT NOT NULL
);


create table image_asset_pointer_mmpart (
  uid BIGSERIAL PRIMARY KEY,
  mmpart_fk bigint references multimodal_parts(uid),
  asset_pointer TEXT NOT NULL,
  size_bytes BIGINT NOT NULL,
  width INT NOT NULL,
  height INT NOT NULL,
  fovea JSONB,
  metadata_fk BIGINT REFERENCES metadatas(uid)
);

create table text_mmpart (
  mmpart_fk bigint primary key references multimodal_parts(uid),
  text TEXT NOT NULL
);


CREATE TABLE dalles (
  uid BIGSERIAL PRIMARY KEY,
  gen_id TEXT,
  prompt TEXT NOT NULL,
  seed INT,
  parent_gen_id TEXT,
  edit_op TEXT,
  serialization_title TEXT NOT NULL
);

CREATE TABLE generations (
  uid BIGSERIAL PRIMARY KEY,
  gen_id TEXT,
  gen_size TEXT NOT NULL,
  seed INT,
  parent_gen_id TEXT,
  height INT NOT NULL,
  width INT NOT NULL,
  transparent_background BOOLEAN NOT NULL,
  serialization_title TEXT NOT NULL,
  orientation TEXT
);

CREATE TABLE metadatas_imgasset (
  uid BIGSERIAL PRIMARY KEY,
  dalle_fk BIGINT REFERENCES dalles(uid),
  gizmo JSONB,
  generation_fk BIGINT REFERENCES generations(uid),
  container_pixel_height INT,
  container_pixel_width INT,
  emu_omit_glimpse_image JSONB,
  emu_patches_override JSONB,
  lpe_keep_patch_ijhw JSONB,
  lpe_delta_encoding_channel JSONB,
  sanitized BOOLEAN NOT NULL,
  asset_pointer_link JSONB,
  watermarked_asset_pointer JSONB,
  is_no_auth_placeholder JSONB
);

create table unknown_contents (
  content_fk BIGINT PRIMARY KEY REFERENCES contents(uid),
  opaqueValue JSONB NOT NULL
);

create table audio_transcription_mmpart (
  mmpart_fk bigint primary key references multimodal_parts(uid),
  text TEXT NOT NULL,
  direction TEXT NOT NULL,
  decoding_id TEXT
);

create table audio_asset_pointer_mmpart (
  uid BIGSERIAL PRIMARY KEY,
  mmpart_fk bigint references multimodal_parts(uid),
  expiry_datetime JSONB,
  asset_pointer TEXT NOT NULL,
  size_bytes BIGINT NOT NULL,
  format TEXT NOT NULL,
  tool_audio_direction TEXT
);

CREATE TABLE metadatas_audioasset (
  assetptr_fk bigint primary key references audio_asset_pointer_mmpart(uid),
  start_timestamp JSONB,
  end_timestamp JSONB,
  pretokenized_vq JSONB,
  interruptions JSONB,
  original_audio_source JSONB,
  transcription JSONB,
  word_transcription JSONB,
  start_stamp DOUBLE PRECISION NOT NULL,
  end_stamp DOUBLE PRECISION NOT NULL
);

create table real_time_user_av_mmpart (
  uid BIGSERIAL PRIMARY KEY,
  mmpart_fk bigint references multimodal_parts(uid),
  expiry_datetime JSONB,
  frames_asset_pointers JSONB,
  video_container_asset_pointer JSONB,
  audio_start_timestamp DOUBLE PRECISION
);

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
-- Context
-- -----------------------------

CREATE TABLE IF NOT EXISTS discourse (
  uid                BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  uuid               UUID NOT NULL DEFAULT gen_random_uuid(),

  created_at         TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at         TIMESTAMPTZ NOT NULL DEFAULT now(),

  CONSTRAINT discourse_uuid_uniq UNIQUE (uuid)
);

-- Issues are a list: Context.issues :: [Text]
CREATE TABLE IF NOT EXISTS discourse_issue (
  uid        BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  uuid       UUID NOT NULL DEFAULT gen_random_uuid(),
  discourse_fk BIGINT NOT NULL,
  seq        INTEGER NOT NULL,
  text       TEXT NOT NULL,

  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),

  CONSTRAINT discourse_issue_uuid_uniq UNIQUE (uuid),
  CONSTRAINT discourse_issue_discourse_fk_fk
    FOREIGN KEY (discourse_fk) REFERENCES discourse(uid) ON DELETE CASCADE,
  CONSTRAINT discourse_issue_discourse_seq_uniq UNIQUE (discourse_fk, seq)
);

-- -----------------------------
-- Messages (MessageFsm)
-- -----------------------------

CREATE TABLE IF NOT EXISTS messagefsm (
  uid        BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  uuid       UUID NOT NULL DEFAULT gen_random_uuid(),

  discourse_fk BIGINT NOT NULL,
  seq        INTEGER NOT NULL, -- order within Context.messages

  kind       message_kind NOT NULL,

  -- Timing (converted from epoch seconds at ingestion)
  created_at TIMESTAMPTZ NULL,
  updated_at TIMESTAMPTZ NULL,

  CONSTRAINT message_uuid_uniq UNIQUE (uuid),
  CONSTRAINT message_discourse_fk_fk
    FOREIGN KEY (discourse_fk) REFERENCES discourse(uid) ON DELETE CASCADE,
  CONSTRAINT message_discourse_seq_uniq UNIQUE (discourse_fk, seq)
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
CREATE TABLE IF NOT EXISTS code (
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

CREATE INDEX IF NOT EXISTS message_discourse_fk_idx ON messagefsm (discourse_fk);
CREATE INDEX IF NOT EXISTS message_kind_idx       ON messagefsm (kind);
CREATE INDEX IF NOT EXISTS message_uuid_idx       ON messagefsm (uuid);

CREATE INDEX IF NOT EXISTS attachment_message_fk_idx ON message_attachment (message_fk);

CREATE INDEX IF NOT EXISTS sub_action_message_fk_idx ON sub_action (message_fk);
CREATE INDEX IF NOT EXISTS sub_action_kind_idx       ON sub_action (kind);

CREATE INDEX IF NOT EXISTS assistant_response_fk_idx ON assistant_message (response_fk);
