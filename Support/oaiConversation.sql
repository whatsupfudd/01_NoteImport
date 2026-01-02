create schema if not exists oai;

set schema 'oai';

CREATE TABLE IF NOT EXISTS conversations (
  uid BIGSERIAL PRIMARY KEY,
  title TEXT NOT NULL,
  eid TEXT NOT NULL,
  create_time DOUBLE PRECISION NOT NULL,
  update_time DOUBLE PRECISION NOT NULL
);

CREATE TABLE IF NOT EXISTS nodes (
  uid BIGSERIAL PRIMARY KEY,
  conversation_fk BIGINT REFERENCES conversations(uid) ON DELETE CASCADE,
  eid TEXT NOT NULL,
  parent_fk BIGINT REFERENCES nodes(uid)
);

CREATE TABLE IF NOT EXISTS messages (
  uid BIGSERIAL PRIMARY KEY,
  node_fk BIGINT REFERENCES nodes(uid) ON DELETE CASCADE,
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

CREATE TABLE IF NOT EXISTS authors (
  uid BIGSERIAL PRIMARY KEY,
  message_fk BIGINT REFERENCES messages(uid),
  role TEXT NOT NULL,
  name TEXT,
  metadata JSONB NOT NULL DEFAULT '{}'
);

CREATE TABLE IF NOT EXISTS contents (
  uid BIGSERIAL PRIMARY KEY,
  message_fk BIGINT REFERENCES messages(uid) ON DELETE CASCADE,
  content_type TEXT NOT NULL
);


CREATE TABLE IF NOT EXISTS code_contents (
  content_fk BIGINT PRIMARY KEY REFERENCES contents(uid) ON DELETE CASCADE,
  language TEXT NOT NULL,
  response_format_name TEXT,
  text TEXT NOT NULL
);


CREATE TABLE IF NOT EXISTS execution_output_contents (
  content_fk BIGINT PRIMARY KEY REFERENCES contents(uid) ON DELETE CASCADE,
  text TEXT NOT NULL
);


--  multimodal_text_contents: it's simply a list of parts -> so multimodal_parts is directly connected to contents.


CREATE TABLE IF NOT EXISTS model_editable_context_contents (
  content_fk BIGINT PRIMARY KEY REFERENCES contents(uid) ON DELETE CASCADE,
  model_set_context TEXT NOT NULL,
  repository JSONB,
  repo_summary JSONB,
  structured_context JSONB
);

CREATE TABLE IF NOT EXISTS reasoning_recap_contents (
  content_fk BIGINT PRIMARY KEY REFERENCES contents(uid) ON DELETE CASCADE,
  content TEXT NOT NULL
);


CREATE TABLE IF NOT EXISTS system_error_contents (
  content_fk BIGINT PRIMARY KEY REFERENCES contents(uid) ON DELETE CASCADE,
  name TEXT NOT NULL,
  text TEXT NOT NULL
);


CREATE TABLE IF NOT EXISTS tether_browsing_display_contents (
  content_fk BIGINT PRIMARY KEY REFERENCES contents(uid) ON DELETE CASCADE,
  results TEXT NOT NULL,
  summary JSONB,
  assets JSONB,
  tether_id TEXT
);

CREATE TABLE IF NOT EXISTS tether_quote_contents (
  content_fk BIGINT PRIMARY KEY REFERENCES contents(uid) ON DELETE CASCADE,
  url TEXT NOT NULL,
  domain TEXT NOT NULL,
  text TEXT NOT NULL,
  title TEXT NOT NULL,
  tether_id TEXT
);

CREATE TABLE IF NOT EXISTS text_contents (
  content_fk BIGINT PRIMARY KEY REFERENCES contents(uid) ON DELETE CASCADE,
  parts TEXT[] NOT NULL
);

CREATE TABLE IF NOT EXISTS thoughts_contents (
  content_fk BIGINT PRIMARY KEY REFERENCES contents(uid) ON DELETE CASCADE,
  source_analysis_msg_id TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS thoughts (
  uid BIGSERIAL PRIMARY KEY,
  thoughts_content_fk BIGINT REFERENCES thoughts_contents(content_fk) ON DELETE CASCADE,
  summary TEXT NOT NULL,
  content TEXT NOT NULL,
  chunks JSONB NOT NULL DEFAULT '[]',
  finished BOOLEAN NOT NULL
);



CREATE TABLE IF NOT EXISTS multimodal_parts (
  uid BIGSERIAL PRIMARY KEY
, content_fk BIGINT REFERENCES contents(uid) ON DELETE CASCADE
, content_type TEXT NOT NULL
);


create table image_asset_pointer_mmpart (
  uid BIGSERIAL PRIMARY KEY,
  mmpart_fk bigint references multimodal_parts(uid) ON DELETE CASCADE,
  asset_pointer TEXT NOT NULL,
  size_bytes BIGINT NOT NULL,
  width INT NOT NULL,
  height INT NOT NULL,
  fovea JSONB
);

create table text_mmpart (
  mmpart_fk bigint primary key references multimodal_parts(uid) ON DELETE CASCADE,
  text TEXT NOT NULL
);


CREATE TABLE IF NOT EXISTS metadatas_imgasset (
  uid BIGSERIAL PRIMARY KEY,
  imgptr_fk BIGINT REFERENCES image_asset_pointer_mmpart(uid) ON DELETE CASCADE,
  gizmo JSONB,
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


CREATE TABLE IF NOT EXISTS dalles (
  metadata_fk bigint primary key references metadatas_imgasset(uid) ON DELETE CASCADE,
  gen_id TEXT,
  prompt TEXT NOT NULL,
  seed INT,
  parent_gen_id TEXT,
  edit_op TEXT,
  serialization_title TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS generations (
  metadata_fk bigint primary key references metadatas_imgasset(uid) ON DELETE CASCADE,
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

create table real_time_user_av_mmpart (
  uid BIGSERIAL PRIMARY KEY,
  mmpart_fk bigint references multimodal_parts(uid),
  expiry_datetime JSONB,
  frames_asset_pointers JSONB,
  video_container_asset_pointer JSONB,
  audio_start_timestamp DOUBLE PRECISION
);

-- Note: metadatas_audioasset is used as overloaded dependent for real_time_user_av_mmpart (part_kind = 2)
-- and audio_asset_pointer_mmpart (part_kind = 1).
CREATE TABLE IF NOT EXISTS metadatas_audioasset (
  assetptr_fk bigint
  , part_kind int not null    -- 1: audio_asset_pointer_mmpart, 2: real_time_user_av_mmpart
  , start_timestamp JSONB
  , end_timestamp JSONB
  , pretokenized_vq JSONB
  , interruptions JSONB
  , original_audio_source JSONB
  , transcription JSONB
  , word_transcription JSONB
  , start_stamp DOUBLE PRECISION NOT NULL
  , end_stamp DOUBLE PRECISION NOT NULL
  , primary key (assetptr_fk, part_kind)
);
