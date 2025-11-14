-- KMS — PostgreSQL schema v2


CREATE EXTENSION IF NOT EXISTS citext;
CREATE EXTENSION IF NOT EXISTS pg_trgm;
CREATE EXTENSION IF NOT EXISTS btree_gin;
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE SCHEMA IF NOT EXISTS kms;
-- SET search_path = kms, public;

-- -----------------------------
-- ENUMS
-- -----------------------------
DO $$ BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_type t join pg_namespace n on t.typnamespace = n.oid WHERE n.nspname = 'kms' and typname = 'principal_kind') THEN
    CREATE TYPE kms.principal_kind AS ENUM ('user','group','role','org');
  END IF;
  IF NOT EXISTS (SELECT 1 FROM pg_type t join pg_namespace n on t.typnamespace = n.oid WHERE n.nspname = 'kms' and typname = 'scope_kind') THEN
    CREATE TYPE kms.scope_kind AS ENUM ('tenant','domaintb','bundle','resource');
  END IF;
END $$;

-- -----------------------------
-- 1) Organizations, Users, Groups, Roles
-- -----------------------------
CREATE TABLE IF NOT EXISTS kms.organization (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  name TEXT NOT NULL UNIQUE,
  kind TEXT NOT NULL CHECK (kind IN ('internal','law_firm','regulator','auditor','vendor','other')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS kms.roletb (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  code TEXT NOT NULL UNIQUE,      -- 'PMO','Sponsor/Executive','Counsel','Domain Owner','Auditor/Regulator'
  name TEXT NOT NULL,
  description TEXT
);

CREATE TABLE IF NOT EXISTS kms.grouptb (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  org_fk INT REFERENCES kms.organization(uid) ON DELETE SET NULL,
  code TEXT NOT NULL UNIQUE,      -- 'PMO','CorpSec','Compliance','Tax','Structuring','Capital Markets'
  name TEXT NOT NULL,
  kind TEXT NOT NULL CHECK (kind IN ('internal','cross-entity')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS kms.usertb (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY
  , org_fk INT REFERENCES kms.organization(uid) ON DELETE SET NULL
  , email CITEXT NOT NULL UNIQUE
  , full_name TEXT NOT NULL
  , display_name text
  , is_external BOOLEAN NOT NULL DEFAULT FALSE
  , status TEXT NOT NULL DEFAULT 'active' CHECK (status IN ('active','suspended','disabled'))
  , created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS kms.group_member (
  group_fk INT NOT NULL REFERENCES kms.grouptb(uid) ON DELETE CASCADE,
  user_fk INT NOT NULL REFERENCES kms.usertb(uid) ON DELETE CASCADE,
  PRIMARY KEY (group_fk, user_fk)
);

-- -----------------------------
-- 2) Catalogs (Domain / Type / Tier / Status)
-- -----------------------------
CREATE TABLE IF NOT EXISTS kms.domaintb (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  code TEXT NOT NULL UNIQUE,      -- 'Governance','Regulatory','Tax','Compliance','Structuring','Capital Markets'
  name TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS kms.doc_type (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  code TEXT NOT NULL UNIQUE,      -- 'Memo','Policy','Template','Agreement','Register','Procedure'
  name TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS kms.tier (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  code TEXT NOT NULL UNIQUE,      -- 'Public','Internal','Restricted','Secret'
  name TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS kms.statustb (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  code TEXT NOT NULL UNIQUE,      -- 'Idea','Draft','Review','Counsel','Approved','Final'
  name TEXT NOT NULL,
  order_index INT NOT NULL DEFAULT 0
);

-- -----------------------------
-- 3) RBAC + ABAC overlay
-- -----------------------------
CREATE TABLE IF NOT EXISTS kms.permission (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  code TEXT NOT NULL UNIQUE              -- 'view','comment','edit','export','finalize','share','assign_workflow','ai_use','admin'
);

INSERT INTO kms.permission(code) VALUES
  ('view'), ('comment'), ('edit'), ('export'), ('finalize'),
  ('share'), ('assign_workflow'), ('ai_use'), ('admin')
ON CONFLICT DO NOTHING;

CREATE TABLE IF NOT EXISTS kms.role_permission (
  role_fk INT NOT NULL REFERENCES kms.roletb(uid) ON DELETE CASCADE,
  permission_fk INT NOT NULL REFERENCES kms.permission(uid) ON DELETE CASCADE,
  PRIMARY KEY (role_fk, permission_fk)
);

-- Role bindings attach roles to principals with an optional ABAC scope.
CREATE TABLE IF NOT EXISTS kms.role_binding (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  principal kms.principal_kind NOT NULL,
  user_fk INT REFERENCES kms.usertb(uid) ON DELETE CASCADE,
  group_fk INT REFERENCES kms.grouptb(uid) ON DELETE CASCADE,
  org_fk INT REFERENCES kms.organization(uid) ON DELETE CASCADE,
  role_fk INT NOT NULL REFERENCES kms.roletb(uid) ON DELETE CASCADE,
  scope kms.scope_kind NOT NULL DEFAULT 'tenant',
  scope_value TEXT,                       -- domaintb.code / bundle.code / document.uid
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT chk_rb_one_principal CHECK (
    (user_fk IS NOT NULL)::int + (group_fk IS NOT NULL)::int + (org_fk IS NOT NULL)::int = 1
  ),
  CONSTRAINT chk_rb_principal_match CHECK (
    (principal='user' AND user_fk IS NOT NULL) OR
    (principal='group' AND group_fk IS NOT NULL) OR
    (principal='org' AND org_fk IS NOT NULL)
  )
);
create unique index unique_role_binding on kms.role_binding(coalesce(user_fk,-1), coalesce(group_fk,-1), coalesce(org_fk,-1), role_fk, scope, scope_value);


-- -----------------------------
-- 4) Documents & versions
-- -----------------------------

-- noteTaking original def:
-- create table basicdoc (
--   uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY
--   , eid uuid not null DEFAULT uuid_generate_v4()
--   -- Basic title in a given locale; translations are stored in a support table.
--   , title varchar(255) not null
--   , locale varchar(8) not null
--   , created_at timestamptz not null default now()
-- );
--

CREATE TABLE IF NOT EXISTS kms.document (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY
  , eid uuid not null DEFAULT public.uuid_generate_v4()
  , code TEXT UNIQUE
  , title TEXT NOT NULL
  , domain_fk INT NOT NULL REFERENCES kms.domaintb(uid) ON DELETE RESTRICT
  , doc_type_fk INT NOT NULL REFERENCES kms.doc_type(uid) ON DELETE RESTRICT
  , tier_fk INT NOT NULL REFERENCES kms.tier(uid) ON DELETE RESTRICT
  , status_fk INT NOT NULL REFERENCES kms.statustb(uid) ON DELETE RESTRICT
  , owner_user_fk INT REFERENCES kms.usertb(uid) ON DELETE SET NULL
  , residency TEXT              -- 'EU','UAE','US' etc.
  , ai_allowed BOOLEAN NOT NULL DEFAULT TRUE
  , legal_hold BOOLEAN NOT NULL DEFAULT FALSE
  , due_date DATE
  , created_by_user_fk INT REFERENCES kms.usertb(uid) ON DELETE SET NULL
  , created_at TIMESTAMPTZ NOT NULL DEFAULT now()
  , updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
  , deleted_at TIMESTAMPTZ
);

CREATE TABLE IF NOT EXISTS kms.document_version (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  document_fk INT NOT NULL REFERENCES kms.document(uid) ON DELETE CASCADE,
  version_no INT NOT NULL,
  note TEXT,
  content_ref TEXT,               -- object storage pointer (node-level tree)
  content_text TEXT,               -- denormalized text for FTS / preview
  content_sha256 TEXT,
  created_by_user_fk INT REFERENCES kms.usertb(uid) ON DELETE SET NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  is_snapshot BOOLEAN NOT NULL DEFAULT TRUE,
  UNIQUE (document_fk, version_no)
);

CREATE OR REPLACE VIEW kms.v_document_latest AS
SELECT DISTINCT ON (dv.document_fk)
  d.uid AS document_uid,
  d.title, d.domain_fk, d.doc_type_fk, d.tier_fk, d.status_fk,
  d.owner_user_fk, d.residency, d.ai_allowed, d.legal_hold, d.due_date,
  d.created_at, d.updated_at,
  dv.uid AS document_version_uid,
  dv.version_no, dv.note, dv.content_ref, dv.content_text, dv.content_sha256, dv.created_at AS version_created_at
FROM kms.document d
JOIN kms.document_version dv ON dv.document_fk = d.uid
WHERE d.deleted_at IS NULL
ORDER BY dv.document_fk, dv.version_no DESC;

ALTER TABLE kms.document_version ALTER COLUMN content_text SET STORAGE EXTENDED;
CREATE INDEX IF NOT EXISTS idx_docver_content_fts ON kms.document_version USING GIN (to_tsvector('english', coalesce(content_text,'')));

-- Tags
CREATE TABLE IF NOT EXISTS kms.tagtb (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  name TEXT NOT NULL UNIQUE
);

CREATE TABLE IF NOT EXISTS kms.document_tag (
  document_fk INT NOT NULL REFERENCES kms.document(uid) ON DELETE CASCADE,
  tag_fk INT NOT NULL REFERENCES kms.tagtb(uid) ON DELETE CASCADE,
  PRIMARY KEY (document_fk, tag_fk)
);

-- Relations (canonical ordering)
CREATE TABLE IF NOT EXISTS kms.document_relation (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  document_a_fk INT NOT NULL REFERENCES kms.document(uid) ON DELETE CASCADE,
  document_b_fk INT NOT NULL REFERENCES kms.document(uid) ON DELETE CASCADE,
  relation_type TEXT NOT NULL DEFAULT 'related',  -- 'related','refers_to','derived_from','supersedes','cites'
  directed BOOLEAN NOT NULL DEFAULT FALSE,
  created_by_user_fk INT REFERENCES kms.usertb(uid) ON DELETE SET NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT chk_rel_canonical CHECK (document_a_fk <> document_b_fk)
  -- Implemented as unique index:
  -- CONSTRAINT uq_rel UNIQUE (LEAST(tdocument_a_fk, document_b_fk), GREATEST(tdocument_a_fk, document_b_fk), relation_type)
);
create unique index unique_document_relation on kms.document_relation (LEAST(document_a_fk, document_b_fk), GREATEST(document_a_fk, document_b_fk), relation_type);

-- Comments
CREATE TABLE IF NOT EXISTS kms.commenttb (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  document_fk INT NOT NULL REFERENCES kms.document(uid) ON DELETE CASCADE,
  parent_comment_fk INT REFERENCES kms.commenttb(uid) ON DELETE CASCADE,
  author_user_fk INT REFERENCES kms.usertb(uid) ON DELETE SET NULL,
  body TEXT NOT NULL,
  resolved BOOLEAN NOT NULL DEFAULT FALSE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- Attachments
CREATE TABLE IF NOT EXISTS kms.attachment (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  document_fk INT NOT NULL REFERENCES kms.document(uid) ON DELETE CASCADE,
  filename TEXT NOT NULL,
  content_type TEXT,
  size_bytes BIGINT,
  storage_ref TEXT NOT NULL,
  sha256 TEXT,
  uploaded_by_user_fk INT REFERENCES kms.usertb(uid) ON DELETE SET NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- Document-level ACLs
CREATE TABLE IF NOT EXISTS kms.document_acl (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  document_fk INT NOT NULL REFERENCES kms.document(uid) ON DELETE CASCADE,
  principal kms.principal_kind NOT NULL,
  user_fk INT REFERENCES kms.usertb(uid) ON DELETE CASCADE,
  group_fk INT REFERENCES kms.grouptb(uid) ON DELETE CASCADE,
  role_fk INT REFERENCES kms.roletb(uid) ON DELETE CASCADE,
  org_fk INT REFERENCES kms.organization(uid) ON DELETE CASCADE,
  rights TEXT[] NOT NULL,            -- subset of permission.code
  scope kms.scope_kind,                 -- NULL | 'domaintb' | 'bundle' | 'resource'
  scope_value TEXT,
  created_by_user_fk INT REFERENCES kms.usertb(uid) ON DELETE SET NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT chk_acl_one_principal CHECK (
    (user_fk IS NOT NULL)::int + (group_fk IS NOT NULL)::int + (role_fk IS NOT NULL)::int + (org_fk IS NOT NULL)::int = 1
  ),
  CONSTRAINT chk_acl_principal_match CHECK (
    (principal='user' AND user_fk IS NOT NULL) OR
    (principal='group' AND group_fk IS NOT NULL) OR
    (principal='role' AND role_fk IS NOT NULL) OR
    (principal='org' AND org_fk IS NOT NULL)
  )
);
CREATE INDEX IF NOT EXISTS idx_docacl_doc ON kms.document_acl(document_fk);
CREATE INDEX IF NOT EXISTS idx_docacl_user ON kms.document_acl(user_fk);
CREATE INDEX IF NOT EXISTS idx_docacl_group ON kms.document_acl(group_fk);
CREATE INDEX IF NOT EXISTS idx_docacl_role ON kms.document_acl(role_fk);
CREATE INDEX IF NOT EXISTS idx_docacl_org ON kms.document_acl(org_fk);

-- -----------------------------
-- 5) Workflow
-- -----------------------------
CREATE TABLE IF NOT EXISTS kms.workflow_template (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  code TEXT NOT NULL UNIQUE,
  name TEXT NOT NULL,
  steps JSONB NOT NULL,        -- [{name:'Internal Draft'}, {name:'Client Review'}, ...]
  active BOOLEAN NOT NULL DEFAULT TRUE,
  created_by_user_fk INT REFERENCES kms.usertb(uid) ON DELETE SET NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS kms.workflow_instance (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  document_fk INT NOT NULL REFERENCES kms.document(uid) ON DELETE CASCADE,
  workflow_template_fk INT REFERENCES kms.workflow_template(uid) ON DELETE SET NULL,
  name TEXT NOT NULL,
  current_step INT NOT NULL DEFAULT 0,
  state TEXT NOT NULL DEFAULT 'active' CHECK (state IN ('active','completed','cancelled','blocked')),
  created_by_user_fk INT REFERENCES kms.usertb(uid) ON DELETE SET NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
CREATE UNIQUE INDEX IF NOT EXISTS uq_wf_doc_active ON kms.workflow_instance(document_fk) WHERE state='active';

CREATE TABLE IF NOT EXISTS kms.workflow_step (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  workflow_instance_fk INT NOT NULL REFERENCES kms.workflow_instance(uid) ON DELETE CASCADE,
  step_index INT NOT NULL,
  name TEXT NOT NULL,
  state TEXT NOT NULL DEFAULT 'todo' CHECK (state IN ('todo','in_progress','done','blocked')),
  assigned_user_fk INT REFERENCES kms.usertb(uid) ON DELETE SET NULL,
  assigned_group_fk INT REFERENCES kms.grouptb(uid) ON DELETE SET NULL,
  started_at TIMESTAMPTZ,
  done_at TIMESTAMPTZ,
  UNIQUE (workflow_instance_fk, step_index)
);

-- -----------------------------
-- 6) Bundles (Regulator/Auditor packages)
-- -----------------------------
CREATE TABLE IF NOT EXISTS kms.bundle (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  code TEXT NOT NULL UNIQUE,     -- e.g., 'CSSF-2025-Q4'
  name TEXT NOT NULL,
  purpose TEXT NOT NULL CHECK (purpose IN ('regulator','auditor','other')),
  created_by_user_fk INT REFERENCES kms.usertb(uid) ON DELETE SET NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS kms.bundle_item (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  bundle_fk INT NOT NULL REFERENCES kms.bundle(uid) ON DELETE CASCADE,
  document_fk INT NOT NULL REFERENCES kms.document(uid) ON DELETE CASCADE,
  document_version_fk INT NOT NULL REFERENCES kms.document_version(uid) ON DELETE RESTRICT,
  fingerprint_sha256 TEXT NOT NULL,
  serialized_uri TEXT,
  added_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (bundle_fk, document_fk)
);

-- -----------------------------
-- 7) Policies (export/import/AI/residency)
-- -----------------------------
CREATE TABLE IF NOT EXISTS kms.policy_rule (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  domain_fk INT REFERENCES kms.domaintb(uid) ON DELETE CASCADE,
  tier_fk INT REFERENCES kms.tier(uid) ON DELETE CASCADE,
  status_fk INT REFERENCES kms.statustb(uid) ON DELETE CASCADE,
  residency TEXT,                          -- 'EU','UAE', etc.
  block_export BOOLEAN NOT NULL DEFAULT FALSE,
  block_import BOOLEAN NOT NULL DEFAULT FALSE,
  view_only BOOLEAN NOT NULL DEFAULT FALSE,
  ai_default BOOLEAN,                       -- NULL = inherit
  priority INT NOT NULL DEFAULT 0,
  created_by_user_fk INT REFERENCES kms.usertb(uid) ON DELETE SET NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
CREATE INDEX IF NOT EXISTS idx_policy_scope ON kms.policy_rule(domain_fk, tier_fk, status_fk, residency, priority DESC);

-- -----------------------------
-- 8) Audit / Security events
-- -----------------------------
CREATE TABLE IF NOT EXISTS kms.audit_event (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  ts TIMESTAMPTZ NOT NULL DEFAULT now(),
  actor_user_fk INT REFERENCES kms.usertb(uid) ON DELETE SET NULL,
  actor_org_fk INT REFERENCES kms.organization(uid) ON DELETE SET NULL,
  action_code TEXT NOT NULL,            -- 'ui.view:sankey','doc.create','acl.add','export.md'
  document_fk INT REFERENCES kms.document(uid) ON DELETE SET NULL,
  target_type TEXT,                     -- 'document','comment','workflow','bundle','policy','attachment','auth'
  target_uid INT,                      -- polymorphic target id
  ip INET,
  user_agent TEXT,
  meta JSONB
);
CREATE INDEX IF NOT EXISTS idx_audit_doc ON kms.audit_event(document_fk, ts DESC);
CREATE INDEX IF NOT EXISTS idx_audit_actor ON kms.audit_event(actor_user_fk, ts DESC);

-- -----------------------------
-- 9) Convenience views for UI
-- -----------------------------
CREATE OR REPLACE VIEW kms.v_counts_domain_status AS
SELECT d.domain_fk, d.status_fk, COUNT(*) AS cnt
FROM kms.document d
WHERE d.deleted_at IS NULL
GROUP BY d.domain_fk, d.status_fk;

CREATE OR REPLACE VIEW kms.v_counts_tier AS
SELECT d.tier_fk, COUNT(*) AS cnt
FROM kms.document d
WHERE d.deleted_at IS NULL
GROUP BY d.tier_fk;

CREATE OR REPLACE VIEW kms.v_constellation AS
SELECT d.uid AS document_uid,
       d.title,
       d.domain_fk,
       d.doc_type_fk,
       d.tier_fk,
       d.status_fk,
       s.order_index AS lifecycle_index
FROM kms.document d
JOIN kms.statustb s ON s.uid = d.status_fk
WHERE d.deleted_at IS NULL;

-- -----------------------------
-- 10) Permission check function (baseline)
-- -----------------------------
CREATE OR REPLACE FUNCTION kms.can_user(p_user INT, p_perm TEXT, p_document INT)
RETURNS BOOLEAN
LANGUAGE plpgsql
AS $$
DECLARE
  v_dom INT; v_tier INT; v_dom_code TEXT; v_tier_code TEXT;
  v_ok BOOLEAN := FALSE;
BEGIN
  -- Resolve document context (domain/tier codes are used for scoped checks)
  SELECT d.domain_fk, d.tier_fk, dm.code, tr.code
  INTO v_dom, v_tier, v_dom_code, v_tier_code
  FROM kms.document d
  JOIN kms.domaintb dm ON dm.uid = d.domain_fk
  JOIN kms.tier     tr ON tr.uid  = d.tier_fk
  WHERE d.uid = p_document;
  IF NOT FOUND THEN
    RETURN FALSE;
  END IF;

  -- Effective role-bindings for the user (direct, via group, via org)
  WITH u_org AS (
    SELECT org_fk FROM kms.usertb WHERE uid = p_user
  ), u_groups AS (
    SELECT group_fk FROM kms.group_member WHERE user_fk = p_user
  ), eff_bind AS (
    SELECT rb.*
    FROM kms.role_binding rb
    WHERE
      (rb.principal = 'user'  AND rb.user_fk  = p_user) OR
      (rb.principal = 'group' AND rb.group_fk IN (SELECT group_fk FROM u_groups)) OR
      (rb.principal = 'org'   AND rb.org_fk   = (SELECT org_fk FROM u_org))
  )
  -- 1) Document ACL direct grants (including principal='role' using eff_bind)
  SELECT TRUE INTO v_ok
  FROM kms.document_acl a
  WHERE a.document_fk = p_document
    AND p_perm = ANY(a.rights)
    AND (
      (a.principal='user'  AND a.user_fk  = p_user) OR
      (a.principal='group' AND a.group_fk IN (SELECT group_fk FROM kms.group_member WHERE user_fk = p_user)) OR
      (a.principal='org'   AND a.org_fk   = (SELECT org_fk FROM kms.usertb WHERE uid = p_user)) OR
      (a.principal='role'  AND a.role_fk  IN (SELECT role_fk FROM eff_bind
                                              WHERE eff_bind.scope = 'tenant'
                                                 OR (eff_bind.scope='domaintb' AND eff_bind.scope_value = v_dom_code)
                                                 OR (eff_bind.scope='resource' AND eff_bind.scope_value::int = p_document)))
    )
    AND (
      a.scope IS NULL
      OR (a.scope='domaintb' AND a.scope_value = v_dom_code)
      OR (a.scope='resource' AND a.scope_value::int = p_document)
    )
  LIMIT 1;

  IF v_ok THEN
    RETURN TRUE;
  END IF;

  -- 2) Role-based grants (role -> permission), using eff_bind + scope
  WITH eff_bind AS (
    SELECT rb.*
    FROM kms.role_binding rb
    WHERE
      (rb.principal='user'  AND rb.user_fk  = p_user) OR
      (rb.principal='group' AND rb.group_fk IN (SELECT group_fk FROM kms.group_member WHERE user_fk = p_user)) OR
      (rb.principal='org'   AND rb.org_fk   = (SELECT org_fk FROM kms.usertb WHERE uid = p_user))
  )
  SELECT TRUE INTO v_ok
  FROM eff_bind rb
  JOIN kms.role_permission rp ON rp.role_fk = rb.role_fk
  JOIN kms.permission      p  ON p.uid = rp.permission_fk AND p.code = p_perm
  WHERE
    rb.scope = 'tenant'
    OR (rb.scope='domaintb' AND rb.scope_value = v_dom_code)
    OR (rb.scope='resource' AND rb.scope_value::int = p_document)
  LIMIT 1;

  RETURN COALESCE(v_ok, FALSE);
END;
$$;

-- -----------------------------
-- 11) Indexing & triggers
-- -----------------------------
CREATE INDEX IF NOT EXISTS idx_doc_title_trgm ON kms.document USING GIN (title gin_trgm_ops);
CREATE INDEX IF NOT EXISTS idx_doc_status ON kms.document(status_fk);
CREATE INDEX IF NOT EXISTS idx_doc_domaintb ON kms.document(domain_fk);
CREATE INDEX IF NOT EXISTS idx_doc_tier ON kms.document(tier_fk);
CREATE INDEX IF NOT EXISTS idx_doc_due ON kms.document(due_date);

CREATE OR REPLACE FUNCTION kms.f_set_updated_at() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN NEW.updated_at = now(); RETURN NEW; END; $$;
DROP TRIGGER IF EXISTS trg_document_mtime ON kms.document;
CREATE TRIGGER trg_document_mtime BEFORE UPDATE ON kms.document FOR EACH ROW EXECUTE FUNCTION kms.f_set_updated_at();

-- -----------------------------
-- 12) RLS scaffolding (optional)
-- -----------------------------
-- ALTER TABLE document ENABLE ROW LEVEL SECURITY;
-- CREATE POLICY doc_policy_read ON document FOR SELECT USING (can_user(current_setting('app.user_uid', true)::int, 'view', uid));
-- CREATE POLICY doc_policy_write ON document FOR UPDATE USING (can_user(current_setting('app.user_uid', true)::int, 'edit', uid));

-- -----------------------------
-- 13) Seed catalogs
-- -----------------------------
INSERT INTO kms.domaintb(code,name) VALUES
  ('Governance','Governance'),('Regulatory','Regulatory')
  ,('Tax','Tax'),('Compliance','Compliance')
  ,('Structuring','Structuring'),('Capital Markets','Capital Markets')
ON CONFLICT (code) DO NOTHING;

INSERT INTO kms.doc_type(code,name) VALUES
  ('Memo','Memo'),('Policy','Policy')
  ,('Template','Template'),('Agreement','Agreement')
  ,('Register','Register'),('Procedure','Procedure')
ON CONFLICT (code) DO NOTHING;

INSERT INTO kms.tier(code,name) VALUES
  ('Public','Public'), ('Internal','Internal')
  ,('Restricted','Restricted'), ('Secret','Secret')
ON CONFLICT (code) DO NOTHING;

INSERT INTO kms.statustb(code,name,order_index) VALUES
    ('Idea','Idea',0)
  , ('Draft','Draft',1)
  , ('Review','Review',2)
  , ('Counsel','Counsel',3)
  , ('Approved','Approved',4)
  , ('Final','Final',5)
ON CONFLICT (code) DO NOTHING;



---- **** BEGIN: Pitcher.noteTaking.sql **** ----


CREATE SEQUENCE IF NOT EXISTS kms.document_version_seq
  START WITH 1 INCREMENT BY 1;

drop table if exists kms.blocks_bd;
CREATE TABLE kms.blocks_bd (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY
  , eid uuid not null DEFAULT public.uuid_generate_v4()
  , doc_fk int references kms.document(uid) on delete cascade
  , parent_fk INT NULL REFERENCES kms.blocks_bd(uid) ON DELETE CASCADE
  , kind text
  , attrs jsonb default '{}'::jsonb
  , content TEXT NOT NULL
  , itype varchar(255)
  , asset_fk uuid
  , format jsonb
  , seq_pos NUMERIC(38,10) NOT NULL
  , valid_from_seq BIGINT NOT NULL DEFAULT nextval('kms.document_version_seq')
  , valid_to_seq BIGINT NULL
  , created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
CREATE INDEX ON kms.blocks_bd(doc_fk, parent_fk, seq_pos, valid_to_seq);

-- Map semantic labels to sequence/time
CREATE TABLE kms.version_labels (
  document_fk INT NOT NULL REFERENCES kms.document(uid),
  label TEXT NOT NULL,
  at_seq BIGINT NOT NULL,
  at_time TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY(document_fk, label)
);

-- --------------------------------------------------
-- 3. Change-event audit (undo stack)
-- --------------------------------------------------

CREATE TABLE kms.change_events (
  uid BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY
  , document_fk INT NOT NULL REFERENCES kms.document(uid)
  , block_id INT NOT NULL
  , event_type TEXT NOT NULL
  , event_data JSONB NOT NULL
  , occurred_at TIMESTAMPTZ NOT NULL DEFAULT now()
  , undone BOOLEAN NOT NULL DEFAULT FALSE
  -- Added to support user management in the document system.
  , actor_fk int
);
CREATE INDEX ON kms.change_events(document_fk, occurred_at DESC) WHERE NOT undone;


CREATE TYPE kms.block_insert_result AS (
  block_id int,
  block_eid uuid
);

-- Update the seq_pos values to increase the gaps between the blocks for further insertions.
CREATE OR REPLACE FUNCTION kms.resequence_blocks(
  p_document_id INT,
  p_parent_block INT DEFAULT NULL
) RETURNS VOID LANGUAGE plpgsql AS $$
DECLARE
  r RECORD;
  idx BIGINT := 0;
  step NUMERIC := 10000;
BEGIN
  FOR r IN
    SELECT uid
    FROM kms.blocks_bd
     WHERE doc_fk = p_document_id
       AND parent_fk IS NOT DISTINCT FROM p_parent_block
     ORDER BY seq_pos
  LOOP
    idx := idx + 1;
    UPDATE kms.blocks_bd
      SET seq_pos = idx * step
     WHERE uid = r.uid;
  END LOOP;
END;
$$;


CREATE OR REPLACE FUNCTION kms.insert_block_after(
  p_document_id INT
  , p_content TEXT
  , p_parent_block INT DEFAULT NULL
  , p_after_block INT DEFAULT NULL
) RETURNS kms.block_insert_result LANGUAGE plpgsql AS $$
DECLARE
  left_key NUMERIC;
  right_key NUMERIC;
  new_key NUMERIC;
  new_id INT;
  new_eid UUID;
  seqval BIGINT := nextval('kms.document_version_seq');
BEGIN
  -- compute left_key/right_key among siblings
  IF p_after_block IS NULL THEN
    left_key := 0;
    SELECT seq_pos INTO right_key
      FROM kms.blocks_bd
     WHERE doc_fk = p_document_id AND parent_fk IS NOT DISTINCT FROM p_parent_block
     ORDER BY seq_pos LIMIT 1;
  ELSE
    SELECT seq_pos, parent_fk INTO left_key, p_parent_block
      FROM kms.blocks_bd WHERE uid = p_after_block;
    SELECT seq_pos INTO right_key
      FROM kms.blocks_bd
     WHERE doc_fk = p_document_id
       AND parent_fk is not distinct from p_parent_block
       AND seq_pos > left_key
     ORDER BY seq_pos LIMIT 1;
  END IF;
  IF right_key IS NULL THEN right_key := left_key + 5000; END IF;
  new_key := (left_key + right_key)/2;
  -- allocate new row (COW)
  INSERT INTO kms.blocks_bd(uid, eid, doc_fk, parent_fk, content, seq_pos, valid_from_seq)
  VALUES (DEFAULT, DEFAULT, p_document_id, p_parent_block, p_content, new_key, seqval)
  RETURNING uid,eid INTO new_id,new_eid;
  -- retire any previous version of this id? none, new id
  -- log event
  INSERT INTO kms.change_events(document_fk, block_id, event_type, event_data)
  VALUES (
    p_document_id,
    new_id,
    'insert',
    jsonb_build_object('new', jsonb_build_object(
      'parent', p_parent_block,
      'order', new_key,
      'content', p_content
    ))
  );
  RETURN (new_id, new_eid);
END;
$$;


CREATE OR REPLACE FUNCTION kms.insert_block_before(
  p_document_id INT
  , p_content TEXT
  , p_parent_block INT DEFAULT NULL
  , p_before_block INT DEFAULT NULL
) RETURNS kms.block_insert_result LANGUAGE plpgsql AS $$
DECLARE
  left_key NUMERIC;
  right_key NUMERIC;
  new_key NUMERIC;
  new_id INT;
  new_eid UUID;
  seqval BIGINT := nextval('kms.document_version_seq');
BEGIN
  IF p_before_block IS NULL THEN
    SELECT seq_pos INTO left_key
      FROM kms.blocks_bd
     WHERE doc_fk = p_document_id AND parent_fk IS NOT DISTINCT FROM p_parent_block
     ORDER BY seq_pos DESC LIMIT 1;
    IF left_key IS NULL THEN left_key := 0; END IF;
    right_key := left_key + 5000;
  ELSE
    SELECT seq_pos, parent_fk INTO right_key, p_parent_block
      FROM kms.blocks_bd WHERE uid = p_before_block;
    SELECT seq_pos INTO left_key
      FROM kms.blocks_bd
     WHERE doc_fk = p_document_id
       AND parent_fk is not distinct from p_parent_block
       AND seq_pos < right_key
     ORDER BY seq_pos DESC LIMIT 1;
    IF left_key IS NULL THEN left_key := 0; END IF;
  END IF;

  new_key := (left_key + right_key)/2;

  INSERT INTO kms.blocks_bd(uid,eid,doc_fk,parent_fk,content,seq_pos,valid_from_seq)
  VALUES (DEFAULT, DEFAULT, p_document_id,p_parent_block,p_content,new_key,seqval)
  RETURNING uid,eid INTO new_id,new_eid;
  INSERT INTO kms.change_events(document_fk, block_id, event_type, event_data)
    VALUES (p_document_id
      , new_id
      , 'insert'
      , jsonb_build_object('new',jsonb_build_object('parent',p_parent_block,'order',new_key,'content',p_content))
    );
  RETURN (new_id,new_eid);
END;
$$;


CREATE OR REPLACE FUNCTION kms.move_block(
  p_block_id INT,
  p_parent_block INT DEFAULT NULL,
  p_after_block INT DEFAULT NULL,
  p_before_block INT DEFAULT NULL
) RETURNS VOID LANGUAGE plpgsql AS $$
DECLARE
  seqval BIGINT := nextval('kms.document_version_seq');
  rec RECORD;
  old_parent INT;
  old_order NUMERIC;
  new_parent INT := p_parent_block;
  left_key NUMERIC;
  right_key NUMERIC;
  new_key NUMERIC;
BEGIN
  -- fetch old
  SELECT doc_fk,parent_fk,seq_pos INTO rec.document_fk, old_parent ,old_order
    FROM kms.blocks_bd
    WHERE uid = p_block_id AND valid_to_seq IS NULL;
  -- retire
  UPDATE kms.blocks_bd
    SET valid_to_seq = seqval
    WHERE uid = p_block_id AND valid_to_seq IS NULL;
  -- determine new_parent from anchor
  IF p_after_block IS NOT NULL THEN
    SELECT parent_fk INTO new_parent FROM kms.blocks_bd WHERE uid = p_after_block;
  ELSIF p_before_block IS NOT NULL THEN
    SELECT parent_fk INTO new_parent FROM kms.blocks_bd WHERE uid = p_before_block;
  END IF;
  -- compute neighbors under (rec.document_fk,new_parent)
  -- [reuse logic from insert]
  IF p_after_block IS NOT NULL THEN
    SELECT seq_pos INTO left_key FROM kms.blocks_bd WHERE uid = p_after_block;
    SELECT seq_pos INTO right_key FROM kms.blocks_bd
      WHERE doc_fk = rec.document_fk
        AND parent_fk is not distinct from new_parent
        AND seq_pos > left_key ORDER BY seq_pos LIMIT 1;
  ELSIF p_before_block IS NOT NULL THEN
    SELECT seq_pos INTO right_key FROM kms.blocks_bd WHERE uid = p_before_block;
    SELECT seq_pos INTO left_key FROM kms.blocks_bd
      WHERE doc_fk = rec.document_fk
        AND parent_fk is not distinct from new_parent
        AND seq_pos < right_key ORDER BY seq_pos DESC LIMIT 1;
  ELSE
    left_key := 0;
    SELECT seq_pos INTO right_key FROM kms.blocks_bd
      WHERE doc_fk = rec.document_fk
        AND parent_fk IS NOT DISTINCT FROM new_parent
      ORDER BY seq_pos LIMIT 1;
  END IF;
  IF right_key IS NULL THEN right_key := left_key + 5000; END IF;
  new_key := (left_key + right_key)/2;
  -- insert new version
  INSERT INTO kms.blocks_bd(uid,eid,doc_fk,parent_fk,content,seq_pos,valid_from_seq)
    SELECT id,eid,doc_fk,new_parent,content,new_key,seqval
      FROM kms.blocks_bd_old_snap  -- snapshot of old current row
     WHERE uid = p_block_id;
  -- log
  INSERT INTO kms.change_events(document_fk,block_id,event_type,event_data)
    VALUES (
      rec.document_fk
      , p_block_id
      , 'move'
      , jsonb_build_object('old', jsonb_build_object('parent', old_parent, 'order', old_order), 'new', jsonb_build_object('parent', new_parent, 'order', new_key))
    );
END;
$$;

-- --------------------------------------------------
-- 7. Delete
-- --------------------------------------------------

CREATE OR REPLACE FUNCTION kms.delete_block(p_block_id INT) RETURNS VOID LANGUAGE plpgsql AS $$
DECLARE rec RECORD; seqval BIGINT := nextval('kms.document_version_seq');
BEGIN
  SELECT doc_fk,parent_fk,seq_pos,content INTO rec
    FROM kms.blocks_bd
    WHERE uid = p_block_id AND valid_to_seq IS NULL;
  UPDATE kms.blocks_bd
    SET valid_to_seq = seqval
    WHERE uid = p_block_id AND valid_to_seq IS NULL;
  INSERT INTO kms.change_events(document_fk, block_id, event_type, event_data)
    VALUES (
      rec.doc_fk
      , p_block_id
      , 'delete'
      , jsonb_build_object('old', jsonb_build_object('parent', rec.parent_fk, 'order', rec.seq_pos, 'content', rec.content))
    );
END;
$$;


-- --------------------------------------------------
-- 8. Version labeling
-- --------------------------------------------------
CREATE OR REPLACE FUNCTION kms.stamp_version_label(p_document_id INT,p_label TEXT) RETURNS VOID LANGUAGE plpgsql AS $$
DECLARE v_seq BIGINT := currval('kms.document_version_seq');
BEGIN
  INSERT INTO kms.version_labels(document_fk,label,at_seq,at_time)
    VALUES(p_document_id,p_label,v_seq,now());
END;
$$;


-- --------------------------------------------------
-- 9. DFS at sequence (pagination + has_more)
-- --------------------------------------------------
CREATE OR REPLACE FUNCTION kms.get_blocks_dfs_at_seq(
  p_document_id INT
  , p_as_of_seq BIGINT
  , p_max_depth INT DEFAULT NULL
  , p_offset BIGINT DEFAULT 0
  , p_limit BIGINT DEFAULT NULL
) RETURNS TABLE(
  depth INT,
  block_id INT,
  parent_block_id INT,
  content TEXT,
  seq_pos NUMERIC(38,10),
  has_more BOOLEAN
) LANGUAGE sql AS $$
WITH RECURSIVE tree AS (
  SELECT
    1 AS depth, b.uid AS block_id, b.parent_fk AS parent_block_id,
    b.content, b.seq_pos,
    (p_max_depth IS NOT NULL
      AND 1 = p_max_depth
      AND EXISTS (
        SELECT 1 FROM kms.blocks_bd c
        WHERE c.parent_fk = b.uid
          AND c.valid_from_seq <= p_as_of_seq
          AND (c.valid_to_seq IS NULL OR c.valid_to_seq > p_as_of_seq)
      )
    ) AS has_more,
    ARRAY[b.seq_pos] AS path
  FROM kms.blocks_bd b
  WHERE b.doc_fk = p_document_id AND b.parent_fk IS NULL
    AND b.valid_from_seq <= p_as_of_seq AND (b.valid_to_seq IS NULL OR b.valid_to_seq > p_as_of_seq)

  UNION ALL

  SELECT
    t.depth+1, c.uid, c.parent_fk, c.content, c.seq_pos,
    (p_max_depth IS NOT NULL AND t.depth+1 = p_max_depth
      AND EXISTS (SELECT 1 FROM kms.blocks_bd x WHERE x.parent_fk = c.uid AND x.valid_from_seq <= p_as_of_seq AND (x.valid_to_seq IS NULL OR x.valid_to_seq > p_as_of_seq))) AS has_more,
    (t.path || c.seq_pos)::numeric(38,10)[] AS path
  FROM tree t
  JOIN kms.blocks_bd c ON c.parent_fk = t.block_id
    AND c.doc_fk = p_document_id
    AND c.valid_from_seq <= p_as_of_seq AND (c.valid_to_seq IS NULL OR c.valid_to_seq > p_as_of_seq)
  WHERE p_max_depth IS NULL OR t.depth < p_max_depth
)
SELECT depth,block_id,parent_block_id,content,seq_pos,has_more
FROM tree ORDER BY path
  OFFSET GREATEST(p_offset,0)
  LIMIT CASE WHEN p_limit IS NULL OR p_limit<0 THEN NULL ELSE p_limit END;
$$;


-- --------------------------------------------------
-- 10. Subtree DFS at sequence
-- --------------------------------------------------
CREATE OR REPLACE FUNCTION kms.get_subtree_dfs_at_seq(
  p_start_block_id INT,
  p_as_of_seq BIGINT,
  p_max_depth INT DEFAULT NULL,
  p_offset BIGINT DEFAULT 0,
  p_limit BIGINT DEFAULT NULL
) RETURNS TABLE(
  depth INT,
  block_id INT,
  parent_block_id INT,
  content TEXT,
  seq_pos NUMERIC(38,10),
  has_more BOOLEAN
) LANGUAGE sql AS $$
WITH RECURSIVE tree AS (
  SELECT
    1 AS depth, b.uid as b_id, b.parent_fk as pb_id, b.content, b.seq_pos,
    (p_max_depth IS NOT NULL AND 1 = p_max_depth
      AND EXISTS (
        SELECT 1 FROM kms.blocks_bd c
        WHERE c.parent_fk = b.uid
          AND c.valid_from_seq <= p_as_of_seq
          AND (c.valid_to_seq IS NULL OR c.valid_to_seq > p_as_of_seq)
      )
    ) AS has_more,
    ARRAY[b.seq_pos] AS path
  FROM kms.blocks_bd b
  WHERE b.parent_fk = p_start_block_id
    AND b.valid_from_seq <= p_as_of_seq AND (b.valid_to_seq IS NULL OR b.valid_to_seq > p_as_of_seq)

  UNION ALL

  SELECT
    t.depth+1, c.uid as b_id, c.parent_fk as pb_id, c.content, c.seq_pos,
    (p_max_depth IS NOT NULL AND t.depth+1 = p_max_depth
      AND EXISTS (
        SELECT 1 FROM kms.blocks_bd x WHERE x.parent_fk = c.uid
          AND x.valid_from_seq <= p_as_of_seq
          AND (x.valid_to_seq IS NULL OR x.valid_to_seq > p_as_of_seq)
      )
    ) AS has_more,
    (t.path || c.seq_pos)::numeric(38,10)[] AS path
  FROM tree t
  JOIN kms.blocks_bd c ON c.parent_fk = t.b_id
    AND c.valid_from_seq <= p_as_of_seq AND (c.valid_to_seq IS NULL OR c.valid_to_seq > p_as_of_seq)
  WHERE p_max_depth IS NULL OR t.depth< p_max_depth
)
SELECT depth,b_id,pb_id,content,seq_pos,has_more
FROM tree ORDER BY path
  OFFSET GREATEST(p_offset,0)
  LIMIT CASE WHEN p_limit IS NULL OR p_limit<0 THEN NULL ELSE p_limit END;
$$;


CREATE OR REPLACE FUNCTION kms.document_blocks_as_of_seq(
  p_document_id INT,
  p_as_of_seq BIGINT
)
RETURNS TABLE (
  block_id INT,
  document_id INT,
  parent_block_id INT,
  seq_pos NUMERIC(38,10),
  content TEXT
) LANGUAGE sql STABLE AS $$
  SELECT uid, doc_fk, parent_fk, seq_pos, content
    FROM kms.blocks_bd
   WHERE doc_fk = p_document_id
     AND valid_from_seq <= p_as_of_seq
     AND (valid_to_seq IS NULL OR valid_to_seq > p_as_of_seq)
   ORDER BY parent_fk NULLS FIRST, seq_pos;
$$;

---- END: Pitcher.noteTaking.sql ---


-- KMS Block Import — SQL Patch v4
-- Aligns the DOCX importer with the SQL hierarchical block model (blocks_bd).
-- Safe to apply idempotently (uses IF NOT EXISTS where possible).

-- 0) Optional: block kind catalog (if you already have one, skip)
create table if not exists kms.block_kind (
  code text primary key
);
insert into kms.block_kind(code)
  values
    ('heading'),('paragraph'),('list_item'),('table'),('table_row'),('table_cell'),
    ('image'),('footnote_ref'),('endnote_ref')
  on conflict do nothing;

-- 1) Link blocks to attachments (e.g., images)
create table if not exists kms.block_asset (
  block_fk int not null references kms.blocks_bd(uid) on delete cascade,
  attachment_fk int not null references kms.attachment(uid) on delete cascade,
  constraint pk_block_asset primary key (block_fk)
);

-- 2) Ensure blocks_bd has metadata columns (attrs, kind)
--    If your schema already has equivalents, adjust the importer to use them.

-- 3) Convenience: ensure a document root block exists
create or replace function kms.ensure_doc_root(p_doc int, p_actor int)
returns int language plpgsql as $$
declare v_root int;
begin
  select uid into v_root
  from kms.blocks_bd
  where doc_fk = p_doc and parent_fk is null and valid_to_seq is null
  order by seq_pos asc
  limit 1;

  if v_root is null then
    perform nextval('kms.document_version_seq');
    insert into kms.blocks_bd(doc_fk, parent_fk, seq_pos, kind, content, attrs, valid_from_seq)
    values (p_doc, null, 1000000, 'document', 'TEST DOC', '{}'::jsonb, currval('kms.document_version_seq'))
    returning uid into v_root;
    insert into kms.change_events(document_fk, block_id, event_type, event_data, actor_fk)
    values (p_doc, v_root, 'insert', jsonb_build_object('version', currval('kms.document_version_seq'), 'kind','document'), p_actor);
  end if;
  return v_root;
end $$;

-- 4) Append-at-end helper (fractional ordering friendly)
--    Uses +1000 spacing; call resequence_blocks() later if needed.
create or replace function kms.append_child_import(
  p_doc int,
  p_parent int,
  p_kind text,
  p_text text,
  p_attrs jsonb,
  p_actor int
) returns int language plpgsql as $$
declare v_pos numeric; v_seq bigint; v_uid int;
begin
  select coalesce(max(seq_pos),0) + 1000 into v_pos
  from kms.blocks_bd
  where doc_fk=p_doc and ((parent_fk is null and p_parent is null) or parent_fk = p_parent)
    and valid_to_seq is null;

  v_seq := nextval('kms.document_version_seq');

  insert into kms.blocks_bd(doc_fk, parent_fk, seq_pos, kind, content, attrs, valid_from_seq)
  values (p_doc, p_parent, v_pos, p_kind, p_text, coalesce(p_attrs,'{}'::jsonb), v_seq)
  returning uid into v_uid;

  insert into kms.change_events(document_fk, block_id, event_type, event_data, actor_fk)
  values (p_doc, v_uid, 'insert', jsonb_build_object('sequence', v_seq, 'parent_fk', p_parent, 'kind', p_kind, 'seq_pos', v_pos), p_actor);
  return v_uid;
end $$;

-- 5) Link a block to an attachment (image, etc.)
create or replace function kms.link_block_attachment(p_block int, p_attachment int, p_actor int)
returns void language plpgsql as $$
begin
  insert into kms.block_asset(block_fk, attachment_fk)
  values (p_block, p_attachment)
  on conflict (block_fk) do update set attachment_fk=excluded.attachment_fk;

  insert into kms.change_events(doc_fk, block_id, event_type, event_data, actor_fk)
  select b.doc_fk, p_block, 'link_asset',
         jsonb_build_object('seq_pos', b.seq_pos, 'attachment_uid', p_attachment), p_actor
  from kms.blocks_bd b where b.uid = p_block;
end $$;

-- 6) Optional helper: last child (for advanced placements)
create or replace function kms.last_child_uid(p_doc int, p_parent int)
returns int language sql as $$
  select uid
    from kms.blocks_bd
    where doc_fk=p_doc and ((parent_fk is null and p_parent is null) or parent_fk=p_parent)
      and valid_to_seq is null
    order by seq_pos desc
    limit 1
$$;


--- Categorisation ---
create table kms.catalog (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY
  , label character varying(250)
  , owner_fk integer not null references kms.usertb(uid)
);

create table kms.nodecat (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY
  , arboid integer NOT NULL references kms.catalog(uid)
  , label character varying(250)
  , parentid int
  , assetid int
  , lastmod timestamp with time zone
  , umode character(4)
);


