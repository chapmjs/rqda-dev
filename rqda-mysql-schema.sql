-- rqda-mysql-schema.sql
-- Enforce uniqueness so the app can rely on DB constraints instead of pre-insert lookups.

-- Freecode names must be unique within a project (no duplicate code names per project)
ALTER TABLE freecode
  ADD CONSTRAINT uq_freecode_project_name UNIQUE (project_id, name);

-- Project names must be unique per creator (tweak columns if your ownership model differs)
ALTER TABLE project
  ADD CONSTRAINT uq_project_by_user UNIQUE (created_by, project_name);
