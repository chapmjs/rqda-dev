-- migrations/2025-08-15-add-uniques.sql
-- Adds uniqueness constraints so the app can drop pre-insert existence checks.

ALTER TABLE freecode
  ADD CONSTRAINT uq_freecode_project_name UNIQUE (project_id, name);

ALTER TABLE project
  ADD CONSTRAINT uq_project_by_user UNIQUE (created_by, project_name);
