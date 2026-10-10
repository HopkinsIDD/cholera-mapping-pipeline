-- Roles for the cholera covariates database.
-- Run once per server, connected to the `postgres` database as the superuser
-- created by initdb.
--
--   psql -v app_password="'secret'" -f 00_roles.sql postgres
--
-- cholera_owner : NOLOGIN group role that owns every schema and table.
-- cholera_app   : LOGIN role used by Slurm jobs; acts as cholera_owner.
-- cholera_ro    : NOLOGIN read-only role for notebooks and laptops.
-- Human users are added with hpc/yggdrasil/tools/create_db_user.sh.

\set ON_ERROR_STOP on

DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'cholera_owner') THEN
    CREATE ROLE cholera_owner NOLOGIN;
  END IF;
  IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'cholera_ro') THEN
    CREATE ROLE cholera_ro NOLOGIN;
  END IF;
  IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'cholera_app') THEN
    CREATE ROLE cholera_app LOGIN;
  END IF;
END
$$;

ALTER ROLE cholera_app PASSWORD :app_password;
GRANT cholera_owner TO cholera_app;
-- Objects created by cholera_app are owned by the group role, so any member
-- can later drop or alter them.
ALTER ROLE cholera_app SET role = 'cholera_owner';
