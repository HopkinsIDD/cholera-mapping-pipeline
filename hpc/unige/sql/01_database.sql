-- Create one covariates database (one per area of interest).
-- Run connected to the `postgres` database as the superuser.
--
--   psql -v dbname=cholera_covariates_bdi -f 01_database.sql postgres

\set ON_ERROR_STOP on

SELECT format('CREATE DATABASE %I OWNER cholera_owner ENCODING %L TEMPLATE template0',
              :'dbname', 'UTF8')
WHERE NOT EXISTS (SELECT 1 FROM pg_database WHERE datname = :'dbname')
\gexec

SELECT format('REVOKE ALL ON DATABASE %I FROM PUBLIC', :'dbname') \gexec
SELECT format('GRANT CONNECT, TEMPORARY ON DATABASE %I TO cholera_owner, cholera_ro', :'dbname') \gexec
