-- Extensions, schemas and privileges inside one covariates database.
-- Run connected to that database as the superuser.
--
--   psql -f 02_schemas_grants.sql cholera_covariates_bdi
--
-- Schemas:
--   grids      master grid and modelling grids (+ _centroids, _polys)
--   covariates covariate rasters, covariates.metadata, covariates.bands
--   data       output_shapefiles and other reference vector data
--   runs       per-run tables (location periods, intersections, centroids)
--              and scratch tables; unqualified CREATE TABLE lands here via
--              the database search_path

\set ON_ERROR_STOP on

CREATE EXTENSION IF NOT EXISTS postgis;
CREATE EXTENSION IF NOT EXISTS postgis_raster;

CREATE SCHEMA IF NOT EXISTS grids      AUTHORIZATION cholera_owner;
CREATE SCHEMA IF NOT EXISTS covariates AUTHORIZATION cholera_owner;
CREATE SCHEMA IF NOT EXISTS data       AUTHORIZATION cholera_owner;
CREATE SCHEMA IF NOT EXISTS runs       AUTHORIZATION cholera_owner;

-- PostGIS types and functions live in public; nobody but the owner creates
-- tables there (Postgres 15+ default, made explicit for older servers).
-- (In Postgres 15+ public is owned by the database owner, cholera_owner, so
-- members can still create there explicitly; unqualified tables go to runs.)
REVOKE CREATE ON SCHEMA public FROM PUBLIC;
GRANT USAGE ON SCHEMA public TO cholera_owner, cholera_ro;

GRANT USAGE, CREATE ON SCHEMA grids, covariates, data, runs TO cholera_owner;
GRANT USAGE ON SCHEMA grids, covariates, data, runs TO cholera_ro;

ALTER DEFAULT PRIVILEGES FOR ROLE cholera_owner IN SCHEMA grids, covariates, data, runs
  GRANT SELECT ON TABLES TO cholera_ro;

SELECT format('ALTER DATABASE %I SET search_path = runs, public, grids, covariates, data',
              current_database()) \gexec
SELECT format('ALTER DATABASE %I SET postgis.gdal_enabled_drivers = %L',
              current_database(), 'GTiff netCDF') \gexec
SELECT format('ALTER DATABASE %I SET postgis.enable_outdb_rasters = false',
              current_database()) \gexec

-- Covariate metadata: one row per ingested covariate table.
CREATE TABLE IF NOT EXISTS covariates.metadata (
  covariate      TEXT PRIMARY KEY,
  src_res_x      DOUBLE PRECISION,
  src_res_y      DOUBLE PRECISION,
  src_res_time   TEXT,
  first_tl       DATE,
  last_tl        DATE,
  src_dir        TEXT,
  res_x          DOUBLE PRECISION,
  res_y          DOUBLE PRECISION,
  res_time       TEXT,
  space_agg      TEXT,
  time_agg       TEXT,
  aoi_name       TEXT,
  aoi_buffer_km  DOUBLE PRECISION,
  aoi_xmin       DOUBLE PRECISION,
  aoi_xmax       DOUBLE PRECISION,
  aoi_ymin       DOUBLE PRECISION,
  aoi_ymax       DOUBLE PRECISION,
  ref_grid       TEXT,
  n_bands        INTEGER,
  ingested_at    TIMESTAMPTZ DEFAULT now()
);
ALTER TABLE covariates.metadata OWNER TO cholera_owner;

-- Band-to-date map: band i of covariate c covers [tl, tr].
CREATE TABLE IF NOT EXISTS covariates.bands (
  covariate TEXT NOT NULL REFERENCES covariates.metadata(covariate) ON DELETE CASCADE,
  band      INTEGER NOT NULL,
  tl        DATE,
  tr        DATE,
  PRIMARY KEY (covariate, band)
);
ALTER TABLE covariates.bands OWNER TO cholera_owner;

-- Grid provenance: which area of interest a grid was built for.
CREATE TABLE IF NOT EXISTS grids.metadata (
  grid          TEXT PRIMARY KEY,
  aoi_name      TEXT,
  aoi_buffer_km DOUBLE PRECISION,
  bbox_xmin     DOUBLE PRECISION,
  bbox_xmax     DOUBLE PRECISION,
  bbox_ymin     DOUBLE PRECISION,
  bbox_ymax     DOUBLE PRECISION,
  res_km        DOUBLE PRECISION,
  built_at      TIMESTAMPTZ DEFAULT now()
);
ALTER TABLE grids.metadata OWNER TO cholera_owner;
