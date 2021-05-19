-- Create master grid:
-- Relies on shapes.shape and shapes.box having srid 4326 (same as grid) edan gnivah xob.s shapee
-- Download ftp://ftp.worldpop.org.uk/GIS/Population/Global_2000_2020/2020/0_Mosaicked/ppp_2020_1km_Aggregated.tif
-- raster2pgsql -s EPSGS:4326 -I -t auto -d {master_spatial_grid_filename}  grids.master_spatial_grid

-- Create Raw Covariate Tables

-- Relies on covariates.<covariate_name>_<time-range> already existing
-- Get shapefiles from github
-- raster2pgsql -s EPSGS:4326 -I -t auto -d {covariate_for_year}  covariates.{covar_name}_{covar_year}

-- Set SRID for shapes table in dan's grid

CREATE TABLE IF NOT EXISTS grids.resized_spatial_grids(rid integer, rast raster, width int, height int);
CREATE INDEX IF NOT EXISTS grids_resized_spatial_grids_size_idx ON grids.resized_spatial_grids (width, height, rid);

CREATE TABLE IF NOT EXISTS covariates.all_covariates(covariate_name text, time_left date, time_right date, rid integer, rast raster);
CREATE INDEX IF NOT EXISTS covariate_name_size_idx ON covariates.all_covariates(covariate_name, time_left, time_right, rid);

DROP TABLE IF EXISTS grids.time_bounds CASCADE;
CREATE TABLE grids.time_bounds(time_left date, time_right date);
INSERT INTO grids.time_bounds VALUES('2000-01-01', '2020-12-31');

CREATE OR REPLACE VIEW grids.master_temporal_grid AS
SELECT
  generate_series(
    time_bounds.time_left::timestamp,
    time_bounds.time_right::timestamp,
    '1 day'::interval
  )::date as time_midpoint
FROM grids.time_bounds;

--- CREATE INDEX grids_resized_temporal_timescale_name_idx ON grids.resized_temporal_grids(timescale_name);
--- CREATE INDEX grids_resized_temporal_time_midpoint_idx ON grids.resized_temporal_grids(time_midpoint);

CREATE MATERIALIZED VIEW IF NOT EXISTS grids.resized_spatial_grid_polygons
AS
  SELECT
    rid,
    width,
    height,
    dp.*
  FROM
    grids.resized_spatial_grids, LATERAL ST_PixelAsPolygons(rast, 1) AS dp;

CREATE MATERIALIZED VIEW IF NOT EXISTS grids.resized_spatial_grid_centroids
AS
  SELECT
    rid,
    width,
    height,
    dp.*
  FROM
    grids.resized_spatial_grids, LATERAL ST_PixelAsCentroids(rast, 1) AS dp;

CREATE MATERIALIZED VIEW IF NOT EXISTS location_period_raster_map AS
  SELECT
    shapes.id as shape_id,
    location_period_id,
    rid,
    width,
    height
  FROM
    shapes
      LEFT JOIN
    grids.resized_spatial_grids
      on
        ST_INTERSECTS(shapes.box, resized_spatial_grids.rast);

CREATE MATERIALIZED VIEW IF NOT EXISTS covariate_grid_map AS
SELECT
  all_covariates.covariate_name,
  all_covariates.time_left,
  all_covariates.time_right,
  all_covariates.rid as covar_rid,
  resized_spatial_grids.rid as grid_rid,
  resized_spatial_grids.width,
  resized_spatial_grids.height
FROM
  grids.resized_spatial_grids
    LEFT JOIN
  covariates.all_covariates
    ON
      st_intersects(all_covariates.rast, resized_spatial_grids.rast);

-- Create functions for extracting from the grid

create or replace function resize_spatial_grid(width_in_km int, height_in_km int)
  returns table(rid integer, rast raster)
  as $$
  SELECT
    master_spatial_grid.rid,
    st_resample(
      master_spatial_grid.rast,
      (st_metadata(master_spatial_grid.rast)).width / width_in_km,
      (st_metadata(master_spatial_grid.rast)).height / height_in_km
    ) as rast
  FROM
    grids.master_spatial_grid;
  $$ LANGUAGE SQL;

create or replace function ingest_resized_spatial_grid(width_in_km int, height_in_km int)
  returns void
  LANGUAGE plpgsql
  as $$
  begin
  PERFORM * FROM grids.resized_spatial_grids where resized_spatial_grids.width = width_in_km AND resized_spatial_grids.height = height_in_km;
  if not found then
     INSERT INTO grids.resized_spatial_grids SELECT rid, rast, width_in_km as width, height_in_km as height FROM resize_spatial_grid(width_in_km, height_in_km);
  end if;
  end;
  $$ SECURITY DEFINER;

create or replace function ingest_covariate(name text, table_name text, ingest_time_left date, ingest_time_right date)
  returns void
  LANGUAGE plpgsql
  as $$
  begin
  PERFORM * FROM covariates.all_covariates where all_covariates.covariate_name = name AND all_covariates.time_left = ingest_time_left and all_covariates.time_right = ingest_time_right;
  if not found then
  EXECUTE FORMAT ('INSERT INTO covariates.all_covariates SELECT %L as name, %L as time_left, %L as time_right, rid, rast FROM covariates.%I', name, ingest_time_left, ingest_time_right, table_name);
  end if;
  end;
  $$ SECURITY DEFINER;

create or replace function filter_location_periods(location_name text)
  returns table(id bigint, location_id bigint, qualified_name text, location_period_id bigint) AS $$
  SELECT
    ROW_NUMBER() OVER (ORDER BY 1),
    descendants.id as location_id,
    descendants.qualified_name as qualified_name,
    location_periods.id as location_period_id
  FROM
    locations as descendants
      LEFT JOIN
        location_hierarchies
           ON descendants.id = location_hierarchies.descendant_id
      LEFT JOIN
        locations as ancestors
          ON ancestors.id = location_hierarchies.ancestor_id
      LEFT JOIN
        location_periods
          ON descendants.id = location_periods.location_id
  WHERE ancestors.qualified_name = location_name;
  $$ LANGUAGE SQL;

create or replace function location_period_raster_centroid_map(location_name text, width_in_km int, height_in_km int)
  returns table(location_period_id bigint, rid int, x int, y int) AS $$
  SELECT
    filtered_location_periods.location_period_id,
    resized_spatial_grid_centroids.rid,
    resized_spatial_grid_centroids.x,
    resized_spatial_grid_centroids.y
  FROM
    filter_location_periods(location_name) as filtered_location_periods
      LEFT JOIN
        shapes
          ON
            filtered_location_periods.location_period_id = shapes.location_period_id
      LEFT JOIN
        location_period_raster_map
	  ON
	    location_period_raster_map.location_period_id = filtered_location_periods.location_period_id AND
	    location_period_raster_map.shape_id = shapes.id
      LEFT JOIN
        grids.resized_spatial_grid_centroids
          ON
	    location_period_raster_map.rid = grids.resized_spatial_grid_centroids.rid AND
            st_within(resized_spatial_grid_centroids.geom, shapes.shape)
  WHERE
    resized_spatial_grid_centroids.width = width_in_km AND
    resized_spatial_grid_centroids.height = height_in_km
  $$ LANGUAGE SQL;

create or replace function resize_temporal_grid(time_unit text)
RETURNS table(id bigint, time_midpoint date, time_min date, time_max date) AS
$$
  BEGIN
  IF time_unit = 'year'
  THEN
  RETURN QUERY
  SELECT
    ROW_NUMBER() OVER (ORDER BY EXTRACT(year from tbl.time_midpoint)),
    (timestamp without time zone '1970-01-01' + cast(avg(EXTRACT(epoch from tbl.time_midpoint))::text as interval))::date,
    (timestamp without time zone '1970-01-01' + cast(min(EXTRACT(epoch from tbl.time_midpoint))::text as interval))::date,
    (timestamp without time zone '1970-01-01' + cast(max(EXTRACT(epoch from tbl.time_midpoint))::text as interval))::date
  FROM
    grids.master_temporal_grid as tbl
  GROUP BY
    extract(year from tbl.time_midpoint);
  END IF;
  IF time_unit = 'month'
  THEN
  RETURN QUERY
  SELECT
    ROW_NUMBER() OVER (ORDER BY EXTRACT(year from tbl.time_midpoint), EXTRACT(month from tbl.time_midpoint)),
    (timestamp without time zone '1970-01-01' + cast(avg(EXTRACT(epoch from tbl.time_midpoint))::text as interval))::date,
    (timestamp without time zone '1970-01-01' + cast(min(EXTRACT(epoch from tbl.time_midpoint))::text as interval))::date,
    (timestamp without time zone '1970-01-01' + cast(max(EXTRACT(epoch from tbl.time_midpoint))::text as interval))::date
  FROM
    grids.master_temporal_grid as tbl
  GROUP BY
    extract(year from tbl.time_midpoint),
    extract(month from tbl.time_midpoint);
  END IF;
  END;
$$ LANGUAGE plpgsql;

-- Pipeline Specific Functionality

create or replace function pull_observation_data(location_name text, start_date date, end_date date)
  returns table(
    id bigint,
    observation_collection_id bigint,
    observation_id bigint,
    location_name text,
    location_id bigint,
    location_period_id bigint,
    suspected_cases int,
    confirmed_cases int,
    deaths int,
    time_left date,
    time_right date,
    is_primary bool,
    is_phantom bool,
    shape geometry
  ) AS $$
  SELECT
    ROW_NUMBER() OVER (ORDER BY observation_collection_id, observations.id),
    observations.observation_collection_id,
    observations.id as observation_id,
    filtered_location_periods.qualified_name as location_name,
    filtered_location_periods.location_id,
    observations.location_period_id,
    observations.suspected_cases,
    observations.confirmed_cases,
    observations.deaths,
    observations.time_left,
    observations.time_right,
    observations.primary,
    observations.phantom,
    shapes.shape
  FROM
    observations
      INNER JOIN
        filter_location_periods(location_name) as filtered_location_periods
          ON
	    observations.location_period_id = filtered_location_periods.location_period_id
      LEFT JOIN
        shapes
	  ON
	    observations.location_period_id = shapes.location_period_id
  WHERE
    observations.time_left >= start_date AND
    observations.time_right <= end_date;
  $$ LANGUAGE SQL;

create or replace function pull_covar_cube(location_name text, start_date date, end_date date, width_in_km int, height_in_km int, time_scale text)
RETURNS TABLE(covariate_name text, t bigint, rid int, x int, y int, value double precision)AS $$
  SELECT
    all_covariates.covariate_name,
    temporal_grid.id as t,
    grid_centroids.rid,
    grid_centroids.x,
    grid_centroids.y,
    ST_VALUE(all_covariates.rast, grid_centroids.geom) as value
  FROM filter_resized_spatial_grid_centroids_to_location(location_name, width_in_km, height_in_km) as grid_centroids
    INNER JOIN
      covariate_grid_map
        ON
          grid_centroids.rid = covariate_grid_map.grid_rid
    FULL JOIN
      resize_temporal_grid(time_scale) as temporal_grid
        ON
	  1 = 1
    INNER JOIN
      covariates.all_covariates
        ON
	  covariate_grid_map.covar_rid = all_covariates.rid AND
	  covariate_grid_map.time_left = all_covariates.time_left AND
	  covariate_grid_map.time_right = all_covariates.time_right AND
	  all_covariates.time_left <= temporal_grid.time_midpoint AND
	  all_covariates.time_right >= temporal_grid.time_midpoint AND
	  covariate_grid_map.covariate_name = all_covariates.covariate_name AND
          st_intersects(all_covariates.rast, grid_centroids.geom)
  WHERE
    temporal_grid.time_midpoint >= start_date AND
    temporal_grid.time_midpoint <= end_date
  $$ LANGUAGE SQL;

create or replace function pull_grid_adjacency(location_name text, width_in_km int, height_in_km int)
RETURNS TABLE(id_1 bigint, rid_1 int, x_1 int, y_1 int, id_2 bigint, rid_2 int, x_2 int, y_2 int)AS $$
  SELECT
    lhs.id as id_1,
    lhs.rid as rid_1,
    lhs.x as x_1,
    lhs.y as y_1,
    rhs.id as id_2,
    rhs.rid as rid_2,
    rhs.x as x_2,
    rhs.y as y_2
  FROM filter_resized_grid_centroids_to_location(location_name, width_in_km, height_in_km) as lhs
    LEFT JOIN
      grids.resized_grid_polygons as lhs_poly
        ON
	  lhs.rid = lhs_poly.rid AND
	  lhs.x = lhs_poly.x AND
	  lhs.y = lhs_poly.y
    INNER JOIN
      grids.resized_grid_polygons as rhs_poly
        ON
	  st_intersects(lhs_poly.geom, rhs_poly.geom)
    LEFT JOIN
      filter_resized_grid_centroids_to_location(location_name, width_in_km, height_in_km) as rhs
        ON
	  rhs.rid = rhs_poly.rid AND
	  rhs.x = rhs_poly.x AND
	  rhs.y = rhs_poly.y

  WHERE
    lhs.id < rhs.id
  $$ LANGUAGE SQL;

-- This function is stupid and just a long way to do something easy
create or replace function pull_observation_location_period_map(location_name text, start_date date, end_date date)
  returns table(
    observation_id bigint,
    location_periods_id bigint
  ) AS $$
  SELECT
    observation_data.id as observation_id,
    location_periods.location_period_id
  FROM
    pull_observation_data(location_name, start_date, end_date) as observation_data
      LEFT JOIN
    filter_location_periods(location_name) as location_periods
      ON
        observation_data.location_period_id = location_periods.location_period_id;
  $$ LANGUAGE SQL;

create or replace function pull_observation_temporal_grid_map(location_name text, start_date date, end_date date, time_scale text)
  returns table(
    observation_id bigint,
    t bigint,
    tfrac numeric
  ) AS $$
  SELECT
    observation_data.id as observation_id,
    temporal_grid.id as t,
    (
      least(observation_data.time_right,temporal_grid.time_max) -
      greatest(observation_data.time_left, temporal_grid.time_min)
    ) * 1.0 / (temporal_grid.time_max - temporal_grid.time_min)
  FROM
    pull_observation_data(location_name, start_date, end_date) as observation_data
      LEFT JOIN
    resize_temporal_grid(time_scale) as temporal_grid
      ON
        observation_data.time_left <= temporal_grid.time_midpoint AND
        observation_data.time_right >= temporal_grid.time_midpoint
  $$ LANGUAGE SQL;

create or replace function pull_location_period_grid_map(location_name text, width_in_km int, height_in_km int)
RETURNS TABLE(location_period_id bigint, spatial_grid_id bigint, rid int, x int, y int)AS $$
  SELECT
    shapes.location_period_id,
    spatial_grid.id as spatial_grid_id,
    spatial_grid.rid,
    spatial_grid.x,
    spatial_grid.y
  FROM
    filter_location_periods('AFR::KEN') as location_periods
      left join
    shapes
      on
        location_periods.location_period_id = shapes.location_period_id
      left join
    filter_resized_spatial_grid_centroids_to_location('AFR::KEN', 20, 20) as spatial_grid
      on
        ST_CONTAINS(shapes.shape, spatial_grid.geom);
  $$ LANGUAGE SQL;

--- Check on covar rid grid rid equivalence
--- Check on grid adjacency
--- Fix time_midpoint


--- For testing:
---
