#' @description Throw an error (message) unless test is True
#' @param test boolean This function will do nothing if TRUE, and throw an error otherwise
#' @param message The message the error should contain if test is not TRUE
assert <- function(test, message = "Assertion failed") {
  if (!isTRUE(test)) {
    stop(message)
  }
}

#' @description run add_query, then run drop_query if drop
#' @name add_and_or_drop
#' @title add_and_or_drop
#' @param psql_connection a connection to a database made with dbConnect
#' @param add_query The query to use to add the thing
#' @param drop_query The query to use to drop the thing
#' @param drop Whether to drop an existing table
add_and_or_drop <- function(psql_connection, add_query, drop_query, drop = FALSE) {
  if (drop) {
    lapply(drop_query, function(q) {
      DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, q))
    })
  }

  lapply(add_query, function(q) {
    DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, q))
  })
}

#' @description Create an observations table for use in testing
#' @name create_observations_table
#' @title create_observations_table
#' @param psql_connection a connection to a database made with dbConnect
#' @param drop Whether to drop an existing table
create_observations_table <- function(psql_connection, drop = FALSE) {
  drop_query <- "DROP TABLE IF EXISTS observations CASCADE;"
  add_query <- "
CREATE TABLE observations(
  id BIGSERIAL PRIMARY KEY,
  observation_collection_id bigint,
  time_left date,
  time_right date,
  location_id bigint REFERENCES locations(id),
  location_period_id bigint REFERENCES location_periods(id),
  \"primary\" boolean,
  phantom boolean,
  suspected_cases integer,
  confirmed_cases integer,
  deaths integer
);"

  add_and_or_drop(psql_connection, add_query, drop_query, drop)
}

#' @description Create a locations table for use in testing
#' @name create_locations_table
#' @title create_locations_table
#' @param psql_connection a connection to a database made with dbConnect
#' @param drop Whether to drop an existing table
create_locations_table <- function(psql_connection, drop = FALSE) {
  drop_query <- "DROP TABLE IF EXISTS locations CASCADE;"
  add_query <- "CREATE TABLE locations(id BIGSERIAL PRIMARY KEY, qualified_name text UNIQUE);"

  add_and_or_drop(psql_connection, add_query, drop_query, drop)
}

#' @description Create a location_periods table for use in testing
#' @name create_location_periods_table
#' @title create_location_periods_table
#' @param psql_connection a connection to a database made with dbConnect
#' @param drop Whether to drop an existing table
create_location_periods_table <- function(psql_connection, drop = FALSE) {
  drop_query <- "DROP TABLE IF EXISTS location_periods CASCADE;"
  add_query <- "
CREATE TABLE location_periods(
  id BIGSERIAL PRIMARY KEY,
  location_id bigint REFERENCES locations(id),
  start_date date,
  end_date date
);"

  add_and_or_drop(psql_connection, add_query, drop_query, drop)
}

#' @description Create a shapes table for use in testing
#' @name create_shapes_table
#' @title create_shapes_table
#' @param psql_connection a connection to a database made with dbConnect
#' @param drop Whether to drop an existing table
create_shapes_table <- function(psql_connection, drop = FALSE) {
  drop_query <- "DROP TABLE IF EXISTS shapes CASCADE;"
  add_query <- "
CREATE TABLE shapes(
  id BIGSERIAL PRIMARY KEY,
  location_period_id bigint REFERENCES location_periods(id) UNIQUE,
  shape GEOMETRY,
  box GEOMETRY
);"

  add_and_or_drop(psql_connection, add_query, drop_query, drop)
}

#' @description Create a location_hierarchies table for use in testing
#' @name create_location_hierarchies_table
#' @title create_location_hierarchies_table
#' @param psql_connection a connection to a database made with dbConnect
#' @param drop Whether to drop an existing table
create_location_hierarchies_table <- function(psql_connection, drop = FALSE) {
  drop_query <- "DROP TABLE IF EXISTS location_hierarchies CASCADE;"
  add_query <- "CREATE TABLE location_hierarchies(ancestor_id bigint REFERENCES locations(id), descendant_id bigint REFERENCES locations(id), generations integer);"

  add_and_or_drop(psql_connection, add_query, drop_query, drop)
}

#' @description Create a master_spatial_grid table for use in testing
#' @name create_master_spatial_grid_table
#' @title create_master_spatial_grid_table
#' @param psql_connection a connection to a database made with dbConnect
#' @param drop Whether to drop an existing table
create_master_spatial_grid_table <- function(psql_connection, drop = FALSE) {
  drop_query <- "DROP MATERIALIZED VIEW IF EXISTS grids.master_spatial_grid CASCADE;"
  add_query <- "
CREATE MATERIALIZED VIEW grids.master_spatial_grid as
  select 1 as rid, st_asraster(shape, 10, 10, '32BF') as rast
  from (SELECT shape from shapes where id = 1) as local_shapes;"

  add_and_or_drop(psql_connection, add_query, drop_query, drop)
}

create_spatial_resolutions_table <- function(psql_connection, drop = FALSE) {
  add_query <- "CREATE TABLE grids.spatial_resolutions(width_in_km int, height_in_km int, UNIQUE(width_in_km, height_in_km));"
  drop_query <- "DROP TABLE IF EXISTS grids.spatial_resolutions CASCADE;"
  add_and_or_drop(psql_connection, add_query, drop_query, drop)
}

create_resized_spatial_grids_view <- function(psql_connection, drop = FALSE) {
  add_query <- "
CREATE MATERIALIZED VIEW grids.resized_spatial_grids AS
   SELECT
     spatial_resolutions.width_in_km as width,
     spatial_resolutions.height_in_km as height,
     resized_grid.*
   FROM
     grids.spatial_resolutions,
   LATERAL
     resize_spatial_grid(width_in_km, height_in_km) as resized_grid;
"
  drop_query <- "DROP MATERIALIZED VIEW IF EXISTS grids.resized_spatial_grids CASCADE;"
  add_and_or_drop(psql_connection, add_query, drop_query, drop)
}

#' @description Create a covariates schema for use in testing
#' @name create_covariates_schema
#' @title create_covariates_schema
#' @param psql_connection a connection to a database made with dbConnect
#' @param drop Whether to drop an existing table
create_covariates_schema <- function(psql_connection, drop = FALSE) {
  add_query <- "CREATE SCHEMA IF NOT EXISTS covariates;"
  drop_query <- "DROP SCHEMA IF EXISTS covariates CASCADE;"
  add_and_or_drop(psql_connection, add_query, drop_query, drop)
}
#' @description Create a all_covariates table for use in testing
#' @name create_all_covariates_table
#' @title create_all_covariates_table
#' @param psql_connection a connection to a database made with dbConnect
#' @param drop Whether to drop an existing table
create_all_covariates_table <- function(psql_connection, drop = FALSE) {
  create_covariates_schema(psql_connection, drop = drop)
  drop_query <- "DROP TABLE IF EXISTS covariates.all_covariates"
  add_query <- c(
    "CREATE TABLE covariates.all_covariates(covariate_name text, time_left date, time_right date, rid integer, rast raster);",
    "CREATE INDEX covariate_name_size_idx ON covariates.all_covariates(covariate_name, time_left, time_right, rid);"
  )

  add_and_or_drop(psql_connection, add_query, drop_query, drop)
}

#' @description Create a time_bounds table for use in testing
#' @name create_time_bounds_table
#' @title create_time_bounds_table
#' @param psql_connection a connection to a database made with dbConnect
#' @param drop Whether to drop an existing table
create_time_bounds_table <- function(psql_connection, drop = FALSE) {
  drop_query <- "DROP TABLE IF EXISTS grids.time_bounds CASCADE;"
  add_query <- "CREATE TABLE grids.time_bounds(time_left date, time_right date);"
  add_data_query <- "INSERT INTO grids.time_bounds VALUES('2000-01-01', '2020-12-31');"

  add_and_or_drop(psql_connection, c(add_query, add_data_query), drop_query, drop)
}

#' @description Create a master_temporal_grid view for use in testing
#' @name create_master_temporal_grid_view
#' @title create_master_temporal_grid_view
#' @param psql_connection a connection to a database made with dbConnect
create_master_temporal_grid_view <- function(psql_connection, drop = FALSE) {
  add_query <- "
CREATE OR REPLACE VIEW grids.master_temporal_grid AS
SELECT
  generate_series(
    time_bounds.time_left::timestamp,
    time_bounds.time_right::timestamp,
    '1 day'::interval
  )::date as time_midpoint
FROM grids.time_bounds;"

  drop_query <- "DROP VIEW IF EXISTS grids.master_temporal_grid"
  add_and_or_drop(psql_connection, add_query, drop_query, drop)
}

#' @description Create a resized_spatial_grid_pixels view for use in testing
#' @name create_resized_spatial_grid_pixels_view
#' @title create_resized_spatial_grid_pixels_view
#' @param psql_connection a connection to a database made with dbConnect
create_resized_spatial_grid_pixels_view <- function(psql_connection, drop = FALSE) {
  add_query <- "
CREATE MATERIALIZED VIEW IF NOT EXISTS grids.resized_spatial_grid_pixels
AS
  SELECT
    ROW_NUMBER() OVER (ORDER BY 1) as id,
    rid,
    width,
    height,
    dp.x,
    dp.y,
    dp.geom as polygon,
    st_centroid(dp.geom) as centroid
  FROM
    grids.resized_spatial_grids, LATERAL ST_PixelAsPolygons(rast, 1) AS dp;"

  drop_query <- "DROP MATERIALIZED VIEW IF EXISTS grids.resized_spatial_grid_pixels"
  add_and_or_drop(psql_connection, add_query, drop_query, drop)
}

#' @description Create a master_spatial_grid_centroids view for use in testing
#' @name create_master_spatial_grid_centroids_view
#' @title create_master_spatial_grid_centroids_view
#' @param psql_connection a connection to a database made with dbConnect
create_master_spatial_grid_centroids_view <- function(psql_connection, drop = FALSE) {
  add_query <- "
CREATE MATERIALIZED VIEW IF NOT EXISTS grids.master_spatial_grid_centroids
AS
  SELECT
    ROW_NUMBER() OVER (ORDER BY 1) as id,
    rid,
    dp.x,
    dp.y,
    dp.geom
  FROM
    grids.master_spatial_grid, LATERAL ST_PixelAsCentroids(rast, 1) AS dp;"
  drop_query <- "DROP MATERIALIZED VIEW IF EXISTS grids.master_spatial_grid_centroids"

  add_and_or_drop(psql_connection, add_query, drop_query, drop)
}

#' @description Create a location_period_raster_map_view for use in testing
#' @name create_location_period_raster_map_view
#' @title create_location_period_raster_map_view
#' @param psql_connection a connection to a database made with dbConnect
create_location_period_raster_map_view <- function(psql_connection, drop = FALSE) {
  add_query <- "
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
        ST_INTERSECTS(shapes.box, resized_spatial_grids.rast);"

  drop_query <- "DROP MATERIALIZED VIEW IF EXISTS location_period_raster_map"

  add_and_or_drop(psql_connection, add_query, drop_query, drop)
}

#' @description Create a covariate_grid_map view for use in testing
#' @name create_covariate_grid_map_view
#' @title create_covariate_grid_map_view
#' @param psql_connection a connection to a database made with dbConnect
create_covariate_grid_map_view <- function(psql_connection, drop = FALSE) {
  add_query <- "
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
"

  DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, add_query))
}

#' @description Create the portion of the testing database that mimics
#' the database located at cholera-taxonomy.middle-distance.com
#' @name create_testing_base_database
#' @title create_testing_base_database
#' @param psql_connection a connection to a database made with dbConnect
#' @param drop Whether to drop existing database elements
create_testing_base_database <- function(psql_connection, drop = FALSE) {
  create_locations_table(psql_connection, drop)
  create_location_periods_table(psql_connection, drop)
  create_shapes_table(psql_connection, drop)
  create_observations_table(psql_connection, drop)
  create_location_hierarchies_table(psql_connection, drop)
  create_master_spatial_grid_table(psql_connection, drop)
}

#' @description Create the portion of the testing database that we intend
#' to add to the database located at cholera-taxonomy.middle-distance.com
#' @name create_testing_additional_database
#' @title create_testing_additional_database
#' @param psql_connection a connection to a database made with dbConnect
#' @param drop Whether to drop existing database elements
create_testing_additional_database <- function(psql_connection, drop = FALSE) {
  create_spatial_resolutions_table(psql_connection, drop)
  create_resize_spatial_grid_function(psql_connection)
  create_resized_spatial_grids_view(psql_connection, drop)
  create_all_covariates_table(psql_connection, drop)
  create_time_bounds_table(psql_connection, drop)
  create_master_temporal_grid_view(psql_connection, drop)
  create_resized_spatial_grid_pixels_view(psql_connection, drop)
  create_master_spatial_grid_centroids_view(psql_connection, drop)
  create_location_period_raster_map_view(psql_connection, drop)
  create_covariate_grid_map_view(psql_connection, drop)
  create_shapes_with_names_view(psql_connection, drop)
  create_shape_resized_spatial_grid_map_view(psql_connection, drop)
  create_shape_resized_spatial_grid_populations_view(psql_connection, drop)
  create_resized_covariates_view(psql_connection, drop)
  invisible(NULL)
}

create_resize_spatial_grid_function <- function(psql_connection) {
  function_query <- "
create or replace function resize_spatial_grid(width_in_km int, height_in_km int)
  returns table(rid integer, rast raster)
  as $$
  SELECT
    master_spatial_grid.rid,
    st_resample(
      master_spatial_grid.rast,
      (st_metadata(master_spatial_grid.rast)).width / width_in_km,
      (st_metadata(master_spatial_grid.rast)).height / height_in_km
    ) as new_rast
  FROM
    grids.master_spatial_grid;
  $$ LANGUAGE SQL SECURITY DEFINER;
"

  DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, function_query))
}

create_lookup_location_period_function <- function(psql_connection) {
  function_query <- "
create or replace function lookup_location_period(target_location_id bigint, target_start_date date, target_end_date date)
  returns bigint
  as $$
  SELECT
    location_periods.id
  FROM
    location_periods
  WHERE
    location_periods.location_id = target_location_id AND
    location_periods.start_date <= target_start_date AND
    location_periods.end_date >= target_end_date
  $$ LANGUAGE SQL SECURITY DEFINER;
"

  trigger_function_query <- "
CREATE OR REPLACE FUNCTION trigger_lookup_missing_location_period_for_observations()
  RETURNS trigger
  LANGUAGE plpgsql AS
$func$
BEGIN
   NEW.location_period_id := lookup_location_period(NEW.location_id, NEW.time_left, NEW.time_right);
   RETURN NEW;
END
$func$;"


  trigger_add_query <- "
CREATE TRIGGER observations_location_period_id_default
BEFORE INSERT ON observations
FOR EACH ROW
WHEN (
  NEW.location_period_id IS NULL AND
  NEW.location_id IS NOT NULL AND
  NEW.time_left IS NOT NULL AND
  NEW.time_right IS NOT NULL
) EXECUTE PROCEDURE trigger_lookup_missing_location_period_for_observations();
"

  DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, function_query))
  DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, trigger_function_query))
  DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, trigger_add_query))
}

create_ingest_covariate_function <- function(psql_connection) {
  function_query <- "
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
"

  DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, function_query))
  invisible(NULL)
}

create_pull_grid_adjacency_function <- function(psql_connection) {
  create_filter_resized_spatial_grid_pixels_to_location_function(psql_connection)
  function_query <- "
CREATE OR REPLACE FUNCTION pull_grid_adjacency(location_name text, width_in_km int, height_in_km int)
RETURNS TABLE(id_1 BIGINT, rid_1 INT, x_1 INT, y_1 INT, id_2 BIGINT, rid_2 INT, x_2 INT, y_2 INT)AS $$
  SELECT
    lhs.id AS id_1,
    lhs.rid AS rid_1,
    lhs.x AS x_1,
    lhs.y AS y_1,
    rhs.id AS id_2,
    rhs.rid AS rid_2,
    rhs.x AS x_2,
    rhs.y AS y_2
  FROM filter_resized_spatial_grid_pixels_to_location(location_name, width_in_km, height_in_km) AS lhs
    INNER JOIN
      filter_resized_spatial_grid_pixels_to_location(location_name, width_in_km, height_in_km) AS rhs
        ON
          ST_INTERSECTS(ST_BUFFER(lhs.polygon, SQRT(ST_AREA(lhs.polygon))*.01), rhs.polygon)
  WHERE
    lhs.id < rhs.id
  $$ LANGUAGE SQL SECURITY DEFINER;
"

  DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, function_query))
  invisible(NULL)
}

create_pull_symmetric_grid_adjacency_function <- function(psql_connection) {
  create_filter_resized_spatial_grid_pixels_to_location_function(psql_connection)
  function_query <- "
CREATE OR REPLACE FUNCTION pull_symmetric_grid_adjacency(location_name text, width_in_km int, height_in_km int)
RETURNS TABLE(id_1 BIGINT, rid_1 INT, x_1 INT, y_1 INT, id_2 BIGINT, rid_2 INT, x_2 INT, y_2 INT)AS $$
  SELECT
    lhs.id AS id_1,
    lhs.rid AS rid_1,
    lhs.x AS x_1,
    lhs.y AS y_1,
    rhs.id AS id_2,
    rhs.rid AS rid_2,
    rhs.x AS x_2,
    rhs.y AS y_2
  FROM filter_resized_spatial_grid_pixels_to_location(location_name, width_in_km, height_in_km) AS lhs
    INNER JOIN
      filter_resized_spatial_grid_pixels_to_location(location_name, width_in_km, height_in_km) AS rhs
        ON
          ST_INTERSECTS(ST_BUFFER(lhs.polygon, SQRT(ST_AREA(lhs.polygon))*.01), rhs.polygon)
  WHERE
    lhs.id != rhs.id
  $$ LANGUAGE SQL SECURITY DEFINER;
"

  DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, function_query))
}

create_filter_resized_spatial_grid_pixels_to_location_function <- function(psql_connection) {
  create_filter_location_periods_function(psql_connection)
  function_query <- "
create or replace function filter_resized_spatial_grid_pixels_to_location(location_name text, width_in_km int, height_in_km int)
  returns table(id bigint, rid int, x int, y int, centroid geometry, polygon geometry) AS $$
 SELECT
    DISTINCT
    spatial_grid.id,
    spatial_grid.rid,
    spatial_grid.x,
    spatial_grid.y,
    spatial_grid.centroid,
    spatial_grid.polygon
  FROM
    filter_location_periods(location_name) as location_periods
  LEFT JOIN
    shapes
      on
        location_periods.location_period_id = shapes.location_period_id
  LEFT JOIN
    shape_resized_spatial_grid_populations
      ON
        shape_resized_spatial_grid_populations.shape_id = shapes.id
  LEFT JOIN
    grids.resized_spatial_grid_pixels as spatial_grid
      ON
        shape_resized_spatial_grid_populations.grid_id = spatial_grid.id
  WHERE
    spatial_grid.width = width_in_km AND
    spatial_grid.height = height_in_km AND
    (
      (shape_resized_spatial_grid_populations.grid_population > 0) OR
      (shape_resized_spatial_grid_populations.intersection_population IS NULL)
    )
  $$ LANGUAGE SQL SECURITY DEFINER;
"

  DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, function_query))
}

create_filter_location_periods_function <- function(psql_connection) {
  function_query <- "
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
  $$ LANGUAGE SQL SECURITY DEFINER;"

  DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, function_query))
  invisible(NULL)
}

create_pull_observation_data_function <- function(psql_connection) {
  create_filter_location_periods_function(psql_connection)
  function_query <- "
create or replace function pull_observation_data(location_name text, start_date date, end_date date)
  returns table(
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
  $$ LANGUAGE SQL SECURITY DEFINER;
"

  DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, function_query))
  invisible(NULL)
}

create_pull_location_period_grid_map_function <- function(psql_connection) {
  # TODO This function needs work
  function_query_boundary <- "
create or replace function pull_location_period_grid_map_boundary(location_name text, start_date date, end_date date, width_in_km int, height_in_km int, time_scale text)
RETURNS TABLE(qualified_name text, location_id bigint, location_period_id bigint, shape_id bigint, spatial_grid_id bigint, rid int, x int, y int, t bigint, sfrac double precision) AS $$
  SELECT
    location_periods.qualified_name as qualified_name,
    location_periods.location_id as location_id,
    location_periods.location_period_id as location_period_id,
    shapes.id as shape_id,
    spatial_grid.id as spatial_grid_id,
    spatial_grid.rid,
    spatial_grid.x,
    spatial_grid.y,
    temporal_grid.id as t,
    shape_resized_spatial_grid_populations.intersection_population / shape_resized_spatial_grid_populations.grid_population
  FROM
    filter_location_periods(location_name) as location_periods
  LEFT JOIN
    shapes
      on
        location_periods.location_period_id = shapes.location_period_id
  LEFT JOIN
    shape_resized_spatial_grid_populations
      on
        shapes.id = shape_resized_spatial_grid_populations.shape_id
  LEFT JOIN
    grids.resized_spatial_grid_pixels as spatial_grid
      ON
        shape_resized_spatial_grid_populations.grid_id = spatial_grid.id
  FULL JOIN
    resize_temporal_grid(time_scale) as temporal_grid
      ON
        (temporal_grid.time_midpoint >= shape_resized_spatial_grid_populations.time_left) AND (temporal_grid.time_midpoint <= shape_resized_spatial_grid_populations.time_right)
  WHERE
    temporal_grid.time_midpoint <= end_date
    AND temporal_grid.time_midpoint >= start_date
    AND spatial_grid.width = width_in_km
    AND spatial_grid.height = height_in_km
    AND (shape_resized_spatial_grid_populations.intersection_population IS NOT NULL)
    AND (shape_resized_spatial_grid_populations.grid_population > 0)
  $$ LANGUAGE SQL SECURITY DEFINER;
"

  function_query_interior <- "
create or replace function pull_location_period_grid_map_interior(location_name text, start_date date, end_date date, width_in_km int, height_in_km int, time_scale text)
RETURNS TABLE(qualified_name text, location_id bigint, location_period_id bigint, shape_id bigint, spatial_grid_id bigint, rid int, x int, y int, t bigint, sfrac double precision) AS $$
  SELECT
    location_periods.qualified_name as qualified_name,
    location_periods.location_id as location_id,
    location_periods.location_period_id as location_period_id,
    shapes.id as shape_id,
    spatial_grid.id as spatial_grid_id,
    spatial_grid.rid,
    spatial_grid.x,
    spatial_grid.y,
    temporal_grid.id as t,
    1.::double precision as sfrac
  FROM
    filter_location_periods(location_name) as location_periods
  LEFT JOIN
    shapes
      on
        location_periods.location_period_id = shapes.location_period_id
  LEFT JOIN
    grids.resized_spatial_grid_pixels as spatial_grid
      ON
        st_intersects(shapes.shape,spatial_grid.centroid)
  FULL JOIN
    resize_temporal_grid(time_scale) as temporal_grid
      ON
        1=1
  WHERE
    temporal_grid.time_midpoint <= end_date
    AND temporal_grid.time_midpoint >= start_date
    AND spatial_grid.width = width_in_km
    AND spatial_grid.height = height_in_km
  $$ LANGUAGE SQL;
"

  function_query <- "
create or replace function pull_location_period_grid_map(location_name text, start_date date, end_date date, width_in_km int, height_in_km int, time_scale text)
RETURNS TABLE(qualified_name text, location_id bigint, location_period_id bigint, shape_id bigint, spatial_grid_id bigint, rid int, x int, y int, t bigint, sfrac double precision) AS $$
SELECT
  qualified_name, location_id, location_period_id, shape_id, spatial_grid_id, rid, x, y, t, min(sfrac)
FROM (
  SELECT * FROM pull_location_period_grid_map_interior(location_name, start_date, end_date, width_in_km, height_in_km, time_scale)
    UNION ALL
  SELECT * FROM pull_location_period_grid_map_boundary(location_name, start_date, end_date, width_in_km, height_in_km, time_scale)
) AS tmp
GROUP BY
  qualified_name, location_id, location_period_id, shape_id, spatial_grid_id, rid, x, y, t
;
  $$ LANGUAGE SQL;
"
  DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, function_query_interior))
  DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, function_query_boundary))
  DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, function_query))
  invisible(NULL)
}

create_resize_temporal_grid_function <- function(psql_connection) {
  function_query <- "
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
$$ LANGUAGE plpgsql SECURITY DEFINER;
"

  DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, function_query))
  invisible(NULL)
}

create_pull_covar_cube_function <- function(psql_connection) {
  create_resize_temporal_grid_function(psql_connection)
  function_query <- "
create or replace function pull_covar_cube(location_name text, start_date date, end_date date, width_in_km int, height_in_km int, time_scale text)
RETURNS TABLE(covariate_name text, t bigint, id bigint, rid int, x int, y int, value double precision, geometry geometry)AS $$
SELECT
  resized_covariates.covariate_name,
  temporal_grid.id as t,
  grid_pixels.id as grid_id,
  grid_pixels.rid as grid_rid,
  grid_pixels.x,
  grid_pixels.y,
  resized_covariates.sum as value,
  grid_pixels.polygon as geometry
FROM
  filter_resized_spatial_grid_pixels_to_location(location_name, width_in_km, height_in_km) as grid_pixels
INNER JOIN
  resized_covariates
    ON
      grid_pixels.id = resized_covariates.grid_id
      AND grid_pixels.rid = resized_covariates.grid_rid
INNER JOIN
  resize_temporal_grid(time_scale) as temporal_grid
    ON
      resized_covariates.time_left <= temporal_grid.time_midpoint AND
      resized_covariates.time_right >= temporal_grid.time_midpoint
WHERE
    temporal_grid.time_midpoint >= start_date AND
    temporal_grid.time_midpoint <= end_date
$$ LANGUAGE SQL SECURITY DEFINER;
"

  DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, function_query))
  invisible(NULL)
}

create_pull_observation_location_period_map <- function(psql_connection) {
  function_query <- "
create or replace function pull_observation_location_period_map(location_name text, start_date date, end_date date, time_scale text)
  returns table(
    observation_id bigint,
    location_period_id bigint,
    t bigint,
    tfrac double precision
  ) AS $$
WITH
  observation_data AS
  (SELECT * FROM pull_observation_data(location_name, start_date, end_date))
SELECT
  observation_data.observation_id,
  location_periods.id as location_period_id,
  temporal_grid.id as temporal_grid_id,
  ( 1 + least(observation_data.time_right, temporal_grid.time_max) - greatest(observation_data.time_left, temporal_grid.time_min)) * 1.::double precision / ( 1 + temporal_grid.time_max - temporal_grid.time_min) as tfrac
FROM
  observation_data
INNER JOIN
  location_periods
    ON
      observation_data.location_period_id = location_periods.id
INNER JOIN
  resize_temporal_grid(time_scale) AS temporal_grid
    ON
      observation_data.time_left < temporal_grid.time_max AND
      observation_data.time_right >= temporal_grid.time_min
$$ LANGUAGE SQL SECURITY DEFINER;
"

  DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, function_query))
  invisible(NULL)
}

create_pull_boundary_polygon <- function(psql_connection) {
  function_query <- "
create or replace function pull_boundary_polygon(location_name text)
  returns table(
    shape geometry
  ) AS $$
SELECT shapes.shape
FROM
  locations
INNER JOIN
  location_periods
    ON
      locations.id = location_periods.location_id
INNER JOIN
  shapes
    ON
      location_periods.id = shapes.location_period_id
WHERE
  locations.qualified_name = location_name
$$ LANGUAGE SQL SECURITY DEFINER;
"

  DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, function_query))
  invisible(NULL)
}

create_pull_minimal_grid_population <- function(psql_connection) {
  function_query <- "
create or replace function pull_minimal_grid_population(location_name text, start_date date, end_date date, time_scale text)
  returns table(
    rid int,
    temporal_grid_id bigint,
    rast raster
  ) AS $$
SELECT
  rid,
  temporal_grid.id as t,
  rast
FROM
  locations
INNER JOIN
  location_periods
    ON
      locations.id = location_periods.location_id
INNER JOIN
  shapes
    ON
      location_periods.id = shapes.location_period_id
INNER JOIN
  covariates.all_covariates
    ON
      st_intersects(shapes.shape, st_envelope(rast))
INNER JOIN
  resize_temporal_grid(time_scale) as temporal_grid
    ON
      covariates.all_covariates.time_left <= temporal_grid.time_midpoint
      AND covariates.all_covariates.time_right >= temporal_grid.time_midpoint
WHERE
  time_midpoint >=start_date
  AND time_midpoint <= end_date
  AND covariate_name = 'population'
  AND locations.qualified_name = location_name
$$ LANGUAGE SQL SECURITY DEFINER;
"

  DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, function_query))
  invisible(NULL)
}

#' @description Create the functions we will use as part of the testing database
#' @name create_testing_database_functions
#' @title create_testing_database_functions
#' @param psql_connection a connection to a database made with dbConnect
create_testing_database_functions <- function(psql_connection) {
  create_lookup_location_period_function(psql_connection)
  create_ingest_covariate_function(psql_connection)
  create_pull_grid_adjacency_function(psql_connection)
  create_pull_symmetric_grid_adjacency_function(psql_connection)
  create_pull_observation_data_function(psql_connection)
  create_pull_covar_cube_function(psql_connection)
  create_pull_observation_location_period_map(psql_connection)
  create_pull_location_period_grid_map_function(psql_connection)
  create_pull_boundary_polygon(psql_connection)
  create_pull_minimal_grid_population(psql_connection)
}

#' @description Refresh the materialized views in the database to account for new data
#' @name refresh_sql_materialized_views
#' @title refresh_sql_materialized_views
#' @param psql_connection a connection to a database made with dbConnect
#' @export
refresh_materialized_views <- function(psql_connection) {
  queries <- paste("REFRESH MATERIALIZED VIEW", c(
    "grids.master_spatial_grid",
    "grids.resized_spatial_grids", "grids.resized_spatial_grid_pixels", "grids.master_spatial_grid_centroids",
    "location_period_raster_map", "covariate_grid_map", "shapes_with_names",
    "shape_resized_spatial_grid_map_view", "shape_resized_spatial_grid_populations",
    "resized_covariates"
  ))
  for (query in queries) {
    DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, query))
  }
}

#' @description Set up a database with no data to use as part of writing test cases
#' @name setup_testing_database
#' @title setup_testing_database
#' @param psql_connection a connection to a database made with dbConnect
#' @param drop Whether to drop an existing table
#' @export
setup_testing_database <- function(psql_connection, drop = FALSE) {
  if (drop) {
    destroy_testing_database(psql_connection)
  }
  create_testing_base_database(psql_connection, drop)
  create_testing_additional_database(psql_connection, drop)
  create_testing_database_functions(psql_connection)
}

#' @description Create a testing database out of a list of data frames
#' @param psql_connection A connection to the database to create the testing database in
#' @param data_frame_list A list of data frames to create the data out of.  In particular, there should be the following dataframes with associated columns:
#'  - location_df : see insert_testing_locations
#'  - location_period_df : see insert_testin_location_periods
#'  - shapes_df : see insert_testing_shapefiles
#'  - observations_df : see insert_testing_observations
#' @param covariate_creation_function_list A list of covariate descriptions with the following fields:
#'  - name : A string name for the covariate
#'  - time_left : The first time the covariate should be used
#'  - time_right : The last time the covariate should be used
#'  - fun : A function which takes a psql_connection as an argument and produces a covariate. It should return a raster whose extent is the same as the extent of the polygon associated with the first location in locations_df
#' @seealso insert_testing_locations
#' @seealso insert_testing_location_periods
#' @seealso insert_testing_shapefiles
#' @seealso insert_testing_observations
#' @seealso ingest_covariate_from_raster
#' @export
setup_testing_database_from_dataframes <- function(psql_connection, data_frame_list,
                                                   covariate_creation_function_list, drop = TRUE) {
  setup_testing_database(psql_connection, drop = drop)
  insert_testing_locations(psql_connection, data_frame_list$location_df)
  insert_testing_location_periods(psql_connection, data_frame_list$location_period_df)
  insert_testing_shapefiles(psql_connection, data_frame_list$shapes_df)
  refresh_materialized_views(psql_connection)
  ingest_spatial_grid(psql_connection, width = 1, height = 1)
  ingest_spatial_grid(psql_connection, width = 2, height = 2)
  ingest_spatial_grid(psql_connection, width = 5, height = 5)
  ingest_spatial_grid(psql_connection, width = 10, height = 10)
  refresh_materialized_views(psql_connection)
  insert_testing_observations(psql_connection, data_frame_list$observations_df)
  for (fun in covariate_creation_function_list) {
    ingest_covariate_from_raster(psql_connection, fun$name, fun$fun(psql_connection),
      fun$start_date, fun$end_date,
      overwrite = drop
    )
  }
  refresh_materialized_views(psql_connection)
}

drop_testing_database_functions <- function(psql_connection) {
  drop_queries <- rev(c(
    "DROP FUNCTION IF EXISTS resize_spatial_grid CASCADE", "DROP FUNCTION IF EXISTS lookup_location_period CASCADE",
    "DROP FUNCTION IF EXISTS ingest_covariate CASCADE",
    "DROP FUNCTION IF EXISTS filter_resized_spatial_grid_pixels_to_location CASCADE", "DROP FUNCTION IF EXISTS pull_symmetric_grid_adjacency CASCADE",
    "DROP FUNCTION IF EXISTS pull_observation_data CASCADE", "DROP FUNCTION IF EXISTS filter_location_periods CASCADE",
    "DROP FUNCTION IF EXISTS resize_temporal_grid CASCADE", "DROP FUNCTION IF EXISTS pull_observation_location_period_map CASCADE",
    "DROP FUNCTION IF EXISTS pull_location_period_grid_map CASCADE", "DROP FUNCTION IF EXISTS resized_spatial_grid_pixels"
  ))

  sapply(drop_queries, function(query) {
    DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, query))
  })
  invisible(NULL)
}

#' @description delete a testing database
#' @name destory_testing_database
#' @title destroy_testing_database
#' @param psql_connection a connection to a database made with dbConnect
#' @export
destroy_testing_database <- function(psql_connection) {
  drop_query <- c(
    "DROP TABLE IF EXISTS location_periods CASCADE", "DROP TABLE IF EXISTS locations CASCADE",
    "DROP TABLE IF EXISTS shapes CASCADE", "DROP TABLE IF EXISTS observations CASCADE",
    "DROP TABLE IF EXISTS location_hierarchies CASCADE", "DROP MATERIALIZED VIEW IF EXISTS grids.master_spatial_grid CASCADE",
    "DROP TABLE IF EXISTS grids.spatial_resolutions CASCADE", "DROP TABLE IF EXISTS grids.time_bounds CASCADE",
    "DROP TABLE IF EXISTS covariates.all_covariates CASCADE", "DROP MATERIALIZED VIEW IF EXISTS grids.resized_spatial_grid_pixels",
    "DROP MATERIALIZED VIEW IF EXISTS grids.master_spatial_grid_centroids", "DROP MATERIALIZED VIEW IF EXISTS location_period_raster_map",
    "DROP MATERIALIZED VIEW IF EXISTS covariate_grid_map CASCADE", "DROP SCHEMA IF EXISTS covariates CASCADE", "DROP MATERIALIZED VIEW IF EXISTS shapes_with_names"
  )
  drop_testing_database_functions(psql_connection)
  sapply(drop_query, function(query) {
    DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, query))
  })
  invisible(NULL)
}

#' @description Pull data from the api, and parse it into the objects needed to create a testing database. This function returns those data frames, so they can be modified before a database is created with them.
#' @name create_testing_dfs_from_api
#' @title create_testing_dfs_from_api
#' @param username api username
#' @param api_key api_key
#' @param locations vector of locations to pull data for
#' @param time_left pull only data after this time
#' @param time_right pull only data before this time
#' @param uids pull only specific uids (default to all uids)
#' @param website where to access the data, defaults to the api website
#' @export
create_testing_dfs_from_api <- function(username, api_key, locations = NULL, time_left = NULL,
                                        time_right = NULL, uids = NULL, website = "https://api.cholera-taxonomy.middle-distance.com/") {
  api_results <- read_taxonomy_data_api(
    username, api_key, locations, time_left,
    time_right, uids, website
  )
  location_df <- api_results %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(qualified_name = gsub("[.][^:]*$", "", attributes.location_name)) %>%
    dplyr::select(qualified_name) %>%
    dplyr::group_by(qualified_name) %>%
    dplyr::summarize(.groups = "drop")

  location_period_df <- api_results %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(
      qualified_name = gsub("[.][^:]*$", "", attributes.location_name),
      start_date = lubridate::ymd(attributes.time_left), end_date = lubridate::ymd(attributes.time_right)
    ) %>%
    dplyr::select(sf_id, qualified_name, start_date, end_date) %>%
    dplyr::group_by(qualified_name, sf_id) %>%
    dplyr::summarize(
      start_date = min(start_date), end_date = max(end_date),
      .groups = "drop"
    ) %>%
    dplyr::select(qualified_name, start_date, end_date)

  shapes_df <- api_results %>%
    dplyr::mutate(
      qualified_name = gsub("[.][^:]*$", "", attributes.location_name),
      start_date = lubridate::ymd(attributes.time_left), end_date = lubridate::ymd(attributes.time_right)
    ) %>%
    dplyr::select(sf_id, qualified_name, start_date, end_date) %>%
    dplyr::group_by(qualified_name, sf_id) %>%
    dplyr::summarize(
      start_date = min(start_date), end_date = max(end_date),
      .groups = "drop"
    ) %>%
    dplyr::select(qualified_name, start_date, end_date)
  names(shapes_df)[[4]] <- "geom"
  sf::st_geometry(shapes_df) <- "geom"

  observations_df <- api_results %>%
    dplyr::mutate(
      qualified_name = gsub("[.][^:]*$", "", attributes.location_name),
      time_left = lubridate::ymd(attributes.time_left), time_right = lubridate::ymd(attributes.time_right),
      observation_collection_id = relationships.observation_collection.data.id,
      primary = attributes.primary, phantom = attributes.phantom, suspected_cases = attributes.fields.suspected_cases,
      confirmed_cases = attributes.fields.confirmed_cases, deaths = attributes.fields.deaths
    )

  return(list(
    location_df = location_df, location_period_df = location_period_df,
    shapes_df = shapes_df, observations_df = observations_df
  ))
}

#' @description Insert locations into the testing database
#' @name insert_testing_locations
#' @title insert_testing_locations
#' @param psql_connection a connection to a database made with dbConnect
#' @param location_df A data frame with one row per location to insert with information about the location
#' @export
insert_testing_locations <- function(psql_connection, location_df) {
  assert("qualified_name" %in% names(location_df), "insert_testing_locations cannot insert a location without a qualified name")
  insert_query <- paste("INSERT INTO locations(\"qualified_name\") VALUES", paste("(",
    glue::glue_sql(.con = psql_connection, "{location_df[['qualified_name']]}"),
    ")",
    collapse = ", "
  ))

  DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, insert_query))

  location_parts <- stringr::str_split(pattern = "::", string = location_df$qualified_name)
  for (location_part_set in location_parts) {
    full_location <- paste(location_part_set, collapse = "::")
    full_location_id_query <- glue::glue_sql(.con = psql_connection, "SELECT id FROM LOCATIONS WHERE qualified_name = {full_location}")
    full_location_id <- DBI::dbGetQuery(conn = psql_connection, full_location_id_query)[["id"]]
    partial_locations <- lapply(seq_len(length(location_part_set)), function(x) {
      return(paste(location_part_set[seq_len(x)], collapse = "::"))
    })
    for (partial_location_idx in seq_len(length(partial_locations))) {
      location <- partial_locations[[partial_location_idx]]
      location_id_query <- glue::glue_sql(.con = psql_connection, "SELECT id FROM LOCATIONS WHERE qualified_name = {location}")
      ## check if exists
      location_id <- DBI::dbGetQuery(conn = psql_connection, location_id_query)[["id"]]
      ## create if not exists
      if (length(location_id) == 0) {
        insert_testing_locations(psql_connection, data.frame(qualified_name = location))
        location_id <- DBI::dbGetQuery(conn = psql_connection, location_id_query)[["id"]]
      }
      assert(length(location_id) > 0, paste(
        "Failed to create the location",
        location
      ))
      ## add to location hierarchy
      hierarchy_query <- glue::glue_sql(.con = psql_connection, "INSERT INTO location_hierarchies VALUES ({location_id}, {full_location_id}, {length(partial_locations) - partial_location_idx})")
      DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, hierarchy_query))
    }
  }

  invisible(NULL)
}

#' @description Lookup the location_id associated with a set of qualified names
#' @name lookup_location_ids
#' @param qualified_names a character vector of names to look up the location_ids for
#' @return A bigint vector of location ids
lookup_location_ids <- function(psql_connection, qualified_names) {
  location_name_and_ids <- DBI::dbGetQuery(conn = psql_connection, glue::glue_sql(
    .con = psql_connection,
    "SELECT qualified_name, id FROM locations where qualified_name in ({qualified_names*})"
  ))
  location_name_changer <- setNames(location_name_and_ids[["id"]], location_name_and_ids[["qualified_name"]])
  return(location_name_changer[qualified_names])
}


lookup_location_period_ids <- function(psql_connection, location_period_df) {
  location_period_info <- DBI::dbGetQuery(conn = psql_connection, glue::glue_sql(
    .con = psql_connection,
    "SELECT location_id, start_date, end_date, id
       FROM location_periods
       WHERE location_id in ({location_period_df[['location_id']]*})
       AND start_date in ({location_period_df[['start_date']]*})
       AND end_date in ({location_period_df[['end_date']]*})"
  ))
  location_period_id_changer <- setNames(location_period_info$id, paste(location_period_info[["location_id"]],
    location_period_info[["start_date"]], location_period_info[["end_date"]],
    sep = "_"
  ))
  rc <- location_period_id_changer[paste(location_period_df[["location_id"]], location_period_df[["start_date"]],
    location_period_df[["end_date"]],
    sep = "_"
  )]
  if (any(is.na(rc))) {
    stop("Could not find all location period ids")
  }
  return(rc)
}

#' @description Insert location_periods into the testing database
#' @name insert_testing_location_periods
#' @title insert_testing_location_periods
#' @param psql_connection a connection to a database made with dbConnect
#' @param location_period_df A data frame with one row per location_period to insert with information about the location_period
#' @export
insert_testing_location_periods <- function(psql_connection, location_period_df) {
  assert("qualified_name" %in% names(location_period_df), "insert_testing_location_periods cannot insert a location_period without a qualified name")
  assert("start_date" %in% names(location_period_df), "insert_testing_location_periods cannot insert a location_period without a start date")
  assert("end_date" %in% names(location_period_df), "insert_testing_location_periods cannot insert a location_period without a end date")

  location_period_df[["location_id"]] <- lookup_location_ids(psql_connection, location_period_df[["qualified_name"]])

  insert_query <- paste(
    "INSERT INTO location_periods(\"location_id\", \"start_date\", \"end_date\") VALUES",
    paste("(", glue::glue_sql(.con = psql_connection, "{location_period_df[['location_id']]}"),
      ",", glue::glue_sql(.con = psql_connection, "{location_period_df[['start_date']]}"),
      ",", glue::glue_sql(.con = psql_connection, "{location_period_df[['end_date']]}"),
      ")",
      collapse = ", "
    )
  )
  DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, insert_query))
  invisible(NULL)
}

#' @description Insert shapefiles into the testing database
#' @name insert_testing_shapefiles
#' @title insert_testing_shapefiles
#' @param psql_connection a connection to a database made with dbConnect
#' @param shape_df A data frame with one row per shape to insert with information about the shape
#' @export
insert_testing_shapefiles <- function(psql_connection, shape_df, create_location_periods = FALSE,
                                      check = FALSE) {
  assert("qualified_name" %in% names(shape_df), "insert_testing_shapefiles cannot insert a shape without a qualified name")
  assert("start_date" %in% names(shape_df), "insert_testing_shapefiles cannot insert a shape without a start date")
  assert("end_date" %in% names(shape_df), "insert_testing_shapefiles cannot insert a shape without an end date")
  assert("geom" %in% names(shape_df), "insert_testing_shapefiles cannot insert a shape without a geometry")

  if (create_location_periods) {
    stop("This functionality is not yet written")
  }

  shape_df[["location_id"]] <- lookup_location_ids(psql_connection, shape_df[["qualified_name"]])

  shape_df[["location_period_id"]] <- lookup_location_period_ids(
    psql_connection,
    shape_df[, c("location_id", "start_date", "end_date")]
  )
  names(shape_df)[names(shape_df) == "geom"] <- "shape"
  sf::st_geometry(shape_df) <- "shape"
  srid <- sf::st_crs(shape_df)$epsg
  if (is.na(srid)) {
    srid <- 4326
    sf::st_crs(shape_df) <- srid
  }
  shape_df[["box"]] <- sf::st_as_sfc(sf::st_bbox(shape_df[["shape"]]))

  # insert_query <- paste( 'INSERT INTO shapes(\'location_period_id\',
  # \'shape\', \'box\') VALUES', paste( '(', glue::glue_sql(.con =
  # psql_connection, '{shape_df[['location_period_id']]}'), ',',
  # glue::glue_sql(.con = psql_connection,
  # '{sf::st_as_binary(shape_df[['geom']])}', digits = 20, EWKT = TRUE), ',',
  # glue::glue_sql(.con = psql_connection,
  # '{sf::st_as_binary(shape_df[['box']])}', digits = 20, EWKT = TRUE), ')',
  # collapse = ', ' ) )

  # srid_query <- glue::glue_sql(.con = psql_connection, 'UPDATE shapes set
  # shape = st_setsrid(shape, {srid}), box = st_setsrid(box, {srid})')

  # DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection,
  # insert_query)) DBI::dbClearResult(DBI::dbSendQuery(conn =
  # psql_connection, srid_query))

  sf::st_write(
    obj = shape_df[, c("location_period_id", "shape", "box")], dsn = psql_connection,
    layer = "shapes", append = TRUE
  )
  if (check) {
    read_shapes <- DBI::dbGetQuery(conn = psql_connection, glue::glue_sql(
      .con = psql_connection,
      "SELECT * FROM shapes where location_period_id in ({shape_df[['location_period_id']]*})"
    ))
    read_shapes$shape <- sf::st_as_sfc(read_shapes$shape, crs = srid)
    read_shapes$box <- sf::st_as_sfc(read_shapes$box, crs = srid)
    read_shapes <- sf::st_as_sf(read_shapes)
    read_shapes_2 <- sf::st_read(psql_connection, layer = "shapes")
    assert(nrow(shape_df) == nrow(read_shapes), "insert_testing_shapefiles object changed number of polygons during writing")
    shape_df <- dplyr::arrange(shape_df, as.character(location_period_id))
    read_shapes <- dplyr::arrange(read_shapes, as.character(location_period_id))
    for (row in seq_len(nrow(shape_df))) {
      if (!(sf::st_is_empty(read_shapes[row, ]) && sf::st_is_empty(shape_df[row, ])) && !(sf::st_geometry(read_shapes[row, ]) == sf::st_geometry(shape_df[row, ]))) {
        lhs <- sf::st_geometry(read_shapes[row, ])
        rhs <- sf::st_geometry(shape_df[row, ])
        if (as.numeric(sf::st_area(sf::st_intersection(lhs, rhs)) / sf::st_area(sf::st_union(
          lhs,
          rhs
        ))) < 0.999) {
          stop("Bad things")
        }
      }
    }
  }
  invisible(NULL)
}

#' @description Insert observations into the testing database
#' @name insert_testing_observations
#' @title insert_testing_observations
#' @param psql_connection a connection to a database made with dbConnect
#' @param observation_df A data frame with one row per observation to insert with information about the observation
#' @export
insert_testing_observations <- function(psql_connection, observation_df) {
  assert("observation_collection_id" %in% names(observation_df), "insert_testing_observations cannot insert a shape without a observation_collection_id")
  assert("phantom" %in% names(observation_df), "insert_testing_observations cannot insert a shape without a phantom")
  assert("primary" %in% names(observation_df), "insert_testing_observations cannot insert a shape without a primary")
  assert("qualified_name" %in% names(observation_df), "insert_testing_observations cannot insert a shape without a qualified name")
  assert("time_left" %in% names(observation_df), "insert_testing_observations cannot insert a shape without a time left")
  assert("time_right" %in% names(observation_df), "insert_testing_observations cannot insert a shape without a time right")
  assert(
    any(c("suspected_cases", "confirmed_cases", "deaths") %in% names(observation_df)),
    "insert_testing_observations cannot insert a shape without at least one case column"
  )

  observation_df$location_id <- lookup_location_ids(psql_connection, observation_df[["qualified_name"]])

  location_period_id_query <- glue::glue_sql(.con = psql_connection, "SELECT id FROM location_periods WHERE location_id = {observation_df$location_id} AND {observation_df$time_left} >= start_date AND {observation_df$time_right} <= end_date")
  observation_df$location_period_id <- unname(sapply(
    location_period_id_query,
    function(query) {
      rc <- DBI::dbGetQuery(conn = psql_connection, query)[["id"]]
      if (length(rc) == 0) {
        rc <- bit64::integer64(1) * NA
      }
      return(as.character(rc))
    }
  ))


  insert_query <- paste(
    "INSERT INTO observations(\"observation_collection_id\", \"time_left\", \"time_right\", \"location_period_id\", \"location_id\", \"primary\", \"phantom\", \"suspected_cases\", \"confirmed_cases\", \"deaths\") VALUES",
    paste("(", glue::glue_sql(.con = psql_connection, "{observation_df[['observation_collection_id']]}"),
      ",", glue::glue_sql(.con = psql_connection, "{observation_df[['time_left']]}"),
      ",", glue::glue_sql(.con = psql_connection, "{observation_df[['time_right']]}"),
      ",", glue::glue_sql(.con = psql_connection, "{observation_df[['location_period_id']]}"),
      ",", glue::glue_sql(.con = psql_connection, "{observation_df[['location_id']]}"),
      ",", glue::glue_sql(.con = psql_connection, "{observation_df[['primary']]}"),
      ",", glue::glue_sql(.con = psql_connection, "{observation_df[['phantom']]}"),
      ",", glue::glue_sql(.con = psql_connection, "{observation_df[['suspected_cases']]}"),
      ",", glue::glue_sql(.con = psql_connection, "{observation_df[['confirmed_cases']]}"),
      ",", glue::glue_sql(.con = psql_connection, "{observation_df[['deaths']]}"),
      ")",
      collapse = ", "
    )
  )

  DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, insert_query))
}

#' @description Tell the postgres database to create a resampled grid with appropriate grid size
#' @param psql_connection a connection to a database made with dbConnect
#' @param width width of the grid cells in km
#' @param heigh height of the grid cells in km
#' @export
ingest_spatial_grid <- function(psql_connection, width = 1, height = 1, do_refresh = TRUE) {
  ingest_query <- "INSERT INTO grids.spatial_resolutions VALUES({width}, {height});"
  ingest_query <- glue::glue_sql(.con = psql_connection, ingest_query)
  DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, ingest_query))
  if (do_refresh) {
    refresh_materialized_views(psql_connection)
  }
  invisible(NULL)
}

#' @description Write a raster to postgres
#' @param raster The raster to write to postgres
#' @param psql_connection A connection to the postgres database
#' @param table_name The name of the table (including schema) to put the raster in
#' @param overwrite Whether to delete an existing table before writing this one
write_raster_to_postgres <- function(psql_connection, raster_to_write, table_name,
                                     crs = sf::st_crs(raster_to_write), overwrite = FALSE) {
  if (overwrite) {
    DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, paste(
      "DROP TABLE IF EXISTS",
      table_name
    )))
  }
  table_file_name <- paste0("/tmp/", table_name, ".tif")
  raster::writeRaster(raster_to_write, table_file_name, overwrite = TRUE)
  attach(DBI::dbGetInfo(psql_connection))
  command <- "raster2pgsql -s '{crs$input}' -I -C -t auto {table_file_name} {table_name} | psql -d {dbname} -p {port} -U {username}"
  system(glue::glue(command))
}

#' @description Tell the postgres database to create a resampled grid with appropriate grid size
#' @param psql_connection a connection to a database made with dbConnect
#' @param width width of the grid cells in km
#' @param heigh height of the grid cells in km
#' @export
ingest_covariate_from_raster <- function(psql_connection, covariate_name, covariate_raster,
                                         time_left, time_right, overwrite = FALSE) {
  assert(!is.null(time_left), "ingest_covariate requires a time_left argument")
  assert(!is.null(time_right), "ingest_covariate requires a time_right argument")
  assert(!is.null(covariate_name), "ingest_covariate requires a covariate_name argument")
  table_name <- paste(covariate_name, time_left, time_right, sep = "_")
  table_name <- gsub("-", "_", table_name)
  table_name <- gsub("[.]", "_", table_name)
  write_raster_to_postgres(
    raster = covariate_raster, psql_connection = psql_connection,
    table_name = paste0("covariates.", table_name), overwrite = overwrite
  )
  # rpostgis::pgWriteRast(raster = covariate_raster, conn = psql_connection,
  # name = c('covariates', table_name))
  ingest_query <- "SELECT ingest_covariate({covariate_name}, {table_name}, {time_left}, {time_right});"
  ingest_query <- glue::glue_sql(.con = psql_connection, ingest_query)
  DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, ingest_query))
  invisible(NULL)
}

#' @description Get the upper_triangular adjacency structure of a grid.
#' @name pull_grid_adjacency
#' @title pull_grid_adjacency
#' @param psql_connection a connection to a database made with dbConnect
#' @param location_name only include gridcells whose centroids intersect the shape of this location
#' @param width width in km of the grid to consider
#' @param height height in km of the grid to consider
#' @export
pull_grid_adjacency <- function(psql_connection, location_name, width, height) {
  results_query <- "SELECT * FROM pull_grid_adjacency({location_name}, {width}, {height})"
  results_query <- glue::glue_sql(.con = psql_connection, results_query)
  return(DBI::dbGetQuery(conn = psql_connection, results_query))
}

#' @description Get the symmetric adjacency structure of a grid.
#' @name pull_symmetric_grid_adjacency
#' @title pull_symmetric_grid_adjacency
#' @param psql_connection a connection to a database made with dbConnect
#' @param location_name only include gridcells whose centroids intersect the shape of this location
#' @param width width in km of the grid to consider
#' @param height height in km of the grid to consider
#' @export
pull_symmetric_grid_adjacency <- function(psql_connection, location_name, width,
                                          height) {
  results_query <- "SELECT * FROM pull_symmetric_grid_adjacency({location_name}, {width}, {height})"
  results_query <- glue::glue_sql(.con = psql_connection, results_query)
  return(DBI::dbGetQuery(conn = psql_connection, results_query))
}

#' @description Get the symmetric adjacency structure of a grid.
#' @name pull_observation_data
#' @title pull_observation_data
#' @param psql_connection a connection to a database made with dbConnect
#' @param location_name only include gridcells whose centroids intersect the shape of this location
#' @param start_date Only pull data starting on or after this date
#' @param end_date Only pull data ending on or before this date
#' @export
pull_observation_data <- function(psql_connection, location_name, start_date, end_date) {
  results_query <- "SELECT * FROM pull_observation_data({location_name}, {start_date}, {end_date})"
  results_query <- glue::glue_sql(.con = psql_connection, results_query)
  return(DBI::dbGetQuery(conn = psql_connection, results_query))
}


#' @description Convert covariates from the format used by the simulation framework to the format used by the testing framework
#' @name simulation_covariate_to_test_covariate_funs
#' @title simulation_covariate_to_test_covariate_funs
#' @param simulated_covariates The covariates (created by create_multiple_test_covariates) you want to convert.
#' @return A list of function + metadata, which can be used as a covariate function list by other test database function
#' @seealso setup_testing_database_from_dataframes
#' @export
convert_simulated_covariates_to_test_covariate_funs <- function(original_simulated_covariates,
                                                                min_time_left, max_time_right) {
  simulated_covariates <- original_simulated_covariates
  simulated_covariates[[1]]$covariate <- 10^simulated_covariates[[1]][["covariate"]] +
    1
  simulated_covariates[[1]][["covariate"]][simulated_covariates[[1]][["covariate"]] >
    2^32] <- 2^32
  simulated_covariates[[1]][["covariate"]][simulated_covariates[[1]][["covariate"]] <
    1 + 2^(-31)] <- 1 + 2^(-31)
  nrow <- max(simulated_covariates[[1]]$row)
  ncol <- max(simulated_covariates[[1]]$col)
  if (any(abs(log(simulated_covariates[[1]]$covariate - 1) / log(10) - original_simulated_covariates[[1]]$covariate) >
    1e-10)) {
    warning("Some covariates were truncated during conversion")
  }
  lapply(seq_len(length(simulated_covariates)), function(covariate_idx) {
    covariate <- simulated_covariates[[covariate_idx]]
    covariate_name <- names(simulated_covariates)[[covariate_idx]]
    if (is.null(covariate_name)) {
      covariate_name <- ifelse(covariate_idx == 1, "population", paste("covariate",
        covariate_idx,
        sep = ""
      ))
    }
    min_time_index <- min(covariate$t)
    max_time_index <- max(covariate$t)
    lapply(unique(covariate$t), function(time_index) {
      return(list(
        name = covariate_name,
        start_date = min_time_left + (time_index -
          min_time_index) / (max_time_index - min_time_index + 1) * (max_time_right -
          min_time_left),
        end_date = min_time_left + (time_index + 1 - min_time_index) / (max_time_index +
          -min_time_index + 1) * (max_time_right - min_time_left), fun = function(psql_connection) {
          rc <- raster::raster(sf::st_sf(sf::st_as_sfc(sf::st_bbox(covariate))),
            nrow = nrow, ncol = ncol, vals = as.numeric(NA)
          )
          filtered_covariate <- sf::st_drop_geometry(dplyr::filter(
            covariate,
            t == time_index
          ))
          for (idx in seq_len(nrow(filtered_covariate))) {
            row <- filtered_covariate[idx, "row"]
            col <- filtered_covariate[idx, "col"]
            val <- filtered_covariate[idx, "covariate"]
            rc[nrow - row + 1, col] <- val
          }
          return(rc)
        }
      ))
    })
  }) %>%
    unlist(recursive = FALSE) %>%
    return()
}

#' @description Convert from the testing covariate function list format to the simulation covariate format
#' @name test_covariate_funs_to_simulation_covariates
#' @title test_covariate_funs_to_simulation_covariates
#' @param covariate_creation_function_list A list of covariate descriptions with the following fields:
#'  - name : A string name for the covariate
#'  - time_left : The first time the covariate should be used
#'  - time_right : The last time the covariate should be used
#'  - fun : A function which takes a psql_connection as an argument and produces a covariate. It should return a raster whose extent is the same as the extent of the polygon associated with the first location in locations_df
#' @export
convert_test_covariate_funs_to_simulation_covariates <- function(covariate_creation_function_list) {
  rc <- lapply(covariate_creation_function_list, function(x) {
    tibble::tibble(
      name = x$name, start_date = x$start_date, end_date = x$end_date,
      covar = list(x$fun())
    )
  }) %>%
    do.call(what = dplyr::bind_rows) %>%
    dplyr::group_by(name) %>%
    dplyr::summarize(covar = list(do.call(what = dplyr::bind_rows, mapply(
      SIMPLIFY = FALSE,
      covar, start_date, FUN = function(covar, time_left) {
        tmp <- reshape2::melt(array(raster::values(covar), dim(covar[[1]])))
        tmp$geometry <- sf::st_geometry(sf::st_as_sf(raster::rasterToPolygons(covar)))
        tmp$row <- rev(tmp$Var2)
        tmp$col <- tmp$Var1
        tmp <- dplyr::arrange(tmp, row, col)
        tmp$id <- seq_len(nrow(tmp))
        tmp$t <- which(start_date == time_left)
        tmp$covariate <- tmp$value
        return(sf::st_as_sf(tmp[, c(
          "id", "row", "col", "t", "geometry",
          "covariate"
        )]))
      }
    ))), .groups = "drop") %>%
    dplyr::arrange(as.numeric(gsub("covariate", "", gsub("population", "0", name))))
  rc$covar[[1]]$covariate[rc$covar[[1]]$covariate < 1 + 2^(-32)] <- 1 + 2^(-32)
  rc$covar[[1]]$covariate <- log(rc$covar[[1]]$covariate - 1) / log(10)
  return(rc$covar)
}

#' @export
convert_test_dfs_to_simulation_observed_polygons <- function(shapes_df, observations_df) {
  #' @importFrom magrittr `%>%`
  if ("sf" %in% class(observations_df)) {
    observations_df <- sf::st_drop_geometry(observations_df)
  }
  rc <- observations_df %>%
    dplyr::left_join(shapes_df) %>%
    dplyr::mutate(
      location = qualified_name, draw = observation_collection_id,
      geometry = geom, cases = suspected_cases
    ) %>%
    dplyr::select(-primary, -phantom, -deaths, -confirmed_cases, -suspected_cases, -start_date, -end_date, -qualified_name, -observation_collection_id, -geom) %>%
    sf::st_as_sf()
  return(rc)
}

#' @export
convert_simulated_polygons_to_test_dataframes <- function(observed_polygons) {
  rc <- list()
  rc$location_df <- data.frame(qualified_name = sort(unique(observed_polygons$location)))

  rc$location_period_df <- observed_polygons %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(qualified_name = location) %>%
    dplyr::group_by(qualified_name) %>%
    dplyr::summarize(
      start_date = min(time_left), end_date = max(time_right),
      .groups = "drop"
    ) %>%
    as.data.frame()

  rc$shapes_df <- observed_polygons %>%
    dplyr::mutate(qualified_name = location) %>%
    dplyr::group_by(qualified_name) %>%
    dplyr::summarize(
      start_date = min(time_left), end_date = max(time_right),
      .groups = "drop"
    ) %>%
    as.data.frame() %>%
    sf::st_as_sf()
  names(rc$shapes_df)[4] <- "geom"
  sf::st_geometry(rc$shapes_df) <- "geom"
  return(rc)
}

#' @export
convert_simulated_data_to_test_dataframes <- function(simulated_data) {
  all_dfs <- list()

  min_time_left <- min(simulated_data$observed_polygons$time_left)
  max_time_right <- max(simulated_data$observed_polygons$time_right)

  all_dfs <- convert_simulated_polygons_to_test_dataframes(simulated_data$observed_polygons)

  all_dfs$observations_df <- simulated_data$observed_polygons %>%
    dplyr::mutate(
      observation_collection_id = draw, time_left = time_left, time_right = time_right,
      qualified_name = location, primary = TRUE, phantom = FALSE, suspected_cases = cases,
      deaths = NA, confirmed_cases = NA
    )

  covariate_raster_funs <- convert_simulated_covariates_to_test_covariate_funs(simulated_data$covariates,
    min_time_left = min_time_left, max_time_right = max_time_right
  )

  return(list(dataframes = all_dfs, covariate_function_list = covariate_raster_funs))
}


create_shapes_with_names_view <- function(psql_connection, drop = FALSE) {
  add_query <- "
CREATE MATERIALIZED VIEW shapes_with_names AS
SELECT locations.qualified_name, locations.id as location_id, location_periods.id as location_period_id, shapes.id as shape_id, shapes.shape as geom
FROM locations inner join location_periods on locations.id = location_periods.location_id inner join shapes on location_periods.id = shapes.location_period_id
"

  index_queries <- c(
    "create index on shapes_with_names using gist(geom);", "create index on shapes_with_names(location_period_id);",
    "create index on shapes_with_names(shape_id);"
  )

  drop_query <- "DROP MATERIALIZED VIEW IF EXISTS shapes_with_names"
  add_and_or_drop(psql_connection, c(add_query, index_queries), drop_query, drop)
}

create_shape_resized_spatial_grid_map_view <- function(psql_connection, drop = FALSE) {
  add_query <- "
CREATE MATERIALIZED VIEW IF NOT EXISTS shape_resized_spatial_grid_map_view AS(
SELECT  l.qualified_name, l.location_period_id as location_period_id, l.shape_id, p.id as grid_id, ST_Intersection(p.polygon, l.geom) as intersection_geom, p.polygon as grid_geom
FROM
  grids.resized_spatial_grid_pixels p,
  shapes_with_names l
WHERE
ST_IsValid(l.geom)
AND ST_Intersects(p.polygon, ST_Boundary(l.geom))
);
"

  index_queries <- c(
    "create index on shape_resized_spatial_grid_map_view using gist(intersection_geom);",
    "create index on shape_resized_spatial_grid_map_view using gist(grid_geom);",
    "create index on shape_resized_spatial_grid_map_view(grid_id);", "create index on shape_resized_spatial_grid_map_view(location_period_id);",
    "create index on shape_resized_spatial_grid_map_view(shape_id);"
  )

  drop_query <- "DROP MATERIALIZED VIEW IF EXISTS shape_resized_spatial_grid_map_view"
  add_and_or_drop(psql_connection, c(add_query, index_queries), drop_query, drop)

  invisible(NULL)
}

create_shape_resized_spatial_grid_populations_view <- function(psql_connection, drop = FALSE) {
  add_query <- "
CREATE MATERIALIZED VIEW shape_resized_spatial_grid_populations as (select
  shape_resized_spatial_grid_map_view.location_period_id,
  shape_resized_spatial_grid_map_view.shape_id,
  shape_resized_spatial_grid_map_view.grid_id,
  all_covariates.time_left,
  all_covariates.time_right,
  all_covariates.covariate_name,
  (st_summarystats(st_clip(rast,intersection_geom))).sum as intersection_population,
  (st_summarystats(st_clip(rast,grid_geom))).sum as grid_population
FROM
  shape_resized_spatial_grid_map_view
INNER JOIN
  covariates.all_covariates
    on
      st_intersects(shape_resized_spatial_grid_map_view.intersection_geom, st_envelope(all_covariates.rast))
WHERE
  covariate_name = 'population');
"

  drop_query <- "DROP MATERIALIZED VIEW IF EXISTS shape_resized_spatial_grid_populations"
  add_and_or_drop(psql_connection, add_query, drop_query, drop)
  invisible(NULL)
}

create_resized_covariates_view <- function(psql_connection, drop = FALSE) {
  add_query <- "
CREATE MATERIALIZED VIEW resized_covariates as (
select
  resized_spatial_grid_pixels.rid as grid_rid,
  resized_spatial_grid_pixels.id as grid_id,
  all_covariates.covariate_name,
  all_covariates.time_left,
  all_covariates.time_right,
  all_covariates.rid,
  (st_summarystats(st_clip(all_covariates.rast, resized_spatial_grid_pixels.polygon))).sum
from
  grids.resized_spatial_grid_pixels
inner join
  covariate_grid_map
    on
      resized_spatial_grid_pixels.rid = covariate_grid_map.grid_rid
      AND resized_spatial_grid_pixels.height = covariate_grid_map.height
      AND resized_spatial_grid_pixels.width = covariate_grid_map.width
inner join
  covariates.all_covariates
    on
      all_covariates.covariate_name = covariate_grid_map.covariate_name
      and covariate_grid_map.time_left = all_covariates.time_left
      and covariate_grid_map.time_right = all_covariates.time_right
      and covariate_grid_map.covar_rid = all_covariates.rid
      and st_intersects(st_envelope(all_covariates.rast), resized_spatial_grid_pixels.polygon)
);
"
  drop_query <- "DROP MATERIALIZED VIEW IF EXISTS resized_covariates"
  add_and_or_drop(psql_connection, add_query, drop_query, drop)
  invisible(NULL)
}


#' @export
#' @name cast_to_int32
#' @title cast_to_int32
#' @description For casting int64 to 32 bit integers since R is bad at dealing with them mostly
#' @param x A 64 bit integer (see bit64)
#' @return A number equal to x
cast_to_int32 <- function(x) {
  if (!is.integer(x)) {
    rc <- as.integer(x)
    if (all(rc == x)) {
      return(rc)
    }
    stop(paste("Conversion failed", x[rc != x], "converted to", rc[rc != x]))
  }
  return(x)
}
