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

#' @description Create a resized_spatial_grids table for use in testing
#' @name create_resized_spatial_grids_table
#' @title create_resized_spatial_grids_table
#' @param psql_connection a connection to a database made with dbConnect
#' @param drop Whether to drop an existing table
create_resized_spatial_grids_table <- function(psql_connection, drop = FALSE) {
    drop_query <- "DROP TABLE IF EXISTS grids.resized_spatial_grids CASCADE;"
    add_query <- c("CREATE TABLE grids.resized_spatial_grids(rid integer, rast raster, width int, height int);", 
        "CREATE INDEX grids_resized_spatial_grids_size_idx ON grids.resized_spatial_grids (width, height, rid);")

    add_and_or_drop(psql_connection, add_query, drop_query, drop)
}

#' @description Create a all_covariates table for use in testing
#' @name create_all_covariates_table
#' @title create_all_covariates_table
#' @param psql_connection a connection to a database made with dbConnect
#' @param drop Whether to drop an existing table
create_all_covariates_table <- function(psql_connection, drop = FALSE) {
    schema_query <- "CREATE SCHEMA IF NOT EXISTS covariates;"
    drop_query <- "DROP TABLE IF EXISTS covariates.all_covariates CASCADE;"
    add_query <- c("CREATE TABLE covariates.all_covariates(covariate_name text, time_left date, time_right date, rid integer, rast raster);", 
        "CREATE INDEX covariate_name_size_idx ON covariates.all_covariates(covariate_name, time_left, time_right, rid);")

    add_and_or_drop(psql_connection, c(schema_query, add_query), drop_query, drop)
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
create_master_temporal_grid_view <- function(psql_connection) {
    add_query <- "
CREATE OR REPLACE VIEW grids.master_temporal_grid AS
SELECT
  generate_series(
    time_bounds.time_left::timestamp,
    time_bounds.time_right::timestamp,
    '1 day'::interval
  )::date as time_midpoint
FROM grids.time_bounds;"

    DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, add_query))
}

#' @description Create a resized_spatial_grid_polygons view for use in testing
#' @name create_resized_spatial_grid_polygons_view
#' @title create_resized_spatial_grid_polygons_view
#' @param psql_connection a connection to a database made with dbConnect
create_resized_spatial_grid_polygons_view <- function(psql_connection) {
    add_query <- "
CREATE MATERIALIZED VIEW IF NOT EXISTS grids.resized_spatial_grid_polygons
AS
  SELECT
    rid,
    width,
    height,
    dp.*
  FROM
    grids.resized_spatial_grids, LATERAL ST_PixelAsPolygons(rast, 1) AS dp;"

    DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, add_query))
}

#' @description Create a resized_spatial_grid_centroids view for use in testing
#' @name create_resized_spatial_grid_centroids_view
#' @title create_resized_spatial_grid_centroids_view
#' @param psql_connection a connection to a database made with dbConnect
create_resized_spatial_grid_centroids_view <- function(psql_connection, drop = FALSE) {
    add_query <- "
CREATE MATERIALIZED VIEW IF NOT EXISTS grids.resized_spatial_grid_centroids
AS
  SELECT
    rid,
    width,
    height,
    dp.*
  FROM
    grids.resized_spatial_grids, LATERAL ST_PixelAsCentroids(rast, 1) AS dp;"

    DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, add_query))
}

#' @description Create a location_period_raster_map_view for use in testing
#' @name create_location_period_raster_map_view
#' @title create_location_period_raster_map_view
#' @param psql_connection a connection to a database made with dbConnect
create_location_period_raster_map_view <- function(psql_connection) {
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

    DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, add_query))
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
      st_intersects(all_covariates.rast, resized_spatial_grids.rast);"

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
    create_resized_spatial_grids_table(psql_connection, drop)
    create_all_covariates_table(psql_connection, drop)
    create_time_bounds_table(psql_connection, drop)
    create_master_temporal_grid_view(psql_connection)
    create_resized_spatial_grid_polygons_view(psql_connection)
    create_resized_spatial_grid_centroids_view(psql_connection)
    create_location_period_raster_map_view(psql_connection)
    create_covariate_grid_map_view(psql_connection)
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
    ) as rast
  FROM
    grids.master_spatial_grid;
  $$ LANGUAGE SQL;"

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
  $$ LANGUAGE SQL;"

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

create_ingest_resized_spatial_grid_function <- function(psql_connection) {
    function_query <- "
create or replace function ingest_resized_spatial_grid(width_in_km int, height_in_km int)
  returns void
  LANGUAGE plpgsql
  as $$
  BEGIN
  PERFORM * FROM grids.resized_spatial_grids
    WHERE resized_spatial_grids.width = width_in_km AND resized_spatial_grids.height = height_in_km;
  IF NOT FOUND THEN
     INSERT INTO grids.resized_spatial_grids
     SELECT rid, rast, width_in_km AS width, height_in_km AS height FROM resize_spatial_grid(width_in_km, height_in_km);
  END IF;
  END;
  $$ SECURITY DEFINER;"

    DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, function_query))
    invisible(NULL)
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
  $$ SECURITY DEFINER;"

    DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, function_query))
    invisible(NULL)
}

create_pull_grid_adjacency_function <- function(psql_connection) {
    create_filter_resized_spatial_grid_centroids_to_location_function(psql_connection)
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
  FROM filter_resized_spatial_grid_centroids_to_location(location_name, width_in_km, height_in_km) AS lhs
    INNER JOIN
      grids.resized_spatial_grid_polygons AS lhs_poly
        ON
          lhs.rid = lhs_poly.rid AND
          lhs.x = lhs_poly.x AND
          lhs.y = lhs_poly.y
    INNER JOIN
      grids.resized_spatial_grid_polygons AS rhs_poly
        ON
          ST_INTERSECTS(st_buffer(lhs_poly.geom, sqrt(st_area(lhs_poly.geom))*.01), rhs_poly.geom)
    INNER JOIN
      filter_resized_spatial_grid_centroids_to_location(location_name, width_in_km, height_in_km) AS rhs
        ON
          rhs.rid = rhs_poly.rid AND
          rhs.x = rhs_poly.x AND
          rhs.y = rhs_poly.y

  WHERE
    lhs.id < rhs.id AND
    lhs_poly.width = width_in_km AND
    lhs_poly.height = height_in_km AND
    rhs_poly.width = width_in_km AND
    rhs_poly.height = height_in_km
  $$ LANGUAGE SQL;"

    DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, function_query))
    invisible(NULL)
}

create_pull_symmetric_grid_adjacency_function <- function(psql_connection) {
    create_filter_resized_spatial_grid_centroids_to_location_function(psql_connection)
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
  FROM filter_resized_spatial_grid_centroids_to_location(location_name, width_in_km, height_in_km) AS lhs
    INNER JOIN
      grids.resized_spatial_grid_polygons AS lhs_poly
        ON
          lhs.rid = lhs_poly.rid AND
          lhs.x = lhs_poly.x AND
          lhs.y = lhs_poly.y
    INNER JOIN
      grids.resized_spatial_grid_polygons AS rhs_poly
        ON
          ST_INTERSECTS(st_buffer(lhs_poly.geom, sqrt(st_area(lhs_poly.geom))*.01), rhs_poly.geom)
    INNER JOIN
      filter_resized_spatial_grid_centroids_to_location(location_name, width_in_km, height_in_km) AS rhs
        ON
          rhs.rid = rhs_poly.rid AND
          rhs.x = rhs_poly.x AND
          rhs.y = rhs_poly.y

  WHERE
    lhs.id != rhs.id AND
    lhs_poly.width = width_in_km AND
    lhs_poly.height = height_in_km AND
    rhs_poly.width = width_in_km AND
    rhs_poly.height = height_in_km
  $$ LANGUAGE SQL;"

    DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, function_query))
}

create_filter_resized_spatial_grid_centroids_to_location_function <- function(psql_connection) {
    function_query <- "
create or replace function filter_resized_spatial_grid_centroids_to_location(location_name text, width_in_km int, height_in_km int)
  returns table(id bigint, rid int, x int, y int, geom geometry) AS $$
  SELECT
    ROW_NUMBER() OVER (ORDER BY 1) as id,
    resized_spatial_grid_centroids.rid,
    resized_spatial_grid_centroids.x,
    resized_spatial_grid_centroids.y,
    resized_spatial_grid_centroids.geom
  FROM
    locations
  LEFT JOIN
    location_periods
      ON
        locations.id = location_periods.location_id
  LEFT JOIN
    shapes
      ON
        location_periods.id = shapes.location_period_id
  LEFT JOIN
    location_period_raster_map
      ON
        location_period_raster_map.shape_id = shapes.id
  LEFT JOIN
    grids.resized_spatial_grid_centroids
      ON
        location_period_raster_map.rid = resized_spatial_grid_centroids.rid AND
        resized_spatial_grid_centroids.width = location_period_raster_map.width AND
        resized_spatial_grid_centroids.height = location_period_raster_map.height AND
\tst_contains(shapes.shape, resized_spatial_grid_centroids.geom)
  WHERE
    locations.qualified_name = location_name AND
    resized_spatial_grid_centroids.width = width_in_km AND
    resized_spatial_grid_centroids.height = height_in_km
  $$ LANGUAGE SQL;"

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
  $$ LANGUAGE SQL;"

    DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, function_query))
    invisible(NULL)
}

create_pull_observation_data_function <- function(psql_connection) {
    create_filter_location_periods_function(psql_connection)
    function_query <- "
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
\t    observations.location_period_id = filtered_location_periods.location_period_id
      LEFT JOIN
        shapes
\t  ON
\t    observations.location_period_id = shapes.location_period_id
  WHERE
    observations.time_left >= start_date AND
    observations.time_right <= end_date;
  $$ LANGUAGE SQL;"

    DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, function_query))
    invisible(NULL)
}

create_pull_location_period_grid_map_function <- function(psql_connection) {
    function_query <- "
create or replace function pull_location_period_grid_map(location_name text, width_in_km int, height_in_km int)
RETURNS TABLE(qualified_name text, location_id bigint, location_period_id bigint, shape_id bigint, spatial_grid_id bigint, rid int, x int, y int)AS $$
  SELECT
    location_periods.qualified_name as qualified_name,
    location_periods.location_id as location_id,
    location_periods.location_period_id as location_period_id,
    shapes.id as shape_id,
    spatial_grid.id as spatial_grid_id,
    spatial_grid.rid,
    spatial_grid.x,
    spatial_grid.y
  FROM
    filter_location_periods(location_name) as location_periods
      LEFT JOIN
    shapes
      on
        location_periods.location_period_id = shapes.location_period_id
      LEFT JOIN
    filter_resized_spatial_grid_centroids_to_location(location_name, width_in_km, height_in_km) as spatial_grid
      on
        ST_CONTAINS(shapes.shape, spatial_grid.geom);
  $$ LANGUAGE SQL;"

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
$$ LANGUAGE plpgsql;"

    DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, function_query))
    invisible(NULL)
}

create_pull_covar_cube_function <- function(psql_connection) {
    create_resize_temporal_grid_function(psql_connection)
    function_query <- "
create or replace function pull_covar_cube(location_name text, start_date date, end_date date, width_in_km int, height_in_km int, time_scale text)
RETURNS TABLE(covariate_name text, t bigint, id bigint, rid int, x int, y int, value double precision)AS $$
  SELECT
    all_covariates.covariate_name,
    temporal_grid.id as t,
    grid_centroids.id,
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
    LEFT JOIN
      covariates.all_covariates
        ON
          covariate_grid_map.covar_rid = all_covariates.rid AND
          covariate_grid_map.time_left = all_covariates.time_left AND
          covariate_grid_map.time_right = all_covariates.time_right AND
          covariate_grid_map.width = width_in_km AND
          covariate_grid_map.height = height_in_km AND
          all_covariates.time_left <= temporal_grid.time_midpoint AND
            all_covariates.time_right >= temporal_grid.time_midpoint AND
          covariate_grid_map.covariate_name = all_covariates.covariate_name AND
          st_intersects(all_covariates.rast, grid_centroids.geom)
  WHERE
    temporal_grid.time_midpoint >= start_date AND
    temporal_grid.time_midpoint <= end_date
  $$ LANGUAGE SQL;"

    DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, function_query))
    invisible(NULL)
}

create_pull_observation_location_period_map <- function(psql_connection) {
    function_query <- "
create or replace function pull_observation_location_period_map(location_name text, start_date date, end_date date)
  returns table(
    observation_id bigint,
    location_period_id bigint
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
  $$ LANGUAGE SQL;"

    DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, function_query))
    invisible(NULL)
}

#' @description Create the functions we will use as part of the testing database
#' @name create_testing_database_functions
#' @title create_testing_database_functions
#' @param psql_connection a connection to a database made with dbConnect
create_testing_database_functions <- function(psql_connection) {
    create_resize_spatial_grid_function(psql_connection)
    create_lookup_location_period_function(psql_connection)
    create_ingest_resized_spatial_grid_function(psql_connection)
    create_ingest_covariate_function(psql_connection)
    create_pull_grid_adjacency_function(psql_connection)
    create_pull_symmetric_grid_adjacency_function(psql_connection)
    create_pull_observation_data_function(psql_connection)
    create_pull_covar_cube_function(psql_connection)
    create_pull_observation_location_period_map(psql_connection)
    create_pull_location_period_grid_map_function(psql_connection)
}

#' @description Refresh the materialized views in the database to account for new data
#' @name refresh_sql_materialized_views
#' @title refresh_sql_materialized_views
#' @param psql_connection a connection to a database made with dbConnect
#' @export
refresh_materialized_views <- function(psql_connection) {
    queries <- paste("REFRESH MATERIALIZED VIEW", c("grids.master_spatial_grid", 
        "grids.resized_spatial_grid_polygons", "grids.resized_spatial_grid_centroids", 
        "location_period_raster_map", "covariate_grid_map"))
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

#' @export
setup_testing_database_from_dataframes <- function(psql_connection, data_frame_list, 
    covariate_creation_function_list) {
    setup_testing_database(psql_connection, drop = TRUE)
    insert_testing_locations(psql_connection, data_frame_list$location_df)
    insert_testing_location_periods(psql_connection, data_frame_list$location_period_df)
    insert_testing_shapefiles(psql_connection, data_frame_list$shapes_df)
    refresh_materialized_views(psql_connection)
    ingest_spatial_grid(psql_connection, width = 1, height = 1)
    refresh_materialized_views(psql_connection)
    insert_testing_observations(psql_connection, data_frame_list$observations_df)
    for (fun in covariate_creation_function_list) {
        ingest_covariate(psql_connection, fun$name, fun$fun(psql_connection), fun$start_date, 
            fun$end_date)
    }
    refresh_materialized_views(psql_connection)
}

drop_testing_database_functions <- function(psql_connection) {
    drop_queries <- c("DROP FUNCTION resize_spatial_grid", "DROP FUNCTION lookup_location_period", 
        "DROP FUNCTION ingest_resized_spatial_grid", "DROP FUNCTION ingest_covariate", 
        "DROP FUNCTION filter_resized_spatial_grid", "DROP FUNCTION filter_resized_spatial_grid_centroids_to_location", 
        "DROP FUNCTION pull_symmetric_grid_adjacency", "DROP FUNCTION pull_observation_data", 
        "DROP FUNCTION filter_location_periods", "DROP FUNCTION resize_temporal_grid", 
        "DROP FUNCTION pull_observation_location_period_map", "DROP FUNCTION pull_location_period_grid_map")

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
    drop_query <- c("DROP TABLE IF EXISTS location_periods CASCADE", "DROP TABLE IF EXISTS locations CASCADE", 
        "DROP TABLE IF EXISTS shapes CASCADE", "DROP TABLE IF EXISTS observations CASCADE", 
        "DROP TABLE IF EXISTS location_hierarchies CASCADE", "DROP MATERIALIZED VIEW IF EXISTS grids.master_spatial_grid CASCADE", 
        "DROP TABLE IF EXISTS grids.resized_spatial_grids CASCADE", "DROP TABLE IF EXISTS grids.time_bounds CASCADE", 
        "DROP TABLE IF EXISTS covariates.all_covariates CASCADE", "DROP MATERIALIZED VIEW IF EXISTS grids.resized_spatial_grid_polygons", 
        "DROP MATERIALIZED VIEW IF EXISTS grids.resized_spatial_grid_centroids", 
        "DROP MATERIALIZED VIEW IF EXISTS location_period_raster_map", "DROP MATERIALIZED VIEW IF EXISTS covariate_grid_map CASCADE", 
        "DROP SCHEMA IF EXISTS covariates CASCADE")
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
    api_results <- read_taxonomy_data_api(username, api_key, locations, time_left, 
        time_right, uids, website)
    location_df <- api_results %>%
        sf::st_drop_geometry() %>%
        dplyr::mutate(qualified_name = gsub("[.][^:]*$", "", attributes.location_name)) %>%
        dplyr::select(qualified_name) %>%
        dplyr::group_by(qualified_name) %>%
        dplyr::summarize(.groups = "drop")

    location_period_df <- api_results %>%
        sf::st_drop_geometry() %>%
        dplyr::mutate(qualified_name = gsub("[.][^:]*$", "", attributes.location_name), 
            start_date = lubridate::ymd(attributes.time_left), end_date = lubridate::ymd(attributes.time_right)) %>%
        dplyr::select(sf_id, qualified_name, start_date, end_date) %>%
        dplyr::group_by(qualified_name, sf_id) %>%
        dplyr::summarize(start_date = min(start_date), end_date = max(end_date), 
            .groups = "drop") %>%
        dplyr::select(qualified_name, start_date, end_date)

    shapes_df <- api_results %>%
        dplyr::mutate(qualified_name = gsub("[.][^:]*$", "", attributes.location_name), 
            start_date = lubridate::ymd(attributes.time_left), end_date = lubridate::ymd(attributes.time_right)) %>%
        dplyr::select(sf_id, qualified_name, start_date, end_date) %>%
        dplyr::group_by(qualified_name, sf_id) %>%
        dplyr::summarize(start_date = min(start_date), end_date = max(end_date), 
            .groups = "drop") %>%
        dplyr::select(qualified_name, start_date, end_date)
    names(shapes_df)[[4]] <- "geom"
    sf::st_geometry(shapes_df) <- "geom"

    observations_df <- api_results %>%
        dplyr::mutate(qualified_name = gsub("[.][^:]*$", "", attributes.location_name), 
            time_left = lubridate::ymd(attributes.time_left), time_right = lubridate::ymd(attributes.time_right), 
            observation_collection_id = relationships.observation_collection.data.id, 
            primary = attributes.primary, phantom = attributes.phantom, suspected_cases = attributes.fields.suspected_cases, 
            confirmed_cases = attributes.fields.confirmed_cases, deaths = attributes.fields.deaths)

    return(list(location_df = location_df, location_period_df = location_period_df, 
        shapes_df = shapes_df, observations_df = observations_df))
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
        ")", collapse = ", "))

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
            assert(length(location_id) > 0, paste("Failed to create the location", 
                location))
            ## add to location hierarchy
            hierarchy_query <- glue::glue_sql(.con = psql_connection, "INSERT INTO location_hierarchies VALUES ({location_id}, {full_location_id}, {length(partial_locations) - partial_location_idx})")
            DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, hierarchy_query))
        }
    }

    invisible(NULL)
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

    location_period_df$location_id <- DBI::dbGetQuery(conn = psql_connection, glue::glue_sql(.con = psql_connection, 
        "SELECT id FROM locations where qualified_name in ({location_period_df[[\"qualified_name\"]]*})"))[["id"]]

    insert_query <- paste("INSERT INTO location_periods(\"location_id\", \"start_date\", \"end_date\") VALUES", 
        paste("(", glue::glue_sql(.con = psql_connection, "{location_period_df[['location_id']]}"), 
            ",", glue::glue_sql(.con = psql_connection, "{location_period_df[['start_date']]}"), 
            ",", glue::glue_sql(.con = psql_connection, "{location_period_df[['end_date']]}"), 
            ")", collapse = ", "))
    DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, insert_query))
    invisible(NULL)
}

#' @description Insert shapefiles into the testing database
#' @name insert_testing_shapefiles
#' @title insert_testing_shapefiles
#' @param psql_connection a connection to a database made with dbConnect
#' @param shape_df A data frame with one row per shape to insert with information about the shape
#' @export
insert_testing_shapefiles <- function(psql_connection, shape_df, create_location_periods = FALSE) {
    assert("qualified_name" %in% names(shape_df), "insert_testing_shapefiles cannot insert a shape without a qualified name")
    assert("start_date" %in% names(shape_df), "insert_testing_shapefiles cannot insert a shape without a start date")
    assert("end_date" %in% names(shape_df), "insert_testing_shapefiles cannot insert a shape without an end date")
    assert("geom" %in% names(shape_df), "insert_testing_shapefiles cannot insert a shape without a geometry")

    if (create_location_periods) {
        stop("This functionality is not yet written")
    }

    shape_df$location_id <- DBI::dbGetQuery(conn = psql_connection, glue::glue_sql(.con = psql_connection, 
        "SELECT id FROM locations where qualified_name in ({shape_df[[\"qualified_name\"]]*})"))[["id"]]

    shape_df$location_period_id <- shape_df$location_id
    for (row_idx in seq_len(nrow(shape_df))) {
        location_period_query <- "
SELECT lookup_location_period(
  {shape_df$location_id[row_idx]},
  {shape_df$start_date[row_idx]},
  {shape_df$end_date[row_idx]}
)"
        location_period_query <- glue::glue_sql(.con = psql_connection, location_period_query)
        shape_df$location_period_id[row_idx] <- DBI::dbGetQuery(conn = psql_connection, 
            location_period_query)[["lookup_location_period"]]
    }
    # shape_df$location_period_id <- DBI::dbGetQuery(conn = psql_connection,
    # location_period_query)[['id']]
    shape_df$box <- sf::st_as_sfc(sf::st_bbox(shape_df[["geom"]]))

    insert_query <- paste("INSERT INTO shapes(\"location_period_id\", \"shape\", \"box\") VALUES", 
        paste("(", glue::glue_sql(.con = psql_connection, "{shape_df[['location_period_id']]}"), 
            ",", glue::glue_sql(.con = psql_connection, "{sf::st_as_text(shape_df[['geom']])}"), 
            ",", glue::glue_sql(.con = psql_connection, "{sf::st_as_text(shape_df[['box']])}"), 
            ")", collapse = ", "))
    srid <- sf::st_crs(shape_df)$epsg
    if (is.na(srid)) {
        srid <- 4326
    }
    srid_query <- glue::glue_sql(.con = psql_connection, "UPDATE shapes set shape = st_setsrid(shape, {srid}), box = st_setsrid(box, {srid})")

    DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, insert_query))
    DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, srid_query))
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
    assert(any(c("suspected_cases", "confirmed_cases", "deaths") %in% names(observation_df)), 
        "insert_testing_observations cannot insert a shape without at least one case column")

    location_id_query <- glue::glue_sql(.con = psql_connection, "SELECT id FROM LOCATIONS WHERE qualified_name = {observation_df[[\"qualified_name\"]]}")
    observation_df$location_id <- sapply(location_id_query, function(query) {
        as.character(DBI::dbGetQuery(conn = psql_connection, query)[["id"]])
    })


    insert_query <- paste("INSERT INTO observations(\"observation_collection_id\", \"time_left\", \"time_right\", \"location_period_id\", \"location_id\", \"primary\", \"phantom\", \"suspected_cases\", \"confirmed_cases\", \"deaths\") VALUES", 
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
            ")", collapse = ", "))

    DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, insert_query))
}

#' @description Tell the postgres database to create a resampled grid with appropriate grid size
#' @param psql_connection a connection to a database made with dbConnect
#' @param width width of the grid cells in km
#' @param heigh height of the grid cells in km
#' @export
ingest_spatial_grid <- function(psql_connection, width = 1, height = 1, do_refresh = TRUE) {
    if (do_refresh) {
        refresh_materialized_views(psql_connection)
    }
    ingest_query <- "SELECT ingest_resized_spatial_grid({width}, {height});"
    ingest_query <- glue::glue_sql(.con = psql_connection, ingest_query)
    DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, ingest_query))
    invisible(NULL)
}

#' @description Tell the postgres database to create a resampled grid with appropriate grid size
#' @param psql_connection a connection to a database made with dbConnect
#' @param width width of the grid cells in km
#' @param heigh height of the grid cells in km
#' @export
ingest_covariate <- function(psql_connection, covariate_name, covariate_raster, time_left, 
    time_right) {
    assert(!is.null(time_left), "ingest_covariate requires a time_left argument")
    assert(!is.null(time_right), "ingest_covariate requires a time_right argument")
    assert(!is.null(covariate_name), "ingest_covariate requires a covariate_name argument")
    table_name <- paste(covariate_name, time_left, time_right, sep = "_")
    table_name <- gsub("-", "_", table_name)
    table_name <- gsub("[.]", "_", table_name)
    rpostgis::pgWriteRast(raster = covariate_raster, conn = psql_connection, name = c("covariates", 
        table_name))
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
