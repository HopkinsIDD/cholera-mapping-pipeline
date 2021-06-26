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
  id bigint,
  observation_collection_id bigint,
  time_left date,
  time_right date,
  location_period_id bigint,
  location_id bigint,
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
    add_query <- "CREATE TABLE locations(id BIGSERIAL PRIMARY KEY, qualified_name text);"

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
  location_period_id bigint REFERENCES location_periods(id),
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
    add_query <- "CREATE TABLE location_hierarchies(ancestor_id bigint, descendant_id bigint, generations integer);"

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
    drop_query <- "DROP TABLE IF EXISTS covariates.all_covariates CASCADE;"
    add_query <- c("CREATE TABLE covariates.all_covariates(covariate_name text, time_left date, time_right date, rid integer, rast raster);", 
        "CREATE INDEX covariate_name_size_idx ON covariates.all_covariates(covariate_name, time_left, time_right, rid);")

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

    add_and_or_drop(psql_connection, add_query, drop_query, drop)
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

#' @description the database located at cholera-taxonomy.middle-distance.com
#' @name create_testing_base_database
#' @title create_testing_base_database
#' @param psql_connection a connection to a database made with dbConnect
#' @param drop Whether to drop existing database elements
create_testing_base_database <- function(psql_connection, drop = FALSE) {
    create_observations_table(psql_connection, drop)
    create_locations_table(psql_connection, drop)
    create_location_periods_table(psql_connection, drop)
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
    return(NULL)
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

create_lookup_location_period <- function(psql_connection) {
    function_query <- "
create or replace function lookup_location_period(location_id bigint, start_date date, end_date date)
  returns bigint
  as $$
  SELECT
    location_periods.id
  FROM
    location_periods
  WHERE
    location_periods.location_id = location_id AND
    location_periods.start_date <= start_date AND
    location_periods.end_date >= end_date
  $$ LANGUAGE SQL;"

    DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, function_query))
}

#' @description Create the functions we will use as part of the testing database
#' @name create_testing_database_functions
#' @title create_testing_database_functions
#' @param psql_connection a connection to a database made with dbConnect
create_testing_database_functions <- function(psql_connection) {
    create_resize_spatial_grid_function(psql_connection)
    create_lookup_location_period(psql_connection)
    warning("This function is not complete")
}

#' @description Refresh the materialized views in the database to account for new data
#' @name refresh_sql_materialized_views
#' @title refresh_sql_materialized_views
#' @param psql_connection a connection to a database made with dbConnect
#' @export
refresh_materialized_views <- function(psql_connection) {
    queries <- paste("REFRESH MATERIALIZED VIEW", c("grids.resized_spatial_grid_polygons", 
        "grids.resized_spatial_grid_centroids", "location_period_raster_map"))
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

#' @description delete a testing database
#' @name destory_testing_database
#' @title destroy_testing_database
#' @param psql_connection a connection to a database made with dbConnect
destroy_testing_database <- function(psql_connection) {
    drop_query <- c("DROP TABLE IF EXISTS location_periods CASCADE", "DROP TABLE IF EXISTS locations CASCADE", 
        "DROP TABLE IF EXISTS observations CASCADE", "DROP TABLE IF EXISTS location_hierarchies CASCADE", 
        "DROP MATERIALIZED VIEW IF EXISTS grids.master_spatial_grid CASCADE", "DROP TABLE IF EXISTS grids.resized_spatial_grids CASCADE", 
        "DROP TABLE IF EXISTS grids.time_bounds CASCADE", "DROP TABLE IF EXISTS covariates.all_covariates CASCADE", 
        "DROP MATERIALIZED VIEW IF EXISTS grids.resized_spatial_grid_polygons", "DROP MATERIALIZED VIEW IF EXISTS grids.resized_spatial_grid_centroids", 
        "DROP MATERIALIZED VIEW IF EXISTS location_period_raster_map", "DROP MATERIALIZED VIEW IF EXISTS covariate_grid_map CASCADE")
    sapply(drop_query, function(query) {
        DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, query))
    })
    return(NULL)
}

insert_testing_locations <- function(psql_connection, location_df) {
    assert("qualified_name" %in% names(location_df), "insert_testing_locations cannot insert a location without a qualified name")
    insert_query <- paste("INSERT INTO locations(\"qualified_name\") VALUES", paste("(", 
        glue::glue_sql(.con = psql_connection, "{location_df[['qualified_name']]}"), 
        ")", collapse = ", "))
    DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, insert_query))
    return(NULL)
}

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
    return(NULL)
}

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

    location_period_query <- paste("SELECT id FROM location_periods WHERE (\"location_id\", \"start_date\", \"end_date\") IN (", 
        paste("(", glue::glue_sql(.con = psql_connection, "{shape_df[['location_id']]}"), 
            ",", glue::glue_sql(.con = psql_connection, "{shape_df[['start_date']]}"), 
            ",", glue::glue_sql(.con = psql_connection, "{shape_df[['end_date']]}"), 
            ")", collapse = ", "), ")")

    shape_df$location_period_id <- DBI::dbGetQuery(conn = psql_connection, location_period_query)[["id"]]
    shape_df$box <- sf::st_as_sfc(sf::st_bbox(shape_df[["geom"]]))

    insert_query <- paste("INSERT INTO shapes(\"location_period_id\", \"shape\", \"box\") VALUES", 
        paste("(", glue::glue_sql(.con = psql_connection, "{shape_df[['location_period_id']]}"), 
            ",", glue::glue_sql(.con = psql_connection, "{sf::st_as_text(shape_df[['geom']])}"), 
            ",", glue::glue_sql(.con = psql_connection, "{sf::st_as_text(shape_df[['box']])}"), 
            ")", collapse = ", "))

    DBI::dbClearResult(DBI::dbSendQuery(conn = psql_connection, insert_query))
    return(NULL)
}

insert_testing_observations <- function(psql_connection, observation_df) {
    assert("observation_collection_id" %in% names(observation_df), "insert_testing_observations cannot insert a shape without a observation_collection_id")
    assert("phantom" %in% names(observation_df), "insert_testing_observations cannot insert a shape without a phantom")
    assert("primary" %in% names(observation_df), "insert_testing_observations cannot insert a shape without a primary")
    assert("qualified_name" %in% names(observation_df), "insert_testing_observations cannot insert a shape without a qualified name")
    assert("time_left" %in% names(observation_df), "insert_testing_observations cannot insert a shape without a time left")
    assert("time_right" %in% names(observation_df), "insert_testing_observations cannot insert a shape without a time right")
    assert(any(c("suspected_cases", "confirmed_cases", "deaths") %in% names(observation_df)), 
        "insert_testing_observations cannot insert a shape without at least one case column")

    observation_df$location_id <- DBI::dbGetQuery(conn = psql_connection, glue::glue_sql(.con = psql_connection, 
        "SELECT id FROM locations where qualified_name in ({observation_df[[\"qualified_name\"]]*})"))[["id"]]

    location_period_query <- paste("SELECT (", paste("lookup_location_period(", glue::glue_sql(.con = psql_connection, 
        "{observation_df[['location_id']]}"), ",", glue::glue_sql(.con = psql_connection, 
        "{observation_df[['time_left']]}"), ",", glue::glue_sql(.con = psql_connection, 
        "{observation_df[['time_right']]}"), ")", collapse = ", "), ")")
    warning(location_period_query)

    observation_df$location_period_id <- DBI::dbGetQuery(conn = psql_connection, 
        location_period_query)[["lookup_location_period"]]
    # observation_df$location_period_id <- c(1,2,1,2)

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
