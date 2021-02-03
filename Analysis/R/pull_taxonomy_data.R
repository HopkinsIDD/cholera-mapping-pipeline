## Pull case data --------------------------------------------------------------

pull_taxonomy_data <- function(countries_name, map_name, dbuser, data_source) {
  print("Obtaining cholera taxonomy data")

### Set authentication details
  if (data_source == "api") {
    countries <- sapply(countries_name, taxdat::fix_country_name)
    who_region <- sapply(countries_name, taxdat::lookup_WHO_region)
    long_countries <- paste(
      who_region,
      gsub("_", "::", countries_name),
      sep = "::"
    )
    worldpop_region <- unique(sapply(
      countries_name,
      taxdat::lookup_WorldPop_region
    ))
    username <- Sys.getenv("CHOLERA_API_USERNAME", "NONE")
    password <- Sys.getenv("CHOLERA_API_KEY", "NONE")
    if (any(c(username, password) == "NONE")) {
      source("Analysis/R/database_api_key.R")
      username <- database_username
      password <- database_api_key
    }
  } else if (data_source == "sql") {
    long_countries <- countries
    username <- Sys.getenv("CHOLERA_SQL_USERNAME", "NONE")
    password <- Sys.getenv("CHOLERA_SQL_PASSWORD", "NONE")
    if (any(c(username, password) == "NONE")) {
      source("Analysis/R/database_api_key.R")
      username <- taxonomy_username
      password <- taxonomy_password
    }
  } else {
    stop("Unknown data source, must be one of 'api', 'sql', found ", data_source)
  }

### Pull the case and location data
  ## This pulls the data from either
  ## - The web api (available everywhere)
  ## - The postgres database (only on idmodeling2)
  cases_raw <- taxdat::pull_taxonomy_data_raw(
    username = username,
    password = password,
    locations = long_countries,
    time_left = start_time,
    time_right = end_time,
    source = data_source
  )

  ## Drop the locations and keep only the case data for now
  cases <- taxdat::rename_database_fields(cases_raw$observations)
  shapefiles <- cases$location_periods

  ## Sanity checks
                                        # (There should be at least one report)
  if (nrow(cases) == 0) {
    stop("No observations were found.")
  } else {
    print(paste(nrow(cases), "observations were found."))
  }


### Write cases to postgres
                                        # Table name
  cases_table_name <- taxdat::create_table_name(map_name, "raw_cases")
                                        # Create table
  taxdat::create_table_from_data_frame(
    df = cases,
    user_name = dbuser,
    table_name = cases_table_name,
    overwrite = config$cases$overwrite
  )

                                        # Create a view with relevant cases
  taxdat::create_view_from_table_and_string(
    table_name = cases_table_name,
    view_name = taxdat::create_table_name(map_name, "preprocessed_cases"),
    filters = c("{case_column} is not NULL", "is_primary")
  )

                                        # More sanity checks
  if (
    taxdat::database_table_nrow(
      taxdat::create_table_name(map_name, "preprocessed_cases")
    ) == 0
  ) {
    stop("No primary, non-NA observations were found.")
  } else {
    print(paste(nrow(cases), "primary, non-NA observations were found."))
  }

  ## Shapefiles ----------------------------------------------------------------

### Perform checks on shapefiles
  shapefiles_valid <- taxdat::is_shapefile_valid(shapefiles, quiet = TRUE)

  if (!all(shapefiles_valid$valid)) {
    warning(
      "There was a problem with at least one shapefile.  See output for details"
    )
    sf::st_set_geometry(shapefiles_valid, NULL)
    print("The following shapefiles are invalid for reasons listed below")
    print(as.data.frame(shapefiles_valid))
  }

### Write shapefiles to postgres
  shapefiles <- sf::st_cast(
    shapefiles[shapefiles_valid$valid, ],
    "MULTIPOLYGON"
  )
                                        # Table name
  location_periods_table_name <- taxdat::create_table_name(
    map_name,
    "raw_geometries"
  )
                                        # Create table
  taxdat::create_table_from_sf(
    df = shapefiles[shapefiles_valid$valid],
    user_name = dbuser,
    table_name = location_periods_table_name,
    overwrite = config$cases$overwrite
  )

### Determine which observation collections have problems with shapefiles
  query <- "SELECT
  observations.observation_collection_id,
  sum({cases_table_name}.{cases_column}) as missing_cases,
  STRING_AGG({cases_table_name}.location_period_id) as affected_location_periods
FROM
  {cases_table_name}
    LEFT JOIN
  {location_periods_table_name}
    on
    {location_periods_table_name}.id = {cases_table_name}.location_period_id
  WHERE
    {location_periods_table_name}.id is NULL
  GROUP BY
    {cases_table_name}.observation_collection_id
  ORDER BY
    sum({cases_table_name}.{cases_column})
"
  conn_pg <- taxdat::connect_to_db(dbuser)
  oc_without_shapefiles <- DBI::dbGetQuery(
    conn_pg,
    glue::glue_sql(.con = conn_pg, query)
  )
  DBI::dbDisconnect(conn_pg)
  if (nrow(oc_without_shapefiles) > 0) {
    warning(paste(
      "At least one observation could not be linked to a shapefile",
      "See output for details"
    ))
    print(as.data.frame(
      oc_without_shapefiles
    ))
  }

### Create view of observations with appropriate shapefiles
  taxdat::create_view_from_table_and_string(
    table_name = cases_table_name,
    view_name = taxdat::create_table_name(map_name, "modeling_cases"),
    filters = paste(
      "{cases_table_name}.location_period_id in",
      "{location_periods_table_name}.id"
    )
  )
  conn_pg <- taxdat::connect_to_db(dbuser)

### close database
  DBI::dbDisconnect(conn_pg)

}
