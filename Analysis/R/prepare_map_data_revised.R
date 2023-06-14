# Preamble ---------------------------------------------------------------------

# Check if variables exist

setwd(cholera_directory)

# Shapefile check ------------------------------------------------------------
print("Starting Data Preparation Process")

print(paste("Saving output data to ", file_names[["data"]]))


# Define credentials for data pull
if (data_source == "api") {
  # NEED TO ADD WHO REGION LOOKUP AND APPEND FOR ALL LOCATIONS
  countries <- sapply(countries_name, taxdat::fix_country_name)
  who_region <- sapply(countries_name, taxdat::lookup_WHO_region)
  long_countries <- paste("CT-World", who_region, gsub("_", "::", countries_name), sep = "::")
  worldpop_region <- unique(sapply(countries_name, taxdat::lookup_WorldPop_region))
  username <- Sys.getenv("CHOLERA_API_USERNAME", "NONE")
  password <- Sys.getenv("CHOLERA_API_KEY", "NONE")
  website <- Sys.getenv("CHOLERA_API_WEBSITE", "")
  if (any(c(username, password) == "NONE")) {
    source("Analysis/R/database_api_key.R")
    username <- database_username
    password <- database_api_key
  }
  cat("cntry:", long_countries, "u:", ifelse(nchar(username) > 0, "****", ""), "psswd:", ifelse(nchar(password) > 0, "****", ""), "st:", start_time, "et:", end_time, "\n")
} else if (data_source == "sql") {
  long_countries <- countries
  username <- Sys.getenv("CHOLERA_SQL_USERNAME", "NONE")
  password <- Sys.getenv("CHOLERA_SQL_PASSWORD", "NONE")
  website <- Sys.getenv("CHOLERA_SQL_WEBSITE", "")
  if (any(c(username, password) == "NONE")) {
    source("Analysis/R/database_api_key.R")
    try(
      {
        username <- taxonomy_username
        password <- taxonomy_password
      },
      silent = TRUE
    )
  }
} else {
  stop("Unknown data source, must be one of 'api', 'sql', found ", data_source)
}

# This pulls the data either from the mid-distance database or the postgresql
# database on idmodeling2
cases <- taxdat::pull_taxonomy_data(
  username = username,
  password = password,
  locations = long_countries,
  time_left = start_time,
  time_right = end_time,
  source = data_source,
  uids = OCs,
  website = website
) %>%
  taxdat::rename_database_fields(source = data_source)

index <- sf::st_geometry_type(cases) == sf::st_geometry_type(sf::st_geometrycollection())
sf::st_geometry(cases[index, ]) <- sf::st_as_sfc(lapply(sf::st_geometry(cases[index, ]), sf::st_collection_extract, type = "POLYGON"))

# Get OC UIDs for all extracted data
uids <- sort(unique(as.numeric(cases$OC_UID)))

# Filter out NA cases, which represent missing observations, and non-primary observations (primary observations are only space-time stratified and these are the ones we want to focus on in these maps)
cases <- dplyr::filter(cases, !is.na(.data[[cases_column]])) %>%
  dplyr::filter(is_primary) %>%
  dplyr::mutate(shapefile.exists = !is.na(sf::st_dimension(geojson)) & (sf::st_dimension(geojson) > 0))

if ("truncate" %in% names(config)) {
  if (!all(c("method", "smallest_spatial_scale") %in% names(config$truncate))) {
    stop("In order to truncate, we require both an aggregation `method` and `smallest_spatial_scale` to truncate to.")
  }
  if (config$truncate$method == "truncate") {
    cases$depth <- stringr::str_count(pattern = ":", cases$location_name) / 2
    cases <- cases %>% dplyr::filter(depth <= config$truncate$smallest_spatial_scale)
  } else {
    stop(paste("method", method, "is not implemented"))
  }
}

# Sanity check (There should be at least one report)
if (nrow(cases) == 0) {
  stop("No primary, non-NA observations were found.")
} else {
  print(paste(nrow(cases), "observations were found."))
}

### Perform checks on shapefiles
print("Finding Locations")

if (sum(!cases$shapefile.exists) > 0) {
  warning("There was a problem with at least one shapefile. See output for details.")
  print(paste(sum(!cases$shapefile.exists), "of", length(cases$shapefile.exists), "observations have shapefile problems."))
  
  problem_cases <- dplyr::filter(cases, !shapefile.exists)
  problem_indices <- which(!cases$shapefile.exists)
  problem_OCs <- unique(problem_cases$OC_UID) ## relationships.observation_collection.data.id)
  
  # print(paste("The following indexes are affected:", paste(problem_indices, collapse = ", ")))
  print(paste("The following OC UIDs are affected:", paste(problem_OCs, collapse = ", ")))
  print(paste(sum(problem_cases[[cases_column]]), "/", sum(cases[[cases_column]]), cases_column, "are missing due to shapefile problems."))
  
  print("Observations attached to problematic location-periods will be ignored. Here are the problematic location-periods *****************")
  print(dplyr::select(problem_cases, location_name) %>%
          as.data.frame() %>%
          dplyr::distinct(location_name) %>%
          dplyr::arrange(location_name))
  
  cases <- cases %>%
    dplyr::filter(shapefile.exists)
}

print("Validate Shapefiles")
shapefiles <- cases %>%
  dplyr::group_by(attributes.location_period_id) %>%
  dplyr::summarize(n = dplyr::n()) %>%
  dplyr::rename(location_period_id = attributes.location_period_id)

shapefiles$valid <- sf::st_is_valid(shapefiles)
if (!all(shapefiles$valid)) {
  warning("At least one location period is invalid.  See output for details")
  print(paste(sum(!shapefiles$valid), "shapefiles were invalid."))
  print(paste("The following location periods were affected:", paste(shapefiles$location_period_id[!shapefiles$valid], collapse = ", ")))
  shapefiles$geojson[!shapefiles$valid] <- sf::st_make_valid(shapefiles$geojson[!shapefiles$valid])
  shapefiles$valid <- sf::st_is_valid(shapefiles)
  print("An attempt was made to fix the invalid shapefiles")
}

## This is not a long term solution in any capacity
if (any(grepl("GEOMETRYCOLLECTION", sf::st_geometry_type(shapefiles)))) {
  warning("Geometry collections present in locations.  See output for details")
  print(paste("The following location periods are affected:", paste(shapefiles[grepl("GEOMETRYCOLLECTION", sf::st_geometry_type(shapefiles)), ][["location_period_id"]], collapse = ", ")))
  warning("Attempting to fix geometry collections, but not in a smart way.  Please fix the underlying data instead.")
  problem_indices <- which(grepl("GEOMETRYCOLLECTION", sf::st_geometry_type(shapefiles)))
  tmp2 <- do.call(sf:::rbind.sf, lapply(shapefiles$geojson[problem_indices], function(x) {
    sf::st_sf(sf::st_sfc(x[[1]]))
  }))
  shapefiles[["geojson"]][problem_indices] <- sf::st_geometry(tmp2)
}

# Write location periods in data ---------------------------------------
cat("-- Creating tables for observed lps in database \n")

# Database connection
conn_pg <- taxdat::connect_to_db(dbuser)

# Set user-specific name for location_periods table to use
lp_name <- taxdat::make_locationperiods_table_name(config = config)

# Make sf object to multiploygons to be consistent
shapefiles <- sf::st_cast(shapefiles, "MULTIPOLYGON") %>%
  dplyr::rename(geom = geojson)

# Write to database
taxdat::write_shapefiles_table(
  conn_pg = conn_pg,
  shapefiles = shapefiles,
  table_name = lp_name
)

# Create mapping from location periods to grid cells
taxdat::make_grid_lp_mapping_table(
  conn_pg = conn_pg,
  lp_name = lp_name
)

# Create table of spatial intersections between grid polygons and shapefile
# borders to compute population-weighted fractions
intersections_table <- taxdat::make_grid_intersections_table_name(config = config)

taxdat::make_grid_intersections_table(
  conn_pg = conn_pg,
  full_grid_name = full_grid_name,
  lp_name = lp_name,
  intersections_table = intersections_table
)

# Get the dictionary of location periods to pixel ids
cntrd_table <- taxdat::make_grid_centroids_table_name(config = config)

taxdat::make_grid_lp_centroids_table(
  conn_pg = conn_pg,
  full_grid_name = full_grid_name,
  lp_name = lp_name,
  cntrd_table = cntrd_table
)

# Process data for summaries ----------------------------------------------
cat("-- Creating tables for output summary lps in database \n")

# !! This assumes only one country present, would need to be changed if list of countries
iso_code <- taxdat::get_country_isocode(config)

output_shapefiles <- taxdat::get_multi_country_admin_units(
  iso_code = iso_code,
  admin_levels = config$summary_admin_levels,
  lps = shapefiles
)

# Name for output location periods
output_lp_name <- taxdat::make_output_locationperiods_table_name(
  config = config
)

# Write to database
taxdat::write_shapefiles_table(
  conn_pg = conn_pg,
  shapefiles = output_shapefiles,
  table_name = output_lp_name
)

# Create mapping from location periods to grid cells
taxdat::make_grid_lp_mapping_table(
  conn_pg = conn_pg,
  lp_name = output_lp_name
)

# Create table of spatial intersections between grid polygons and shapefile
# borders to compute population-weighted fractions
output_intersections_table <- taxdat::make_output_grid_intersections_table_name(
  config = config
)

taxdat::make_grid_intersections_table(
  conn_pg = conn_pg,
  full_grid_name = full_grid_name,
  lp_name = output_lp_name,
  intersections_table = output_intersections_table
)

# Get the dictionary of location periods to pixel ids
output_cntrd_table <- taxdat::make_output_grid_centroids_table_name(
  config = config
)

taxdat::make_grid_lp_centroids_table(
  conn_pg = conn_pg,
  full_grid_name = full_grid_name,
  lp_name = output_lp_name,
  cntrd_table = output_cntrd_table
)

# Create sf_chol ---------------------------------------------------------------

sf::st_crs(cases) <- sf::st_crs(shapefiles) ## same crs needed for st_join
sf::st_geometry(cases) <- NULL
sf_cases <- sf::st_as_sf(
  dplyr::left_join(cases,
                   shapefiles,
                   by = c("attributes.location_period_id" = "location_period_id")
  )
)

if (any(sf::st_is_empty(sf_cases))) {
  warning(
    paste(
      "Missing shapefiles for ",
      sum(sf::st_is_empty(sf_cases)),
      "observations. They correspond to the following location period IDs:",
      paste(sf_cases[sf::st_is_empty(sf_cases), ]$locationPeriod_id, collapse = ", ")
    )
  )
  sf_cases <- sf_cases[!sf::st_is_empty(sf_cases), ]
}

sf_cases$TL <- lubridate::ymd(sf_cases$TL)
sf_cases$TR <- lubridate::ymd(sf_cases$TR)


# Snap to time period
sf_cases <- taxdat::snap_to_time_period_df(df = sf_cases,
                                           TL_col = "TL",
                                           TR_col = "TR",
                                           res_time = res_time,
                                           tol = snap_tol)

# Set admin level
sf_cases <- sf_cases %>% 
  dplyr::mutate(admin_level = purrr::map_dbl(location_name, ~ taxdat::get_admin_level(.)))

# Drop multi-year observations if present
if (drop_multiyear_adm0) {
  sf_cases <- taxdat::drop_multiyear(df = sf_cases,
                                     admin_levels = 0)
}


save(sf_cases,
     full_grid_name,
     output_shapefiles,
     file = file_names[["data"]]
)

# close database
DBI::dbDisconnect(conn_pg)
taxdat::close_parallel_setup()
