# Preamble ---------------------------------------------------------------------

# Check if variables exist

setwd(cholera_directory)


# Pull data from taxonomy DB ----------------------------------------------

print("Starting Data Preparation Process")
print(paste("Saving output data to ", file_names[["data"]]))


# Define credentials for data pull
if (config$data_source == "api") {
  # NEED TO ADD WHO REGION LOOKUP AND APPEND FOR ALL LOCATIONS
  countries <- sapply(config$countries_name, taxdat::fix_country_name)
  who_region <- sapply(config$countries_name, taxdat::lookup_WHO_region)
  long_countries <- paste("CT-World", who_region, gsub("_", "::", config$countries_name), sep = "::")
  worldpop_region <- unique(sapply(config$countries_name, taxdat::lookup_WorldPop_region))
  username <- Sys.getenv("CHOLERA_API_USERNAME", "NONE")
  password <- Sys.getenv("CHOLERA_API_KEY", "NONE")
  website <- Sys.getenv("CHOLERA_API_WEBSITE", "")
  if (any(c(username, password) == "NONE")) {
    source("Analysis/R/database_api_key.R")
    username <- database_username
    password <- database_api_key
  }
  cat("cntry:", long_countries, "u:", ifelse(nchar(username) > 0, "****", ""), "psswd:",
      ifelse(nchar(password) > 0, "****", ""), "st:", config$start_time, "et:",
      config$end_time, "\n")
} else if (config$data_source == "sql") {
  long_countries <- config$countries
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
  time_left = config$start_time,
  time_right = config$end_time,
  source = config$data_source,
  uids = config$OCs,
  website = website
) %>%
  taxdat::rename_database_fields(source = config$data_source)

# !! Filter out NA cases, which represent missing observations, and non-primary observations (primary observations are only space-time stratified and these are the ones we want to focus on in these maps)
cases <- dplyr::filter(cases, !is.na(.data[[cases_column]])) %>%
  dplyr::filter(is_primary) 


# Shapefile manipulations -------------------------------------------------

# Build shapefiles
cases <- taxdat::build_shapfiles_from_geojson(cases)

# Keep only certain admin level depth
# (This seems to be a rest from previous implementation which we do not use anymore)
cases <- taxdat::truncate_cases_by_location(cases = cases,
                                            config = config)


# Sanity check (There should be at least one report)
if (nrow(cases) == 0) {
  stop("No primary, non-NA observations were found.")
} else {
  print(paste(nrow(cases), "observations were found."))
}

# Drop observations with missing shapefiles
cases <- taxdat::drop_missing_shapefiles(cases = cases,
                                         cases_column = cases_column)

# Get the set of unique valid shapefiles for spatial operations in database
shapefiles <- taxdat::get_valid_shapefiles(cases)

# !! This assumes only one country present, would need to be changed if list of countries
iso_code <- taxdat::get_country_isocode(config)

# Clip to adm0
shapefiles <- taxdat::clip_shapefiles_to_adm0(iso_code = iso_code,
                                              shapefiles = shapefiles)

# Write location periods in data ---------------------------------------
cat("-- Creating tables for observed lps in database \n")

# Database connection
conn_pg <- taxdat::connect_to_db(dbuser)

# Set user-specific name for location_periods table to use
lp_name <- taxdat::make_locationperiods_table_name(config = config)

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

output_shapefiles <- taxdat::get_multi_country_admin_units(
  iso_code = iso_code,
  admin_levels = config$summary_admin_levels,
  lps = shapefiles,
  source = "database",
  dbuser = dbuser
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
  dplyr::inner_join(cases,
                    shapefiles,
                    by = c("attributes.location_period_id" = "location_period_id",
                           "location_name" = "location_name")
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
                                           res_time = config$res_time,
                                           tol = config$snap_tol)

# Set admin level
sf_cases <- sf_cases %>% 
  dplyr::mutate(admin_level = purrr::map_dbl(location_name, ~ taxdat::get_admin_level(.)))

# Drop multi-year observations if present
if (config$drop_multiyear_adm0) {
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
