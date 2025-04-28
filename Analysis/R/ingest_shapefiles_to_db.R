# This script ingests the post-processing shapefiles to the covariates database


# Preamble ----------------------------------------------------------------
library(DBI)
library(RPostgres)
library(purrr)
library(magrittr)
library(sf)
library(taxdat)
library(here)

# Loop over countries -----------------------------------------------------

# Get country list from config directory for which runs where done
# Get the path to the parent directory
parent_dir <- "/home/pfang/Code/cholera-mapping-pipeline/Analysis/configs/Sep_2023_nocovar_production_runs"
yml_files <- list.files(parent_dir,
                       pattern = "\\.yml$",
                       recursive = TRUE,
                       full.names = TRUE)
countries <- yml_files %>% 
  map_chr(~get_country_from_string(.)) %>% 
  unique() %>% 
  sort()

print(countries)
# Connect to covariates database
conn_db <- connect_to_db(Sys.getenv("USER"))
#Check if layer exists before starting the loop
layer_exists <- dbExistsTable(conn_db, "output_shapefiles")

# Delete existing layer if it exists
if (layer_exists) {
  dbExecute(conn_db, "DROP TABLE IF EXISTS output_shapefiles")
  cat("Existing layer deleted\n")
}
first_write <- TRUE

# Loop over countries, pull shapefiles from rgeoboundaries and insert to db
for (country in countries) {
  tryCatch({
    cat("Processing:", country, "\n")
    
    # Pull from rgeoboundaries
    shps <- get_multi_country_admin_units(
      iso_code = country,
      admin_levels = 0:2,
      source = "api"
    )
    
    # Skip if no data
    if (nrow(shps) == 0) {
      cat(country, "has no data, skipping\n")
      next
    }
   shps <- mutate(shps, country = country) 
    # Write to PostgreSQL database
    st_write(
      obj = shps,
      dsn = conn_db,
      layer = "output_shapefiles",
      append = !first_write,  # FALSE for first write, TRUE afterwards
	  quiet = TRUE
    )
    
    # After first successful write, set flag to FALSE
    if (first_write) first_write <- FALSE
    
    cat("Successfully wrote:", country, "\n")
  }, error = function(e) {
    cat("Error processing", country, ":", e$message, "\n")
  })
}

DBI::dbDisconnect(conn_db)
