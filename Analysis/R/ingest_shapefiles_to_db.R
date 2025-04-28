# This script ingests the post-processing shapefiles to the covariates database


# Preamble ----------------------------------------------------------------

library(purrr)
library(magrittr)
library(sf)
library(taxdat)
library(here)

# Loop over countries -----------------------------------------------------

# Get country list from config directory for which runs where done
countries <- dir(here("cholera-configs/Sep_2023_nocovar_production_runs"), full.names = T) %>% 
  map_chr(~get_country_from_string(.)) %>% 
  unique() %>% 
  sort()


# Connect to covariates database
conn_db <- connect_to_db()

# Loop over countries, pull shapefiles from rgeoboundaries and insert to db
for (country in countries) {
  
  # Pull from rgeoboundaries
  shps <- get_multi_country_admin_units(
    iso_code = country, 
    admin_levels = 0:2, 
    source = "api"
  )
  
  # Write to database
  st_write(
    shps, 
    dsn = "conn_db", 
    layer = "output_shapefiles", 
    append = TRUE, 
    overwrite = FALSE
  )
  
  cat("Done ", country, "\n")
}



