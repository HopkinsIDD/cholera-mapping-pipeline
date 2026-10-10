# This script ingests the post-processing shapefiles to the covariates database


# Preamble ----------------------------------------------------------------
library(optparse)
library(DBI)
library(RPostgres)
library(purrr)
library(magrittr)
library(sf)
library(taxdat)

# Define command-line options
option_list <- list(
  make_option(c("-d", "--config_dir"),
    type = "character", default = NULL,
    help = "Path to the parent directory containing config YAML files", metavar = "character"
  )
)

# Parse options
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# Check if parent_dir is provided
if (is.null(opt$config_dir)) {
  stop("Error: Please provide the path to the config files' directory using the -d or --config_dir option.")
}

# Use the parent_dir value from the command-line argument
parent_dir <- opt$config_dir

# Get country list from config directory for which runs where done
yml_files <- list.files(parent_dir,
  pattern = "\\.yml$",
  recursive = TRUE,
  full.names = TRUE
)
countries <- yml_files %>%
  map_chr(~ get_country_from_string(.)) %>%
  unique() %>%
  sort()

print(countries)
sf::sf_use_s2(FALSE)

# Connect to covariates database (PGHOST, PGPORT, PGDATABASE, PGUSER, PGPASSWORD)
conn_db <- taxdat::connect_to_db()
target <- DBI::Id(schema = "data", table = "output_shapefiles")
if (dbExistsTable(conn_db, target)) {
  dbExecute(conn_db, "DROP TABLE data.output_shapefiles")
  cat("Existing data.output_shapefiles deleted\n")
}
# Lets readers detect shapes built with another version of the function
fun_hash <- digest::digest(deparse(taxdat::get_country_admin_units), algo = "md5")
first_write <- TRUE

# Loop over countries; boundaries come from the admin-units cache, or the
# geoBoundaries API on a cache miss (needs internet)
for (country in countries) {
  tryCatch(
    {
      cat("Processing:", country, "\n")
      shps <- get_multi_country_admin_units(iso_code = country, admin_levels = 0:2,
                                            source = "cache")
      if (nrow(shps) == 0) {
        cat(country, "has no data, skipping\n")
        next
      }
      shps <- dplyr::mutate(shps, country = country, get_country_admin_units_hash = fun_hash)
      st_write(obj = shps, dsn = conn_db, layer = target, append = !first_write, quiet = TRUE)
      first_write <- FALSE
      cat("Successfully wrote:", country, "\n")
    },
    error = function(e) {
      cat("Error processing", country, ":", e$message, "\n")
    }
  )
}
if (!first_write) {
  dbExecute(conn_db, "CREATE INDEX ON data.output_shapefiles USING GIST (geom)")
  dbExecute(conn_db, "CREATE INDEX ON data.output_shapefiles (country, admin_level)")
}

DBI::dbDisconnect(conn_db)
