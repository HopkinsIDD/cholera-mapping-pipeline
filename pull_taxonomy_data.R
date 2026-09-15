# pull_taxonomy_data.R
# Pulls data from the cholera taxonomy database using taxdat::read_taxonomy_data_sql
# Credentials are sourced from Analysis/R/database_api_key.R (kept out of version control).

library(taxdat)

# --- Credentials ---
# Defines: database_username, database_api_key, taxonomy_username, taxonomy_password
source("Analysis/R/database_api_key.R")

username <- taxonomy_username
password <- taxonomy_password

if (username == "" || password == "") {
  stop("taxonomy_username and/or taxonomy_password are empty in ",
       "Analysis/R/database_api_key.R. Please fill them in.")
}

# --- Pull parameters (EDIT THESE for your actual pull) ---
locations <- 289                        # numeric location ancestor_id
time_left <- as.POSIXlt("2022-12-31")   # left bound for observation times
time_right <- as.POSIXlt("2023-09-24")  # right bound for observation times
uids <- "5101"                          # observation collection id(s) to pull

# --- Run the pull ---
observations <- read_taxonomy_data_sql(
  username = username,
  password = password,
  locations = locations,
  time_left = time_left,
  time_right = time_right,
  uids = uids
)

cat("Pulled", nrow(observations), "observations.\n")

# --- Save output ---
out_dir <- "Analysis/data"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_path <- file.path(out_dir, paste0("taxonomy_pull_", format(Sys.Date(), "%Y%m%d"), ".rds"))
saveRDS(observations, out_path)

cat("Saved to:", out_path, "\n")
