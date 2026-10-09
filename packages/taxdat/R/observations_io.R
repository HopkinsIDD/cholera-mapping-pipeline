# Observation data: pulling from the taxonomy database, and saving / loading a
# pulled copy so runs on machines without access to the taxonomy database
# (e.g. cluster compute nodes) can use observations pulled elsewhere.

# Config entries that determine what the taxonomy pull returns.
# Normalised so the raw YAML and the validated config give the same key.
observation_pull_key <- function(config) {
  list(countries_name = sort(toupper(as.character(config$countries_name))),
       start_time = as.character(as.Date(config$start_time)),
       end_time = as.character(as.Date(config$end_time)),
       OCs = sort(as.character(unlist(config$OCs))),
       data_source = as.character(config$data_source))
}

#' @title Pull observations from the taxonomy database
#' @name pull_observations
#' @description Pulls the run's observations with `pull_taxonomy_data` using
#' credentials from the environment (CHOLERA_SQL_USERNAME / CHOLERA_SQL_PASSWORD
#' / CHOLERA_SQL_WEBSITE for "sql", CHOLERA_API_USERNAME / CHOLERA_API_KEY /
#' CHOLERA_API_WEBSITE for "api"), falling back to
#' `Analysis/R/database_api_key.R` when present.
#'
#' @param config the run's config
#' @param key_file optional R file defining the credentials
#' @return the pulled observations, fields renamed with `rename_database_fields`
#' @export
pull_observations <- function(config, key_file = "Analysis/R/database_api_key.R") {
  read_key <- function(vars) {
    if (!file.exists(key_file)) {
      stop("No taxonomy credentials: set them in the environment or in ", key_file)
    }
    e <- new.env()
    sys.source(key_file, envir = e)
    missing <- setdiff(vars, ls(e))
    if (length(missing) > 0) stop(key_file, " does not define ", paste(missing, collapse = ", "))
    mget(vars, envir = e)
  }

  if (config$data_source == "api") {
    locations <- paste("CT-World",
                       sapply(config$countries_name, lookup_WHO_region),
                       gsub("_", "::", config$countries_name), sep = "::")
    username <- Sys.getenv("CHOLERA_API_USERNAME", "")
    password <- Sys.getenv("CHOLERA_API_KEY", "")
    website <- Sys.getenv("CHOLERA_API_WEBSITE", "")
    if (username == "" || password == "") {
      k <- read_key(c("database_username", "database_api_key"))
      username <- k$database_username
      password <- k$database_api_key
    }
  } else if (config$data_source == "sql") {
    locations <- config$countries
    username <- Sys.getenv("CHOLERA_SQL_USERNAME", "")
    password <- Sys.getenv("CHOLERA_SQL_PASSWORD", "")
    website <- Sys.getenv("CHOLERA_SQL_WEBSITE", "")
    if (username == "" || password == "") {
      k <- read_key(c("taxonomy_username", "taxonomy_password"))
      username <- k$taxonomy_username
      password <- k$taxonomy_password
    }
  } else {
    stop("Unknown data source, must be one of 'api', 'sql', found ", config$data_source)
  }

  pull_taxonomy_data(username = username, password = password, locations = locations,
                     time_left = config$start_time, time_right = config$end_time,
                     source = config$data_source, uids = config$OCs, website = website) %>%
    rename_database_fields(source = config$data_source)
}

#' @title Save pulled observations
#' @name save_observations_rds
#' @param cases output of `pull_observations`
#' @param config the run's config
#' @param file output .rds path
#' @return file, invisibly
#' @export
save_observations_rds <- function(cases, config, file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(cases = cases,
               key = observation_pull_key(config),
               pulled_at = Sys.time(),
               taxdat_version = as.character(utils::packageVersion("taxdat"))),
          file)
  invisible(file)
}

#' @title Load pulled observations
#' @name load_observations_rds
#' @description Loads observations saved with `save_observations_rds` and
#' stops if they were pulled for different countries, dates, OCs or source.
#'
#' @param file .rds path
#' @param config the run's config
#' @return the observations
#' @export
load_observations_rds <- function(file, config) {
  if (!file.exists(file)) {
    stop("Observations file not found: ", file)
  }
  obs <- readRDS(file)
  want <- observation_pull_key(config)
  diff <- names(want)[!mapply(identical, want, obs$key[names(want)])]
  if (length(diff) > 0) {
    stop("Observations in ", file, " were pulled for a different ",
         paste(diff, collapse = ", "), " than this config.")
  }
  message("-- Loaded ", nrow(obs$cases), " observations pulled at ", format(obs$pulled_at),
          " from ", file)
  obs$cases
}
