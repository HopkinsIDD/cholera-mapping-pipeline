#' @export
#' @name time_unit_to_start_function
#' @title time_unit_to_start_function
#' @description Make a function that returns a date at the start of the appropriate time unit
#' @param unit Human readable string for unit of time aggregation
#' @return a function that returns a date at the start of the appropriate time unit
time_unit_to_start_function <- function(unit) {
  unit <- strsplit(unit, split = " ")[[1]]
  # Remove the 's' at the end of the unit
  unit <- gsub("s$", "", unit)
  unit_type <- unit[[2]]
  unit_count <- as.numeric(unit[[1]])
  
  
  changer <- list(year = function(x) {
    x <- x * unit_count
    return(as.Date(paste(x, "01", "01", sep = "-"), format = "%Y-%m-%d"))
  }, month = function(x) {
    x <- x * unit_count
    month <- ((x - 1) %% 12) + 1
    year <- (x - month) / 12
    return(as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%m-%d"))
  }, isoweek = function(x) {
    x <- x * unit_count
    return(stop("Not yet written"))
  })
  return(changer[[unit_type]])
}

#' @export
#' @name time_unit_to_end_function
#' @title time_unit_to_end_function
#' @description Make a function that returns a date at the end of the appropriate time unit
#' @param unit Human readable string for unit of time aggregation
#' @return a function that returns a date at the end of the appropriate time unit
time_unit_to_end_function <- function(unit) {
  unit <- strsplit(unit, split = " ")[[1]]
  # Remove the 's' at the end of the unit
  unit <- gsub("s$", "", unit)
  unit_type <- unit[[2]]
  unit_count <- as.numeric(unit[[1]])
  
  
  changer <- list(year = function(x) {
    x <- x * unit_count
    return(as.Date(paste(x, "12", "31", sep = "-"), format = "%Y-%m-%d"))
  }, month = function(x) {
    x <- x * unit_count
    month <- ((x - 1) %% 12) + 1
    year <- (x - month) / 12
    return(as.Date(paste(year, month, lubridate::days_in_month(month), sep = "-"),
                   format = "%Y-%m-%d"
    ))
  }, isoweek = function(x) {
    x <- x * unit_count
    return(stop("Not yet written"))
  })
  return(changer[[unit_type]])
}

#' @export
#' @name time_unit_to_aggregate_function
#' @title time_unit_to_aggregate_function
#' @description Returns a function that converts dates into the correct time unit
#' @param unit Human readable string for unit of time aggregation
#' @return a function that converts dates into the correct time unit
time_unit_to_aggregate_function <- function(unit) {
  unit <- strsplit(unit, split = " ")[[1]]
  # Remove the 's' at the end of the unit
  unit <- gsub("s$", "", unit)
  unit_type <- unit[[2]]
  unit_count <- as.numeric(unit[[1]])
  
  changer <- list(year = function(x) {
    return(floor(lubridate::year(x) / unit_count))
  }, month = function(x) {
    return(floor((12 * lubridate::year(x) + lubridate::month(x)) / unit_count))
  }, isoweek = function(x) {
    return(stop("Not yet written"))
  })
  return(changer[[unit_type]])
}

#' @export
#' @name case_definition_to_column_name
#' @title case_definition_to_column_name
#' @description Turns human readable types of cholera case definitions into taxdat codes
#' @param type string of type
#' @param database Whether or not we're using the database
#' @param sql logical for whether data is pulled from sql
#' @return string of column names in the data taxonomy data frame.
case_definition_to_column_name <- function(type, database = FALSE, sql = FALSE) {
  if ((!database) & (!sql)) {
    warning("The svn column names are deprecated, please use database column names.")
    changer <- c(suspected = "sCh", confirmed = "cCh", presence = c(
      "sCh", "sCh_R",
      "sCh_L", "cCh", "cCh_L", "cCh_R", "deaths", "deaths_L", "deaths_R"
    ))
  } else if ((database) & (!sql)) {
    changer <- c(
      suspected = "attributes.fields.suspected_cases", confirmed = "attributes.fields.confirmed_cases",
      presence = c(
        "attributes.fields.suspected_cases", "attributes.fields.suspected_cases_R",
        "attributes.fields.suspected_cases_L", "attributes.fields.confirmed_cases",
        "attributes.fields.confirmed_cases_L", "attributes.fields.confirmed_cases_R",
        "attributes.fields.deaths", "attributes.fields.deaths_L", "attributes.fields.deaths_R"
      )
    )
  } else if ((!database) & (sql)) {
    changer <- c(
      suspected = "suspected_cases", confirmed = "confirmed_cases",
      presence = c(
        "suspected_cases", "suspected_cases_R", "suspected_cases_L",
        "confirmed_cases", "confirmed_cases_L", "confirmed_cases_R", "deaths",
        "deaths_L", "deaths_R"
      )
    )
  }
  return(changer[type])
}

#' @export
#' @name reduce_sf_vector
#' @title reduce_sf_vector
#' @description recursively rbind a list of sf objects
#' @param vec a vector/list of sf objects
#' @return a single sf object which contains all the rows bound together
reduce_sf_vector <- function(vec) {
  if (length(vec) == 0) {
    return(sf::st_sf(sf::st_sfc()))
  }
  if (is.null(names(vec))) {
    names(vec) <- 1:length(vec)
  }
  if (length(names(vec)) != length(vec)) {
    names(vec) <- 1:length(vec)
  }
  k <- 1
  all_columns <- unlist(vec, recursive = FALSE)
  split_names <- strsplit(names(all_columns), ".", fixed = TRUE)
  column_names <- sapply(split_names, function(x) {
    x[[2]]
  })
  geom_columns <- which(column_names == "geometry")
  geometry <- sf::st_as_sfc(unlist(all_columns[geom_columns], recursive = FALSE))
  rc <- sf::st_sf(geometry)
  frame_only <- dplyr::bind_rows(lapply(vec, function(x) {
    x <- as.data.frame(x)
    x <- x[-grep("geometry", names(x))]
    return(x)
  }))
  rc <- dplyr::bind_cols(rc, frame_only)
  return(rc)
}

#' @title Rename cholera data columns
#' @description Renames the columns of the data pulled either from the the
#' API staging database or by SQL from taxdat
#'
#' @param database_df Data who's columns are to be modified
#' @param source Whether the source is the staging database (sing the API) or taxdat (using SQL).
#' @details source is one of 'api' or 'sql'
#' @return the renamed dataframe
#' @export
rename_database_fields <- function(database_df, source = "api") {
  if (source == "api") {
    new_database_df <- database_df %>%
      dplyr::rename(
        TL = attributes.time_left, TR = attributes.time_right,
        is_primary = attributes.primary, is_phantom = attributes.phantom,
        locationPeriod_id = attributes.id, OC_UID = relationships.observation_collection.data.id,
        location_name = attributes.location_name
      )
  } else if (source == "sql") {
    new_database_df <- database_df %>%
      dplyr::rename(
        TL = time_left, TR = time_right, is_primary = primary,
        is_phantom = phantom, locationPeriod_id = location_period_id, OC_UID = observation_collection_id,
        location_name = location_name
      )
  } else {
    stop("Source needs to be one of 'api', 'sql', found ", source)
  }
  # names(new_database_df) <- gsub('attributes.fields.', '',
  # names(new_database_df)) names(new_database_df) <- gsub('attributes.', '',
  # names(new_database_df))
  return(new_database_df)
}


#' @description Flatten the result of a json query into an unnested data frame.  Similar to jsonlite::flatten, but with some tweaks to make it work better for our use case.
#' @param json_results A listlike object convertible to a data frame coming from an api query
#' @results a data frame that matches an unrolled version of json_results
flatten_json_result <- function(json_results) {
  if (!is.data.frame(json_results)) {
    json_results <- as.data.frame(json_results)
  }
  json_results <- jsonlite::flatten(json_results)
  for (colname in names(json_results)) {
    if (mode(json_results[[colname]]) == "list") {
      if ((max(sapply(json_results[[colname]], length)) == 1)) {
        json_results[[colname]] <- sapply(json_results[[colname]], function(x) {
          return(ifelse(length(x) == 1, x, NA))
        })
      }
    }
  }
  return(json_results)
}

## JSON API interface to database
#' @name read_taxonomy_data_api
#' @title read_taxonomy_data_api
#' @export read_taxonomy_data_api
#' @description This function accesses the cholera-taxonomy stored
#'   at https://cholera-taxonomy.middle-distance.com pulls
#'   data based on function parameters, links it together, and
#'   transforms it into a simple features object (sf).
#' @param username The username for a user of the database
#' @param api_key A working api.key for the user of the database
#' @param locations A vector of locations to pull observations from (should be in the form who_region::ISO_L1::ISO_A2_...)
#' @param time_left First time for observations
#' @param time_right Last time for observations
#' @param uids unique observation collections ids to pull
#' @param website Which website to pull from (default is cholera-taxonomy.middle-distance.com)
#' @return An sf object containing data pulled from the database
read_taxonomy_data_api <- function(username, api_key, locations = NULL, time_left = NULL,
                                   time_right = NULL, uids = NULL, website = "https://cholera-taxonomy.middle-distance.com/") {
  
  ## First, we want to set up the https POST request.  We make a list
  ## containing the arguments for the request: If the API changes, we will
  ## just need to change this list
  api_type <- ""
  if (is.null(uids)) {
    api_type <- "by_location"
    if (length(locations == 1)) {
      locations <- c(locations, locations)
    }
    
    ## Prevent continents, or too many countries
    if (any(!grepl("::", locations))) {
      stop("Trying to pull data for a continent is not allowed")
    }
    if ((sum(stringr::str_count(string = unique(locations), pattern = "::") ==
             1) > 2)) {
      stop("Trying to pull data for more than 2 countries at a time is not allowed")
    }
    
    https_post_argument_list <- list(email = username, api_key = api_key, locations = gsub(
      "::",
      " ", locations
    ), time_left = time_left, time_right = time_right)
  } else if (is.null(locations) && is.null(time_left) && is.null(time_right)) {
    api_type <- "by_observation_collections"
    https_post_argument_list <- list(email = username, api_key = api_key, observation_collection_ids = uids)
  } else {
    stop("Not supported")
  }
  
  website <- paste0(website, "/api/v1/observations/", api_type)
  
  ## Every object in R is a vector, even the primitives.  For example,
  ## c(1,5,6) is of type integer.  Because of this, we need to explicitly
  ## tell the JSON parser to treat vectors of length 1 differently.  The
  ## option for this is auto_unbox = T
  json <- jsonlite::toJSON(https_post_argument_list, auto_unbox = T)
  ## Message prints a message to the user.  It's somewhere between a warning
  ## and a normal print.  In this case, this function might take a while to
  ## run, so we let the user know up front.
  message("Fetching results from JSON API")
  
  ## This is the line that actually fetches the results.  The syntax for
  ## adding headers is a little weird.  The function add_headers takes named
  ## arguments and returns whatever the arguments to POST are supposed to be.
  ## body is the body encode is the transformation to perform on the body to
  ## make it into text
  results <- httr::POST(website, httr::add_headers(`Content-Type` = "application/json"),
                        body = json, encode = "form"
  )
  
  ## Now we process the status code to make sure that things are working
  ## correctly
  code <- httr::status_code(results)
  ## Right now, anything that isn't correct is an error
  if (code != 200) {
    stop(paste("Error: Status Code", code))
  }
  
  ## Next we extract just the content of the results
  original_results_data <- httr::content(results)
  ## This returns something correct, but the formatting is really odd.  It is
  ## a little messy, but instead of debugging the formatting, for now I'm
  ## converting to json and back, which fixes the problems.
  jsondata <- rjson::toJSON(original_results_data)
  if (!jsonlite::validate(jsondata)) {
    stop("Could not validate json response")
  }
  results_data <- jsonlite::fromJSON(jsondata)
  
  ## Now we have the results of the api data as a nested list.  We want to do
  ## the following in no particular order for the observations, we want to
  ## turn them into a data frame with one row per observation for the
  ## location_periods, we want to turn them into a geometry object and link
  ## them to the observations
  
  ## We start with the observations The | operator is logical or The results
  ## should have observations The observations should have data The data
  ## should be the only thing in observations
  if ((!("observations" %in% names(results_data))) | (!("data" %in% names(results_data[["observations"]]))) |
      (length(results_data[["observations"]]) > 1)) {
    stop("Could not parse results properly.  Contact package maintainer")
  }
  results_data[["observations"]] <- flatten_json_result(results_data[["observations"]][["data"]])
  
  observation_collections_present <- FALSE
  ## The results should have observations The observations should have data
  ## The data should be the only thing in observations
  if (("observation_collections" %in% names(results_data)) && ("data" %in% names(results_data[["observation_collections"]])) &&
      (length(results_data[["observation_collections"]]) == 1)) {
    results_data[["observation_collections"]] <- flatten_json_result(results_data[["observation_collections"]][["data"]])
    observation_collections_present <- TRUE
  }
  
  ## Check to make sure that the number of ids and number of rows match
  if (!length(unique(results_data$observations$id)) == nrow(results_data$observations)) {
    stop("Could not parse results properly.  Contact package maintainer")
  }
  
  ## Now we want to handle the location periods We need to process these
  ## individually, so we'll loop over location periods to extract the
  ## geojsons We use the original_results_data here, since the formatting
  ## transformation we did earlier prevents this code from working
  tmp_results <- original_results_data[["location_periods"]][["data"]]
  all_shape_ids <- sapply(original_results_data$location_periods$included, function(x) {
    x$id
  })
  all_locations <- list() # This will be a list of the geojson objects
  if (length(tmp_results) > 0) {
    for (idx in 1:length(tmp_results)) {
      message(paste(idx, "/", length(tmp_results)))
      
      ## We process the geojson in three pieces.  #1. Extract the json
      ## string #2.  Convert to sf object #3. Add to location list Ignore
      ## NULL elements.  Undefined list elements default to NULL anyway
      
      ## Determine which shape we are working with
      shape_id <- tmp_results[[idx]][["relationships"]][["shape"]][["data"]][["id"]]
      this_shape_index <- match(shape_id, all_shape_ids)
      unformatted_geojson <- original_results_data[["location_periods"]][["included"]][[this_shape_index]][["attributes"]][["simple_shape"]] # 1.
      if (is.null(unformatted_geojson)) {
        all_locations[[idx]] <- sf::st_sf(geometry = sf::st_sfc(sf::st_point()))
        next
      }
      sf_geojson <- geojsonsf::geojson_sf(unformatted_geojson) # 2.
      all_locations[[idx]] <- sf_geojson # 3.
    }
  }
  ## reduce_sf_vector turns a list of sf objects into a single sf object
  ## containing the same information
  locations_sf <- taxdat::reduce_sf_vector(all_locations)
  ## We are going to take our properly formatted geojson files and replace
  ## the badly formatted ones
  results_data$location_periods$data$geojson <- NULL
  results_data$location_periods$data$attributes$geojson <- NULL
  
  results_data$location_periods <- flatten_json_result(results_data$location_periods$data)
  if (nrow(results_data$location_periods) > 0) {
    results_data$location_periods$sf_id <- seq_len(nrow(results_data$location_periods))
  }
  results_data$observations$attributes.location_period_id <- as(
    results_data$observations$attributes.location_period_id,
    class(results_data$location_periods$id)
  )
  
  ## We then join (as in sql) by the location_periods with the observations
  ## by location_period_id
  all_results <- results_data$observations
  if (observation_collections_present && (nrow(all_results) > 0) && (nrow(results_data$observation_collections) >
                                                                     0)) {
    all_results <- dplyr::left_join(results_data$observations, results_data$observation_collections,
                                    by = c(
                                      relationships.observation_collection.data.id = "id" # lhs column name = rhs column name
                                    )
    )
  }
  if ((nrow(all_results) > 0) && (nrow(results_data$location_periods) > 0)) {
    all_results <- dplyr::left_join(all_results, results_data$location_periods,
                                    by = c(
                                      attributes.location_period_id = "id" # lhs column name = rhs column name
                                    )
    )
  }
  
  geoinput <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(1 * c(NA, NA))))$geometry
  if (nrow(all_results) == 0) {
    geoinput <- geoinput[0]
  }
  all_results$geojson <- geoinput
  all_results$geojson[!is.na(all_results$sf_id)] <- locations_sf$geometry[all_results[!is.na(all_results$sf_id), ][["sf_id"]]]
  return(sf::st_sf(all_results, sf_column_name = "geojson"))
}

#' @title Pull taxonomy data
#' @description Pulls data from the taxonomy database
#'
#' @param username taxonomy username
#' @param api_key A working api.key for the user of the database
#' @param password taxonomy password
#' @param locations list of locations to pull. For now this only supports country ISO codes.
#' @param time_left  left bound for observation times (in date format)
#' @param time_right right bound for observation times (in date format)
#' @param uids list of unique observation collection ids to pull
#' @param website Which website to pull from (default is cholera-taxonomy.middle-distance.com)
#' @param source whether to pull data from the website or using sql on idmodeling2.
#' Needs to be one of 'api' or 'sql'.
#'
#' @details This is a wrapper which calls either read_taxonomy_data_api or
#' read_taxonomy_data_sql depending on the source that the user specifies.
#' @return An sf object containing data pulled from the database
#' @export
pull_taxonomy_data <- function(username, password, locations = NULL, time_left = NULL,
                               time_right = NULL, uids = NULL, website = "",
                               source) {
  if (missing(source) | is.null(source)) {
    stop("No source specified to pull taxonomy data, please specify one of 'api' or 'sql'.")
  }
  
  if (source == "api") {
    if (website == "") {
      website <- "https://cholera-taxonomy.middle-distance.com/"
    }
    if (missing(username) | missing(password) | is.null(username) | is.null(password) | (website == "")) {
      stop("Trying to pull data from API, please provide username and api_key.")
    }
    
    if (!is.null(time_left)) {
      time_left <- as.character(time_left)
    }
    
    
    if (!is.null(time_right)) {
      time_right <- as.character(time_right)
    }
    
    # Return API data pull
    rc <- read_taxonomy_data_api(
      username = username, 
      api_key = password, 
      locations = locations,
      time_left = time_left, 
      time_right = time_right,
      uids = uids, website = website
    )
  } else if (source == "sql") {
    if (website == "") {
      website <- "db.cholera-taxonomy.middle-distance.com"
    }
    if (missing(username) | missing(password) | is.null(username) | is.null(password) | (website == "")) {
      stop("Trying to pull data using sql on idemodelin2, please provide database username and password.")
    }
    
    # Return SQL data pull
    rc <- read_taxonomy_data_sql(
      username = username, password = password, locations = locations,
      time_left = time_left, time_right = time_right, uids = uids, host = website
    )
    rc$attributes.fields.suspected_cases <- rc$suspected_cases
    rc$attributes.fields.confirmed_cases <- rc$confirmed_cases
    rc$attributes.fields.location_id <- rc$location_id
    rc$attributes.location_period_id <- rc$location_period_id
  } else {
    stop("Parameter 'source' needs to be one of 'api' or 'sql'.")
  }
  
  if (nrow(rc) == 0) {
    if (!is.null(uids)) {
      err_mssg <- paste("in uids", paste(uids, collapse = ","))
    } else if (!is.null(locations)) {
      err_mssg <- paste("in locations", paste(locations, collapse = ","))
    } else {
      err_mssg <- ""
    }
    stop("Didn't find any data ", err_mssg, " in time range [", ifelse(is.null(time_left),
                                                                       "-Inf", as.character(time_left)
    ), " - ", ifelse(is.null(time_right),
                     "-nf", as.character(time_right)
    ), "]")
  }
  return(rc)
}


#' @title Taxonomy SQL data pull
#' @description Extracts observations for a set of locations from the taxonomy
#' PostgreSQL database using SQL. The database is selected with \code{dbname}:
#' "CholeraTaxonomy_production" (default) or "CholeraTaxonomy_staging".
#'
#' @details Self-contained by design: this function calls no other function of
#' the package. Changes compared with the previous version:
#' \itemize{
#'   \item Geometry: output contract unchanged (sf object, geometry in column
#'     \code{geojson}), but shapes are now fetched ONCE per distinct
#'     location_period_id in a second query and attached in R, instead of
#'     being joined in SQL and transferred again for every observation.
#'     Observations without location_period_id, or whose
#'     shape is NULL in the database, get an empty geometry, as before.
#'   \item CRS: EPSG:4326 as returned by the database (unchanged); set
#'     explicitly only as a safeguard if the database ever stops declaring it.
#'   \item Geography: EXISTS on location_hierarchies instead of a JOIN, so an
#'     observation is returned once even when several requested locations are
#'     nested ancestors of it (previously duplicated).
#'   \item Location name: resolved through location_period_id first (dated,
#'     consistent with administrative boundary changes and with the geometry,
#'     which joins on location_period_id only), then through
#'     observations.location_id as fallback; new column
#'     \code{location_via_fallback}. Previously: location_id only.
#'   \item Safeguard: the function stops if an observation is returned more
#'     than once (cannot happen with the current query; would reveal a
#'     schema change).
#'   \item Dates: Date, POSIXt or strict "YYYY-MM-DD" string only (ambiguous
#'     formats such as "04/03/2017" are rejected); bound as query parameters
#'     instead of being pasted into the SQL string.
#'   \item Connection always closed, including on error.
#'   \item Local connection (host = "localhost" with empty password) fixed: the
#'     previous version referenced an undefined variable \code{website}.
#'   \item Rows ordered by observation_collection_id, then id (deterministic).
#'   \item Arguments validated before connecting (ids, dates, bound order,
#'     flags); argument errors have class \code{taxdat_arg_error}. An
#'     unrecognised unified_dataset_behaviour now raises a warning.
#' }
#' Unchanged: filters on time, uids, collection status and unified datasets,
#' and their defaults. Observations without location_id are still excluded
#' (they cannot match the geographic filter).
#'
#' @param username taxonomy database username
#' @param password taxonomy database password (may be empty only when
#'   host = "localhost", for a local socket connection)
#' @param locations numeric vector of location ids (ancestor ids). Mandatory.
#' @param time_left left bound for observation times (Date, POSIXt or
#'   "YYYY-MM-DD"); keeps observations with time_left >= time_left
#' @param time_right right bound for observation times (Date, POSIXt or
#'   "YYYY-MM-DD"); keeps observations with time_right <= time_right
#' @param uids vector of observation_collection ids to restrict the pull to
#' @param discard_incomplete_observation_collections if TRUE (default),
#'   excludes collections with status initialized, validated or inprogress
#' @param unified_dataset_behaviour how to handle unified observation
#'   collections (OCs). Unified OCs are cleaner versions of existing OCs: they
#'   duplicate observations already present in those OCs, so keeping both would
#'   double count them, which is why the default is to drop them. "drop"
#'   (default) excludes unified OCs, "keep" keeps only them, any other value
#'   disables the filter.
#' @param host database host
#' @param dbname name of the taxonomy database to query. Either
#'   "CholeraTaxonomy_production" (default) or "CholeraTaxonomy_staging".
#'   A message tells the user which database is being queried.
#' @return An sf object, one row per observation; identifiers as text;
#'   geometry in column \code{geojson}.
#' @seealso \code{\link{pull_taxonomy_data}}
#' @export
read_taxonomy_data_sql <- function(username, password, locations = NULL,
                                   time_left = NULL, time_right = NULL,
                                   uids = NULL,
                                   discard_incomplete_observation_collections = TRUE,
                                   unified_dataset_behaviour = "drop",
                                   host = "db.cholera-taxonomy.middle-distance.com",
                                   dbname = "CholeraTaxonomy_production") {

  # ---- Argument checks (all done before opening any connection) -------------
  # Every argument error carries the class "taxdat_arg_error", so that callers
  # and tests can tell a bad call apart from a connection or query failure.
  arg_error <- function(...) {
    stop(errorCondition(paste0(...), class = "taxdat_arg_error", call = NULL))
  }

  # Positive whole-number ids, given as numbers or as digit-only strings
  is_valid_ids <- function(x) {
    if (is.numeric(x)) {
      length(x) > 0 && all(is.finite(x)) && all(x > 0) && all(x == round(x))
    } else if (is.character(x)) {
      length(x) > 0 && !anyNA(x) && all(grepl("^[1-9][0-9]*$", x))
    } else {
      FALSE
    }
  }
  is_single_string <- function(x) is.character(x) && length(x) == 1 && !is.na(x)

  # Database selection: only the two known taxonomy databases are accepted, so
  # that a typo cannot silently point the pull at an unintended database.
  allowed_dbnames <- c("CholeraTaxonomy_production", "CholeraTaxonomy_staging")
  if (!is_single_string(dbname) || !(dbname %in% allowed_dbnames)) {
    arg_error("dbname must be one of: ",
              paste0("'", allowed_dbnames, "'", collapse = ", "), ".")
  }
  # Tell the user which database is queried: "production" or "staging"
  message("-- Pulling data from the taxonomy '", sub("^CholeraTaxonomy_", "", dbname),
          "' database (", dbname, ") with SQL")

  # Credentials: single non-missing strings, username non-empty
  if (missing(username) || missing(password) ||
      !is_single_string(username) || !is_single_string(password) ||
      !nzchar(username)) {
    arg_error("Please provide username and password to connect to the taxonomy database.")
  }
  # Empty password is only meaningful for a local socket connection
  use_local_socket <- identical(host, "localhost") && !nzchar(password)
  if (!nzchar(password) && !use_local_socket) {
    arg_error("Empty password is only allowed with host = 'localhost'.")
  }

  # Geography: mandatory numeric location ids
  if (is.null(locations)) {
    arg_error("Please use a containing location as the location. Locations can't be NULL.")
  }
  if (!is.numeric(locations)) {
    arg_error("SQL access by location name is not yet implemented")
  }
  if (!is_valid_ids(locations)) {
    arg_error("locations must be positive whole numbers (location ids), without NA.")
  }

  # Observation collections: optional ids (numbers or digit strings)
  if (!is.null(uids) && !is_valid_ids(uids)) {
    arg_error("uids must be positive whole ids (numbers or digit strings), without NA.")
  }

  # Governance flags
  if (!is.logical(discard_incomplete_observation_collections) ||
      length(discard_incomplete_observation_collections) != 1 ||
      is.na(discard_incomplete_observation_collections)) {
    arg_error("discard_incomplete_observation_collections must be TRUE or FALSE.")
  }
  if (!(is.character(unified_dataset_behaviour) &&
        length(unified_dataset_behaviour) == 1 &&
        unified_dataset_behaviour %in% c("drop", "keep"))) {
    # Documented behaviour (any other value disables the filter) is kept for
    # backward compatibility, but made visible: a typo such as "Drop" would
    # otherwise silently include unified datasets.
    warning("unified_dataset_behaviour is neither 'drop' nor 'keep': ",
            "the unified-dataset filter is disabled.")
  }

  # Local date validator, defined inside the function on purpose: it is not a
  # package-level object, so it cannot clash with helpers defined elsewhere.
  # Returns NULL or a "YYYY-MM-DD" string.
  to_iso_date <- function(x, arg_name) {
    if (is.null(x)) return(NULL)
    if (length(x) != 1 || is.na(x)) {
      arg_error(arg_name, " must be a single, non-missing date.")
    }
    if (inherits(x, c("Date", "POSIXt"))) {
      # format() keeps the object's own time zone (as.Date() would use UTC)
      return(format(x, "%Y-%m-%d"))
    }
    if (is.character(x) && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)) {
      d <- as.Date(x, format = "%Y-%m-%d")  # explicit format: 2017-02-30 -> NA
      if (is.na(d)) arg_error(arg_name, " is not a valid calendar date: ", x)
      return(format(d, "%Y-%m-%d"))
    }
    arg_error(arg_name, " must be a Date, a POSIXt or a 'YYYY-MM-DD' string.")
  }
  time_left  <- to_iso_date(time_left,  "time_left")
  time_right <- to_iso_date(time_right, "time_right")
  if (!is.null(time_left) && !is.null(time_right) &&
      as.Date(time_left) > as.Date(time_right)) {
    arg_error("time_left (", time_left, ") is after time_right (", time_right, ").")
  }
  # Keep the "No time filters" prefix: matched by tests/testthat
  if (is.null(time_left))  warning("No time filters: time_left is NULL.")
  if (is.null(time_right)) warning("No time filters: time_right is NULL.")
  if (is.null(uids))       warning("No uid filters.")

  # ---- Connection, always closed (also on error) ----------------------------
  conn_args <- list(
    RPostgres::Postgres(),
    dbname = dbname,
    user   = username,
    port   = Sys.getenv("CHOLERA_POSTGRES_PORT", "5432")
  )
  if (!use_local_socket) {
    # Remote connection; without host/password RPostgres uses the local socket
    conn_args <- c(conn_args, list(host = host, password = password))
  }
  conn <- do.call(DBI::dbConnect, conn_args)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  # ---- Filters (user values are bound by glue_sql, never pasted) ------------
  filters <- c(
    # Geography: EXISTS never duplicates rows (a JOIN does with nested ancestors)
    paste("EXISTS (SELECT 1 FROM location_hierarchies h",
          "WHERE h.descendant_id = o.location_id",
          "AND h.ancestor_id IN ({locations*}))"),
    if (!is.null(time_left))  "o.time_left >= {time_left}::date",
    if (!is.null(time_right)) "o.time_right <= {time_right}::date",
    if (!is.null(uids))       "o.observation_collection_id IN ({uids*})",
    if (isTRUE(discard_incomplete_observation_collections)) {
      paste("(oc.status != 'initialized') AND (oc.status != 'validated')",
            "AND (oc.status != 'inprogress')")
    },
    if (identical(unified_dataset_behaviour, "drop")) {
      "((oc.unified IS NULL) OR (oc.unified != 't'))"
    } else if (identical(unified_dataset_behaviour, "keep")) {
      "((oc.unified IS NOT NULL) AND (oc.unified))"
    }
  )

  # ---- Query ------------------------------------------------------------------
  # Location name: location period first, observations.location_id as fallback.
  # No geometry in this query: shapes are fetched once per location period
  # in a second query below, then attached in R.
  query <- paste(
    "SELECT",
    "  o.id::text                        AS id,",
    "  o.observation_collection_id::text AS observation_collection_id,",
    "  o.time_left, o.time_right,",
    "  o.suspected_cases, o.confirmed_cases, o.deaths,",
    "  o.phantom, o.\"primary\" AS \"primary\",",
    "  COALESCE(l_via_lp.qualified_name, l_direct.qualified_name) AS location_name,",
    "  (l_via_lp.qualified_name IS NULL",
    "   AND l_direct.qualified_name IS NOT NULL)                  AS location_via_fallback,",
    "  o.location_id::text        AS location_id,",
    "  o.location_period_id::text AS location_period_id",
    "FROM observations o",
    "JOIN observation_collections oc ON oc.id = o.observation_collection_id",
    "LEFT JOIN location_periods lp   ON lp.id = o.location_period_id",
    "LEFT JOIN locations l_via_lp    ON l_via_lp.id = lp.location_id",
    "LEFT JOIN locations l_direct    ON l_direct.id = o.location_id",
    "WHERE", paste(filters, collapse = " AND "),
    "ORDER BY o.observation_collection_id, o.id"
  )
  query <- glue::glue_sql(query, .con = conn,
                          locations = locations, uids = uids,
                          time_left = time_left, time_right = time_right)

  observations <- DBI::dbGetQuery(conn, query)

  if (nrow(observations) == 0) {
    stop("No observations found for the given filters.")
  }

  # ---- Invariant: one row per observation -----------------------------------
  # Guaranteed by the EXISTS geographic filter and the many-to-one joins;
  # a failure here means the schema has changed.
  if (anyDuplicated(observations$id) > 0) {
    n_dup <- sum(duplicated(observations$id))
    stop(n_dup, " duplicated observation rows: unexpected, check the joins ",
         "on location_periods and locations.")
  }

  # ---- Geometry: one fetch per distinct location period, attached in R ------
  # shapes.location_period_id has a unique index: at most one shape per period.
  # Bound as numbers so that the index on the bigint column is used.
  lp_ids <- unique(observations$location_period_id[!is.na(observations$location_period_id)])
  shapes <- NULL
  if (length(lp_ids) > 0) {
    shape_query <- glue::glue_sql(
      paste("SELECT sh.location_period_id::text AS location_period_id,",
            "sh.shape AS geojson",
            "FROM shapes sh",
            "WHERE sh.location_period_id IN ({lp_ids*})"),
      .con = conn, lp_ids = as.numeric(lp_ids))
    shapes <- suppressWarnings(sf::st_read(conn, query = shape_query))
    # st_read returns a plain data.frame when no row has a geometry
    if (!inherits(shapes, "sf") || nrow(shapes) == 0) shapes <- NULL
  }

  # Geometry vector: fetched shapes + one empty geometry (last position) used
  # for observations without location_period_id or without a shape.
  empty_geom <- sf::st_sfc(sf::st_geometrycollection())
  if (is.null(shapes)) {
    geoms  <- empty_geom
    lp_key <- character(0)
  } else {
    shape_geoms <- sf::st_geometry(shapes)
    sf::st_crs(empty_geom) <- sf::st_crs(shape_geoms)  # c() requires the same CRS
    geoms  <- c(shape_geoms, empty_geom)
    lp_key <- shapes$location_period_id
  }
  idx <- match(observations$location_period_id, lp_key)
  idx[is.na(idx)] <- length(geoms)
  # Subsetting repeats references to the same geometry objects; it does not
  # transfer or decode them again.
  observations <- sf::st_sf(observations, geojson = geoms[idx])

  # ---- CRS: not declared at column level in the database; geometries are WGS84
  if (is.na(sf::st_crs(observations))) {
    sf::st_crs(observations) <- 4326
  }

  observations
}

#' @title Taxonomy SQL data pull
#' @description Extracts data for a given set of country using SQL from the taxonomy
#' postgresql database stored on idmodeling2
#'
#' @param username taxonomy username
#' @param password taxonomy password
#' @param locations list of locations to pull. For now this only supports country ISO codes.
#' @param time_left  left bound for observation times (in date format)
#' @param time_right right bound for observation times (in date format)
#' @param uids list of unique observation collection ids to pull
#' @param discard_incomplete_observation_collections whether to discard incomplete observation collections (default is TRUE)
#' @param unified_dataset_behaviour whether to keep, drop or select unified observation collections (default is drop)
#' @param remove_private whether observations are from public data source
#'
#' @details Code follows taxdat::read_taxonomy_data_sql template.
#' @return A data frame object containing data extracted from the database
#' @export
read_taxonomy_observations_sql <- function(username, password, locations = NULL, time_left = NULL,
                                   time_right = NULL, uids = NULL, 
                                   discard_incomplete_observation_collections = TRUE, 
                                   unified_dataset_behaviour = "drop", 
                                   remove_private = TRUE,
                                   host = "db.cholera-taxonomy.middle-distance.com") {
  if (missing(username) | missing(password)) {
    stop("Please provide username and password to connect to the taxonomy database.")
  }
  
  # Connect to database
  if ((password == "") && (website == "localhost")) {
    print("HERE")
    conn <- RPostgres::dbConnect(RPostgres::Postgres(),
                                 dbname = "CholeraTaxonomy_production", user = username,
                                 port = Sys.getenv("CHOLERA_POSTGRES_PORT", "5432")
    )
  } else {
    conn <- RPostgres::dbConnect(RPostgres::Postgres(),
                                 host = host,
                                 dbname = "CholeraTaxonomy_production", user = username, password = password,
                                 port = Sys.getenv("CHOLERA_POSTGRES_PORT", "5432")
    )
  }
  
  # Build query for observations
  obs_query <- paste(
    "SELECT", "observations.observation_collection_id, observations.time_left, observations.time_right,observations.suspected_cases, observations.confirmed_cases, observations.deaths, observations.phantom, observations.primary,observations.tested,observations.location_period_id,observations.location_id,",
    "observation_collections.is_public,observation_collections.unified,observation_collections.status,",
    "locations.qualified_name as location_name",
    "FROM", "observations",
    " left join observation_collections on observations.observation_collection_id = observation_collections.id",
    " left join locations on observations.location_id = locations.id",
    " WHERE"
  ) 
  #observations.data includes the all the columns in observations: CFR, etc but also includes all the other observations)
  
  cat("-- Pulling data from taxonomy database with SQL \n")
  
  if (unified_dataset_behaviour == "drop") {
    unified_filter <- c("((observation_collections.unified is NULL) OR (observation_collections.unified!='t'))") # QZ: updated the operator
  } else if (unified_dataset_behaviour == "keep") {
    unified_filter <- c("((observation_collections.unified is NULL) OR (observation_collections.unified='t'))")
  } else if (unified_dataset_behaviour == "select") {
    unified_filter <- c("(observation_collections.unified ='t')")
  } else {
    unified_filter <- NULL
  }
  
  if (discard_incomplete_observation_collections) {
    oc_filter <- c("(observation_collections.status != 'initialized') AND (observation_collections.status != 'validated') AND (observation_collections.status != 'inprogress')")
  } else {
    oc_filter <- NULL
  }
  
  # Add filters
  if (any(c(!is.null(time_left), !is.null(time_right), !is.null(uids)), !is.null(locations))) {
  } else {
    warning("No filters specified on data pull, pulling all data.")
  }
  
  if (!is.null(time_left)) {
    time_left_filter <- paste0(
      "time_left >= '", format(time_left, "%Y-%m-%d"),
      "'"
    )
  } else {
    time_left_filter <- NULL
    warning("No time filters.")
  }
  
  if (!is.null(time_right)) {
    time_right_filter <- paste0(
      "time_right <= '", format(time_right, "%Y-%m-%d"),
      "'"
    )
  } else {
    time_right_filter <- NULL
    warning("No time filters.")
  }
  
  if (!is.null(uids)) {
    uids_filter <- paste0("observation_collection_id IN ({uids*})")
  } else {
    uids_filter <- NULL
    warning("No uid filters.")
  }
  
  if(remove_private){
    private_filter <- paste0("is_public = TRUE")
  } else {
    private_filter <- NULL
    warning("Observations from private sources are pulled.")
  }
  
  # Combine filters
  filters <- c(time_left_filter, time_right_filter, uids_filter, oc_filter, unified_filter,private_filter) %>%
    paste(collapse = " AND ")
  
  # Run query for observations
  obs_query <- glue::glue_sql(paste(obs_query, filters, ";"), .con = conn)
  observations <- DBI::dbGetQuery(conn, obs_query)
  if (nrow(observations) == 0) {
    stop(paste0("No observations found using query ||", obs_query, "||"))
  }
  
  return(observations)
}


#' @title Taxonomy SQL data pull
#' @description Extracts metadata for observation collections using SQL from the taxonomy
#' postgresql database stored on idmodeling2
#'
#' @param username taxonomy username
#' @param password taxonomy password
#' @param locations list of locations to pull. For now this only supports country ISO codes.
#' @param time_left  left bound for observation times (in date format)
#' @param time_right right bound for observation times (in date format)
#' @param uids list of unique observation collection ids to pull
#' @param discard_incomplete_observation_collections whether to discard incomplete observation collections (default is TRUE)
#' @param unified_dataset_behaviour whether to keep, drop or select unified observation collections (default is drop)
#' @param remove_private remove private data sources
#'
#' @details Code follows taxdat::read_taxonomy_data_sql template.
#' @return A data frame object containing data extracted from the database
#' @export
read_taxonomy_oc_metadata_sql <- function(username, password, locations = NULL, time_left = NULL,
                                           time_right = NULL, uids = NULL, 
                                           discard_incomplete_observation_collections = TRUE, 
                                           unified_dataset_behaviour = "drop", 
                                           remove_private = TRUE,
                                           host = "db.cholera-taxonomy.middle-distance.com") {
  if (missing(username) | missing(password)) {
    stop("Please provide username and password to connect to the taxonomy database.")
  }
  
  # Connect to database
  if ((password == "") && (website == "localhost")) {
    print("HERE")
    conn <- RPostgres::dbConnect(RPostgres::Postgres(),
                                 dbname = "CholeraTaxonomy_production", user = username,
                                 port = Sys.getenv("CHOLERA_POSTGRES_PORT", "5432")
    )
  } else {
    conn <- RPostgres::dbConnect(RPostgres::Postgres(),
                                 host = host,
                                 dbname = "CholeraTaxonomy_production", user = username, password = password,
                                 port = Sys.getenv("CHOLERA_POSTGRES_PORT", "5432")
    )
  }
  
  # Build query for observation collection metadata
  oc_query <- paste(
    "SELECT", "id as observation_collection_id, is_public, owner, contact, source, source_url, notes, created_at,unified",
    "FROM", "observation_collections",
    " WHERE"
  ) 

  if (unified_dataset_behaviour == "drop") {
    unified_filter <- c("((observation_collections.unified is NULL) OR (observation_collections.unified!='t'))") # QZ: updated the operator
  } else if (unified_dataset_behaviour == "keep") {
    unified_filter <- c("((observation_collections.unified is NULL) OR (observation_collections.unified='t'))")
  } else if (unified_dataset_behaviour == "select") {
    unified_filter <- c("(observation_collections.unified='t')")
  } else {
    unified_filter <- NULL
  }
  
  if (discard_incomplete_observation_collections) {
    oc_filter <- c("(observation_collections.status != 'initialized') AND (observation_collections.status != 'validated') AND (observation_collections.status != 'inprogress')")
  } else {
    oc_filter <- NULL
  }
  
  if (!is.null(uids)) {
    uids_filter <- paste0("id IN ({uids*})")
  } else {
    uids_filter <- NULL
    warning("No uid filters.")
  }
  
  if(remove_private){
    private_filter <- paste0("is_public = TRUE")
  } else {
    private_filter <- NULL
    warning("Private observation collections are pulled.")
  }
  
  # Combine filters
  filters <- c(uids_filter, unified_filter, oc_filter, private_filter) %>%
    paste(collapse = " AND ")
  
  cat("-- Pulling observation collection metadata from taxonomy database with SQL \n")
  
  # Run query for observation collection metadata
  oc_query <- glue::glue_sql(paste(oc_query, filters, ";"), .con = conn)
  observation_collection <- DBI::dbGetQuery(conn, oc_query)
  if (nrow(observation_collection) == 0) {
    stop(paste0("No observation collection metadata found using query ||", oc_querys, "||"))
  }
  
  return(observation_collection)
}

#' @title Taxonomy SQL data pull
#' @description Extracts data for a given set of country using SQL from the taxonomy
#' postgresql database stored on idmodeling2
#'
#' @param username taxonomy username
#' @param password taxonomy password
#' @param location_period list of location periods to pull.
#'
#' @details Code follows taxdat::read_taxonomy_data_sql template.
#' @return An sf object containing data extracted from the database
#' @export
read_taxonomy_locationperiods_sql <- function(username, password, location_period = NULL,
                                          host = "db.cholera-taxonomy.middle-distance.com") {
  if (missing(username) | missing(password)) {
    stop("Please provide username and password to connect to the taxonomy database.")
  }
  
  # Connect to database
  if ((password == "") && (website == "localhost")) {
    print("HERE")
    conn <- RPostgres::dbConnect(RPostgres::Postgres(),
                                 dbname = "CholeraTaxonomy_production", user = username,
                                 port = Sys.getenv("CHOLERA_POSTGRES_PORT", "5432")
    )
  } else {
    conn <- RPostgres::dbConnect(RPostgres::Postgres(),
                                 host = host,
                                 dbname = "CholeraTaxonomy_production", user = username, password = password,
                                 port = Sys.getenv("CHOLERA_POSTGRES_PORT", "5432")
    )
  }
  
  # Build query for location periods and shapefiles
  lp_query <- paste(
    "SELECT DISTINCT", 
    "shape as geojson, location_period_id",
    "FROM shapes",
    " WHERE"
  )

  if (!is.null(location_period)) {
    if (all(is.numeric(location_period))) {
      location_period_filter <- paste0("location_period_id in ({location_period*})")
    } else {
      stop("SQL access by location_period is not yet implemented")
    }
  } else {
    stop("Please use a containing location as the location. Locations can't be NULL.")
  }
  
  cat("-- Pulling location periods and shapefiles from taxonomy database with SQL \n")

  # Run query for location period and shapefiles
  lp_query <- glue::glue_sql(paste(lp_query,location_period_filter," ;"), .con = conn)
  location_periods <- suppressWarnings(sf::st_as_sf(sf::st_read(conn, query = lp_query)))

  if (nrow(location_periods) == 0) {
    stop(paste0("No location periods found using query ||", lp_query, "||"))
  }
  
  return(location_periods)
}

#' @title Taxonomy SQL age-specific data pull
#' @description Extracts age-specific data for a given set of country using SQL from the taxonomy
#' postgresql database stored on idmodeling2
#'
#' @param username taxonomy username
#' @param password taxonomy password
#' @param locations list of locations to pull. For now this only supports country ISO codes.
#' @param time_left  left bound for observation times (in date format)
#' @param time_right right bound for observation times (in date format)
#' @param uids list of unique observation collection ids to pull
#' @param discard_incomplete_observation_collections whether to include OCs that are not in Initial Data Entry Complete status
#' @param unified_dataset_behaviour whether to include unified OCs
#' @param include_geojson whether to pull shapefiles together
#'
#' @details Code follows taxdat::read_taxonomy_data_api template.
#' @return An sf or non sf object containing data extracted from the database
#' @export
read_taxonomy_age_data_sql <- function(username, password, locations = NULL, time_left = NULL,
                                       time_right = NULL, uids = NULL, discard_incomplete_observation_collections = TRUE, unified_dataset_behaviour = "drop", host = "db.cholera-taxonomy.middle-distance.com", include_geojson = TRUE) {
  library(dplyr)
  if (missing(username) | missing(password)) {
    stop("Please provide username and password to connect to the taxonomy database.")
  }
  
  # Connect to database
  if ((password == "") && (website == "localhost")) {
    print("HERE")
    conn <- RPostgres::dbConnect(RPostgres::Postgres(),
                                 dbname = "CholeraTaxonomy_production", user = username,
                                 port = Sys.getenv("CHOLERA_POSTGRES_PORT", "5432")
    )
  } else {
    conn <- RPostgres::dbConnect(RPostgres::Postgres(),
                                 host = host,
                                 dbname = "CholeraTaxonomy_production", user = username, password = password,
                                 port = Sys.getenv("CHOLERA_POSTGRES_PORT", "5432")
    )
  }
  
  # Build query for observations
  obs_query <- paste(
    "SELECT",
    "observation_collections.is_public,",
    "observation_collections.unified,",
    "observation_collections.status,",
    "observations.observation_collection_id::text,",
    "observations.time_left AS TL,",
    "observations.time_right AS TR,",
    "observations.confirmed_cases AS cCh,",
    "observations.suspected_cases AS sCh,",
    "observations.data->>'age_L' AS age_l,",
    "observations.data->>'age_R' AS age_r,",
    "observations.data->>'age'  AS age,",
    "observations.deaths  AS deaths,",
    "observations.phantom AS phantom,",
    "observations.primary AS data_primary,",
    "locations.qualified_name AS location_name,",
    "locations.id::text AS location_id,",
    "location_periods.id::text AS location_period_id",
    if (include_geojson) ", shapes.shape AS geojson" else "",
    "FROM observations",
    "JOIN observation_collections ON observation_collections.id = observations.observation_collection_id",
    "LEFT JOIN location_hierarchies ON observations.location_id = location_hierarchies.descendant_id",
    "LEFT JOIN locations ON observations.location_id = locations.id",
    "LEFT JOIN location_periods ON observations.location_period_id = location_periods.id",
    if (include_geojson) "LEFT JOIN shapes ON shapes.location_period_id = location_periods.id" else "",
    "WHERE EXISTS (",
    "  SELECT 1 FROM custom_fields",
    "  WHERE custom_fields.observation_collection_id = observations.observation_collection_id",
    ")",
    "AND (observations.data->>'age' IS NOT NULL",
    "  OR observations.data->>'age_L' IS NOT NULL",
    "  OR observations.data->>'age_R' IS NOT NULL)"
  )
  
  cat("-- Pulling data from taxonomy database with SQL \n")
  
  if (unified_dataset_behaviour == "drop") {
    unified_filter <- c("((observation_collections.unified is NULL) OR (observation_collections.unified!='t'))") # QZ: updated the operator
  } else if (unified_dataset_behaviour == "keep") {
    unified_filter <- c("((observation_collections.unified is NOT NULL) AND (observation_collections.unified))")
  } else {
    unified_filter <- NULL
  }
  if (discard_incomplete_observation_collections) {
    oc_filter <- c("(observation_collections.status != 'initialized') AND (observation_collections.status != 'validated') AND (observation_collections.status != 'inprogress')")
  } else {
    oc_filter <- NULL
  }
  # Add filters
  if (!is.null(time_left)) {
    time_left_filter <- paste0("observations.time_left >= DATE '", format(time_left, "%Y-%m-%d"), "'")
  } else {
    time_left_filter <- NULL
    warning("No time filters.")
  }
  
  if (!is.null(time_right)) {
    time_right_filter <- paste0("observations.time_right <= DATE '", format(time_right, "%Y-%m-%d"), "'")
  } else {
    time_right_filter <- NULL
    warning("No time filters.")
  }
  
  if (!is.null(locations)) {
    if (all(is.numeric(locations))) {
      locations_filter <- paste0("location_hierarchies.ancestor_id in ({locations*})")
    } else {
      stop("SQL access by location name is not yet implemented")
    }
  } else {
    locations_filter <- paste0("location_hierarchies.ancestor_id = location_hierarchies.descendant_id")
    stop("Please use a containing location as the location. Locations can't be NULL.")
  }
  
  if (!is.null(uids)) {
    uids_filter <- paste0("observations.observation_collection_id IN ({uids*})")
  } else {
    uids_filter <- NULL
    warning("No uid filters.")
  }
  
  # Combine filters
  filters <- c(time_left_filter, time_right_filter, locations_filter, uids_filter, oc_filter, unified_filter)
  filters <- paste(filters[!is.null(filters)], collapse = " AND ")
  
  # Run query for observations
  obs_query <- glue::glue_sql(paste(obs_query, "AND", filters), .con = conn)
  
  if(include_geojson){
    observations <- suppressWarnings(sf::st_as_sf(sf::st_read(conn, query = obs_query)))
  } else {
    observations <- DBI::dbGetQuery(conn, obs_query)
  }
  if (nrow(observations) == 0) {
    stop(paste0("No observations found using query ||", obs_query, "||"))
  }
  
  # observations <- dplyr::filter(observations, !is.na(nchar(geojson)))
  return(observations)
}

#' @title Taxonomy SQL sex-specific data pull
#' @description Extracts sex-specific data for a given set of country using SQL from the taxonomy
#' postgresql database stored on idmodeling2
#'
#' @param username taxonomy username
#' @param password taxonomy password
#' @param locations list of locations to pull. For now this only supports country ISO codes.
#' @param time_left  left bound for observation times (in date format)
#' @param time_right right bound for observation times (in date format)
#' @param uids list of unique observation collection ids to pull
#' @param discard_incomplete_observation_collections whether to include OCs that are not in Initial Data Entry Complete status
#' @param unified_dataset_behaviour whether to include unified OCs
#' @param include_geojson whether to pull shapefiles together
#'
#' @details Code follows taxdat::read_taxonomy_data_api template.
#' @return An sf or non sf object containing data extracted from the database
#' @export
read_taxonomy_sex_data_sql <- function(username, password, locations = NULL, time_left = NULL,
                                       time_right = NULL, uids = NULL, discard_incomplete_observation_collections = TRUE, unified_dataset_behaviour = "drop", host = "db.cholera-taxonomy.middle-distance.com", include_geojson = TRUE) {
  library(dplyr)
  if (missing(username) | missing(password)) {
    stop("Please provide username and password to connect to the taxonomy database.")
  }
  
  # Connect to database
  if ((password == "") && (website == "localhost")) {
    print("HERE")
    conn <- RPostgres::dbConnect(RPostgres::Postgres(),
                                 dbname = "CholeraTaxonomy_production", user = username,
                                 port = Sys.getenv("CHOLERA_POSTGRES_PORT", "5432")
    )
  } else {
    conn <- RPostgres::dbConnect(RPostgres::Postgres(),
                                 host = host,
                                 dbname = "CholeraTaxonomy_production", user = username, password = password,
                                 port = Sys.getenv("CHOLERA_POSTGRES_PORT", "5432")
    )
  }
  
  # Build query for observations
  obs_query <- paste(
    "SELECT",
    "observation_collections.is_public,",
    "observation_collections.unified,",
    "observation_collections.status,",
    "observations.observation_collection_id::text,",
    "observations.time_left AS TL,",
    "observations.time_right AS TR,",
    "observations.confirmed_cases AS cCh,",
    "observations.suspected_cases AS sCh,",
    "observations.data->>'sex' AS sex,",
    "observations.deaths  AS deaths,",
    "observations.phantom AS phantom,",
    "observations.primary AS data_primary,",
    "locations.qualified_name AS location_name,",
    "locations.id::text AS location_id,",
    "location_periods.id::text AS location_period_id",
    if (include_geojson) ", shapes.shape AS geojson" else "",
    "FROM observations",
    "JOIN observation_collections ON observation_collections.id = observations.observation_collection_id",
    "LEFT JOIN location_hierarchies ON observations.location_id = location_hierarchies.descendant_id",
    "LEFT JOIN locations ON observations.location_id = locations.id",
    "LEFT JOIN location_periods ON observations.location_period_id = location_periods.id",
    if (include_geojson) "LEFT JOIN shapes ON shapes.location_period_id = location_periods.id" else "",
    "WHERE EXISTS (",
    "  SELECT 1 FROM custom_fields",
    "  WHERE custom_fields.observation_collection_id = observations.observation_collection_id",
    ")",
    "AND (observations.data->>'sex' IS NOT NULL)"
  )
  
  cat("-- Pulling data from taxonomy database with SQL \n")
  
  if (unified_dataset_behaviour == "drop") {
    unified_filter <- c("((observation_collections.unified is NULL) OR (observation_collections.unified!='t'))") # QZ: updated the operator
  } else if (unified_dataset_behaviour == "keep") {
    unified_filter <- c("((observation_collections.unified is NOT NULL) AND (observation_collections.unified))")
  } else {
    unified_filter <- NULL
  }
  if (discard_incomplete_observation_collections) {
    oc_filter <- c("(observation_collections.status != 'initialized') AND (observation_collections.status != 'validated') AND (observation_collections.status != 'inprogress')")
  } else {
    oc_filter <- NULL
  }
  # Add filters
  if (!is.null(time_left)) {
    time_left_filter <- paste0("observations.time_left >= DATE '", format(time_left, "%Y-%m-%d"), "'")
  } else {
    time_left_filter <- NULL
    warning("No time filters.")
  }
  
  if (!is.null(time_right)) {
    time_right_filter <- paste0("observations.time_right <= DATE '", format(time_right, "%Y-%m-%d"), "'")
  } else {
    time_right_filter <- NULL
    warning("No time filters.")
  }
  
  if (!is.null(locations)) {
    if (all(is.numeric(locations))) {
      locations_filter <- paste0("location_hierarchies.ancestor_id in ({locations*})")
    } else {
      stop("SQL access by location name is not yet implemented")
    }
  } else {
    locations_filter <- paste0("location_hierarchies.ancestor_id = location_hierarchies.descendant_id")
    stop("Please use a containing location as the location. Locations can't be NULL.")
  }
  
  if (!is.null(uids)) {
    uids_filter <- paste0("observations.observation_collection_id IN ({uids*})")
  } else {
    uids_filter <- NULL
    warning("No uid filters.")
  }
  
  # Combine filters
  filters <- c(time_left_filter, time_right_filter, locations_filter, uids_filter, oc_filter, unified_filter)
  filters <- paste(filters[!is.null(filters)], collapse = " AND ")
  
  # Run query for observations
  obs_query <- glue::glue_sql(paste(obs_query, "AND", filters), .con = conn)
  
  if(include_geojson){
    observations <- suppressWarnings(sf::st_as_sf(sf::st_read(conn, query = obs_query)))
  } else {
    observations <- DBI::dbGetQuery(conn, obs_query)
  }
  if (nrow(observations) == 0) {
    stop(paste0("No observations found using query ||", obs_query, "||"))
  }
  
  # observations <- dplyr::filter(observations, !is.na(nchar(geojson)))
  return(observations)
}
