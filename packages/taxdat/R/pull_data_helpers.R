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
#' @description Extracts data for a given set of country using SQL from the taxonomy
#' postgresql database stored on idmodeling2
#'
#' @param username taxonomy username
#' @param password taxonomy password
#' @param locations list of locations to pull. For now this only supports country ISO codes.
#' @param time_left  left bound for observation times (in date format)
#' @param time_right right bound for observation times (in date format)
#' @param uids list of unique observation collection ids to pull
#'
#' @details Code follows taxdat::read_taxonomy_data_api template.
#' @return An sf object containing data extracted from the database
#' @export
read_taxonomy_data_sql <- function(username, password, locations = NULL, time_left = NULL,
                                   time_right = NULL, uids = NULL, discard_incomplete_observation_collections = TRUE, unified_dataset_behaviour = "drop", host = "db.cholera-taxonomy.middle-distance.com") {
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
    "SELECT", "observations.id::text, observations.observation_collection_id::text, observations.time_left, observations.time_right,observations.suspected_cases, observations.confirmed_cases, observations.deaths, observations.phantom, observations.primary",
    ",locations.qualified_name as location_name, locations.id::text as location_id",
    ",location_periods.id::text as location_period_id", ",shapes.shape as geojson",
    "FROM", "observations", "left join observation_collections on observations.observation_collection_id = observation_collections.id",
    "left join location_hierarchies on observations.location_id = location_hierarchies.descendant_id",
    "left join locations on observations.location_id = locations.id", "left join location_periods on observations.location_period_id = location_periods.id",
    "left join shapes on shapes.location_period_id = location_periods.id", " WHERE"
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
  
  if (!is.null(locations)) {
    if (all(is.numeric(locations))) {
      locations_filter <- paste0("ancestor_id in ({locations*})")
    } else {
      stop("SQL access by location name is not yet implemented")
    }
  } else {
    locations_filter <- paste0("ancestor_id = descendant_id")
    stop("Please use a containing location as the location. Locations can't be NULL.")
  }
  
  if (!is.null(uids)) {
    uids_filter <- paste0("observation_collection_id IN ({uids*})")
  } else {
    uids_filter <- NULL
    warning("No uid filters.")
  }
  
  # Combine filters
  filters <- c(time_left_filter, time_right_filter, locations_filter, uids_filter, oc_filter, unified_filter) %>%
    paste(collapse = " AND ")
  
  # Run query for observations
  obs_query <- glue::glue_sql(paste(obs_query, filters, ";"), .con = conn)
  observations <- suppressWarnings(sf::st_as_sf(sf::st_read(conn, query = obs_query)))
  if (nrow(observations) == 0) {
    stop(paste0("No observations found using query ||", obs_query, "||"))
  }
  
  # observations <- dplyr::filter(observations, !is.na(nchar(geojson)))
  return(observations)
}

#' @title Taxonomy staging SQL data pull
#' @description Extracts data for a given set of country using SQL from the taxonomy staging
#' postgresql database stored on idmodeling2
#'
#' @param username taxonomy username
#' @param password taxonomy password
#' @param locations list of locations to pull. For now this only supports country ISO codes.
#' @param time_left  left bound for observation times (in date format)
#' @param time_right right bound for observation times (in date format)
#' @param uids list of unique observation collection ids to pull
#'
#' @details Code follows taxdat::read_taxonomy_data_api template.
#' @return An sf object containing data extracted from the database
#' @export
read_taxonomy_data_sql_staging <- function(username, password, locations = NULL, time_left = NULL,
                                   time_right = NULL, uids = NULL, discard_incomplete_observation_collections = TRUE, unified_dataset_behaviour = "drop", host = "db.cholera-taxonomy.middle-distance.com") {
  if (missing(username) | missing(password)) {
    stop("Please provide username and password to connect to the taxonomy staging database.")
  }
  
  # Connect to database
  if ((password == "") && (website == "localhost")) {
    print("HERE")
    conn <- RPostgres::dbConnect(RPostgres::Postgres(),
                                 dbname = "CholeraTaxonomy_staging", user = username,
                                 port = Sys.getenv("CHOLERA_POSTGRES_PORT", "5432")
    )
  } else {
    conn <- RPostgres::dbConnect(RPostgres::Postgres(),
                                 host = host,
                                 dbname = "CholeraTaxonomy_staging", user = username, password = password,
                                 port = Sys.getenv("CHOLERA_POSTGRES_PORT", "5432")
    )
  }
  
  # Build query for observations
  obs_query <- paste(
    "SELECT", "observations.id::text, observations.observation_collection_id::text, observations.time_left, observations.time_right,observations.suspected_cases, observations.confirmed_cases, observations.deaths, observations.phantom, observations.primary",
    ",locations.qualified_name as location_name, locations.id::text as location_id",
    ",location_periods.id::text as location_period_id", ",shapes.shape as geojson",
    "FROM", "observations", "left join observation_collections on observations.observation_collection_id = observation_collections.id",
    "left join location_hierarchies on observations.location_id = location_hierarchies.descendant_id",
    "left join locations on observations.location_id = locations.id", "left join location_periods on observations.location_period_id = location_periods.id",
    "left join shapes on shapes.location_period_id = location_periods.id", " WHERE"
  )
  
  cat("-- Pulling data from taxonomy staging database with SQL \n")
  
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
  
  if (!is.null(locations)) {
    if (all(is.numeric(locations))) {
      locations_filter <- paste0("ancestor_id in ({locations*})")
    } else {
      stop("SQL access by location name is not yet implemented")
    }
  } else {
    locations_filter <- paste0("ancestor_id = descendant_id")
    stop("Please use a containing location as the location. Locations can't be NULL.")
  }
  
  if (!is.null(uids)) {
    uids_filter <- paste0("observation_collection_id IN ({uids*})")
  } else {
    uids_filter <- NULL
    warning("No uid filters.")
  }
  
  # Combine filters
  filters <- c(time_left_filter, time_right_filter, locations_filter, uids_filter, oc_filter, unified_filter) %>%
    paste(collapse = " AND ")
  
  # Run query for observations
  obs_query <- glue::glue_sql(paste(obs_query, filters, ";"), .con = conn)
  observations <- suppressWarnings(sf::st_as_sf(sf::st_read(conn, query = obs_query)))
  if (nrow(observations) == 0) {
    stop(paste0("No observations found using query ||", obs_query, "||"))
  }
  
  # observations <- dplyr::filter(observations, !is.na(nchar(geojson)))
  return(observations)
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

#' @title Raise an argument error
#' @description Error of class "taxdat_arg_error", so that callers and tests
#'   can tell a bad call apart from a connection or query failure. Same class
#'   as in read_taxonomy_data_sql (which keeps a local copy, self-contained).
#' @noRd
.taxdat_arg_error <- function(...) {
  stop(errorCondition(paste0(...), class = "taxdat_arg_error", call = NULL))
}

#' @title Positive whole-number ids, as numbers or digit-only strings
#' @noRd
.is_valid_ids <- function(x) {
  if (is.numeric(x)) {
    length(x) > 0 && all(is.finite(x)) && all(x > 0) && all(x == round(x))
  } else if (is.character(x)) {
    length(x) > 0 && !anyNA(x) && all(grepl("^[1-9][0-9]*$", x))
  } else {
    FALSE
  }
}

#' @title Single non-missing string
#' @noRd
.is_single_string <- function(x) is.character(x) && length(x) == 1 && !is.na(x)

#' @title Validate a date argument (strict ISO)
#' @description Accepts a single Date, POSIXt or "YYYY-MM-DD" string and
#'   returns it as an ISO string. Rejects ambiguous inputs such as
#'   "04/03/2017", which a server with DateStyle = MDY would silently read as
#'   3 April 2017. Errors have class "taxdat_arg_error".
#' @param x A Date, a POSIXt, a character string, or NULL.
#' @param arg_name Argument name, used in error messages.
#' @return NULL, or a "YYYY-MM-DD" string.
#' @noRd
.validate_iso_date <- function(x, arg_name) {
  if (is.null(x)) return(NULL)
  if (length(x) != 1 || is.na(x)) {
    .taxdat_arg_error(arg_name, " must be a single, non-missing date.")
  }
  if (inherits(x, c("Date", "POSIXt"))) {
    # format() keeps the object's own time zone (as.Date() would use UTC)
    return(format(x, "%Y-%m-%d"))
  }
  if (is.character(x) && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)) {
    d <- as.Date(x, format = "%Y-%m-%d")  # explicit format: 2017-02-30 -> NA
    if (is.na(d)) .taxdat_arg_error(arg_name, " is not a valid calendar date: ", x)
    return(format(d, "%Y-%m-%d"))
  }
  .taxdat_arg_error(arg_name, " must be a Date, a POSIXt or a 'YYYY-MM-DD' string.")
}

# ---- Internal helpers shared by the stratified pulls (age, sex) -------------

#' @title Merge jsonb key variants into one SQL expression
#' @description First non-empty value among the given keys ('' -> NULL).
#'   Key names are hard-coded constants, never user input.
#' @noRd
.sql_merge_keys <- function(keys) {
  parts <- vapply(keys, function(k) sprintf("NULLIF(o.data->>'%s', '')", k), character(1))
  paste0("COALESCE(", paste(parts, collapse = ", "), ")")
}

#' @title SQL select list for the merged age columns (raw text)
#' @noRd
.sql_age_columns <- function() {
  paste0(
    .sql_merge_keys(c("age_L", "ageL", "Age_L", "AgeL")), " AS age_l, ",
    .sql_merge_keys(c("age_R", "ageR", "Age_R", "AgeR")), " AS age_r, ",
    .sql_merge_keys(c("age", "Age")),                     " AS age"
  )
}

#' @title SQL select list for the sex columns (recoded + raw)
#' @description sex: 1 = male, 0 = female (data-entry convention); letters are
#'   recoded case-insensitively; any other value -> NULL. sex_raw: original value.
#' @noRd
.sql_sex_columns <- function() {
  raw <- .sql_merge_keys(c("sex", "Sex", "gender", "Gender"))
  paste0(
    "CASE WHEN lower(", raw, ") IN ('1', 'm', 'male') THEN 1 ",
    "WHEN lower(", raw, ") IN ('0', 'f', 'female') THEN 0 ",
    "ELSE NULL END AS sex, ",
    raw, " AS sex_raw"
  )
}

#' @title Columns defining "identical observations" for the duplicate flags
#' @description Every returned column except id and the columns derived from
#'   others (sex from sex_raw; location_name and location_via_fallback from
#'   the location ids).
#' @noRd
.strata_partition_cols <- c(
  "observation_collection_id", "is_public", "unified", "status",
  "time_left", "time_right", "suspected_cases", "confirmed_cases", "deaths",
  "phantom", "\"primary\"", "age_l", "age_r", "age", "sex_raw",
  "location_id", "location_period_id"
)

#' @title Attach location-period shapes to a pulled data frame
#' @description Fetches the shape of each distinct location_period_id once and
#'   attaches it as an sf geometry column \code{geojson}. Observations without
#'   location_period_id, or whose shape is NULL, get an empty geometry. Same
#'   logic as in read_taxonomy_data_sql, which keeps its own copy on purpose
#'   (self-contained function).
#' @param conn open DBI connection to the taxonomy database
#' @param df data.frame with a character column location_period_id
#' @return sf object, geometry column "geojson", CRS EPSG:4326
#' @noRd
.attach_location_period_shapes <- function(conn, df) {
  lp_ids <- unique(df$location_period_id[!is.na(df$location_period_id)])
  shapes <- NULL
  if (length(lp_ids) > 0) {
    # shapes.location_period_id has a unique index: at most one shape per period.
    # Bound as numbers so that the index on the bigint column is used.
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
  idx <- match(df$location_period_id, lp_key)
  idx[is.na(idx)] <- length(geoms)
  out <- sf::st_sf(df, geojson = geoms[idx])

  # CRS: not declared at column level in the database; geometries are WGS84
  if (is.na(sf::st_crs(out))) sf::st_crs(out) <- 4326
  out
}

#' @title Shared engine for the stratified pulls (age, sex)
#' @description Runs the common query and keeps the rows matching
#'   \code{row_filter}, an SQL condition on the subquery alias \code{s}.
#' @noRd
.read_taxonomy_strata_sql <- function(username, password, locations,
                                      time_left, time_right, uids,
                                      discard_incomplete_observation_collections,
                                      unified_dataset_behaviour, host,
                                      row_filter, label,
                                      include_geojson = FALSE) {

  # ---- Argument checks (before any connection) ------------------------------
  # Same contract as read_taxonomy_data_sql: every argument error has class
  # "taxdat_arg_error" and is raised before any connection attempt.
  # A missing argument in the calling wrapper is still seen as missing here.

  # Credentials: single non-missing strings, username non-empty
  if (missing(username) || missing(password) ||
      !.is_single_string(username) || !.is_single_string(password) ||
      !nzchar(username)) {
    .taxdat_arg_error("Please provide username and password to connect to the taxonomy database.")
  }
  # Empty password is only meaningful for a local socket connection
  use_local_socket <- identical(host, "localhost") && !nzchar(password)
  if (!nzchar(password) && !use_local_socket) {
    .taxdat_arg_error("Empty password is only allowed with host = 'localhost'.")
  }

  # Geography: mandatory numeric location ids
  if (is.null(locations)) {
    .taxdat_arg_error("Please use a containing location as the location. Locations can't be NULL.")
  }
  if (!is.numeric(locations)) {
    .taxdat_arg_error("SQL access by location name is not yet implemented")
  }
  if (!.is_valid_ids(locations)) {
    .taxdat_arg_error("locations must be positive whole numbers (location ids), without NA.")
  }

  # Observation collections: optional ids (numbers or digit strings)
  if (!is.null(uids) && !.is_valid_ids(uids)) {
    .taxdat_arg_error("uids must be positive whole ids (numbers or digit strings), without NA.")
  }

  # Flags
  if (!is.logical(discard_incomplete_observation_collections) ||
      length(discard_incomplete_observation_collections) != 1 ||
      is.na(discard_incomplete_observation_collections)) {
    .taxdat_arg_error("discard_incomplete_observation_collections must be TRUE or FALSE.")
  }
  if (!is.logical(include_geojson) || length(include_geojson) != 1 ||
      is.na(include_geojson)) {
    .taxdat_arg_error("include_geojson must be TRUE or FALSE.")
  }
  if (!(is.character(unified_dataset_behaviour) &&
        length(unified_dataset_behaviour) == 1 &&
        unified_dataset_behaviour %in% c("drop", "keep"))) {
    # Documented behaviour (any other value disables the filter) is kept, but
    # made visible: a typo such as "Drop" would otherwise silently include
    # unified datasets.
    warning("unified_dataset_behaviour is neither 'drop' nor 'keep': ",
            "the unified-dataset filter is disabled.")
  }

  # Dates: strict format, then bound order
  time_left  <- .validate_iso_date(time_left,  "time_left")
  time_right <- .validate_iso_date(time_right, "time_right")
  if (!is.null(time_left) && !is.null(time_right) &&
      as.Date(time_left) > as.Date(time_right)) {
    .taxdat_arg_error("time_left (", time_left, ") is after time_right (", time_right, ").")
  }

  # Missing filters: one warning per missing bound, as in read_taxonomy_data_sql
  if (is.null(time_left))  warning("No time filters: time_left is NULL.")
  if (is.null(time_right)) warning("No time filters: time_right is NULL.")
  if (is.null(uids))       warning("No uid filters.")

  # ---- Connection, always closed (also on error) ----------------------------
  conn_args <- list(
    RPostgres::Postgres(),
    dbname = "CholeraTaxonomy_production",
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
    # geography: EXISTS never duplicates rows (a JOIN does with nested ancestors)
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
  # Layer s: one row per observation, shared columns.
  # Layer d: rows kept by row_filter + number of identical observations
  #          (window computed AFTER the row filter; copies are identical on
  #          location, dates, age and sex, so they are always kept together).
  # Outer  : potential_duplicate flag (aggregated strata only).
  partition <- paste(paste0("s.", .strata_partition_cols), collapse = ", ")
  query <- paste(
    "SELECT d.*,",
    "  (d.n_identical_obs > 1",
    "   AND COALESCE(GREATEST(d.suspected_cases, d.confirmed_cases, d.deaths) > 1, FALSE)) AS potential_duplicate",
    "FROM (",
    "  SELECT s.*,",
    "    count(*) OVER (PARTITION BY", partition, ")::int AS n_identical_obs",
    "  FROM (",
    "    SELECT",
    "      o.id::text                        AS id,",
    "      o.observation_collection_id::text AS observation_collection_id,",
    "      oc.is_public, oc.unified, oc.status::text AS status,",
    "      o.time_left, o.time_right,",
    "      o.suspected_cases, o.confirmed_cases, o.deaths,",
    "      o.phantom, o.\"primary\" AS \"primary\",",
    "     ", .sql_age_columns(), ",",
    "     ", .sql_sex_columns(), ",",
    "      COALESCE(l_via_lp.qualified_name, l_direct.qualified_name) AS location_name,",
    "      (l_via_lp.qualified_name IS NULL",
    "       AND l_direct.qualified_name IS NOT NULL)                  AS location_via_fallback,",
    "      o.location_id::text        AS location_id,",
    "      o.location_period_id::text AS location_period_id",
    "    FROM observations o",
    "    JOIN observation_collections oc ON oc.id = o.observation_collection_id",
    "    LEFT JOIN location_periods lp   ON lp.id = o.location_period_id",
    "    LEFT JOIN locations l_via_lp    ON l_via_lp.id = lp.location_id",
    "    LEFT JOIN locations l_direct    ON l_direct.id = o.location_id",
    "    WHERE", paste(filters, collapse = " AND "),
    "  ) s",
    "  WHERE (", row_filter, ")",
    ") d",
    "ORDER BY d.observation_collection_id::bigint, d.id::bigint"
  )
  query <- glue::glue_sql(query, .con = conn,
                          locations = locations, uids = uids,
                          time_left = time_left, time_right = time_right)

  cat("-- Pulling", label, "data from taxonomy database with SQL \n")
  observations <- DBI::dbGetQuery(conn, query)

  if (nrow(observations) == 0) {
    stop("No observations found for the given filters.")
  }
  # Optional geometry: fetched before on.exit() closes the connection
  if (include_geojson) {
    observations <- .attach_location_period_shapes(conn, observations)
  }
  observations
}

#' @title Taxonomy SQL age- and sex-specific data pulls
#'
#' @description \code{read_taxonomy_age_sql} extracts observations carrying at
#' least one usable age value; \code{read_taxonomy_sex_sql} extracts
#' observations carrying a sex value; \code{read_taxonomy_age_sex_sql}
#' extracts observations carrying both. The three functions return the same
#' columns; only the row selection differs. Interface, filters and defaults
#' mirror \code{read_taxonomy_data_sql}.
#'
#' @details Design decisions (validated against the database before release):
#' \itemize{
#'   \item Governance filters identical to \code{read_taxonomy_data_sql}.
#'   \item Age: no native column. Key variants merged (age_L/ageL/Age_L/AgeL,
#'     age_R/ageR/Age_R/AgeR, age/Age); '' treated as missing; values
#'     returned raw (no trimming, no conversion).
#'   \item Sex: no native column. Key variants merged (sex, Sex, gender,
#'     Gender); '' treated as missing. \code{sex_raw} keeps the original
#'     value; \code{sex} recodes it following the data-entry convention
#'     (male = 1, female = 0): 1/m/male -> 1, 0/f/female -> 0,
#'     case-insensitive; any other value -> NA (row kept). For numerically
#'     coded collections the convention cannot be verified from the values.
#'     The keys male/female are not read (sex-stratified aggregate counts).
#'   \item Age and sex columns are returned by all three functions, NA when absent.
#'   \item Many rows are aggregated strata (primary = FALSE) of a primary
#'     total row, and some collections store zero-filled grids: analyses must
#'     weight by the count columns rather than count rows.
#'   \item Location name: location period first (consistent with geometries,
#'     which join on location_period_id only), observations.location_id as
#'     fallback (\code{location_via_fallback}).
#'   \item Geography: EXISTS on observations.location_id (no duplication with
#'     nested ancestors); observations without any location are excluded.
#'   \item Dates: only observations fully inside [time_left, time_right];
#'     bounds must be Date, POSIXt or strict "YYYY-MM-DD" strings, with
#'     time_left <= time_right.
#'   \item Arguments are validated before any connection, with the same
#'     contract as \code{read_taxonomy_data_sql}: argument errors have class
#'     \code{taxdat_arg_error}; an empty password is only allowed with
#'     host = "localhost" (local socket); a warning is raised for each
#'     missing time bound, for missing uids, and for an unrecognised
#'     unified_dataset_behaviour.
#'   \item Potential duplicates: \code{n_identical_obs} counts the
#'     observations identical on every returned column except id (1 =
#'     unique); \code{potential_duplicate} is TRUE when such copies exist and
#'     at least one count exceeds 1 (aggregated strata, most likely entered
#'     twice). Identical line-list rows (counts <= 1) are expected and not
#'     flagged. Nothing is removed: flagged observations need manual
#'     inspection.
#'   \item Geometry: with \code{include_geojson = TRUE}, an sf object is
#'     returned, built as in \code{read_taxonomy_data_sql}: one shape fetch
#'     per distinct location_period_id, geometry column \code{geojson},
#'     EPSG:4326, empty geometry for observations without location period or
#'     without shape. Default FALSE: plain data.frame, no geometry.
#' }
#'
#' @param username taxonomy username
#' @param password taxonomy password
#' @param locations numeric vector of location ids (ancestor ids). Mandatory.
#' @param time_left left bound for observation times (Date, POSIXt or
#'   "YYYY-MM-DD")
#' @param time_right right bound for observation times (Date, POSIXt or
#'   "YYYY-MM-DD")
#' @param uids list of unique observation collection ids to pull
#' @param discard_incomplete_observation_collections whether to exclude OCs
#'   with status initialized, validated or inprogress
#' @param unified_dataset_behaviour "drop" (default), "keep", or any other
#'   value to disable the filter
#' @param host database host
#' @param include_geojson if TRUE, attach the location-period geometry and
#'   return an sf object (default FALSE)
#' @return A data.frame, one row per observation; identifiers as text. With
#'   \code{include_geojson = TRUE}, an sf object with geometry column
#'   \code{geojson}.
#' @seealso \code{\link{read_taxonomy_data_sql}}
#' @name taxonomy_strata_pulls
NULL

#' @rdname taxonomy_strata_pulls
#' @export
read_taxonomy_age_sql <- function(username, password, locations = NULL,
                                  time_left = NULL, time_right = NULL,
                                  uids = NULL,
                                  discard_incomplete_observation_collections = TRUE,
                                  unified_dataset_behaviour = "drop",
                                  host = "db.cholera-taxonomy.middle-distance.com",
                                  include_geojson = FALSE) {
  .read_taxonomy_strata_sql(
    username, password, locations, time_left, time_right, uids,
    discard_incomplete_observation_collections, unified_dataset_behaviour, host,
    row_filter = "s.age_l IS NOT NULL OR s.age_r IS NOT NULL OR s.age IS NOT NULL",
    label      = "age",
    include_geojson = include_geojson
  )
}

#' @rdname taxonomy_strata_pulls
#' @export
read_taxonomy_sex_sql <- function(username, password, locations = NULL,
                                  time_left = NULL, time_right = NULL,
                                  uids = NULL,
                                  discard_incomplete_observation_collections = TRUE,
                                  unified_dataset_behaviour = "drop",
                                  host = "db.cholera-taxonomy.middle-distance.com",
                                  include_geojson = FALSE) {
  .read_taxonomy_strata_sql(
    username, password, locations, time_left, time_right, uids,
    discard_incomplete_observation_collections, unified_dataset_behaviour, host,
    row_filter = "s.sex_raw IS NOT NULL",
    label      = "sex",
    include_geojson = include_geojson
  )
}

#' @title Marginalize age/sex strata
#'
#' @description Sums the observations of a stratified pull:
#' \code{over = "sex"} turns the output of \code{read_taxonomy_age_sql} into
#' an age table (all sexes merged); \code{over = "age"} turns the output of
#' \code{read_taxonomy_sex_sql} into a sex table (all ages merged);
#' \code{over = "none"} keeps both age and sex (output of
#' \code{read_taxonomy_age_sex_sql}) and only merges observations identical on
#' the full key (e.g. line-list cases of the same age and sex).
#'
#' @details Rules (validated against an independent SQL implementation):
#' \itemize{
#'   \item Observations are merged only if they share the collection,
#'     governance fields, dates, location, \code{primary}, \code{phantom}
#'     and the kept variables (age columns and/or recoded sex).
#'   \item An observation whose kept variable is missing is never merged
#'     (it is returned alone). A missing value of the summed variable does
#'     not prevent merging (e.g. a case of unknown sex counts in its age).
#'   \item Count columns are summed separately. \code{na_counts = "as_zero"}
#'     (default): in a group where some observations have a count and others
#'     NA, the NAs count as 0 (blank cells at import); a group where all
#'     values are NA stays NA. \code{na_counts = "strict"}: any NA in the
#'     group gives NA.
#'   \item With \code{drop_exact_duplicates = TRUE}, only the lowest id of
#'     each set of potential duplicates (identical aggregated strata) is
#'     summed.
#'   \item A message always reports the number of groups where NAs were
#'     counted as 0 and the number of groups containing potential
#'     duplicates, whatever the value of \code{metadata}.
#' }
#'
#' @param df output of \code{read_taxonomy_age_sql} (over = "sex"),
#'   \code{read_taxonomy_sex_sql} (over = "age") or
#'   \code{read_taxonomy_age_sex_sql} (over = "none").
#' @param over variable to sum over: "sex", "age", or "none" (keep both).
#' @param na_counts "as_zero" (default) or "strict", see Details.
#' @param drop_exact_duplicates keep one copy of each set of potential
#'   duplicates before summing (default FALSE).
#' @param metadata if TRUE, adds traceability columns: \code{n_source_rows},
#'   \code{sex_values} / \code{age_values} (values of the summed variable;
#'   absent when over = "none"), \code{sex_raw_values} (over = "age" or
#'   "none"), \code{na_as_zero}, \code{potential_duplicate},
#'   \code{n_duplicate_sources}, \code{n_duplicates_dropped} and
#'   \code{source_ids}. Default FALSE: group key, kept variables and summed
#'   counts only.
#' @return A data.frame, one row per group; an sf object (geometry
#'   column \code{geojson}, one shape per location period) when \code{df} is
#'   an sf object (pulls with \code{include_geojson = TRUE}).
#' @seealso \code{\link{read_taxonomy_age_sql}}, \code{\link{read_taxonomy_sex_sql}},
#'   \code{\link{read_taxonomy_age_sex_sql}}
#' @export
marginalize_strata <- function(df, over = c("sex", "age", "none"),
                               na_counts = c("as_zero", "strict"),
                               drop_exact_duplicates = FALSE,
                               metadata = FALSE) {
  over      <- match.arg(over)
  na_counts <- match.arg(na_counts)

  # ---- Input checks -----------------------------------------------------------
  required <- c("id", "observation_collection_id", "is_public", "unified", "status",
                "time_left", "time_right", "suspected_cases", "confirmed_cases", "deaths",
                "phantom", "primary", "age_l", "age_r", "age", "sex", "sex_raw",
                "location_name", "location_via_fallback", "location_id",
                "location_period_id", "n_identical_obs", "potential_duplicate")
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    stop("df is not a stratified pull (missing columns: ", paste(missing_cols, collapse = ", "),
         "). Use read_taxonomy_age_sql(), read_taxonomy_sex_sql() or read_taxonomy_age_sex_sql().")
  }
  if (anyDuplicated(df$id) > 0) stop("df contains duplicated ids.")

  # ---- Geometry (sf input): set aside, re-attached per group at the end ------
  # location_period_id is part of every grouping key, so all observations of a
  # group share the same shape: the geometry is restored without loss.
  geom_map <- NULL
  if (inherits(df, "sf")) {
    lp    <- df$location_period_id
    geoms <- sf::st_geometry(df)
    first <- !is.na(lp) & !duplicated(lp)          # one shape per location period
    geom_map <- list(key = lp[first], geoms = geoms[first], crs = sf::st_crs(df))
    df <- sf::st_drop_geometry(df)                 # summarise() must not union shapes
  }

  count_cols <- c("suspected_cases", "confirmed_cases", "deaths")
  # Same key for the three modes: collection, governance, period, location, flags
  common_key <- c("observation_collection_id", "is_public", "unified", "status",
                  "time_left", "time_right", "phantom", "primary",
                  "location_id", "location_period_id", "location_name", "location_via_fallback")
  age_cols <- c("age_l", "age_r", "age")
  kept <- switch(over,
                 sex  = age_cols,              # age table: sum over sex
                 age  = "sex",                 # sex table: sum over age
                 none = c(age_cols, "sex"))    # age x sex table: nothing summed over

  # ---- Rows whose kept variable is missing are never merged (id joins the key)
  no_age <- is.na(df$age_l) & is.na(df$age_r) & is.na(df$age)
  kept_missing <- switch(over,
                         sex  = no_age,
                         age  = is.na(df$sex),
                         none = no_age | is.na(df$sex))
  df$.solo <- ifelse(kept_missing, df$id, NA_character_)

  # ---- Optional: keep one copy (lowest id) of each set of potential duplicates
  df$.dropped <- FALSE
  if (isTRUE(drop_exact_duplicates)) {
    dup_cols <- c("observation_collection_id", "is_public", "unified", "status",
                  "time_left", "time_right", count_cols, "phantom", "primary",
                  "age_l", "age_r", "age", "sex_raw", "location_id", "location_period_id")
    ord <- order(as.numeric(df$id))
    is_extra <- logical(nrow(df))
    is_extra[ord] <- duplicated(df[ord, dup_cols]) & df$potential_duplicate[ord]
    df$.dropped <- is_extra
  }

  # ---- Label of the summed variable, per observation (traceability only)
  df$.value <- switch(over,
    sex  = ifelse(is.na(df$sex), "NA", as.character(df$sex)),
    age  = apply(as.matrix(df[, age_cols]), 1, function(r) {
             if (all(is.na(r))) "NA" else paste(r[!is.na(r)], collapse = "-")
           }),
    none = rep(NA_character_, nrow(df)))   # nothing summed over

  # ---- Per-group helpers -------------------------------------------------------
  sum_count <- function(x, keep) {
    x <- x[keep]
    if (all(is.na(x))) return(NA_integer_)                  # all NA: no information
    if (na_counts == "strict" && anyNA(x)) return(NA_integer_)
    as.integer(sum(x, na.rm = TRUE))                         # "as_zero": NAs count as 0
  }
  na_detail <- function(sch, cch, dth, keep) {
    if (na_counts == "strict") return(NA_character_)         # nothing imputed in strict mode
    vals  <- list(suspected_cases = sch, confirmed_cases = cch, deaths = dth)
    parts <- character(0)
    for (nm in count_cols) {
      x <- vals[[nm]][keep]
      n_na <- sum(is.na(x))
      if (n_na > 0 && n_na < length(x)) parts <- c(parts, paste0(nm, ":", n_na))
    }
    if (length(parts) == 0) NA_character_ else paste(parts, collapse = " | ")
  }

  # ---- Grouping and summaries ---------------------------------------------------
  # Output columns get temporary names (prefixed with ".") so that no summary
  # column masks an input column: in summarise(), a column created earlier is
  # visible to later expressions, including through .data$.
  grouped <- dplyr::group_by(df, dplyr::across(dplyr::all_of(c(common_key, kept, ".solo"))))
  out <- dplyr::summarise(
    grouped,
    .sex_raw_values       = paste(sort(unique(.data$sex_raw)), collapse = ","),
    .suspected_cases      = sum_count(.data$suspected_cases, !.data$.dropped),
    .confirmed_cases      = sum_count(.data$confirmed_cases, !.data$.dropped),
    .deaths               = sum_count(.data$deaths,          !.data$.dropped),
    .n_source_rows        = dplyr::n(),
    .values               = paste(sort(unique(.data$.value)), collapse = ","),
    .na_as_zero           = na_detail(.data$suspected_cases, .data$confirmed_cases,
                                      .data$deaths, !.data$.dropped),
    .potential_duplicate  = any(.data$potential_duplicate),
    .n_duplicate_sources  = sum(.data$potential_duplicate),
    .n_duplicates_dropped = sum(.data$.dropped),
    .source_ids           = paste(.data$id[order(as.numeric(.data$id))], collapse = ","),
    .groups = "drop"
  )
  out <- as.data.frame(out)
  out$.solo <- NULL
  # Final names: drop the temporary "." prefix (group columns never start with ".")
  names(out) <- sub("^\\.", "", names(out))
  if (over == "none") {
    out$values <- NULL                       # nothing summed over: no *_values column
  } else {
    names(out)[names(out) == "values"] <- if (over == "sex") "sex_values" else "age_values"
  }
  if (over == "sex") out$sex_raw_values <- NULL   # sex_values is enough in the age table

  out <- out[order(as.numeric(out$observation_collection_id), out$time_left, out$location_name), ]
  rownames(out) <- NULL

  # ---- Message: always shown, whatever `metadata` ----------------------------
  message(sprintf(paste0(
    "marginalize_strata(over = '%s'): %d groups from %d observations; ",
    "%d groups with NA counted as 0; %d groups containing potential duplicates%s.%s"),
    over, nrow(out), nrow(df), sum(!is.na(out$na_as_zero)), sum(out$potential_duplicate),
    if (isTRUE(drop_exact_duplicates)) paste0(" (", sum(out$n_duplicates_dropped), " copies dropped)") else "",
    if (!isTRUE(metadata)) " Use metadata = TRUE to see which groups." else ""))

  # ---- Default output: group key, kept variables and summed counts only -------
  if (!isTRUE(metadata)) {
    out <- out[, c(common_key, kept, count_cols)]
  }

  # ---- Re-attach geometry (sf input only) -------------------------------------
  if (!is.null(geom_map)) {
    empty_geom <- sf::st_sfc(sf::st_geometrycollection(), crs = geom_map$crs)
    geoms <- c(geom_map$geoms, empty_geom)        # empty geometry in last position
    idx <- match(out$location_period_id, geom_map$key)
    idx[is.na(idx)] <- length(geoms)
    out <- sf::st_sf(out, geojson = geoms[idx])
  }
  out
}

#' @rdname taxonomy_strata_pulls
#' @export
read_taxonomy_age_sex_sql <- function(username, password, locations = NULL,
                                      time_left = NULL, time_right = NULL,
                                      uids = NULL,
                                      discard_incomplete_observation_collections = TRUE,
                                      unified_dataset_behaviour = "drop",
                                      host = "db.cholera-taxonomy.middle-distance.com",
                                      include_geojson = FALSE) {
  .read_taxonomy_strata_sql(
    username, password, locations, time_left, time_right, uids,
    discard_incomplete_observation_collections, unified_dataset_behaviour, host,
    # age AND sex present (intersection of the age and sex pulls)
    row_filter = paste("(s.age_l IS NOT NULL OR s.age_r IS NOT NULL OR s.age IS NOT NULL)",
                       "AND s.sex_raw IS NOT NULL"),
    label      = "age and sex",
    include_geojson = include_geojson
  )
}
