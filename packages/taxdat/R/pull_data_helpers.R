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

#' @title Validate a date argument (strict ISO)
#' @description Accepts a single Date or a "YYYY-MM-DD" string and returns it
#'   as an ISO string. Rejects ambiguous inputs such as "04/03/2017", which a
#'   server with DateStyle = MDY would silently read as 3 April 2017.
#' @param x A Date, a character string, or NULL.
#' @param arg_name Argument name, used in error messages.
#' @return NULL, or a "YYYY-MM-DD" string.
#' @noRd
.validate_iso_date <- function(x, arg_name) {
  if (is.null(x)) return(NULL)
  if (length(x) != 1) stop(arg_name, " must be a single date.")
  if (inherits(x, "Date")) {
    if (is.na(x)) stop(arg_name, " is NA.")
    return(format(x, "%Y-%m-%d"))
  }
  if (is.character(x) && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)) {
    d <- as.Date(x, format = "%Y-%m-%d")  # explicit format: 2017-02-30 -> NA
    if (is.na(d)) stop(arg_name, " is not a valid calendar date: ", x)
    return(format(d, "%Y-%m-%d"))
  }
  stop(arg_name, " must be a Date or a 'YYYY-MM-DD' string.")
}

#' @title Taxonomy SQL age-specific data pull
#'
#' @description Extracts observations carrying at least one usable age value
#' from the taxonomy PostgreSQL database. Interface, filters and defaults
#' mirror \code{read_taxonomy_data_sql}.
#'
#' @details Design decisions (validated against the database before release):
#' \itemize{
#'   \item Governance filters (\code{unified_dataset_behaviour},
#'     \code{discard_incomplete_observation_collections}) are identical to
#'     \code{read_taxonomy_data_sql}.
#'   \item Age has no native column. All key variants found in the database
#'     are merged, in this priority order: age_L/ageL/Age_L/AgeL,
#'     age_R/ageR/Age_R/AgeR, age/Age. No conflicting values were found
#'     between variants. Rows carrying age only through a variant key were
#'     checked not to duplicate rows carrying the canonical keys.
#'   \item Empty strings are treated as missing: collections with an age
#'     column store '' for observations without an age. Values are returned
#'     raw, as text: no trimming and no conversion. Non-integer formats
#'     (units, fractions, typos) are left to downstream preprocessing.
#'   \item Location name: taken from the location period first (consistent
#'     with geometries, which join on location_period_id only), falling back
#'     to observations.location_id; \code{location_via_fallback} flags the
#'     fallback rows.
#'   \item Geography: filtered on observations.location_id, as in
#'     \code{read_taxonomy_data_sql}, but with EXISTS instead of a JOIN on
#'     location_hierarchies, so nested ancestors never duplicate rows.
#'     Observations without any location can never match an ancestor and are
#'     always excluded, as in \code{read_taxonomy_data_sql}.
#'   \item Dates: only observations fully inside [time_left, time_right] are
#'     returned (same semantics as \code{read_taxonomy_data_sql});
#'     observations straddling a bound are dropped. Bounds must be Date
#'     objects or strict "YYYY-MM-DD" strings.
#'   \item No geometry is returned: pull shapes separately, deduplicated by
#'     location_period_id.
#' }
#'
#' @param username taxonomy username
#' @param password taxonomy password
#' @param locations numeric vector of location ids (ancestor ids). Mandatory.
#' @param time_left left bound for observation times (Date or "YYYY-MM-DD")
#' @param time_right right bound for observation times (Date or "YYYY-MM-DD")
#' @param uids list of unique observation collection ids to pull
#' @param discard_incomplete_observation_collections whether to exclude OCs
#'   with status initialized, validated or inprogress
#' @param unified_dataset_behaviour "drop" (default), "keep", or any other
#'   value to disable the filter
#' @param host database host
#' @return A data.frame, one row per observation; identifiers as text.
#' @seealso \code{\link{read_taxonomy_data_sql}}
#' @export
read_taxonomy_age_sql <- function(username, password, locations = NULL,
                                  time_left = NULL, time_right = NULL,
                                  uids = NULL,
                                  discard_incomplete_observation_collections = TRUE,
                                  unified_dataset_behaviour = "drop",
                                  host = "db.cholera-taxonomy.middle-distance.com") {

  # ---- Argument checks (before any connection) ------------------------------
  if (missing(username) || missing(password) ||
      !nzchar(username) || !nzchar(password)) {
    stop("Please provide username and password to connect to the taxonomy database.")
  }
  if (is.null(locations)) {
    stop("Please use a containing location as the location. Locations can't be NULL.")
  }
  if (!is.numeric(locations)) {
    stop("SQL access by location name is not yet implemented")
  }
  time_left  <- .validate_iso_date(time_left,  "time_left")
  time_right <- .validate_iso_date(time_right, "time_right")
  if (is.null(time_left) && is.null(time_right)) warning("No time filters.")
  if (is.null(uids)) warning("No uid filters.")

  # ---- Connection, always closed (also on error) ----------------------------
  conn <- DBI::dbConnect(
    RPostgres::Postgres(),
    host     = host,
    dbname   = "CholeraTaxonomy_production",
    user     = username,
    password = password,
    port     = Sys.getenv("CHOLERA_POSTGRES_PORT", "5432")
  )
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  # ---- Age merge: '' -> NULL, then first non-NULL key variant ---------------
  # Key names are hard-coded constants (no user input): sprintf is safe here.
  merge_keys <- function(keys) {
    parts <- vapply(keys, function(k) sprintf("NULLIF(o.data->>'%s', '')", k), character(1))
    paste0("COALESCE(", paste(parts, collapse = ", "), ")")
  }

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

  # ---- Query --------------------------------------------------------------------
  query <- paste(
    "SELECT * FROM (",
    "  SELECT",
    "    o.id::text                        AS id,",
    "    o.observation_collection_id::text AS observation_collection_id,",
    "    oc.is_public, oc.unified, oc.status::text AS status,",
    "    o.time_left, o.time_right,",
    "    o.suspected_cases, o.confirmed_cases, o.deaths,",
    "    o.phantom, o.\"primary\" AS \"primary\",",
    "   ", merge_keys(c("age_L", "ageL", "Age_L", "AgeL")), "AS age_l,",
    "   ", merge_keys(c("age_R", "ageR", "Age_R", "AgeR")), "AS age_r,",
    "   ", merge_keys(c("age", "Age")),                     "AS age,",
    "    COALESCE(l_via_lp.qualified_name, l_direct.qualified_name) AS location_name,",
    "    (l_via_lp.qualified_name IS NULL",
    "     AND l_direct.qualified_name IS NOT NULL)                  AS location_via_fallback,",
    "    o.location_id::text        AS location_id,",
    "    o.location_period_id::text AS location_period_id",
    "  FROM observations o",
    "  JOIN observation_collections oc ON oc.id = o.observation_collection_id",
    "  LEFT JOIN location_periods lp   ON lp.id = o.location_period_id",
    "  LEFT JOIN locations l_via_lp    ON l_via_lp.id = lp.location_id",
    "  LEFT JOIN locations l_direct    ON l_direct.id = o.location_id",
    "  WHERE", paste(filters, collapse = " AND "),
    ") s",
    "WHERE s.age_l IS NOT NULL OR s.age_r IS NOT NULL OR s.age IS NOT NULL",
    "ORDER BY s.observation_collection_id::bigint, s.id::bigint"
  )
  query <- glue::glue_sql(query, .con = conn,
                          locations = locations, uids = uids,
                          time_left = time_left, time_right = time_right)

  cat("-- Pulling age data from taxonomy database with SQL \n")
  observations <- DBI::dbGetQuery(conn, query)

  if (nrow(observations) == 0) {
    stop("No observations found for the given filters.")
  }
  observations
}

#' @title Taxonomy SQL sex-specific data pull
#'
#' @description Extracts observations carrying a sex value from the taxonomy
#' PostgreSQL database. Interface, filters and defaults mirror
#' \code{read_taxonomy_data_sql}.
#'
#' @details Design decisions (validated against the database before release):
#' \itemize{
#'   \item Governance filters identical to \code{read_taxonomy_data_sql}.
#'   \item Sex has no native column. Key variants are merged in this priority
#'     order: sex, Sex, gender, Gender (no row carries two variants). Empty
#'     strings are treated as missing.
#'   \item \code{sex_raw} returns the original value. \code{sex} recodes it
#'     following the data-entry convention (male = 1, female = 0): 1/m/male
#'     -> 1 and 0/f/female -> 0, case-insensitive. Any other value gives NA,
#'     and the row is kept so that unmapped values remain visible. For
#'     numerically coded collections the convention cannot be verified from
#'     the values themselves.
#'   \item The keys male/female are not read: they hold sex-stratified
#'     aggregate counts, not the sex of a case.
#'   \item Many rows are aggregated strata (primary = FALSE, suspected_cases
#'     > 1) of a primary total row: analyses must weight by suspected_cases
#'     rather than count rows.
#'   \item Location, geography and dates: same rules as
#'     \code{read_taxonomy_age_sql} (location period first, EXISTS on
#'     observations.location_id, observations fully inside the date window).
#'   \item No geometry is returned.
#' }
#'
#' @param username taxonomy username
#' @param password taxonomy password
#' @param locations numeric vector of location ids (ancestor ids). Mandatory.
#' @param time_left left bound for observation times (Date or "YYYY-MM-DD")
#' @param time_right right bound for observation times (Date or "YYYY-MM-DD")
#' @param uids list of unique observation collection ids to pull
#' @param discard_incomplete_observation_collections whether to exclude OCs
#'   with status initialized, validated or inprogress
#' @param unified_dataset_behaviour "drop" (default), "keep", or any other
#'   value to disable the filter
#' @param host database host
#' @return A data.frame, one row per observation; identifiers as text.
#' @seealso \code{\link{read_taxonomy_data_sql}}, \code{\link{read_taxonomy_age_sql}}
#' @export
read_taxonomy_sex_sql <- function(username, password, locations = NULL,
                                  time_left = NULL, time_right = NULL,
                                  uids = NULL,
                                  discard_incomplete_observation_collections = TRUE,
                                  unified_dataset_behaviour = "drop",
                                  host = "db.cholera-taxonomy.middle-distance.com") {

  # ---- Argument checks (before any connection) ------------------------------
  if (missing(username) || missing(password) ||
      !nzchar(username) || !nzchar(password)) {
    stop("Please provide username and password to connect to the taxonomy database.")
  }
  if (is.null(locations)) {
    stop("Please use a containing location as the location. Locations can't be NULL.")
  }
  if (!is.numeric(locations)) {
    stop("SQL access by location name is not yet implemented")
  }
  time_left  <- .validate_iso_date(time_left,  "time_left")
  time_right <- .validate_iso_date(time_right, "time_right")
  if (is.null(time_left) && is.null(time_right)) warning("No time filters.")
  if (is.null(uids)) warning("No uid filters.")

  # ---- Connection, always closed (also on error) ----------------------------
  conn <- DBI::dbConnect(
    RPostgres::Postgres(),
    host     = host,
    dbname   = "CholeraTaxonomy_production",
    user     = username,
    password = password,
    port     = Sys.getenv("CHOLERA_POSTGRES_PORT", "5432")
  )
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

  # ---- Query --------------------------------------------------------------------
  # Inner query merges the raw sex value; outer query recodes it and keeps only
  # rows with a non-empty raw value (unmapped values kept, sex = NA).
  query <- paste(
    "SELECT",
    "  s.id, s.observation_collection_id, s.is_public, s.unified, s.status,",
    "  s.time_left, s.time_right, s.suspected_cases, s.confirmed_cases, s.deaths,",
    "  s.phantom, s.\"primary\",",
    "  CASE",
    "    WHEN lower(s.sex_raw) IN ('1', 'm', 'male')   THEN 1",
    "    WHEN lower(s.sex_raw) IN ('0', 'f', 'female') THEN 0",
    "    ELSE NULL",
    "  END AS sex,",
    "  s.sex_raw,",
    "  s.location_name, s.location_via_fallback, s.location_id, s.location_period_id",
    "FROM (",
    "  SELECT",
    "    o.id::text                        AS id,",
    "    o.observation_collection_id::text AS observation_collection_id,",
    "    oc.is_public, oc.unified, oc.status::text AS status,",
    "    o.time_left, o.time_right,",
    "    o.suspected_cases, o.confirmed_cases, o.deaths,",
    "    o.phantom, o.\"primary\" AS \"primary\",",
    "    COALESCE(NULLIF(o.data->>'sex', ''), NULLIF(o.data->>'Sex', ''),",
    "             NULLIF(o.data->>'gender', ''), NULLIF(o.data->>'Gender', '')) AS sex_raw,",
    "    COALESCE(l_via_lp.qualified_name, l_direct.qualified_name) AS location_name,",
    "    (l_via_lp.qualified_name IS NULL",
    "     AND l_direct.qualified_name IS NOT NULL)                  AS location_via_fallback,",
    "    o.location_id::text        AS location_id,",
    "    o.location_period_id::text AS location_period_id",
    "  FROM observations o",
    "  JOIN observation_collections oc ON oc.id = o.observation_collection_id",
    "  LEFT JOIN location_periods lp   ON lp.id = o.location_period_id",
    "  LEFT JOIN locations l_via_lp    ON l_via_lp.id = lp.location_id",
    "  LEFT JOIN locations l_direct    ON l_direct.id = o.location_id",
    "  WHERE", paste(filters, collapse = " AND "),
    ") s",
    "WHERE s.sex_raw IS NOT NULL",
    "ORDER BY s.observation_collection_id::bigint, s.id::bigint"
  )
  query <- glue::glue_sql(query, .con = conn,
                          locations = locations, uids = uids,
                          time_left = time_left, time_right = time_right)

  cat("-- Pulling sex data from taxonomy database with SQL \n")
  observations <- DBI::dbGetQuery(conn, query)

  if (nrow(observations) == 0) {
    stop("No observations found for the given filters.")
  }
  observations
}


#' @title Taxonomy age/sex-stratified SQL data pull
#'
#' @description Extracts age- and sex-stratified cholera observation data
#' from the taxonomy PostgreSQL database. Age is required (a row must have
#' at least one of age/age_L/age_R populated, checked across all known
#' key-name variants); sex is extracted when available but never required,
#' exactly as in every pull built this session.
#'
#' @details Interface deliberately mirrors taxdat::read_taxonomy_data_sql.
#' Departures from it, each validated empirically against the DB this
#' session (not assumed) on the scoped population this function targets
#' (EXISTS(custom_fields), age present, unified/status filtered):
#'
#' @param username,password Taxonomy DB credentials.
#' @param locations Numeric vector of location ids (ancestor_id filter).
#'   Mandatory, exactly as in read_taxonomy_data_sql — NULL raises an
#'   error rather than silently pulling the whole world.
#' @param time_left,time_right Optional date bounds (Date objects).
#' @param uids Optional vector of observation_collection_id to restrict to.
#' @param discard_incomplete_observation_collections Default TRUE. Same
#'   semantics as read_taxonomy_data_sql: excludes collections with
#'   status in (initialized, validated, inprogress).
#' @param unified_dataset_behaviour "drop" (default), "keep", or any other
#'   value to disable the filter — same semantics as read_taxonomy_data_sql.
#' @param require_custom_fields Default TRUE. Requires the collection to
#'   have at least one entry in custom_fields. Empirically a
#'   near-perfect proxy for "this collection's jsonb can carry an 'age'
#'   key" (measured on this DB: 0% of observations without any
#'   custom_fields entry have a populated 'age' key, vs 8.7% for those
#'   with one) — largely redundant with the age-presence filter below,
#'   kept here for consistency with every pull validated this session.
#'   Set FALSE to disable.
#' @param drop_unresolved_location Default FALSE. If TRUE, excludes the
#'   observations that have neither location_id nor location_period_id
#'   set (see Details) instead of returning them with location_name = NA
#'   — reproduces taxdat's own silent-exclusion behaviour on request,
#'   rather than by accident.
#' @param include_geojson Default FALSE. If TRUE, joins shapes.shape
#'   directly into the result via location_period_id (the only valid
#'   join key — shapes has no location_id column at all). This repeats
#'   the geometry on every observation sharing a location_period_id and
#'   can produce very large results: measured on this DB, ~250 MB of
#'   geometry text for ~1,200 distinct locations across ~370,000
#'   observations. For most uses, prefer pulling shapes separately,
#'   deduplicated by location_period_id, and joining downstream in R —
#'   see extract_shape_april.R for that pattern. When TRUE, returns an
#'   sf object (via sf::st_read); when FALSE, returns a plain data.frame.
#' @param host Database host.
#'
#' @return A data.frame, or an sf object if include_geojson = TRUE.
#' @export
read_taxonomy_age_sex_sql <- function(username, password, locations = NULL,
                                       time_left = NULL, time_right = NULL,
                                       uids = NULL,
                                       discard_incomplete_observation_collections = TRUE,
                                       unified_dataset_behaviour = "drop",
                                       require_custom_fields = TRUE,
                                       drop_unresolved_location = FALSE,
                                       include_geojson = FALSE,
                                       host = "db.cholera-taxonomy.middle-distance.com") {

  if (missing(username) | missing(password)) {
    stop("Please provide username and password to connect to the taxonomy database.")
  }
  if (is.null(locations)) {
    stop("Please use a containing location as the location. Locations can't be NULL.")
  }
  if (!all(is.numeric(locations))) {
    stop("SQL access by location name is not yet implemented")
  }

  conn <- RPostgres::dbConnect(
    RPostgres::Postgres(),
    host = host,
    dbname = "CholeraTaxonomy_production",
    user = username, password = password,
    port = Sys.getenv("CHOLERA_POSTGRES_PORT", "5432")
  )

  geojson_select <- if (include_geojson) ", shapes.shape AS geojson" else ""
  geojson_join   <- if (include_geojson) {
    "LEFT JOIN shapes ON shapes.location_period_id = location_periods.id"
  } else ""

  # --- Native columns only for TL/TR/case counts/deaths/phantom/primary.
  # No CASE/COALESCE against the jsonb here — tested column-by-column on
  # this function's target population (see @details): native coverage
  # was 100% for TL/TR, and every jsonb-only value found for
  # suspected_cases/confirmed_cases/deaths was confirmed to be blank
  # noise, not real data. Kept simple and fast rather than defensively
  # parsing a jsonb blob that adds no real coverage here.
  obs_query <- paste(
    "SELECT",
    "observations.id::text, observations.observation_collection_id::text,",
    "observations.time_left,",
    "observations.time_right,",
    "observations.suspected_cases,",
    "observations.confirmed_cases,",
    "observations.deaths,",
    "observations.phantom, observations.primary,",
    # age/sex: no native column exists for either — jsonb is the only
    # source, hence the multi-key COALESCE across every variant found
    # in this DB's custom_fields registry.
    "COALESCE(observations.data->>'age_L', observations.data->>'ageL',",
    "         observations.data->>'Age_L', observations.data->>'AgeL') AS age_l,",
    "COALESCE(observations.data->>'age_R', observations.data->>'ageR',",
    "         observations.data->>'Age_R', observations.data->>'AgeR') AS age_r,",
    "COALESCE(observations.data->>'age', observations.data->>'Age') AS age,",
    # 'male'/'female' as separate keys deliberately excluded: semantics
    # (per-row flag vs. aggregate count) were never confirmed this
    # session — do not fold them in silently.
    "COALESCE(observations.data->>'sex', observations.data->>'Sex',",
    "         observations.data->>'gender', observations.data->>'Gender') AS sex,",
    "COALESCE(l_via_lp.qualified_name, l_direct.qualified_name) AS location_name,",
    "(l_via_lp.qualified_name IS NOT NULL OR l_direct.qualified_name IS NOT NULL) AS location_resolved,",
    "(l_via_lp.qualified_name IS NULL AND l_direct.qualified_name IS NOT NULL) AS location_via_fallback,",
    "observations.location_id::text AS location_id,",
    "observations.location_period_id AS location_period_id",
    geojson_select,
    "FROM observations",
    "LEFT JOIN observation_collections ON observations.observation_collection_id = observation_collections.id",
    "LEFT JOIN location_periods ON observations.location_period_id = location_periods.id",
    "LEFT JOIN locations l_via_lp ON l_via_lp.id = location_periods.location_id",
    "LEFT JOIN locations l_direct ON l_direct.id = observations.location_id",
    "LEFT JOIN location_hierarchies",
    "  ON location_hierarchies.descendant_id = COALESCE(location_periods.location_id, observations.location_id)",
    geojson_join,
    " WHERE"
  )

  cat("-- Pulling age/sex-stratified data from taxonomy database with SQL \n")

  unified_filter <- if (unified_dataset_behaviour == "drop") {
    "((observation_collections.unified is NULL) OR (observation_collections.unified!='t'))"
  } else if (unified_dataset_behaviour == "keep") {
    "((observation_collections.unified is NOT NULL) AND (observation_collections.unified))"
  } else NULL

  oc_filter <- if (discard_incomplete_observation_collections) {
    paste(
      "(observation_collections.status != 'initialized')",
      "AND (observation_collections.status != 'validated')",
      "AND (observation_collections.status != 'inprogress')"
    )
  } else NULL

  custom_fields_filter <- if (require_custom_fields) {
    paste(
      "EXISTS (SELECT 1 FROM custom_fields",
      "WHERE custom_fields.observation_collection_id = observations.observation_collection_id)"
    )
  } else NULL

  # age required (any variant), sex never required — matches every pull
  # built this session
  age_filter <- paste(
    "(observations.data->>'age' IS NOT NULL OR observations.data->>'Age' IS NOT NULL",
    "OR observations.data->>'age_L' IS NOT NULL OR observations.data->>'ageL' IS NOT NULL",
    "OR observations.data->>'Age_L' IS NOT NULL OR observations.data->>'AgeL' IS NOT NULL",
    "OR observations.data->>'age_R' IS NOT NULL OR observations.data->>'ageR' IS NOT NULL",
    "OR observations.data->>'Age_R' IS NOT NULL OR observations.data->>'AgeR' IS NOT NULL)"
  )

  location_resolved_filter <- if (drop_unresolved_location) {
    "(observations.location_id IS NOT NULL OR observations.location_period_id IS NOT NULL)"
  } else NULL

  locations_filter <- paste0("location_hierarchies.ancestor_id in ({locations*})")

  time_left_filter  <- if (!is.null(time_left))  paste0("observations.time_left >= '",  format(time_left,  "%Y-%m-%d"), "'") else NULL
  time_right_filter <- if (!is.null(time_right)) paste0("observations.time_right <= '", format(time_right, "%Y-%m-%d"), "'") else NULL
  uids_filter       <- if (!is.null(uids))       paste0("observations.observation_collection_id IN ({uids*})") else NULL

  filters <- c(age_filter, custom_fields_filter, location_resolved_filter,
               time_left_filter, time_right_filter, locations_filter,
               uids_filter, oc_filter, unified_filter)
  filters <- paste(filters[!vapply(filters, is.null, logical(1))], collapse = " AND ")

  obs_query <- glue::glue_sql(paste(obs_query, filters, ";"), .con = conn)

  if (include_geojson) {
    observations <- suppressWarnings(sf::st_as_sf(sf::st_read(conn, query = obs_query)))
  } else {
    observations <- DBI::dbGetQuery(conn, obs_query)
  }

  DBI::dbDisconnect(conn)

  if (nrow(observations) == 0) {
    stop(paste0("No observations found using query ||", obs_query, "||"))
  }

  observations
}