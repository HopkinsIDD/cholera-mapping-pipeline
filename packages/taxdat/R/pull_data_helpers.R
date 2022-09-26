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
#'   at https://api.cholera-taxonomy.middle-distance.com pulls
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
                                   time_right = NULL, uids = NULL, website = "https://api.cholera-taxonomy.middle-distance.com/") {

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
