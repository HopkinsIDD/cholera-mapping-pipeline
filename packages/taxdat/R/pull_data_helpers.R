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
  
  
  changer <- list(
    "year" = function(x) {
      x <- x * unit_count
      return(as.Date(paste(x, "01", "01", sep = "-"), format = "%Y-%m-%d"))
    },
    "month" = function(x) {
      x <- x * unit_count
      month <- ((x - 1) %% 12) + 1
      year <- (x - month) / 12
      return(as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%m-%d"))
    },
    "isoweek" = function(x) {
      x <- x * unit_count
      return(stop("Not yet written"))
    }
  )
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
  
  
  changer <- list(
    "year" = function(x) {
      x <- x * unit_count
      return(as.Date(paste(x, "12", "31", sep = "-"), format = "%Y-%m-%d"))
    },
    "month" = function(x) {
      x <- x * unit_count
      month <- ((x - 1) %% 12) + 1
      year <- (x - month) / 12
      return(as.Date(paste(year, month, lubridate::days_in_month(month), sep = "-"), format = "%Y-%m-%d"))
    },
    "isoweek" = function(x) {
      x <- x * unit_count
      return(stop("Not yet written"))
    }
  )
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
  
  changer <- list(
    "year" = function(x) {
      return(floor(lubridate::year(x) / unit_count))
    },
    "month" = function(x) {
      return(floor((12 * lubridate::year(x) + lubridate::month(x)) / unit_count))
    },
    "isoweek" = function(x) {
      return(stop("Not yet written"))
    }
  )
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
case_definition_to_column_name = function(type,database=FALSE,sql=FALSE){
  if((!database) & (!sql)){
    warning("The svn column names are deprecated, please use database column names.")
    changer <- c(
      'suspected' = 'sCh',
      'confirmed' = 'cCh',
      "presence"=c("sCh","sCh_R","sCh_L","cCh","cCh_L","cCh_R","deaths","deaths_L","deaths_R")
    )
  } else if((database) & (!sql)){
    changer <- c(
      'suspected' = 'attributes.fields.suspected_cases',
      'confirmed' = 'attributes.fields.confirmed_cases',
      "presence"=c(
        "attributes.fields.suspected_cases",
        "attributes.fields.suspected_cases_R",
        "attributes.fields.suspected_cases_L",
        "attributes.fields.confirmed_cases",
        "attributes.fields.confirmed_cases_L",
        "attributes.fields.confirmed_cases_R",
        "attributes.fields.deaths",
        "attributes.fields.deaths_L",
        "attributes.fields.deaths_R"
      )
    )
  } else if((!database) & (sql)){
    changer <- c(
      "suspected" = "suspected_cases",
      "confirmed" = "confirmed_cases",
      "presence"=c(
        "suspected_cases",
        "suspected_cases_R",
        "suspected_cases_L",
        "confirmed_cases",
        "confirmed_cases_L",
        "confirmed_cases_R",
        "deaths",
        "deaths_L",
        "deaths_R"
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
reduce_sf_vector <- function(vec){
  
  if(length(vec) == 0){
    return(sf::st_sf(sf::st_sfc()))
  }
  if(is.null(names(vec))){
    names(vec) = 1:length(vec)
  }
  if(length(names(vec)) != length(vec)){
    names(vec) = 1:length(vec)
  }
  k = 1
  all_columns = unlist(vec,recursive=FALSE)
  split_names = strsplit(names(all_columns),'.',fixed=TRUE)
  column_names = sapply(split_names,function(x){x[[2]]})
  geom_columns = which(column_names == 'geometry')
  geometry = sf::st_as_sfc(unlist(all_columns[geom_columns],recursive=FALSE))
  rc = sf::st_sf(geometry)
  frame_only = dplyr::bind_rows(lapply(vec,function(x){
    x = as.data.frame(x)
    x = x[-grep('geometry',names(x))]
    return(x)
  }))
  rc = dplyr::bind_cols(rc,frame_only)
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
rename_database_fields <- function(database_df,
                                   source = "api") {
  
  if (source == "api") {
    new_database_df <- database_df %>%
      dplyr::rename(
        TL = attributes.time_left,
        TR = attributes.time_right,
        is_primary = attributes.primary,
        is_phantom = attributes.phantom,
        locationPeriod_id = attributes.id,
        OC_UID = relationships.observation_collection.data.id,
        location_name = attributes.location_name
      )
  } else if (source == "sql") {
    new_database_df <- database_df %>%
      dplyr::rename(
        TL = time_left,
        TR = time_right,
        is_primary = primary,
        is_phantom = phantom,
        locationPeriod_id = location_period_id,
        OC_UID = observation_collection_id,
        location_name = location_name
      )
  } else {
    stop("Source needs to be one of 'api', 'sql', found ", source)
  }
  # names(new_database_df) <- gsub("attributes.fields.", "", names(new_database_df))
  # names(new_database_df) <- gsub("attributes.", "", names(new_database_df))
  return(new_database_df)
}


#' @description Flatten the result of a json query into an unnested data frame.  Similar to jsonlite::flatten, but with some tweaks to make it work better for our use case.
#' @param json_results A listlike object convertible to a data frame coming from an api query
#' @result a data frame that matches an unrolled version of json_results
flatten_json_result <- function(json_results) {
  if (!is.data.frame(json_results)) {
    json_results <- as.data.frame(json_results)
  }
  json_results <- jsonlite::flatten(json_results)
  for (colname in names(json_results)) {
    if (mode(json_results[[colname]]) == "list") {
      if (
        (max(sapply(json_results[[colname]], length)) == 1)
      ) {
        json_results[[colname]] <- sapply(json_results[[colname]], function(x){return(ifelse(length(x) == 1, x, NA))})
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
read_taxonomy_data_api <- function(username, 
                                   api_key, 
                                   locations = NULL, 
                                   time_left = NULL, 
                                   time_right = NULL, 
                                   uids = NULL, 
                                   website = "https://api.cholera-taxonomy.middle-distance.com/"){
  
  ## First, we want to set up the https POST request.
  ## We make a list containing the arguments for the request:
  ## If the API changes, we will just need to change this list
  api_type = ""
  if(is.null(uids)){
    api_type = "by_location"
    if(length(locations == 1)){
      locations = c(locations,locations)
    }
    
    ## Prevent continents, or too many countries
    if(any(!grepl('::',locations))){
      stop("Trying to pull data for a continent is not allowed")
    }
    if((sum(stringr::str_count(string = unique(locations),pattern='::') == 1) > 2)){
      stop("Trying to pull data for more than 2 countries at a time is not allowed")
    }
    
    https_post_argument_list = list(
      email=username,
      api_key=api_key,
      locations=gsub('::',' ',locations),
      time_left=time_left,
      time_right=time_right
    )
  } else if(is.null(locations) && is.null(time_left) && is.null(time_right)){
    api_type = "by_observation_collections"
    https_post_argument_list = list(
      email=username,
      api_key=api_key,
      observation_collection_ids = uids
    )
  } else {
    stop("Not supported")
  }
  
  website = paste0(website,"/api/v1/observations/",api_type)
  
  ## Every object in R is a vector, even the primitives.  For example, c(1,5,6) is of type
  ## integer.  Because of this, we need to explicitly tell the JSON parser to treat vectors
  ## of length 1 differently.  The option for this is auto_unbox = T
  json = jsonlite::toJSON(https_post_argument_list,  auto_unbox = T)
  ## Message prints a message to the user.  It's somewhere between a warning and a normal print.
  ## In this case, this function might take a while to run, so we let the user know up front.
  message("Fetching results from JSON API")
  
  ## This is the line that actually fetches the results.
  ## The syntax for adding headers is a little weird.  The function add_headers takes named arguments
  ## and returns whatever the arguments to POST are supposed to be.
  ## body is the body
  ## encode is the transformation to perform on the body to make it into text
  results = httr::POST(
    website,
    httr::add_headers("Content-Type" = "application/json"),
    body=json,
    encode='form'
  )
  
  ## Now we process the status code to make sure that things are working correctly
  code = httr::status_code(results)
  ## Right now, anything that isn't correct is an error
  if(code != 200){
    stop(paste('Error: Status Code',code))
  }
  
  ## Next we extract just the content of the results
  original_results_data = httr::content(results)
  ## This returns something correct, but the formatting is really
  ## odd.  It is a little messy, but instead of debugging the
  ## formatting, for now I'm converting to json and back, which
  ## fixes the problems.
  jsondata = rjson::toJSON(original_results_data)
  if(!jsonlite::validate(jsondata)){
    stop("Could not validate json response")
  }
  results_data = jsonlite::fromJSON(jsondata)
  
  ## Now we have the results of the api data as a nested list.
  ## We want to do the following in no particular order
  ## for the observations, we want to turn them into a data frame
  ## with one row per observation for the location_periods, we want
  ## to turn them into a geometry object and link them to the
  ## observations
  
  ## We start with the observations
  if(                                                      # The | operator is logical or
    (!("observations" %in% names(results_data)))           | # The results should have observations
    (!("data" %in% names(results_data[['observations']]))) | # The observations should have data
    (length(results_data[['observations']]) > 1)             # The data should be the only thing in observations
  ){
    stop("Could not parse results properly.  Contact package maintainer")
  }
  results_data[["observations"]] = flatten_json_result(results_data[["observations"]][["data"]])
  
  observation_collections_present <- FALSE
  if(
    ("observation_collections" %in% names(results_data))           && # The results should have observations
    ("data" %in% names(results_data[['observation_collections']])) && # The observations should have data
    (length(results_data[['observation_collections']]) == 1)          # The data should be the only thing in observations
  ){
    results_data[["observation_collections"]] <- flatten_json_result(results_data[["observation_collections"]][["data"]])
    observation_collections_present <- TRUE
  }
  
  ## Check to make sure that the number of ids and number of rows match
  if(!length(unique(results_data$observations$id)) == nrow(results_data$observations)){
    stop("Could not parse results properly.  Contact package maintainer")
  }
  
  ## Now we want to handle the location periods
  ## We need to process these individually, so we'll loop over
  ## location periods to extract the geojsons
  ## We use the original_results_data here, since the formatting
  ## transformation we did earlier prevents this code from working
  tmp_results = original_results_data[['location_periods']][['data']]
  all_locations = list() # This will be a list of the geojson objects
  if(length(tmp_results) > 0){
    for(idx in 1:length(tmp_results)){
      ## We process the geojson in three pieces.
      ## 1. Convert to json string
      ## 2. Convert to sf object
      ## 3. Add to location list
      message(paste(idx,'/',length(tmp_results)))
      ## Ignore NULL elements.  Undefined list elements default to
      ## NULL anyway
      if(is.null(tmp_results[[idx]]$attributes$geojson)){
        all_locations[[idx]] = sf::st_sf(geometry = sf::st_sfc(sf::st_point()))
        next;
      }
      unformatted_geojson = tmp_results[[idx]][['attributes']][['geojson']]
      json_geojson = jsonlite::toJSON(unformatted_geojson,auto_unbox = TRUE) # 1.
      sf_geojson = geojsonsf::geojson_sf(json_geojson) # 2.
      all_locations[[idx]] = sf_geojson #3.
    }
  }
  ## reduce_sf_vector turns a list of sf objects into a single sf
  ## object containing the same information
  locations_sf = taxdat::reduce_sf_vector(all_locations)
  ## We are going to take our properly formatted geojson files and
  ## replace the badly formatted ones
  results_data$location_periods$data$geojson <- NULL
  results_data$location_periods$data$attributes$geojson <- NULL
  
  results_data$location_periods <- flatten_json_result(results_data$location_periods$data)
  if(nrow(results_data$location_periods) > 0){
    results_data$location_periods$sf_id = seq_len(nrow(results_data$location_periods))
  }
  results_data$observations$attributes.location_period_id = as(results_data$observations$attributes.location_period_id,class(results_data$location_periods$id))
  
  ## We then join (as in sql) by the location_periods with the
  ## observations by location_period_id
  all_results <- results_data$observations
  if(
    observation_collections_present &&
    (nrow(all_results) > 0) &&
    (nrow(results_data$observation_collections) > 0)
  ){
    all_results <- dplyr::left_join(
      results_data$observations,
      results_data$observation_collections,
      by=c(
        'relationships.observation_collection.data.id' = 'id' # lhs column name = rhs column name
      )
    )
  }
  if(
    (nrow(all_results) > 0) &&
    (nrow(results_data$location_periods) > 0)
  ){
    all_results = dplyr::left_join(
      all_results,
      results_data$location_periods,
      by=c(
        'attributes.location_period_id' = 'id' # lhs column name = rhs column name
      )
    )
  }
  
  geoinput <- sf::st_sf(geometry=sf::st_sfc(sf::st_point(1.*c(NA,NA))))$geometry
  if(nrow(all_results) == 0){
    geoinput <- geoinput[0]
  }
  all_results$geojson = geoinput
  all_results$geojson[!is.na(all_results$sf_id)] = locations_sf$geometry[all_results[!is.na(all_results$sf_id),][['sf_id']] ]
  return(sf::st_sf(all_results,sf_column_name = 'geojson'))
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
pull_taxonomy_data <- function(username,
                               password,
                               locations = NULL,
                               time_left = NULL,
                               time_right = NULL,
                               uids = NULL, 
                               website = "https://api.cholera-taxonomy.middle-distance.com/",
                               source) {
  
  if (missing(source) | is.null(source))
    stop("No source specified to pull taxonomy data, please specify one of 'api' or 'sql'.")
  
  if (source == 'api') {
    if (missing(username) | missing(password) | is.null(username) | is.null(password))
      stop("Trying to pull data from API, please provide username and api_key.")
    
    # Return API data pull
    rc <- read_taxonomy_data_api(username = username,
                                 api_key = password,
                                 locations = locations,
                                 time_left = as.character(time_left),
                                 time_right = as.character(time_right),
                                 uids = uids,
                                 website = website)
    
  } else if (source == 'sql') {
    if (missing(username) | missing(password) | is.null(username) | is.null(password))
      stop("Trying to pull data using sql on idemodelin2, please provide database username and password.")
    
    # Return SQL data pull
    rc <- read_taxonomy_data_sql(username = username,
                                 password = password,
                                 locations = locations,
                                 time_left = time_left,
                                 time_right = time_right,
                                 uids = uids)
    rc$attributes.fields.suspected_cases <- rc$suspected_cases
    rc$attributes.fields.confirmed_cases <- rc$confirmed_cases
    rc$attributes.fields.location_id <- rc$location_id
    rc$attributes.location_period_id <- rc$location_period_id
    
  } else {
    stop("Parameter 'source' needs to be one of 'api' or 'sql'.")
  }
  
  if(nrow(rc) == 0) {
    if (!is.null(uids)) {
      err_mssg <- paste("in uids", paste(uids, collapse = ","))
    } else if (!is.null(locations)) {
      err_mssg <- paste("in locations", paste(locations, collapse = ","))
    } else {
      err_mssg <- ""
    }
    stop("Didn't find any data ", err_mssg, " in time range [", 
         ifelse(is.null(time_left), "-Inf", as.character(time_left)), " - ",
         ifelse(is.null(time_right), "-nf", as.character(time_right)), "]")
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
read_taxonomy_data_sql <- function(username,
                                   password,
                                   locations = NULL,
                                   time_left = NULL,
                                   time_right = NULL,
                                   uids = NULL) {
  #library(tidyverse)
  library(sf)
  
  if (missing(username) | missing(password)){
    stop("Please provide username and password to connect to the taxonomy database.")}
  
  # Connect to database
  conn <- RPostgres::dbConnect(RPostgres::Postgres(),
                               host = "db.cholera-taxonomy.middle-distance.com",
                               dbname = "CholeraTaxonomy_production",
                               user = username,
                               password = password,
                               port = "5432")
  
  # Build query for observations
  obs_query <- paste("SELECT observations.id::text, observations.observation_collection_id::text, observations.time_left, observations.time_right,", 
                     "observations.suspected_cases, observations.confirmed_cases, observations.deaths, observations.location_period_id::text, observations.location_id::text,",
                     "observations.phantom, observations.primary
                     FROM observations left join location_hierarchies on observations.location_id = location_hierarchies.descendant_id")
  
  cat("-- Pulling data from taxonomy database with SQL \n")
  
  # Add filters
  if (any(c(!is.null(time_left),
            !is.null(time_right), 
            !is.null(uids)))) {
    obs_query <- paste(obs_query, "\n WHERE ")
  } else {
    warning("No filters specified on data pull, pulling all data.")
  }
  
  if (!is.null(time_left)) {
    time_left_filter <- paste0("time_left >= '", format(time_left, "%Y-%m-%d"), "'")
  } else {
    time_left_filter <- NULL
  }
  
  if (!is.null(time_right)) {
    time_right_filter <- paste0("time_right <= '", format(time_right, "%Y-%m-%d"), "'")
  } else {
    time_right_filter <- NULL
  }
  
  if (!is.null(locations)) {
    if(all(is.numeric(locations))){
      locations_filter <- paste0("ancestor_id in ({locations*})")
    } else {
      stop("SQL access by location name is not yet implemented")
    }
  } else {
    #locations_filter <- NULL
    stop("Please use a containing location as the location. Locations can't be NULL.")
  }
  
  if (!is.null(uids)) {
    uids_filter <- paste0("observation_collection_id IN ({uids*})")
  } else {
    uids_filter <- NULL
  }
  
  # Combine filters
  filters <- c(time_left_filter, time_right_filter, 
               locations_filter, uids_filter) %>% 
    paste(collapse = " AND ")
  
  # Run query for observations
  obs_query <- glue::glue_sql(paste(obs_query, filters, ";"), .con = conn)
  observations <- DBI::dbGetQuery(conn = conn, obs_query)
  if(nrow(observations) == 0){
    stop(paste0("No observations found using query ||",obs_query,"||"))
  }
  
  # Pull location_periods
  u_lps <- unique(observations$location_period_id)    # unique location period ids
  u_lps <- u_lps[!is.na(u_lps)]
  if(all(u_lps == as.numeric(u_lps))){
    u_lps <- as.numeric(u_lps)
  } else {
    stop("Location period id exceeds max integer in R, and glue doesn't work on int64s")
  }
  
  lp_query <- glue::glue_sql("SELECT a.location_period_id::text as location_period_id, b.qualified_name as location_name, shape 
  FROM shapes a
  JOIN locations b
  ON a.location_period_id = b.id
  WHERE a.location_period_id IN ({u_lps*});", .con = conn)
  location_periods <- DBI::dbGetQuery(conn = conn, lp_query)
  
  # Get missing geometries
  location_period_issues <- location_periods %>%   
    dplyr::filter(is.na(shape) | shape == "{}")
  
  # Get unique valid geojsons
  location_periods <- location_periods  %>%   
    dplyr::filter(!is.na(shape), shape != "{}") %>% 
    dplyr::group_by(location_period_id) %>% 
    dplyr::slice(1)
  
   
  # Convert to sf object
  # location_periods.sf <- purrr::map(location_periods$geojson, ~try(geojsonsf::geojson_sf(.), silent = F))

  # Get errors
  errors <- purrr::map2(location_periods.sf, seq_along(location_periods.sf), ~ if (inherits(.x, "try-error")) .y) %>% 
    unlist()
  if (length(errors) > 0) {
    cat("Found unreadable geojson for location periods:", str_c(errors, collapse = ", "))
    location_periods.sf <- location_periods.sf[-errors]
    location_periods <- location_periods[-errors, ]
  }
  
  # extract geometries and metadata
  location_periods.sf <- location_periods.sf %>% 
  #location_periods.sf <- do.call(rbind, location_periods.sf) %>% 
    # dplyr::mutate(location_period_id = location_periods$location_period_id,
    #               location_name = location_periods$location_name,
    #               times = ifelse(is.na(location_name), NA, stringr::str_extract(location_name, "([0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{4}-[0-9]{2}-[0-9]{2})"))
    # ) %>% 
    # dplyr::select(-times) %>% 
    dplyr::rename(shape = geometry)
  
  # Combine observations and geojsons
  res <- dplyr::left_join(observations, as.data.frame(location_periods.sf), by = "location_period_id")
  res <- sf::st_as_sf(res)
  
  detach("package:tidyverse", unload = T)
  detach("package:sf", unload = T)
  return(res)
}
