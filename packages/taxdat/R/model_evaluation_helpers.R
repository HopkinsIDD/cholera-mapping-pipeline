#' @include pull_data_helpers.R

#' @export
#' @name read_file_of_type
#' @title read_file_of_type
#' @description Read pipeline files
#' @param filename filename
#' @param variable variable present in file
#' @return
read_file_of_type <- function(filename, variable) {
  if (grepl("output.\\d+.csv$", filename)) {
    # Stan output csv
    model.rand <- rstan::read_stan_csv(filename)
  }
  if (grepl("json$", filename)) {
    # stan input json
    stan_data <- jsonlite::read_json(filename, simplifyVector = TRUE)
  }
  if (grepl("rds$", filename)) {
    # some kind of rds file
    cmdstan_fit <- readRDS(filename)
  }
  if (grepl("rdata$", filename)) {
    # some kind of rdata file
    load(filename)
  }
  if (!exists(variable)) {
    stop(paste0(
      "The variable (", variable, ") isn't present in the file", filename,
      ")"
    ))
  }
  return(eval(expr = parse(text = variable)))
}


#' @export
#' @name get_disjoint_set_sf_cases
#' @title get_disjoint_set_sf_cases
#' @description add
#' @param preprocessed_data_filename Rdata filename with the preprocess suffix
#' @return
get_disjoint_set_sf_cases <- function(.x = NULL, preprocessed_data_filename = NULL,
                                      stan_input_filename = NULL) {
  if (!is.null(.x)) {
    stop("This function only allows named arguments")
  }

  if (!is.null(preprocessed_data_filename)) {
    sf_cases <- read_file_of_type(preprocessed_data_filename, "observation_data")
  }
  if (!is.null(stan_input_filename)) {
    sf_cases <- read_file_of_type(stan_input_filename, "stan_input")$observation_data
  }

  my_names <- names(sf_cases)[c(grep("location", names(sf_cases)), grep(
    "name_",
    names(sf_cases)
  ))]
  sf_cases$attributes.location_period_id <- sf_cases[[my_names[[1]]]]
  for (i in (1 + seq_len(length(my_names) - 1))) {
    sf_cases$attributes.location_period_id <- paste(
      sf_cases$attributes.location_period_id,
      sf_cases[[my_names[i]]]
    )
  }

  aggregate_sf_cases <- dplyr::summarize(dplyr::group_by(sf_cases, attributes.location_period_id),
    cases = mean(suspected_cases / as.numeric(time_right - time_left + 1) * 365),
    variance = var(suspected_cases / as.numeric(time_right - time_left + 1) * 365),
    observations = length(suspected_cases)
  )
  aggregate_sf_cases <- sf::st_as_sf(aggregate_sf_cases)

  aggregate_sf_cases$area <- as.numeric(sf::st_area(aggregate_sf_cases)) / 1000 / 1000
  aggregate_sf_cases$area_adjusted_cases <- aggregate_sf_cases$cases / aggregate_sf_cases$area

  aggregate_sf_cases <- dplyr::arrange(aggregate_sf_cases, -area)
  overlaps <- sf::st_relate(aggregate_sf_cases, aggregate_sf_cases, "2********")
  non_overlapping_sets <- list()

  aggregate_sf_cases$not_included <- TRUE
  index <- 0
  while (any(aggregate_sf_cases$not_included)) {
    leftovers <- which(aggregate_sf_cases$not_included)
    index <- index + 1
    non_overlapping_sets[[index]] <- NA
    not_allowed <- NA
    for (idx in leftovers) {
      if (idx %in% not_allowed) {
      } else {
        aggregate_sf_cases$not_included[idx] <- FALSE
        non_overlapping_sets[[index]] <- c(
          non_overlapping_sets[[index]],
          idx
        )
        not_allowed <- sort(unique(c(not_allowed, overlaps[[idx]])))
      }
    }
    non_overlapping_sets[[index]] <- non_overlapping_sets[[index]][!is.na(non_overlapping_sets[[index]])]
  }

  aggregate_sf_cases$set <- as.integer(NA)

  for (set in seq_len(length(non_overlapping_sets))) {
    aggregate_sf_cases$set[non_overlapping_sets[[set]]] <- set
  }
  return(aggregate_sf_cases)
}
