#' @export
project_to_groups <- function(df, grouping_columns, base_df = df) {
  unique_columns_by_group <- get_unique_columns_by_group(df, grouping_columns)
  missing_cols <- names(base_df)[!(names(base_df) %in% c(grouping_columns, unique_columns_by_group))]
  if (length(missing_cols) > 0) {
    warning(paste("The following columns were lost during aggregation :", paste(missing_cols,
      collapse = ", "
    )))
  }
  unique_columns_by_group <- unique_columns_by_group[unique_columns_by_group %in%
    names(base_df)]
  rc <- df %>%
    as.data.frame() %>%
    dplyr::group_by(!!!rlang::syms(grouping_columns)) %>%
    dplyr::summarize_at(.vars = unique_columns_by_group, .funs = ~ unique(.))
  return(rc)
}


#' @export
get_unique_columns_by_group <- function(df, grouping_columns, skip_columns = grouping_columns) {
  rc <- df %>%
    as.data.frame() %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!!rlang::syms(grouping_columns)) %>%
    dplyr::summarize_all(~ 1 == length(unique(.))) %>%
    dplyr::ungroup() %>%
    dplyr::summarize_all(all)
  rc <- names(rc)[!sapply(as.logical(rc), isFALSE)]
  rc <- rc[!(rc %in% skip_columns)]
  return(rc)
}


#' @export
remove_inconsistent_duplicates <- function(case_data, columns_to_mean_over, unique_column_names = c("loctime")) {
  ## aggregate observations:
  ocrs <- sf::st_crs(case_data)
  # TODO : Remove observation_collection_id, location_period_id from this
  # list
  case_data <- case_data %>%
    dplyr::group_by(
      observation_collection_id,
      location_period_id, time_left, time_right, !!!rlang::syms(unique_column_names)
    ) %>%
    dplyr::summarize(
      dplyr::across(columns_to_mean_over, mean, na.rm = TRUE)
      ## Do something to account for other columns which are relatively unique here
    ) %>%
    return()
}
#' @export
aggregate_case_data <- function(case_data, unique_column_names = c("loctime"), columns_to_sum_over = c("tfrac")) {
  ## aggregate observations:
  ocrs <- sf::st_crs(case_data)
  # TODO : Remove observation_collection_id, location_period_id from this
  # list
  case_data <- case_data %>%
    dplyr::group_by(
      !!!rlang::syms(unique_column_names), observation_collection_id,
      location_period_id
    ) %>%
    dplyr::group_modify(function(.x, .y) {
      # cat('iter', unlist(.y), '\n')
      if (nrow(.x) <= 1) {
        .x %>%
          dplyr::select(time_left, time_right, !!!rlang::syms(columns_to_sum_over)) %>%
          return()
      }
      ## combine non-adjacent but overlapping observations
      .x <- dplyr::arrange(dplyr::mutate(.x, set = as.integer(NA)), desc(time_right))
      .x$set[[1]] <- 0
      current_set <- 1
      something_changed <- TRUE
      while (any(is.na(.x$set))) {
        new_set_indices <- (rev(cummax(rev(!is.na(.x$set)))) == 1) & is.na(.x$set)
        if (any(new_set_indices) & (!something_changed)) {
          .x$set[[which(new_set_indices)[[1]]]] <- current_set
          current_set <- current_set + 1
          something_changed <- TRUE
        } else if (!something_changed) {
          .x$set[[which(is.na(.x$set))[[1]]]] <- current_set
          current_set <- current_set + 1
          something_changed <- TRUE
        }
        something_changed <- FALSE
        for (set_idx in (seq_len(current_set) - 1)) {
          possible_extensions <- (.x$time_right < .x$time_left[max(which((.x$set ==
            set_idx) & !is.na(.x$set)))]) & is.na(.x$set)
          if (any(possible_extensions)) {
            .x$set[[min(which(possible_extensions))]] <- set_idx
            something_changed <- TRUE
          }
        }
      }
      .x$duration <- .x$time_right - .x$time_left + 1
      .x <- .x %>%
        dplyr::group_by(set) %>%
        dplyr::summarize(
          time_left = min(time_left), time_right = min(time_left) +
            sum(duration) - 1, dplyr::across(columns_to_sum_over, ~ sum(., na.rm = TRUE)),
          unique_observation_ids = paste(sort(unique(sprintf(paste0(
            "%0",
            ceiling(log(max(case_data$observation_id)) / log(10)), "d"
          ), as.integer(observation_id)))),
          collapse = " "
          ), .groups = "drop"
        ) %>%
        dplyr::select(-set)
      return(.x[!duplicated(.x), ])
    }) %>%
    dplyr::ungroup()
  case_data$observation_id <- bit64::as.integer64(factor(
    case_data$unique_observation_ids,
    sort(unique(case_data$unique_observation_ids))
  ))
  case_data <- sf::st_as_sf(case_data)
  sf::st_crs(case_data) <- ocrs
  return(case_data)
}


#' @export
remove_overlapping_observations <- function(case_data, unique_column_names = c("loctime")) {
  case_data %>%
    dplyr::group_by(
      !!!rlang::syms(unique_column_names), observation_collection_id,
      location_period_id
    ) %>%
    dplyr::group_modify(function(.x, .y) {
      .x <- dplyr::arrange(.x, desc(time_right), time_left)
      for (to_remove in rev(seq_len(nrow(.x)))) {
        other_indices <- seq_len(nrow(.x))
        other_indices <- other_indices[other_indices != to_remove]
        inner_interval <- lubridate::interval(
          start = .x$time_left[to_remove],
          end = .x$time_right[to_remove]
        )
        removed <- FALSE
        for (possible_container in other_indices) {
          if (!removed) {
            outer_interval <- lubridate::interval(
              start = .x$time_left[possible_container],
              end = .x$time_right[possible_container]
            )
            if (lubridate::intersect(inner_interval, outer_interval) == inner_interval) {
              removed <- TRUE
              .x <- .x[other_indices, ]
            }
          }
        }
      }
      return(.x)
    }) %>%
    return()
}

#' @export
do_censoring <- function(case_data, colnames, unique_column_names = c("loctime"),
                         threshold = 0.95) {
  ocrs <- sf::st_crs(case_data)
  max_observation_id <- max(case_data$observation_id)
  case_data <- case_data %>%
    dplyr::group_by(!!!rlang::syms(unique_column_names), observation_id) %>%
    dplyr::group_modify(function(.x, .y) {
      good_indices <- .x$tfrac > threshold
      bad_indices <- .x$tfrac <= threshold
      .x$new_observation_id <- .y$observation_id
      ## only do censoring if there is at least one bad observation
      if (any(bad_indices)) {
        overestimate <- .x[good_indices, ]
        underestimate <- .x
        for (colname in colnames) {
          underestimate[[paste0(colname, "_R")]] <- underestimate[[colname]]
          overestimate[[paste0(colname, "_L")]] <- overestimate[[colname]][good_indices]
          overestimate[[colname]] <- as.numeric(NA)
          underestimate[[colname]] <- as.numeric(NA)
        }
        underestimate$new_observation_id <- .y$observation_id
        overestimate$new_observation_id <- .y$observation_id + max_observation_id
        .x <- dplyr::bind_rows(underestimate, overestimate)
      }
      return(.x)
    }) %>%
    dplyr::ungroup()
  case_data$original_observation_id <- case_data$observation_id
  case_data$observation_id <- case_data$new_observation_id
  case_data$new_observation_id <- NULL
  case_data <- sf::st_as_sf(case_data)
  sf::st_crs(case_data) <- ocrs
  return(case_data)
}

#' @title Reorder for single source
#'
#' @description Reorder adjacency matrix to have single source
#'
#' @param A
#' @param coords
#'
#' @return the reodered adjacency matrix
#' @export
reorder_adjacency_matrix <- function(adjacency_frame, element_bias, id_cols = c(
                                       "id_1",
                                       "id_2"
                                     )) {

  ## element_bias <- covar_cube %>% dplyr::filter(t == min(t)) %>%
  ## mutate(bias = x - y) %>% .[['bias']]

  N <- length(element_bias)

  element_changer <- setNames(seq_len(N), seq_len(N)[order(element_bias)])

  adjacency_matrix <- Matrix::sparseMatrix(
    i = adjacency_frame[[id_cols[[1]]]],
    adjacency_frame[[id_cols[[2]]]], x = TRUE, symmetric = TRUE, dims = rep(length(element_changer), times = 2)
  )[
    element_changer,
    element_changer
  ]

  print("Reordering adjacency to have single source")

  # Number of vertices
  if (N != nrow(adjacency_matrix)) {
    stop(paste(
      "Adjacency frame does not imply the correct number of gridcells. This probably means there are gridcells with no neighbors:",
      N, "expected, but", nrow(adjacency_matrix), "found."
    ))
  }

  adjacency_graph <- igraph::graph_from_adjacency_matrix(adjacency_matrix, "undirected")
  bfs_order <- as.vector(igraph::bfs(adjacency_graph, root = 1)$order)
  reordered_matrix <- Matrix::triu(adjacency_matrix[bfs_order, bfs_order])[
    order(bfs_order),
    order(bfs_order)
  ][order(element_changer), order(element_changer)]
  reordered_frame <- Matrix::summary(reordered_matrix)[, c("i", "j")]
  names(reordered_frame) <- id_cols
  return(reordered_frame)
}
