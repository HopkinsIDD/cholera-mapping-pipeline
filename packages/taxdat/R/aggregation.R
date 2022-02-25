#' @export
project_to_groups <- function(df, grouping_columns, base_df = df) {
    unique_columns_by_group <- get_unique_columns_by_group(df, grouping_columns)
    missing_cols <- names(base_df)[!(names(base_df) %in% c(grouping_columns, unique_columns_by_group))]
    if (length(missing_cols) > 0) {
        warning(paste("The following columns were lost during aggregation :", paste(missing_cols,
            collapse = ", ")))
    }
    unique_columns_by_group <- unique_columns_by_group[unique_columns_by_group %in%
        names(base_df)]
    rc <- df %>%
        as.data.frame() %>%
        dplyr::group_by(!!!rlang::syms(grouping_columns)) %>%
        dplyr::summarize_at(.vars = unique_columns_by_group, .funs = ~unique(.))
    return(rc)
}


#' @export
get_unique_columns_by_group <- function(df, grouping_columns, skip_columns = grouping_columns) {
    rc <- df %>%
        as.data.frame() %>%
        dplyr::group_by(!!!rlang::syms(grouping_columns)) %>%
        dplyr::summarize_all(~1 == length(unique(.))) %>%
        dplyr::ungroup() %>%
        dplyr::summarize_all(all)
    rc <- names(rc)[!sapply(as.logical(rc), isFALSE)]
    rc <- rc[!(rc %in% skip_columns)]
    return(rc)
}


#' @export
aggregate_case_data <- function(case_data, unique_column_names = c("loctime")) {
    ## aggregate observations:
    ocrs <- sf::st_crs(case_data)
    # TODO : Remove observation_collection_id, location_period_id from this
    # list
    case_data <- case_data %>%
        dplyr::group_by(!!!rlang::syms(unique_column_names), observation_collection_id,
            location_period_id) %>%
        dplyr::group_modify(function(.x, .y) {
            cat("iter", unlist(.y), "\n")
            if (nrow(.x) <= 1) {
                .x %>%
                  dplyr::select(time_left, time_right, !!rlang::sym(cases_column)) %>%
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
                dplyr::summarize(time_left = min(time_left), time_right = min(time_left) +
                  sum(duration) - 1, `:=`(!!cases_column, sum(!!rlang::sym(cases_column),
                  na.rm = TRUE)), unique_observation_ids = paste(sort(unique(sprintf(paste0("%0",
                  ceiling(log(max(case_data$observation_id))/log(10)), "d"), as.integer(observation_id)))),
                  collapse = " ")) %>%
                dplyr::ungroup() %>%
                dplyr::select(-set)
            return(.x[!duplicated(.x), ])
        }) %>%
        dplyr::ungroup()
    case_data$observation_id <- bit64::as.integer64(factor(case_data$unique_observation_ids,
        sort(unique(case_data$unique_observation_ids))))
    case_data <- sf::st_as_sf(case_data)
    sf::st_crs(case_data) <- ocrs
    return(case_data)
}


#' @export
remove_overlapping_observations <- function(case_data, unique_column_names = c("loctime")) {
    case_data %>%
        dplyr::group_by(!!!rlang::syms(unique_column_names), observation_collection_id,
            location_period_id) %>%
        dplyr::group_modify(function(.x, .y) {
            .x <- dplyr::arrange(.x, desc(time_right), time_left)
            for (to_remove in rev(seq_len(nrow(.x)))) {
                other_indices <- seq_len(nrow(.x))
                other_indices <- other_indices[other_indices != to_remove]
                inner_interval <- lubridate::interval(start = .x$time_left[to_remove],
                  end = .x$time_right[to_remove])
                removed <- FALSE
                for (possible_container in other_indices) {
                  if (!removed) {
                    outer_interval <- lubridate::interval(start = .x$time_left[possible_container],
                      end = .x$time_right[possible_container])
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
