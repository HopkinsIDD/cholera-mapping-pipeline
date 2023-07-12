#' This function snaps an observation to a time period
#'
#' @param TL 
#' @param TR 
#' @param TL_ref 
#' @param TR_ref 
#' @export
compute_tfrac <- function(TL, 
                          TR, 
                          TL_ref, 
                          TR_ref) {
  
  ref_period_length <- lubridate::days(lubridate::days(1) + TR_ref - TL_ref)
  obs_period_length <- lubridate::days(lubridate::days(1) + min(TR, TR_ref) - max(TL, TL_ref)) 
  
  tfrac <- obs_period_length/ref_period_length
  
  tfrac
}

#' get_start_timeslice
#' Computes the start of the time slices corresponding to the given date
#' 
#' @param x date for which to compute the start of the time slice
#' @param res_time 
#'
#' @return
#' @export
#'
#' @examples
get_start_timeslice <- function(x, res_time) {
  time_change_func <- time_unit_to_aggregate_function(res_time)
  aggregate_to_start <- time_unit_to_start_function(res_time)
  
  aggregate_to_start(time_change_func(x))
}

#' get_end_timeslice
#' Computes the start of the time slices corresponding to the given date
#'
#' @param x 
#' @param res_time 
#'
#' @return
#' @export
#'
#' @examples
get_end_timeslice <- function(x, res_time) {
  time_change_func <- time_unit_to_aggregate_function(res_time)
  aggregate_to_end <- time_unit_to_end_function(res_time)
  
  aggregate_to_end(time_change_func(x))
}

#'
#' @param TL 
#' @param TR 
#' @param tol 
#'
#' @return
#' @export
#'
snap_to_time_period <- function(TL, 
                                TR,
                                res_time,
                                tol) {
  
  # Get the corresponding aggregator units
  
  ref_period_L <- c("TL" = get_start_timeslice(TL, res_time),
                    "TR" = get_end_timeslice(TL, res_time))
  
  ref_period_R <- c("TL" = get_start_timeslice(TR, res_time),
                    "TR" = get_end_timeslice(TR, res_time))
  
  # Check if observation covers more than 2 time slices
  if (difftime(ref_period_R["TL"], ref_period_L["TR"], units = "days") > 1) {
    warning("---- Observations spans more than two time slices, skipping snap to period.")
    return(c("TL" = TL, "TR" = TR))
  }
  
  if (identical(ref_period_L, ref_period_R)) {
    return(c("TL" = TL, "TR" = TR))
  }
  
  # Get the reference time period as the one with the longest span
  tfrac_L <- compute_tfrac(TL, TR, ref_period_L["TL"], ref_period_L["TR"])
  tfrac_R <- compute_tfrac(TL, TR, ref_period_R["TL"], ref_period_R["TR"])
  
  if (min(tfrac_R, tfrac_L) < tol) {
    if (tfrac_R < tfrac_L) {
      TL_out <- TL
      TR_out <- ref_period_L["TR"]
      names(TR_out) <- NULL
    } else {
      TL_out <- ref_period_R["TL"]
      names(TL_out) <- NULL
      TR_out <- TR
    }
    cat("-- Snapping", paste(TL, TR, sep = "_"), "to", paste(TL_out, TR_out, sep = "_"), "\n")
  } else {
    TL_out <- TL
    TR_out <- TR
  }
  
  return(c("TL" = TL_out, "TR" = TR_out))
}


#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
snap_to_time_period_df <- function(df,
                                   TL_col = "TL",
                                   TR_col = "TR",
                                   res_time,
                                   tol) {
  
  dfcols <- colnames(df)
  
  if (!((TL_col %in% dfcols) & (TR_col %in% dfcols))) {
    stop("Dataframe needs columns left:", TL_col,  " and right:", TR_col, " to exist but could not find them among colnames.")
  }
  
  # Filter out data for which TL and TR do not fall in the same time slices
  aggfunc <- time_unit_to_start_function(res_time)
  time_change_func <- time_unit_to_aggregate_function(res_time)
  ts_TL <- aggfunc(time_change_func(df$TL))
  ts_TR <- aggfunc(time_change_func(df$TR))
  ind_diff <- which(ts_TL != ts_TR)
  
  if (length(ind_diff) > 0) {
    cat("---- Attempting to snap", length(ind_diff), "observations\n")
    
    for (i in ind_diff) {
      new_ts <- snap_to_time_period(TL = df[[TL_col]][i], 
                                    TR = df[[TR_col]][i], 
                                    res_time = res_time,
                                    tol = tol)
      df[[TL_col]][i] <- new_ts[1]
      df[[TR_col]][i] <- new_ts[2]
    }
  }
  
  df
}

#' drop_multiyear
#'
#' @param df 
#' @param TL_col 
#' @param TR_col
#' @param admin_levels Admin levels for which to drop multi-eary observations
#' @return
#' @export
#'
#' @examples
drop_multiyear <- function(df,
                           TL_col = "TL",
                           TR_col = "TR",
                           admin_levels = c(0)) {
  
  res_time <- "1 year"
  dfcols <- colnames(df)
  
  if (!((TL_col %in% dfcols) & (TR_col %in% dfcols))) {
    stop("Dataframe needs columns left:", TL_col,  " and right:", TR_col, " to exist but could not find them among colnames.")
  }
  
  if (!("admin_level" %in% dfcols)) {
    stop("The dataframe needs to have a colmun called 'admin_lev' specifying the admin level")
  }
  
  # Filter out data for which TL and TR do not fall in the same time slices
  aggfunc <- time_unit_to_start_function(res_time)
  time_change_func <- time_unit_to_aggregate_function(res_time)
  ts_TL <- aggfunc(time_change_func(df$TL))
  ts_TR <- aggfunc(time_change_func(df$TR))
  ind_diff <- which(ts_TL != ts_TR & df$admin_level %in% admin_levels)
  
  if (length(ind_diff) > 0){
    
    cat("---- Dropping", length(ind_diff), "observations that span multiple years from admin levels",
        paste(admin_levels, collapse = "-"), ":\n")
    
    print(df[ind_diff, ])
    
    return(df[-c(ind_diff), ])
  } else {
    return(df)
  }
}