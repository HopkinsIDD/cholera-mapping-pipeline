#' This function snaps an observation to a time period
#'
#' @param TL 
#' @param TR 
#' @param TL_ref 
#' @param TR_ref 

compute_tfrac <- function(TL, 
                          TR, 
                          TL_ref, 
                          TR_ref) {
  
  ref_period_length <- lubridate::days(lubridate::days(1) + TR_ref - TL_ref)
  obs_period_length <- lubridate::days(lubridate::days(1) + min(TR, TR_ref) - max(TL, TL_ref)) 
  
  tfrac <- obs_period_length/ref_period_length
  
  tfrac
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
  time_change_func <- time_unit_to_aggregate_function(res_time)
  aggregate_to_start <- time_unit_to_start_function(res_time)
  aggregate_to_end <- time_unit_to_end_function(res_time)
  
  ref_period_L <- c("TL" = aggregate_to_start(time_change_func(TL)),
                    "TR" = aggregate_to_end(time_change_func(TL)))
  
  ref_period_R <- c("TL" = aggregate_to_start(time_change_func(TR)),
                    "TR" = aggregate_to_end(time_change_func(TR)))
  
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
      TL_out <- ref_period_L["TL"]
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