#' @include plot_cache_function.R

#' @export
#' @name get_gam_values_no_cache
#' @description gets the predicted GAM rate and case incidence values that are used 
#' for initializing the model
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return a dataframe based on sf_grid with a column for the log predictions of
#' cases (log_y) and of rates (log_lambda)
get_gam_values_no_cache <- function(
                           config,
                           cholera_directory,
                           cache) {
  
  # Get stan input and covar cube
 get_stan_input(name="stan_input",config=config,cache=cache,cholera_directory=cholera_directory)
 get_initial_values(name="initial_values_data",config=config,cache=cache,cholera_directory=cholera_directory)
  
  coord_frame <- tibble::as_tibble(sf::st_coordinates(cache[["stan_input"]]$sf_grid)) %>% 
    dplyr::group_by(L2) %>% 
    dplyr::summarise(x = mean(X), 
                     y = mean(Y))
  
  # Create matrix of time
  year_df <- tibble::tibble(year = cache[["stan_input"]]$stan_data$map_grid_time)
  year_df$year <- factor(year_df$year)
  
  ## one random effect per year
  if (length(unique(year_df$year)) == 1) {
    mat_grid_time <- matrix(1, nrow(year_df))
  } else {
    mat_grid_time <- model.matrix(as.formula("~ year - 1"), data = year_df)
  }
  
  predict_df <- tibble::tibble(sx = coord_frame$x,
                               sy = coord_frame$y) %>% 
    # Set all years to 0 to get the reference year
    cbind(mat_grid_time %>% 
            tibble::as_tibble() %>%
            magrittr::set_colnames(paste0("year_", 1:ncol(mat_grid_time)))) %>% 
    # Extract the covariates
    cbind(cache[["stan_input"]]$stan_data$covar %>% 
            matrix(ncol = cache[["stan_input"]]$stan_data$ncovar) %>% 
            magrittr::set_colnames(paste0("beta_", 1:cache[["stan_input"]]$stan_data$ncovar))) %>% 
    tibble::as_tibble() %>% 
    mutate(logpop = log(cache[["stan_input"]]$stan_data$pop),
           logoffset = logpop + log(cache[["stan_input"]]$stan_data$meanrate))
  
  # Predict log(lambda) for the reference year with covariates
  log_y_pred_mean <- mgcv::predict.gam(cache[["initial_values_data"]]$gam_fit_output, predict_df)
  
  gam_output_df <- cache[["stan_input"]]$sf_grid %>% 
    dplyr::mutate(log_y = log_y_pred_mean + predict_df$logoffset,
                  y = exp(log_y),
                  log_lambda = log_y_pred_mean + log(cache[["stan_input"]]$stan_data$meanrate),
                  lambda = exp(log_lambda),
                  sx=predict_df$sx,
                  sy=predict_df$sy)
  
  return(gam_output_df)
}
#' @export
#' @name get_gam_values
get_gam_values<-cache_fun_results(name="gam_output_df",
                                  fun = get_gam_values_no_cache,
                                  overwrite = T, 
                                  config = config,
                                  cholera_directory = cholera_directory)
