#' @include plot_cache_function.R

#' @name get_model_rand_no_cache
#' @description load stan output
#' @param config 
#' @param cache 
#' @return covar cube
get_model_rand_no_cache <- function(config, cache, cholera_directory) {
  config <- yaml::read_yaml(config_filename)
  file_names <- taxdat::get_filenames(config, cholera_directory)
  model.rand <- taxdat::read_file_of_type(file_names[["stan_output"]], "model.rand")
  require(bit64)
  require(sf)
  return(model.rand)
}
# cache the results
get_model_rand <- cache_fun_results(name = "model.rand", fun = get_model_rand_no_cache,
                                    overwrite = T, config = config)

#' @name get_modeled_cases_no_cache
#' @title get_modeled_cases_no_cache
#' @description extrac modeled cases from model.rand
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return  modeled_cases
get_modeled_cases_no_cache <- function(config, cache, cholera_directory, ...) {
  get_model_rand(config, cache, cholera_directory, ...)
  modeled_cases <-  as.array(cache[["model.rand"]])[, , grepl("grid_case", names(cache[["model.rand"]])),drop=FALSE]
  return(modeled_cases)
}
# cache the results
get_modeled_cases <- cache_fun_results("modeled_cases", get_modeled_cases_no_cache,
                                       overwrite = T, config = config)

#' @name get_modeled_rates_no_cache
#' @title get_modeled_rates_no_cache
#' @description extrac modeled rates (from log lambda) from model.rand
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return  modeled_rates
get_modeled_rates_no_cache <- function(config, cache, cholera_directory, ...) {
  get_model_rand(config, cache, cholera_directory, ...) 
  modeled_rates <- exp(as.array(model.rand)[, , grepl("log_lambda", names(model.rand)), drop = FALSE])
  return(modeled_rates)
}
# cache the results
get_modeled_rates <- cache_fun_results("modeled_rates", get_modeled_rates_no_cache,
                                       overwrite = T, config = config)


########################
load("C:/IDD/Cholera/2021_Dec_mapping_helper/cmp_dev_20220218/cmp_dev_prior_rates/SSD_stan_output.rdata")
model.rand <- taxdat::read_file_of_type("C:/IDD/Cholera/2021_Dec_mapping_helper/cmp_dev_20220218/cmp_dev_prior_rates/SSD_stan_output.rdata", "model.rand")
model.rand<-eval(expr = parse(text = "model.rand"))
model.rand$grid_cases
modeled_cases <- as.array(model.rand)[, , grepl("grid_case", names(model.rand)),drop=FALSE]
dim(modeled_cases)#iteration, chain and grid*time
modeled_cases_mean <- apply(modeled_cases, c(2,3), mean)
dim(modeled_cases_mean)
########################

#' @name aggregate_modeled_cases_by_chain_gridtime_no_cache
#' @description aggregate the modeled cases by chain
#' @param modeled_cases
#' @param funs the function to aggregate the modeled cases
#' @return aggregated_modeled_cases_by_chain_gridtime
aggregate_modeled_cases_by_chain_gridtime_no_cache <- function(modeled_cases, funs = "mean"){
  aggregated_modeled_cases_by_chain_gridtime <- apply(modeled_cases, c(2,3), funs)
  return(aggregated_modeled_cases_by_chain_gridtime)
  }

#' @name aggregate_modeled_cases_by_chain_gridtime
#' @title aggregate_to_modeled_cases_mean
#' @description get the mean of the modeled cases for each grid cell
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return  modeled cases mean for each grid cell by times
aggregate_modeled_cases_by_chain_gridtime <- cache_fun_results("aggregated_modeled_cases_by_chain_gridtime",aggregate_modeled_cases_by_chain_gridtime_no_cache,
                                                               overwrite=T,config=config)


#' @name aggregate_modeled_rates_by_chain_gridtime_no_cache
#' @description aggregate the modeled rates by chain
#' @param modeled_cases
#' @param funs the function to aggregate the modeled cases
#' @return aggregate_modeled_rates_by_chain_gridtime_no_cache
aggregate_modeled_rates_by_chain_gridtime_no_cache <- function(modeled_rates, funs = "mean"){
  aggregated_modeled_rates_by_chain_gridtime <- apply(modeled_rates, c(2,3), funs)
  return(aggregated_modeled_rates_by_chain_gridtime)
}

#' @name aggregate_modeled_rates_by_chain_gridtime
#' @title aggregate_modeled_rates_by_chain_gridtime
#' @description get the mean of the modeled cases for each grid cell
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return  modeled cases mean for each grid cell by times
aggregate_modeled_rates_by_chain_gridtime <- cache_fun_results("aggregated_modeled_rates_by_chain_gridtime",aggregate_modeled_rates_by_chain_gridtime_no_cache,
                                                               overwrite=T,config=config)