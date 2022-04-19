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

#' @name aggregate_modeled_cases_by_chain_no_cache
#' @title aggregate_modeled_cases_by_chain_no_cache
#' @description get the mean of the modeled cases by chain
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return  modeled cases mean for each grid cell by times
aggregate_modeled_cases_by_chain_no_cache <- function(modeled_cases, funs = "mean") {
  modeled_cases_by_chain <- apply(modeled_cases, 2, funs)
  return(modeled_cases_by_chain)
}
# cache the results
aggregate_modeled_cases_by_chain <- cache_fun_results("modeled_cases_by_chain", aggregate_modeled_cases_by_chain_no_cache,
                                            overwrite = T, config = config)

#' @name aggregate_modeled_rates_by_chain_no_cache
#' @title aggregate_modeled_rates_by_chain_no_cache
#' @description get the mean of the modeled rates by chain
#' @param config config file that contains the parameter information
#' @param cache the cached environment that contains all the parameter information
#' @return  modeled rates mean for each grid cell by times
aggregate_modeled_rates_by_chain_no_cache <- function(modeled_rates, funs = "mean") {
  modeled_rates_by_chain <- apply(modeled_rates, 2, funs)
  return(modeled_rates_by_chain)
}
# cache the results
aggregate_modeled_rates_by_chain <- cache_fun_results("modeled_rates_by_chain", aggregate_modeled_rates_by_chain_no_cache,
                                                      overwrite = T, config = config)

#' @name aggregate_modeled_cases_by_chain_gridtime_no_cache
#' @description aggregate the modeled cases by chain
#' @param modeled_cases
#' @param funs the function to aggregate the modeled cases
#' @return modeled cases across iterations by chains and grid*time
aggregate_modeled_cases_by_chain_gridtime_no_cache <- function(modeled_cases, funs = "mean"){
  aggregated_modeled_cases_by_chain_gridtime <- apply(modeled_cases, c(2,3), funs)
  return(aggregated_modeled_cases_by_chain_gridtime)
  }
# cache the results
aggregate_modeled_cases_by_chain_gridtime <- cache_fun_results("aggregated_modeled_cases_by_chain_gridtime",aggregate_modeled_cases_by_chain_gridtime_no_cache,
                                                               overwrite=T,config=config)

#' @name aggregate_modeled_rates_by_gridtime_no_cache
#' @description aggregate the modeled rates for each grid cell
#' @param modeled_rates
#' @param funs the function to aggregate the modeled rates
#' @return modeled rates mean across iterations by chains and grid*time
aggregate_modeled_rates_by_chain_gridtime_no_cache <- function(modeled_rates, funs = "mean"){
  aggregated_modeled_rates_by_chain_gridtime <- apply(modeled_rates, c(2,3), funs)
  return(aggregated_modeled_rates_by_chain_gridtime)
}

# cache the results
aggregate_modeled_rates_by_chain_gridtime <- cache_fun_results("aggregated_modeled_rates_by_chain_gridtime",aggregate_modeled_rates_by_chain_gridtime_no_cache,
                                                               overwrite=T,config=config)

#' @name aggregate_modeled_cases_by_gridtime_no_cache
#' @description aggregate the modeled cases by grid*time
#' @param modeled_cases
#' @param funs the function to aggregate the modeled cases
#' @return modeled cases mean across chains and iterations by grid*timee
aggregate_modeled_cases_by_gridtime_no_cache <- function(modeled_cases, funs = "mean"){
  aggregated_modeled_cases_by_gridtime <- apply(modeled_cases, c(3), funs)
  return(aggregated_modeled_cases_by_gridtime)
}

# cache the results
aggregate_modeled_cases_by_gridtime <- cache_fun_results("aggregated_modeled_cases_by_gridtime",aggregate_modeled_cases_by_gridtime_no_cache,
                                                               overwrite=T,config=config)

#' @name aggregate_modeled_rates_by_gridtime_no_cache
#' @description aggregate the modeled rates by grid*time
#' @param modeled_rates
#' @param funs the function to aggregate the modeled rates
#' @return modeled rates mean across chains and iterations by grid*timee
aggregate_modeled_rates_by_gridtime_no_cache <- function(modeled_rates, funs = "mean"){
  aggregated_modeled_rates_by_gridtime <- apply(modeled_rates, c(3), funs)
  return(aggregated_modeled_rates_by_gridtime)
}

# cache the results
aggregate_modeled_rates_by_gridtime <- cache_fun_results("aggregated_modeled_rates_by_gridtime",aggregate_modeled_rates_by_gridtime_no_cache,
                                                         overwrite=T,config=config)

#' @name aggregate_averaged_modeled_cases_by_time_across_country_no_cache
#' @description aggregate the averaged modeled cases across chains and iterations by grid*time over the country
#' @param modeled_cases
#' @param average_funs the function to get the averaged modeled cases across chains and iterations
#' @return modeled cases mean across chains and iterations by grid*timee over the country
aggregate_averaged_modeled_cases_by_time_across_country_no_cache <- function(config, modeled_cases, average_funs = "mean"){
  
  aggregated_modeled_cases_by_gridtime <- apply(modeled_cases, c(3), average_funs)
  analysis_years <- lubridate::year(config$start_time):lubridate::year(config$end_time)

  aggregated_averaged_modeled_cases_by_time_across_country<-
    data.frame(aggregated_modeled_cases_by_gridtime)%>%
    dplyr::mutate(analysis_years = rep(analysis_years,each=length(aggregated_modeled_cases_by_gridtime)/length(analysis_years)))%>%
    dplyr::group_by(analysis_years)%>%
    dplyr::summarise(total_cases_by_year_over_country = round(sum(aggregated_modeled_cases_by_gridtime),0))
  
  return(aggregated_averaged_modeled_cases_by_time_across_country)
}

# cache the results
aggregate_averaged_modeled_cases_by_time_across_country <- cache_fun_results("aggregated_averaged_modeled_cases_by_time_across_country",aggregate_modeled_cases_by_gridtime_no_cache,
                                                         overwrite=T,config=config)

#' @name aggregate_averaged_modeled_rates_by_time_across_country_no_cache
#' @description aggregate the averaged modeled rates across chains and iterations by grid*time over the country
#' @param modeled_cases
#' @param average_funs the function to get the averaged modeled rates across chains and iterations
#' @param aggregate_funs the function to get the aggregated averaged modeled rates over country
#' @return modeled rates mean across chains and iterations by grid*timee over the country
aggregate_averaged_modeled_rates_by_time_across_country_no_cache <- function(config, modeled_rates, average_funs = "mean",aggregate_funs="mean"){
  
  aggregated_modeled_rates_by_gridtime <- apply(modeled_cases, c(3), average_funs)
  analysis_years <- lubridate::year(config$start_time):lubridate::year(config$end_time)
  
  aggregated_averaged_modeled_rates_by_time_across_country<-
    data.frame(aggregated_modeled_rates_by_gridtime)%>%
    dplyr::mutate(analysis_years = rep(analysis_years,each=length(aggregated_modeled_rates_by_gridtime)/length(analysis_years)))%>%
    dplyr::group_by(analysis_years)%>%
    dplyr::summarise_each( aggregate_funs)
  
  return(aggregated_averaged_modeled_rates_by_time_across_country)
}

# cache the results
aggregate_averaged_modeled_rates_by_time_across_country <- cache_fun_results("aggregated_averaged_modeled_rates_by_time_across_country",aggregate_averaged_modeled_rates_by_time_across_country_no_cache,
                                                                             overwrite=T,config=config)
