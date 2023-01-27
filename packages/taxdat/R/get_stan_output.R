#' @include plot_cache_function.R

#' @export
#' @name get_genquant_no_cache
#' @description load stan output
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return genquant
get_genquant_no_cache <- function(config, cache, cholera_directory) {
  config <- yaml::read_yaml(paste0(cholera_directory, "/", config))
  file_names <- taxdat::get_filenames(config, cholera_directory)
  genquant <- taxdat::read_file_of_type(file_names[["stan_genquant"]], "chol_gen")
  require(bit64)
  require(sf)
  return(genquant)
}
# cache the results
#' @export
#' @name get_genquant
get_genquant <- cache_fun_results(name = "genquant", fun = get_genquant_no_cache,
                                    overwrite = F, config = config)
#' @export
#' @name get_model_rand_no_cache
#' @description load stan output
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return model.rand
get_model_rand_no_cache <- function(config, cache, cholera_directory) {
  config <- yaml::read_yaml(paste0(cholera_directory, "/", config))
  file_names <- taxdat::get_filenames(config, cholera_directory)
  model.rand <- taxdat::read_file_of_type(file_names[["stan_output"]], "model.rand")
  require(bit64)
  require(sf)
  return(model.rand)
}
# cache the results
#' @export
#' @name get_model_rand
get_model_rand <- cache_fun_results(name = "model.rand", fun = get_model_rand_no_cache,
                                    overwrite = F, config = config)

#' @export
#' @name get_stan_model_niter_per_chain_no_cache
#' @title get_stan_model_niter_per_chain_no_cache
#' @description load stan output based on the config file
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return niter_per_chain
get_stan_model_niter_per_chain_no_cache <- function(config, cache, cholera_directory) {
  get_model_rand(name="model.rand",config=config, cache=cache, cholera_directory=cholera_directory)
  niter_per_chain <- dim(MCMCvis::MCMCchains(cache[["model.rand"]], params='lp__', chain_num=1))[1]
  require(bit64)
  require(sf)
  return(niter_per_chain)
}
#' @export
#' @name get_stan_model_niter_per_chain
get_stan_model_niter_per_chain <- cache_fun_results(name = "niter_per_chain",
                                                    fun = get_stan_model_niter_per_chain_no_cache,
                                                    overwrite = F,
                                                    config=config,
                                                    cholera_directory=cholera_directory)

#' @export
#' @name get_stan_model_nchain_no_cache
#' @title get_stan_model_nchain_no_cache
#' @description load stan input based on the config file
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return nchain
get_stan_model_nchain_no_cache <- function(config, cache, cholera_directory) {
  get_model_rand(name="model.rand",config=config, cache=cache, cholera_directory=cholera_directory)
  nchain <- dim(MCMCvis::MCMCchains(cache[["model.rand"]], params='lp__'))[1] / cache[["niter_per_chain"]]
  
  require(bit64)
  require(sf)
  return(nchain)
}

#' @export
#' @name get_stan_model_nchain
get_stan_model_nchain <- cache_fun_results(name = "nchain", fun = get_stan_model_nchain_no_cache,
                                              overwrite = F,cholera_directory=cholera_directory)


#' @export
#' @name get_stan_output_no_cache
#' @title get_stan_input_no_cache
#' @description load stan input based on the config file
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return stan_output
get_stan_output_no_cache <- function(config, cache, cholera_directory) {
  get_model_rand(name="model.rand",config=config, cache=cache, cholera_directory=cholera_directory)
  stan_output <- lapply(rstan::extract(cache[["model.rand"]]), function(x){array(x,c(cache[["niter_per_chain"]], cache[["nchain"]], dim(x)[-1]))})
  
  require(bit64)
  require(sf)
  return(stan_output)
}
#' @export
#' @name get_stan_output
get_stan_output <- cache_fun_results(name = "stan_output", fun = get_stan_output_no_cache,
                                    overwrite = F,cholera_directory=cholera_directory)