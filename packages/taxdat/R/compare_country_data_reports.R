stitch_configs <- function(output_cache, input_caches){
  stitch_caches(output_cache, 
                name = "configs", 
                input_caches, 
                combination_function = stack_configs_as_in_lists, 
                combination_function_name = "stack_configs_as_in_lists")
}

stack_configs_as_in_lists_no_cache <- function(name = "configs",
                                              output_cache,
                                              ...){
  config1 <- yaml::read_yaml(output_cache[[paste0(name, "_input")]][[1]])
  config2 <- yaml::read_yaml(output_cache[[paste0(name, "_input")]][[2]])
  output_cache[[name]]$config1 <- config1
  output_cache[[name]]$config2 <- config2
  output_cache[[paste0(name, "_input")]] <- NULL
  
}
#' @export
#' @name stitch_configs
stack_configs_as_in_lists <- cache_fun_results_new(name = "configs", fun = stack_configs_as_in_lists_no_cache, cache = output_cache, overwrite = T)

stitch_GAM_input <- function(output_cache, input_caches, ...){
  stitch_caches(output_cache, 
                name = "initial_values_data", 
                input_caches, 
                combination_function = stack_GAM_input, 
                combination_function_name = "stack_GAM_input", 
                ...)
}

stack_GAM_input_no_cache <- function(name = "initial_values_data", 
                                    output_cache,
                                    cholera_directory){
  config1 <- output_cache[[paste0(name, "_input")]][[1]]
  config2 <- output_cache[[paste0(name, "_input")]][[2]]

  taxdat::get_initial_values(name="initial_values_data1",cache=output_cache,config = config1,cholera_directory = cholera_directory)
  taxdat::get_initial_values(name="initial_values_data2",cache=output_cache,config = config2,cholera_directory = cholera_directory)
  output_cache[[name]]$initial_values_data1 <- output_cache$initial_values_data1
  output_cache[[name]]$initial_values_data2 <- output_cache$initial_values_data2

  output_cache[[paste0(name, "_input")]] <- NULL
  rm("initial_values_data1", envir = output_cache)
  rm("initial_values_data2", envir = output_cache)
  
}
#' @export
#' @name stitch_GAMs
stack_GAM_input <- cache_fun_results_new(name = "initial_values_data", fun = stack_GAM_input_no_cache, cache = output_cache, overwrite = T)



# stitch_GAMs(output_cache = comparison_cache, input_caches = list(list(params$config1, params$config2)))
# taxdat::get_initial_values(name="initial_values_data",cache=comparison_cache,config = params$config1,cholera_directory = params$cholera_directory)
# taxdat::get_initial_values(name="initial_values_data1",cache=comparison_cache,config = params$config1,cholera_directory = params$cholera_directory)
# taxdat::get_initial_values(name="initial_values_data2",cache=comparison_cache,config = params$config1,cholera_directory = params$cholera_directory)
