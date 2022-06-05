#* The general stitch function *#
#' @export
#' @name stitch_caches
#' @title stitch_caches
#' @description general stitch function that can take different combination function calls 
#' @param output_cache cached output
#' @param name name of the field in the cached output that will be updated 
#' @param input_caches cached configs
#' @param initial_value initial values 
#' @param combination_function specific function than can stitch cached output 
#' @param combination_function_name 
#' @return  cached output 
stitch_caches <- function(
  output_cache,
  name,
  input_caches,
  initial_value = list(type = "first", value = NULL), #will think more about this (maybe add more options)
  combination_function = function(x,y,...) {return(x + y)},
  combination_function_name, 
  ...
  ){
  if (!is.null(input_caches) & initial_value$type == "first") {
    output_cache[[paste0(name, "_input")]] <- input_caches[[1]]
    input_caches <- input_caches[-1]
  } else if (!is.null(input_caches) & initial_value$type == "fixed") {
    output_cache[[name]] <- initial_value$value
  }
  
  combination_function(name = name, fun = eval(parse(text = paste0(combination_function_name, "_no_cache"))), cache = output_cache, ...) 
#   combination_function(name = name, fun, cache = output_cache, ...) 
  # combination_function(name = name, fun = eval(parse(paste0(deparse(substitute(combination_function)), "_no_cache"))), cache = output_cache, ...) 
#   combination_function(name = name, output_cache = output_cache, ...) 
  
  return(invisible())
}
