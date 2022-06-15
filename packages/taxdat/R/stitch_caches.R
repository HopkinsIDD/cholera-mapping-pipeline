#' @export
#' @name stitch_caches
#' @title stitch_caches
#' @description general stitch function that can take different combination function calls 
#' @param name name of the field in the cached output that will be updated 
#' @param output_cache cached output
#' @param input_caches cached configs
#' @param initial_value initial values 
#' @param combination_function specific function than can stitch cached output 
#' @return  cached output 
stitch_caches <- function(
  name, 
  output_cache,
  input_caches,
  initial_value = list(type = "first", value = NULL), #will think more about this (maybe add more options)
  combination_function = function(x,y,...) {return(x + y)}, 
  ...
  ){

  if (!is.null(input_caches) & initial_value$type == "first") {
    output_cache[[paste0(name, "_initialized")]] <- input_caches[[1]][[name]]
    input_caches <- input_caches[-1]
  } else if (!is.null(input_caches) & initial_value$type == "fixed") {
    output_cache[[name]] <- initial_value$value
  }
  
  combination_function(cache = output_cache, input_cache = input_caches, ...) 

  return(invisible())
}
