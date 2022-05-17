function_cache <- new.env() #should these two be here all the time? 
cache <- new.env()
# cache results
#' @name cache_fun_results
#' @title cache_fun_results
#' @description to cache the output of the function
#' @param name the name of the output that is going to be cached
#' @param fun the function that will generate the output
#' @param overwrite whether to overwrite the current environment
#' @return
cache_fun_results <- function(name, fun, ..., overwrite = FALSE,cholera_directory) {
    if ((!overwrite) && (name %in% names(function_cache))) {
        stop("Please only cache a single function with the same name")
    }
    function_cache[[name]] <- TRUE
    return(function(name,config, cache,cholera_directory, ...) {
        if (name %in% names(cache)) {
            return(invisible())
        }
        cache[[name]] <- fun(config, cache, cholera_directory,...)
        return(invisible())
    })
}
