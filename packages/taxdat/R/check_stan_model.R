#' @title Check stan model
#' @description Checks whether the stan model file exists
#'
#' @param stan_model_path the path to the stan model to use
#' @param stan_dir the directory to the directory with all available stan models
#'
#' @return if valid the stan model path
#' @export
check_stan_model <- function(stan_model_path, stan_dir) {
  if (!file.exists(stan_model_path)) {
    stop("Could not find stan model. Choose among:\n", stringr::str_c(dir(stan_dir),
      collapse = "\n"
    ))
  }
  cat("---- Running with stan model:", stringr::str_replace(
    stan_model_path, stan_dir,
    ""
  ), "\n")

  return(stan_model_path)
}
