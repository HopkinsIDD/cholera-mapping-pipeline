#' @description
#' Supress warnings from a function matching a pattern.
#' @param expr The expression to evaluate
#' @param warning_pattern do not emit warnings using this pattern
#' @param ... Additional arguments to grepl when matching the pattern
#' @return The result of expr
#' @export
suppress_warning <- function(expr, warning_pattern, ...) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl(warning_pattern, w$message, ...)) {
      invokeRestart("muffleWarning")
    }
  })
}

#' @description Set error handling via options
#' @param is_interactive Should we use a profile based on an interactive R session, or one suitable for scripts
#' @export
set_error_handling <- function(is_interactive = FALSE) {
  if (is_interactive == "TRUE") {
    options(warn = max(1, options("warn")$warn), error = recover)
  } else if (is_interactive == "FALSE") {
    options(warn = max(1, options("warn")$warn), error = function(...) {
      quit(..., status = 2)
    })
  }
  invisible(NULL)
}

#' @description Update libraries required for taxdat from a list of packages
#' @param perform logical Whether or not to perform the update
#' @param package_list a list of packages (in addition to taxdat) to update
#' @export
update_libraries <- function(perform, package_list) {
  if (perform) {
    base_search <- search()
    for (package in package_list) {
      if (!require(package = package, character.only = T)) {
        utils::chooseCRANmirror(ind = 1)
        install.packages(pkgs = package)
        library(package = package, character.only = T)
      }
    }

    if (!require(taxdat)) {
      install.packages(
        rprojroot::find_root_file(
          criterion = ".choldir", "packages", "taxdat"
        ),
        type = "source",
        repos = NULL
      )
      library(taxdat)
    }
    for (x in rev(sort(which(!(search() %in% base_search))))) {
      suppress_warning(detach(pos = x, force = T), "may no longer work correctly")
    }
  }
}

#' @description Check observation_data to see if it has any errors
#' @param observation_data A tibble of case observations as returned by pull_observation_data (with any changes made during the pipeline)
#' @export
check_data <- function(observation_data) {
  no_errors <- TRUE
  data_df <- as.data.frame(observation_data)
  if (sum(!is.na(data_df$suspected_cases)) == 0) {
    no_errors <- FALSE
    warning("No non-NA cases observed")
  }
  if (!(no_errors)) {
    stop("At least one error which we cannot recover from occured. See above warnings for details")
  }
  invisible(NULL)
}
