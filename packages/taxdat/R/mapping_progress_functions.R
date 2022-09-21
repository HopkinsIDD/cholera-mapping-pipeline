#' @include file_name_functions.R

#' @title create_mapping_progress_table
#' @name create_mapping_progress_table
#' @description From a list of configs (or directories containing only configs), create a table displaying the files which exist for those configs, detailing which are present and which are absent
#' @param config_path A filename or directory containing configs which should be used.  This function accepts multiple config_paths in this argument, and the results are concatenated by row.
#' @return a data.frame with rows based on configs, and columns based on different stages of progress, with TRUE if the stage has been completed for that config, and FALSE if it has not
#' @export
create_mapping_progress_table <- function(config_path, pipeline_path) {

  ## Turn directories into more files
  if (any(dir.exists(config_path))) {
    dir_indices <- dir.exists(config_path)
    config_dirs <- config_path[dir_indices]
    config_files <- list.files(config_dirs, full.names = TRUE, recursive = TRUE)
    config_path <- c(config_path[!dir_indices], config_files)
  }

  ## bind together the results for each config
  do.call(
    what = rbind,
    lapply(
      config_path,
      create_mapping_progress_table_single_config,
      pipeline_path = pipeline_path
    )
  )
}

create_mapping_progress_table_single_config <- function(config_path, pipeline_path) {
  if (!file.exists(config_path)) {
    stop("Config does not exist")
  }
  config <- yaml::read_yaml(config_path)
  associated_filenames <- get_filenames(config, pipeline_path)
  dplyr::tibble(
    stage = names(associated_filenames),
    filename = associated_filenames
  ) %>%
    dplyr::mutate(finished = file.exists(filename)) %>%
    dplyr::select(-filename) %>%
    tidyr::pivot_wider(names_from = stage, values_from = finished) %>%
    dplyr::mutate(config_path = config_path) %>%
    return()
}

### create_mapping_progress_table(
###   c("Analysis/configs/2010_2016_full_base", "Analysis/configs/2015_2019_full_base"),
###   "~/git/cholera-mapping-pipeline"
### ) %>%
###   group_by(config_path) %>%
###   summarize(stages_complete = ifelse(
###     data,
###     ifelse(covar,
###       ifelse(stan_input,
###         ifelse(stan_output,
###           "Output Produced",
###           "Input Produced"
###         ), "Covariates constructed"
###       ),
###       "Data pulled"
###     ),
###     "None"
###   )) %>%
###   tidyr::separate(config_path,
###     c(NA, NA, "start_year","end_year", NA, NA, "country", NA) # This is file path specific
###   ) %>%
###   ggplot() +
###   geom_tile(aes(x=paste(start_year, end_year, sep = '-'), y=country, fill=stages_complete))
