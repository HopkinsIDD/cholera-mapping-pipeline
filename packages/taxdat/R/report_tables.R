#' @include report_cache.R
#' @include report_data.R

#' @export
#' @name table_observation_summary
#' @title table_observation_summary
#' @description plot the polygon with modeled cases
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return table with observation statistic summary
table_observation_summary <- function(config, cache, cholera_directory) {
  get_observation_data(cache = cache, config = config, cholera_directory = cholera_directory)

  obs_stats <- tibble::as_tibble(cache[["observation_data"]])
  obs_stats <- dplyr::mutate(obs_stats, year = lubridate::year(time_left))
  alldf <- tibble::as_tibble(obs_stats)
  alldf <- dplyr::mutate(alldf, year = "all")
  obs_stats <- rbind(obs_stats, alldf)
  obs_stats <- dplyr::group_by(obs_stats, year)
  obs_stats <- dplyr::summarize(obs_stats,
    n_obs = dplyr::n(),
    n_cases = sum(suspected_cases),
    n_lp = length(unique(location_period_id)),
    u_lps = paste(sort(unique(location_period_id)), collapse = ","),
    n_OCs = length(unique(observation_collection_id)),
    u_OCs = paste(sort(unique(observation_collection_id)), collapse = ",")
  )

  obs_stats %>%
    dplyr::select(-dplyr::contains("u_")) %>%
    dplyr::mutate_if(is.numeric, function(x) {
      format(x, big.mark = ",")
    }) %>%
    dplyr::rename(year = year, `Observations` = n_obs, `Suspected cases` = n_cases, `Location periods` = n_lp, `Observation collections` = n_OCs) %>%
    kableExtra::kable(col.names = c("year", "# observations", "# suspected cases", "# location periods", "# observation collections")) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped")) %>%
    kableExtra::kable_paper(full_width = T) %>%
    kableExtra::row_spec(nrow(obs_stats), bold = T)
}

#' @export
#' @name table_dropped_data
#' @title plot_ObservationSummary_table
#' @description plot the polygon with modeled cases
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return table with observation statistic summary
table_dropped_data <- function(config, cache, cholera_directory) {
  get_observation_data(cache = cache, config = config, cholera_directory = cholera_directory)

  obs_stats <- tibble::as_tibble(cache[["observation_data"]])
  obs_stats <- dplyr::mutate(obs_stats, year = lubridate::year(time_left))
  alldf <- tibble::as_tibble(obs_stats)
  alldf <- dplyr::mutate(alldf, year = "all")
  obs_stats <- rbind(obs_stats, alldf)
  obs_stats <- dplyr::group_by(obs_stats, year)
  obs_stats <- dplyr::summarize(obs_stats,
    n_obs = dplyr::n(),
    n_cases = sum(suspected_cases),
    n_lp = length(unique(location_period_id)),
    u_lps = paste(sort(unique(location_period_id)), collapse = ","),
    n_OCs = length(unique(observation_collection_id)),
    u_OCs = paste(sort(unique(observation_collection_id)), collapse = ",")
  )

  used_obs_stats <- obs_stats[order(obs_stats$year), ]
  # get all obs_stats
  all_obs_stats <- tibble::as_tibble(cache[["observation_data"]])
  all_obs_stats <- dplyr::mutate(all_obs_stats, year = lubridate::year(time_left))
  alldf <- tibble::as_tibble(all_obs_stats)
  alldf <- dplyr::mutate(alldf, year = "all")
  all_obs_stats <- rbind(all_obs_stats, alldf)
  all_obs_stats <- dplyr::group_by(all_obs_stats, year)
  all_obs_stats <- dplyr::summarize(all_obs_stats,
    n_obs = dplyr::n(),
    n_cases = sum(suspected_cases),
    n_lp = length(unique(location_period_id)),
    u_lps = paste(sort(unique(location_period_id)), collapse = ","),
    n_OCs = length(unique(observation_collection_id)),
    u_OCs = paste(sort(unique(observation_collection_id)), collapse = ",")
  )

  all_obs_stats <- all_obs_stats[order(all_obs_stats$year), ]

  dropped_obs_stats <- all_obs_stats %>%
    mutate(n_obs = all_obs_stats$n_obs - used_obs_stats$n_obs) %>%
    mutate(n_cases = all_obs_stats$n_cases - used_obs_stats$n_cases) %>%
    mutate(n_lp = all_obs_stats$n_lp - used_obs_stats$n_lp) %>%
    mutate(n_OCs = all_obs_stats$n_OCs - used_obs_stats$n_OCs)

  for (i in 1:nrow(all_obs_stats)) {
    dropped_obs_stats$u_lps[i] <- paste(sort(unique(setdiff(
      stringr::str_split(all_obs_stats$u_lps[i], ",")[[1]],
      stringr::str_split(used_obs_stats$u_lps[i], ",")[[1]]
    ))), collapse = ",")
    dropped_obs_stats$u_OCs[i] <- paste(sort(unique(setdiff(
      stringr::str_split(all_obs_stats$u_OCs[i], ",")[[1]],
      stringr::str_split(used_obs_stats$u_OCs[i], ",")[[1]]
    ))), collapse = ",")
  }

  dropped_obs_stats %>%
    dplyr::mutate_if(is.numeric, function(x) {
      format(x, big.mark = ",")
    }) %>%
    dplyr::rename(year = year, `Number of dropped observations` = n_obs, `Number of dropped suspected cases` = n_cases, `Number of dropped location periods` = n_lp, `Number of dropped  observation collections` = n_OCs) %>%
    kableExtra::kable(col.names = c("year", "# dropped observations", "# dropped suspected cases", "# dropped location periods", "dropped location periods", "# dropped  observation collections", "dropped observation collections")) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped")) %>%
    kableExtra::kable_paper(full_width = F) %>%
    kableExtra::row_spec(nrow(dropped_obs_stats), bold = T)
}

#' @export
#' @name table_population_by_admin
#' @title table_population_by_admin
#' @description add
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return table
table_population_by_admin <- function(config, cache, cholera_directory) {
  stop("This function is not working since I can't test it")

  ## load pop and sf objects
  get_config(config = config, cache = cache, cholera_directory = cholera_directory)
  get_covar_cube(config = config, cache = cahce, cholera_directory = cholera_directory)
  # get_location_period(config = config, cache = cahce, cholera_directory = cholera_directory)
  ## Move this into the generated quantities part of the code

  ## Use the geo package
  config <- yaml::read_yaml(paste(cholera_directory, config, sep = "/"))
  iso_code <- as.character(stringr::str_extract(config, "[A-Z]{3}"))
  admin_level <- as.numeric(params$admin_level_for_summary_table)
  if (iso_code == "ZNZ" & admin_level == 1) {
    boundary_sf <- rgeoboundaries::gb_adm1("TZA")[rgeoboundaries::gb_adm1("TZA")$shapeName %in% c("Zanzibar South & Central", "Zanzibar North", "Zanzibar Urban/West"), ]
  } else if (iso_code == "ZNZ") {
    stop("Sorry, currently ZNZ only has the admin 1 level shape files available to use, please try again. ")
  } else {
    if (admin_level > 3) {
      stop("Error: the current admin level is unnecessarily high or invalid, please check and change the parameters for the country data report before running again.")
    }
    boundary_sf <- rgeoboundaries::geoboundaries(country = iso_code, adm_lvl = admin_level)
    if (admin_level != 1) {
      warning(paste("The current admin level is set to", admin_level))
    }
  }

  ## For loops and make the table
  admin_pop_table <- data.frame(matrix(NA, length(boundary_sf$shapeName), length(unique(cache[["covar_cube"]][["t"]])) + 1))

  year_list <- lubridate::year(cache[["config"]][["general"]][["start_date"]]):lubridate::year(cache[["config"]][["general"]][["end_date"]])

  if (is.na(start_year)) {
    start_year <- as.numeric(stringr::str_extract(params$config, "[0-9]{1,4}"))
    year_list <- unique(pltdata$t) + (start_year - 1)

    if (!start_year %in% c(2015, 2016, 2017, 2018, 2019)) {
      start_year <- as.numeric(stringr::str_extract(params$config, "\\d+(?=.yml)"))
      year_list <- start_year
    }
  }

  colnames(admin_pop_table) <- c("adminlevel", year_list)
  admin_pop_table$adminlevel <- boundary_sf$shapeName

  pop_raster_data <- raster()
  for (layer in unique(pltdata$t)) {
    empty_raster <- raster::raster(pltdata[which(pltdata$t == layer), ], res = max(0.1666666, 0.1666666)) # 20*20km raster
    pop_raster_data_tmp <- fasterize::fasterize(pltdata[which(pltdata$t == layer), ], empty_raster, field = c("covar"))
    pop_raster_data <- stack(pop_raster_data, pop_raster_data_tmp)
  }

  for (layer in 1:raster::nlayers(pop_raster_data)) {
    for (locs in admin_pop_table$adminlevel) {
      cropped <- raster::crop(pop_raster_data[[layer]], boundary_sf[boundary_sf$shapeName == locs, layer + 1], snap = "out")
      masked <- raster::mask(cropped, boundary_sf[boundary_sf$shapeName == locs, layer + 1], updatevalue = NA)
      sum_pop <- sum(raster::getValues(masked), na.rm = TRUE)
      admin_pop_table[match(locs, admin_pop_table$adminlevel), layer + 1] <- round(sum_pop, 4)
      rm(cropped, masked)
    }
  }

  ### Add a total row, change colnames, and display the table
  total_row <- c("Total", apply(data.frame(admin_pop_table[, -1]), 2, sum))
  admin_pop_table <- rbind(admin_pop_table, total_row)

  admin_pop_table %>%
    dplyr::mutate_if(is.numeric, function(x) {
      format(x, big.mark = ",")
    }) %>%
    kableExtra::kable(col.names = c("Admin Level", year_list)) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped")) %>%
    kableExtra::kable_paper(full_width = F) %>%
    kableExtra::row_spec(nrow(admin_pop_table), bold = T)
}


#' @export
#' @name table_modeled_cases_by_chain_time
#' @title table_modeled_cases_by_chain_time
#' @description plot the polygon with modeled cases
#' @param config config file that contains the parameter information
#' @param cache the cached environment
#' @param cholera_directory  the directory of cholera mapping pipeline folder
#' @return table with modeled cases by time and chain
table_modeled_cases_by_chain_time <- function(config, cache, cholera_directory) {
  aggregate_grid_cases_mean_by_chain(config = config, cache = cache, cholera_directory = cholera_directory)
  get_covar_cube(config = config, cache = cache, cholera_directory = cholera_directory)
  cache[["covar_cube"]] <- cache[["covar_cube"]] %>%
    dplyr::ungroup() %>%
    dplyr::select(t,id)
  cache[["covar_cube"]][,paste("cases", "chain", seq_len(ncol(cache[["grid_cases_mean_by_chain"]])), sep = "_")] <- cache[["grid_cases_mean_by_chain"]]

  get_modeled_years(config = config, cache = cache, cholera_directory = cholera_directory)
  get_analysis_years(config = config, cache = cache, cholera_directory = cholera_directory)
  get_dropped_years(config = config, cache = cache, cholera_directory = cholera_directory)

  if(params$drop_nodata_years  & !all(cache[["modeled_years"]] %in% cache[["analysis_years"]])){
    drop_year_ix <- which(!cache[["modeled_years"]] %in% cache[["analysis_years"]])
    message(paste("Dropping", paste(cache[["modeled_years"]][drop_year_ix], collapse = ", "), "from cases_chains"))
    cache[["covar_cube"]] <- dplyr::filter(cache[["covar_cube"]], !(t %in% drop_year_ix))
  }
  
  sf_grid_wider <- sf::st_drop_geometry(cache[["covar_cube"]])
  
  by_years <- sf_grid_wider %>%
    dplyr::group_by(t) %>%
    dplyr::summarise(dplyr::across(dplyr::contains("cases_chain"), sum)) %>%
    dplyr::mutate(t = as.character(t))
  mai <- by_years %>%
    dplyr::summarise(dplyr::across(dplyr::contains("cases_chain"), mean)) %>%
    dplyr::mutate(t = "mean annual cases")
  
  dplyr::bind_rows(by_years, mai) %>%
    kableExtra::kable(col.names = c("time slice", paste("chain", 1:cache[["config"]][["stan"]][["nchain"]]))) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped"))
}