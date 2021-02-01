#' @include read_data_helpers.R
#' @include setup_helpers.R

#' @export
#' @name get_filenames
#' @title get_filenames
#' @description add
#' @param config object representing the imported YAML config file for the model
#' @param cholera_directory path to cholera directory
#' @return
get_filenames <- function (config, cholera_directory) {
  data_source <- config$data_source
  countries <- config$countries
  countries_name <- config$countries_name
  res_space <- as.numeric(config$res_space)
  spatial_aggregate_iso_a2_level <- Inf
  res_time <- check_time_res(config$res_time)
  # temporal_aggregate_time_unit <- stringr::str_split(res_time, 
  # " ")[[1]][2]
  time_change_func <- time_unit_to_aggregate_function(res_time)
  aggregate_to_start <- time_unit_to_start_function(res_time)
  aggregate_to_end <- time_unit_to_end_function(res_time)
  smooth_covariate_number_timesteps <- config$smoothing_period
  suspected_or_confirmed <- check_case_definition(config$case_definition)
  cases_column <- case_definition_to_column_name(suspected_or_confirmed, 
                                                 database = T)
  start_time <- lubridate::ymd(config$start_time)
  end_time <- lubridate::ymd(config$end_time)
  covariate_dict <- yaml::read_yaml(paste0(cholera_directory, 
                                           "/Layers/covariate_dictionary.yml"))
  all_covariate_choices <- names(covariate_dict)
  short_covariate_choices <- purrr::map_chr(covariate_dict, 
                                            "abbr")
  covariate_choices <- check_covariate_choices(covar_choices = config$covariate_choices, 
                                               available_choices = all_covariate_choices)
  short_covariates <- short_covariate_choices[covariate_choices]
  ncore <- config$stan$ncores
  nchain <- ncore
  niter <- config$stan$niter
  stan_dir <- paste0(cholera_directory, "/Analysis/Stan/")
  stan_model <- config$stan$model
  stan_model_path <- check_stan_model(stan_model_path = paste(stan_dir, 
                                                              stan_model, sep = ""), stan_dir = stan_dir)
  map_name <-  paste(paste(config$countries_name, collapse = '-'),
                     stringr::str_replace(res_time, " ", "_"),
                     paste(start_time, end_time, sep = '-'),
                     paste(res_space, 'km', sep = ''),
                     suspected_or_confirmed,
                     sep = '_')
  covariate_name_part <- paste(short_covariates, collapse = "-")
  preprocessed_data_fname <- make_observations_filename(cholera_directory, 
                                                        map_name)
  preprocessed_covar_fname <- make_covar_filename(cholera_directory, 
                                                  map_name, covariate_name_part)
  stan_input_fname <- make_stan_input_filename(cholera_directory, 
                                               map_name, covariate_name_part, stan_model, niter)
  stan_output_fname <- make_stan_output_filename(cholera_directory, 
                                                 map_name, covariate_name_part, stan_model, niter)
  rc <- setNames(c(preprocessed_data_fname, preprocessed_covar_fname, 
                   stan_input_fname, stan_output_fname), c("data", "covar", 
                                                           "stan_input", "stan_output"))
  return(rc)
}

#' @export
#' @name read_file_of_type
#' @title read_file_of_type
#' @description Read pipeline files
#' @param filename filename
#' @param variable variable present in file
#' @return
read_file_of_type <- function(filename, variable){
  if(grepl('output.\\d+.csv$',filename)) { # Stan output csv
    model.rand <- rstan::read_stan_csv(filename)
  }
  if(grepl('json$',filename)) { #stan input json
    stan_data <- jsonlite::read_json(filename, simplifyVector=TRUE)
  }
  if(grepl('rdata$',filename)) { # some kind of rdata file
    load(filename)
  }
  if(!exists(variable)){stop(paste0("The variable (",variable,") isn't present in the file", filename, ")"))}
  return(eval(expr = parse(text=variable)))
}

#' @export
#' @name get_obs_stats
#' @title get_obs_stats
#' @description Prepare observation statistics from the preprocessed rdata object
#' @param df sf_cases dataframe with observation data
#' @return dataframe with summary statistics 
get_obs_stats <- function(df) {

  rc <- tibble::as_tibble(df)
  rc <- dplyr::mutate(rc, year = lubridate::year(TL))
  alldf <- tibble::as_tibble(df)
  alldf <- dplyr::mutate(alldf, year = "all")
  rc <- rbind(rc, alldf)
  rc <- dplyr::group_by(rc, year) 
  rc <- dplyr::summarize(rc,
      n_obs = dplyr::n(),
      n_cases = sum(attributes.fields.suspected_cases),
      n_lp  = length(unique(locationPeriod_id)),
      u_lps  = paste(sort(unique(locationPeriod_id)), collapse = ","),
      n_OCs  = length(unique(OC_UID)),
      u_OCs  = paste(sort(unique(OC_UID)), collapse = ","),
    )

  return(rc)
}


#' @export
#' @name get_disjoint_set_sf_cases
#' @title get_disjoint_set_sf_cases
#' @description add
#' @param preprocessed_data_filename Rdata filename with the preprocess suffix
#' @return  
get_disjoint_set_sf_cases <- function(preprocessed_data_filename) {
  sf_cases <- read_file_of_type(preprocessed_data_filename,"sf_cases")
  my_names <- names(sf_cases)[
    c(grep("location", names(sf_cases)), grep("name_", names(sf_cases)))
  ]
  sf_cases$attributes.location_period_id <- sf_cases[[my_names[[1]]]]
  for (i in (1 + seq_len(length(my_names) - 1))) {
    sf_cases$attributes.location_period_id <- paste(
      sf_cases$attributes.location_period_id,
      sf_cases[[my_names[i]]]
    )
  }
  
  aggregate_sf_cases <- dplyr::summarize(
    dplyr::group_by(
      sf_cases,
      attributes.location_period_id
    ),
    cases = mean(attributes.fields.suspected_cases / as.numeric(TR - TL + 1) * 365),
    variance = var(attributes.fields.suspected_cases / as.numeric(TR - TL + 1) * 365),
    observations = length(attributes.fields.suspected_cases)
  )
  aggregate_sf_cases <- sf::st_as_sf(aggregate_sf_cases)
  
  aggregate_sf_cases$area <-
    as.numeric(sf::st_area(aggregate_sf_cases)) / 1000 / 1000
  aggregate_sf_cases$area_adjusted_cases <-
    aggregate_sf_cases$cases / aggregate_sf_cases$area
  
  aggregate_sf_cases <- dplyr::arrange(aggregate_sf_cases, -area)
  overlaps <-
    sf::st_relate(aggregate_sf_cases, aggregate_sf_cases, "2********")
  non_overlapping_sets <- list()
  
  aggregate_sf_cases$not_included <- TRUE
  index <- 0
  while (any(aggregate_sf_cases$not_included)) {
    leftovers <- which(aggregate_sf_cases$not_included)
    index <- index + 1
    non_overlapping_sets[[index]] <- NA
    not_allowed <- NA
    for (idx in leftovers) {
      if (idx %in% not_allowed) {
      } else {
        aggregate_sf_cases$not_included[idx] <- FALSE
        non_overlapping_sets[[index]] <- c(non_overlapping_sets[[index]], idx)
        not_allowed <- sort(unique(c(not_allowed, overlaps[[idx]])))
      }
    }
    non_overlapping_sets[[index]] <-
      non_overlapping_sets[[index]][!is.na(non_overlapping_sets[[index]])]
  }
  
  aggregate_sf_cases$set <- as.integer(NA)
  
  for (set in seq_len(length(non_overlapping_sets))) {
    aggregate_sf_cases$set[non_overlapping_sets[[set]]] <- set
  }
  return(aggregate_sf_cases)
}


#' @export
#' @name plot_raw_observed_cases
#' @title plot_raw_observed_cases
#' @description add
#' @param disjoint_set_sf_cases disjoint set of sf cases object
#' @param render default is FALSE
#' @param plot_file default is NULL
#' @param width plot width
#' @param height plot height
#' @return ggplot object with raw observed cases 
plot_raw_observed_cases <- function(disjoint_set_sf_cases,
                                    render = F,
                                    plot_file = NULL,
                                    width = NULL,
                                    height = NULL){
  plt <- ggplot2::ggplot()
  plt <- plt +
    ggplot2::geom_sf(
      data = disjoint_set_sf_cases,
      ggplot2::aes(fill = cases)
    ) + 
    ggplot2::scale_fill_gradient2("Average cases by location period", low="white",mid="orange",high="red",na.value='blue') +
    ggplot2::theme_bw() + 
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::facet_wrap(~set)
  
  if (!is.null(plot_file)) {
    ggplot2::ggsave(plt, plot_file, width = width , heigth = height)
  }
  if(render){
    plt
  }
}


#' @export
#' @name plot_area_adjusted_observed_cases
#' @title plot_area_adjusted_observed_cases
#' @description add
#' @param disjoint_set_sf_cases disjoint set of sf cases object
#' @param render default is FALSE
#' @param plot_file default is NULL
#' @param width plot width
#' @param height plot height
#' @return ggplot object with area-adjusted observed cases 
plot_area_adjusted_observed_cases <- function(
                        disjoint_set_sf_cases,
                        render = F,
                        plot_file = NULL,
                        width = NULL,
                        height = NULL){
  plt <- ggplot2::ggplot()
  plt <- plt +
    ggplot2::geom_sf(
      data = disjoint_set_sf_cases,
      ggplot2::aes(fill = area_adjusted_cases)
    ) + 
    ggplot2::scale_fill_gradient2("Area-adjusted cases", low="white",mid="orange",high="red",na.value='blue') +
    ggplot2::theme_bw() + 
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::facet_wrap(~set)
  
  if (!is.null(plot_file)) {
    ggplot2::ggsave(plt, plot_file, width = width , heigth = height)
  }
  if(render){
    plt
  }
}


#' @export
#' @name plot_area_adjusted_observed_cases
#' @title plot_area_adjusted_observed_cases
#' @description add
#' @param disjoint_set_sf_cases disjoint set of sf cases object
#' @param render default is FALSE
#' @param plot_file default is NULL
#' @param width plot width
#' @param height plot height
#' @return ggplot object with number of observations observed by unique location periods
plot_raw_observations <- function(disjoint_set_sf_cases,
                                  render = F,
                                  plot_file = NULL,
                                  width = NULL,
                                  height = NULL){
  plt <- ggplot2::ggplot()
  plt <- plt +
    ggplot2::geom_sf(
      data = disjoint_set_sf_cases,
      ggplot2::aes(fill = observations)
    ) + 
    ggplot2::scale_fill_viridis_c("Observation") +
    ggplot2::facet_wrap(~set, ncol = 5) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") 
  
  if (!is.null(plot_file)) {
    ggplot2::ggsave(plt, plot_file, width = width , heigth = height)
  }
  
  if (render) {
    plt
  }
}


#' @export
#' @name plot_raster_population
#' @title plot_raster_population
#' @description add
#' @param covar_data_filename covariates rdata filename
#' @param render default is FALSE
#' @return ggplot object with population raster 
plot_raster_population <- function(covar_data_filename,
                                   render = T) {
  plt <- ggplot2::ggplot()
  
  covar_cube_output <- read_file_of_type(covar_data_filename, "covar_cube_output")
  covar_cube <- covar_cube_output$covar_cube
  sf_grid <- covar_cube_output$sf_grid
  pop_layer <- covar_cube[,,1] ## population is always the first layer
  
  if(nrow(sf_grid) == prod(dim(pop_layer))){
    covar <- data.frame(covar = unlist(lapply(1:ncol(pop_layer), function(x){
      pop_layer[,x]
    })))
    pltdata <- dplyr::bind_cols(sf_grid, covar)
    
    ## plots population for all time points
    plt <- plt +
      ggplot2::geom_sf(
        data = pltdata,
        ggplot2::aes(fill = covar,
                     color = covar)
      ) +
      # ggplot2::scale_fill_continuous("Population") +
      ggplot2::scale_fill_viridis_c("Population",
                                    trans = "log",
                                    breaks = c(1e2, 1e3, 1e4, 1e5),
                                    aesthetics = c("colour", "fill"),
                                    guide = ggplot2::guide_colorbar(title = "Population density [per grid cell]"), 
                                    option = "E", na.value = "white")  +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom",
                     legend.text = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)) +
      ggplot2::facet_wrap(~t, ncol = 5)
    
  } else{
    warning("sf_grid has a different number of cells or timepoints than covar_cube")
  }
  
  if (render) {
    plt
  }
}



#' @export
#' @name plot_raster_covariates
#' @title plot_raster_covariates
#' @description add
#' @param covar_data_filename covariates rdata filename
#' @param render default is FALSE
#' @return ggplot object with covariate raster
plot_raster_covariates <- function(covar_data_filename,
                                   render = T) {
  plt <- ggplot2::ggplot()
  covar_cube_output <- read_file_of_type(covar_data_filename, "covar_cube_output")
  covar_cube <- covar_cube_output$covar_cube
  sf_grid <- covar_cube_output$sf_grid
  covar_layers <- covar_cube[,,-1]
  ncovar <- ifelse(length(dim(covar_layers))==2, 1, dim(covar_layers)[3])
  
  if(nrow(sf_grid) == prod(dim(covar_cube[,,1]))){
    
    covar_df <- purrr::map_dfc(seq_len(ncovar), function(x){
      if(ncovar>1){
        covar_layer <- covar_layers[,,x]
      } else{
        covar_layer <- covar_layers
      }
      unlist(lapply(1:ncol(covar_layers), function(x){
        covar_layer[,x]
      }))
    })
    covar_df <- purrr::set_names(covar_df, dimnames(covar_cube_output$covar_cube)[[3]][-1])
    
    pltdata <- dplyr::bind_cols(sf_grid, covar_df)
    
    ## plot first time point of all covariates for now
    pltdata_dummy <- 
      tidyr::gather(
        dplyr::filter(
          pltdata,
          t == 1
        ),
        one_of(dimnames(covar_cube_output$covar_cube)[[3]]), key = "covars", value = "value"
      )
    
    plt <- plt +
      ggplot2::geom_sf(
        data = pltdata_dummy,
        ggplot2::aes(fill = value, color = value)
      ) +
      ggplot2::scale_fill_viridis_c(aesthetics = c("colour", "fill"),
                                    guide = ggplot2::guide_colorbar(title = "Covariate at time 1"), 
                                    option = "B", 
                                    na.value = "white")  +
      ggplot2::theme_bw() +
      # ggplot2::scale_fill_continuous("Covariate at time 1") +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::facet_wrap(~covars)
    
  } else{
    warning("sf_grid has a different number of cells or timepoints than covar_cube")
  }
  
  if (render) {
    plt
  }
}


#' @export
#' @name get_case_raster
#' @title get_case_raster
#' @description add
#' @param preprocessed_data_filename prepreprocess rdata file name
#' @param covar_data_filename covariates rdata filename
#' @param model_output_filenames model output filenames
#' @return 
get_case_raster <- function(preprocessed_data_filename,
                            covar_data_filename,
                            model_output_filenames
) { 
  sf_cases <- read_file_of_type(preprocessed_data_filename,"sf_cases")
  # layer_index <- 1
  covar_cube_output <- read_file_of_type(covar_data_filename, "covar_cube_output")
  sf_grid <- covar_cube_output$sf_grid
  case_raster <- sf_grid
  test_data <- NULL
  
  nchains <- 0
  non_na_gridcells <- get_non_na_gridcells(covar_data_filename)
  
  for (filename in model_output_filenames) {
    nchains <- nchains + 1
    model.rand <- read_file_of_type(filename,"model.rand")
    # Check if stan ran
    stan_divergence <- rstan::check_divergences(model.rand)
    modeled_cases <- as.array(model.rand)[, , grepl("grid_case", names(model.rand)),drop=FALSE]
    modeled_cases_mean <- apply(modeled_cases, 3, mean)
    modeled_rates <- exp(as.array(model.rand)[, , grepl("log_lambda", names(model.rand)), drop = FALSE])
    modeled_rates_mean <- apply(modeled_rates, 3, mean)
    
    case_raster <- dplyr::mutate(case_raster, 
                    modeled_cases_mean = NA,
                    modeled_rates_mean = NA) 
    case_raster[non_na_gridcells,]$modeled_cases_mean <- modeled_cases_mean
    names(case_raster)[which(names(case_raster)=="modeled_cases_mean")] <- paste("modeled cases\n",
      paste(filename_to_stubs(filename)[2:3], collapse = " "), "\niterations: Chain", 
      filename_to_stubs(filename)[5])
    case_raster[non_na_gridcells,]$modeled_rates_mean <- modeled_rates_mean
    names(case_raster)[which(names(case_raster)=="modeled_rates_mean")] <- paste("modeled rates\n", 
      paste(filename_to_stubs(filename)[2:3], collapse = " "), "\niterations: Chain", 
      filename_to_stubs(filename)[5]) 
  }
  case_raster
}


#' @name get_non_na_gridcells
#' @title get_non_na_gridcells
#' @description add
#' @param covar_data_filename covariates rdata filename
#' @return 
get_non_na_gridcells <- function(covar_data_filename){
  covar_cube_output <- read_file_of_type(covar_data_filename, "covar_cube_output")
  non_na_gridcells <- covar_cube_output$non_na_gridcells
  non_na_gridcells
}


#' @export
#' @name filename_to_stubs
#' @title filename_to_stubs
#' @description Parse filename string to get model run information
#' @param x filename string or vector of strings
#' @return 
filename_to_stubs <- function(x){
  if(length(x)==0){return(x)}
  print(x)
  x <- strsplit(x, "/")
  x <- sapply(
    x,
    function(y) {
      y[[length(y)]]
    }
  )
  x <- strsplit(x, ".", fixed = TRUE)
  x <- sapply(
    x,
    function(y) {
      if (y[[1]] == "testing") {
        y[[1]] <- paste(y[1:2], collapse = ".")
        y <- y[-2]
      }
      y <- y[-4]
      y <- y[-2]
      if(length(y) > 3){y <- y[-length(y)]}
      return(y)
    })
  return(x)
}



#' @export
#' @name plot_modeled_cases
#' @title plot_modeled_cases
#' @description add
#' @param case_raster case_raster object
#' @param render default is TRUE
#' @param plot_file default is NULL
#' @param width plot width
#' @param height plot height
#' @return ggplot object with modeled cases map
plot_modeled_cases <- function(case_raster,
                               render = T,
                               plot_file = NULL,
                               width = NULL,
                               height = NULL){
  case_raster <- case_raster %>%
    dplyr::select(dplyr::contains("modeled cases"),id,t) %>%
    tidyr::gather(dplyr::contains("iterations: Chain"), key = "chain", value = "value") %>%
    # tidyr::pivot_longer(contains("iterations: Chain"), names_to = "chain", values_to = "value") %>%
    dplyr::mutate(chain = stringr::str_replace(chain, "modeled cases", ""))
  
  plt <- ggplot2::ggplot()
  plt <- plt +
    ggplot2::geom_sf(
      data = case_raster,
      ggplot2::aes(fill = value, color =  value)) +
    # ggplot2::scale_fill_vidris_c("modeled cases", limits = uniform_scale_fun()) +
    ggplot2::scale_fill_viridis_c(trans = "log", 
                                  breaks = c(1, 10, 100, 1000),
                                  aesthetics = c("colour", "fill"),
                                  guide = ggplot2::guide_colorbar(title = "Incidence\n [cases/year]"))  +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::facet_wrap(~t,ncol = 5) +
    ggplot2::theme(legend.text =  ggplot2::element_text(angle = 45, vjust = 1, hjust = 1))
  
  if (!is.null(plot_file)) {
    ggplot2::ggsave(plt, plot_file, width = width , heigth = height)
  }
  if(render) {
    plt
  }
}


#' @export
#' @name plot_modeled_rates
#' @title plot_modeled_rates
#' @description add
#' @param case_raster case_raster object
#' @param render default is TRUE
#' @param plot_file default is NULL
#' @param width plot width
#' @param height plot height
#' @return ggplot object with modeled rates map
plot_modeled_rates <- function(case_raster,
                               render = T,
                               plot_file = NULL,
                               width = NULL,
                               height = NULL){
  
  case_raster <- case_raster %>%
    dplyr::select(dplyr::contains("modeled rates"),id,t) %>%
    tidyr::gather(dplyr::contains("iterations: Chain"), key = "chain", value = "value") %>%
    dplyr::mutate(chain = stringr::str_replace(chain, "modeled rates", ""))
  
  rate_rescaling <-  1e4  # rescale to have incidence per 10'000 people
  plt <- ggplot2::ggplot()
  plt <- plt +
    ggplot2::geom_sf(
      data = case_raster,
      ggplot2::aes(fill = value * rate_rescaling, color = value * rate_rescaling)) +
    # ggplot2::scale_fill_continuous("modeled rates", limits = uniform_scale_fun()) +
    ggplot2::scale_fill_viridis_c(trans = "log",
                                  breaks = c(0.01, 0.1, 1, 10, 100, 1000),
                                  aesthetics = c("colour", "fill"),
                                  guide = ggplot2::guide_colorbar(title = "Incidence rate\n [cases/10'000/year]"))  +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::facet_wrap(~t,ncol = 5) +
    ggplot2::theme(legend.text =  ggplot2::element_text(angle = 45, vjust = 1, hjust = 1))
  
  if (!is.null(plot_file)) {
    ggplot2::ggsave(plt, plot_file, width = width , heigth = height)
  }
  if (render) {
    plt  
  }
}


#' @export
#' @name get_data_fidelity
#' @title get_data_fidelity
#' @description add
#' @param model_output_filenames model_output_filenames
#' @return 
get_data_fidelity <- function(model_output_filenames){
  rc <- list()
  layer_index <- 1
  for (filename in model_output_filenames) {
    corresponding_input_filename <- gsub('\\d+.csv','json',gsub("stan_output","stan_input", filename))
    print(c(filename, corresponding_input_filename))
    model.rand <- read_file_of_type(filename, "model.rand")
    stan_data <- read_file_of_type(corresponding_input_filename, "stan_input")$stan_data
    modeled_cases <- as.array(model.rand)[, , grepl("modeled_cases", names(model.rand)), drop = FALSE]
    modeled_cases_chain_mean <- apply(modeled_cases, c(2, 3), mean)
    actual_cases <- matrix(stan_data$y, nrow(modeled_cases_chain_mean), ncol(modeled_cases_chain_mean), byrow=TRUE)
    dimnames(actual_cases) <- dimnames(modeled_cases_chain_mean)
    modeled_cases_chain_mean <- reshape2::melt(modeled_cases_chain_mean)
    actual_cases <- reshape2::melt(actual_cases)
    comparison <- dplyr::left_join(modeled_cases_chain_mean, actual_cases, by = c(chains = "chains", parameters = "parameters"))
    names(comparison)[3:4] <- c("modeled cases", "actual cases")
    rc[[filename]] <- comparison
    names(rc)[[layer_index]] <- paste(
      paste(filename_to_stubs(filename)[2:3], collapse = " "),
      "\niterations: Chain", filename_to_stubs(filename)[5])
    layer_index <- layer_index + 1
  }
  return(rc)
}


#' @export
#' @name plot_model_fidelity
#' @title plot_model_fidelity
#' @description add
#' @param data_fidelity data_fidelity object
#' @param case_raster case_raster object
#' @param render default is TRUE
#' @return ggplot object with modeled vs actual cases by observation
plot_model_fidelity <- function(data_fidelity, 
                                case_raster,
                                render = T){
  comparison <- data_fidelity
  rate_raster <- case_raster
  plt <- ggplot2::ggplot(comparison[[1]]) +
    ggplot2::geom_point(ggplot2::aes(y = `modeled cases`, x = `actual cases`, col = chains)) +
    ggplot2::geom_abline(intercept = 0, slope = 1) +
    ggplot2::coord_fixed(ratio = 1, xlim = c(1, max(comparison[[1]][,3:4])), ylim = c(1, max(comparison[[1]][,3:4]))) +
    ggplot2::theme_bw()
  
  if (render) {
    plt
  }
}

#' @export
#' @name plot_rhat
#' @title plot_rhat
#' @description add
#' @param model.rand Stan model output 
#' @param render default is TRUE
#' @return ggplot object with Rhat by observation id
plot_rhat <- function(model.rand,
                      render = T) {
  
  
  fit_summary <- rstan::summary(model.rand)
  rhats <- tibble::tibble(Rhat = round(fit_summary$summary[which(str_detect(row.names(fit_summary$summary), "modeled_cases")), "Rhat"], 2)) %>% 
    dplyr::mutate(x=dplyr::row_number())
  rhat_thresh <- 1.05
  frac_above <- sum(rhats$Rhat > rhat_thresh)/nrow(rhats)
  p_rhat <- ggplot2::ggplot(rhats, ggplot2::aes(x = x, y = Rhat)) +
    ggplot2::xlab("Obs. ID") +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = rhat_thresh, col = "red") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(glue::glue("Fraction above threshold: {format(round(frac_above*100, 2))}%"))
  
  if (render) {
    p_rhat
  }
}


#' @export
#' @name pull_output_by_source
#' @title pull_output_by_source
#' @description Most of this function is a hack to get the source for sql pulls.  The real fix for this problem is to have the sql pull already have the source.
#' @param sf_cases sf_cases output 
#' @param source_match string of reference source (e.g., WHO Annual Cholera Reports)
#' @return 
pull_output_by_source <- function(sf_cases, 
                                  source_match,
                                  database_api_key_rfile = "../R/database_api_key.R"){
  if(missing(source_match)){
    warning("No source filter provided")
    return(sf_cases)
  }

  if(any(grepl("source",names(sf_cases)))){
    source_match <- paste0("^",gsub('%','.*',source_match))
    matches <- grepl(source_match, sf_cases[["source"]])
    return(sf_cases[matches,])
  }

  source(database_api_key_rfile)
  conn <- RPostgres::dbConnect(RPostgres::Postgres(),
                               host = "db.cholera-taxonomy.middle-distance.com",
                               dbname = "CholeraTaxonomy_production",
                               user = taxonomy_username,
                               password = taxonomy_password,
                               port = "5432")
  
  query <- glue::glue_sql(.con=conn,"select id,source from observation_collections where source like {source_match};")
  source_ids <- DBI::dbGetQuery(conn=conn, query)
  matches <- sf_cases$OC_UID %in% source_ids$id
  return(sf_cases[matches,])
}

#' @export
#' @name plot_chain_convergence
#' @title plot_chain_convergence
#' @description add
#' @param model_output_filenames model output filenames
#' @param pars parameters for which to display traceplots
#' @param render default is TRUE
#' @return ggplot object with traceplots by parameter
plot_chain_convergence <- function(model_output_filenames,
                                   pars = c("rho", "betas", "log_std_dev_w"),
                                   render = T){
  model.rand <- read_file_of_type(model_output_filenames,"model.rand")
  
  if (render) {
    rstan::traceplot(model.rand, pars = pars)
  }
}