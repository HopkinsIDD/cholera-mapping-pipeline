#' @include pull_data_helpers.R
#' @include setup_helpers.R
#' @include file_name_functions.R

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
get_disjoint_set_sf_cases <- function(.x = NULL, preprocessed_data_filename = NULL, stan_input_filename = NULL) {
  if (!is.null(.x)) {
    stop("This function only allows named arguments")
  }
  
  if(!is.null(preprocessed_data_filename)){
    sf_cases <- read_file_of_type(preprocessed_data_filename,"sf_cases")
  } 
  if(!is.null(stan_input_filename)){
    sf_cases <- read_file_of_type(stan_input_filename,"stan_input")$sf_cases_resized
  } 
  
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
    taxdat::color_scale(type = "cases", use_case = "ggplot map", use_log = FALSE)+
    ggplot2::labs(fill="Average cases by location period")+
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
    taxdat::color_scale(type = "cases", use_case = "ggplot map", use_log = FALSE)+
    ggplot2::labs(fill="Area-adjusted cases")+
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
  pop_layer <- covar_cube[,,1, drop = F] ## population is always the first layer
  
  if(nrow(sf_grid) == prod(dim(pop_layer))){
    covar <- data.frame(covar = unlist(lapply(1:ncol(pop_layer), function(x){
      pop_layer[, x, 1]
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
  # plt <- ggplot2::ggplot()
  covar_cube_output <- read_file_of_type(covar_data_filename, "covar_cube_output")
  covar_cube <- covar_cube_output$covar_cube
  sf_grid <- covar_cube_output$sf_grid
  covar_layers <- covar_cube[,,-1, drop = F]
  ncovar <- ifelse(length(dim(covar_layers))==2, 1, dim(covar_layers)[3])
  
  if(nrow(sf_grid) == prod(dim(covar_cube[,,1, drop = F]))){
    
    covar_df <- purrr::map_dfc(seq_len(ncovar), function(x){
      if(ncovar>1){
        covar_layer <- cbind(covar_layers[,,x])
      } else {
        covar_layer <- abind::adrop(covar_layers, 3)
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
    
    # plt <- plt +
    #   ggplot2::geom_sf(
    #     data = pltdata_dummy,
    #     ggplot2::aes(fill = value, color = value)
    #   ) +
    #   ggplot2::scale_fill_viridis_c(aesthetics = c("colour", "fill"),
    #                                 guide = ggplot2::guide_colorbar(title = "Covariate at time 1"),
    #                                 option = "B",
    #                                 na.value = "white")  +
    #   ggplot2::theme_bw() +
    #   # ggplot2::scale_fill_continuous("Covariate at time 1") +
    #   ggplot2::theme(legend.position = "bottom") +
    #   ggplot2::facet_wrap(~covars)
    plt<-pltdata_dummy%>%group_by(covars)%>%
      do(gg={ggplot(.,ggplot2::aes(fill = value, color = value)) + ggplot2::geom_sf()+
          ggplot2::facet_wrap(~covars)+ggplot2::scale_fill_viridis_c(aesthetics = c("colour", 
                                                                                    "fill"), guide = ggplot2::guide_colorbar(title = "Covariate at time 1"), 
                                                                     option = "B", na.value = "white") + ggplot2::theme_bw() + 
          ggplot2::theme(legend.position = "bottom",
                         legend.key.size = unit(0.5, 'cm'),
                         legend.title =element_text(size=10))})%>%
      .$gg%>%gridExtra::grid.arrange(grobs=.,nrow=2)
    
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
      ggplot2::aes(fill = value#, color =  value
                   ),color="black",size=0.05) +
    # ggplot2::scale_fill_vidris_c("modeled cases", limits = uniform_scale_fun()) +
    # ggplot2::scale_fill_viridis_c(trans = "log",
    #                               breaks = c(1, 10, 100, 1000),
    #                               aesthetics = c("colour", "fill"),
    #                               guide = ggplot2::guide_colorbar(title = "Incidence\n [cases/year]"))  +
    taxdat::color_scale(type = "cases", use_case = "ggplot map", use_log = TRUE)+
    ggplot2::labs(fill="Incidence\n [cases/year]")+
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
  
  plt <- ggplot2::ggplot()
  plt <- plt +
    ggplot2::geom_sf(
      data = case_raster,
      ggplot2::aes(fill = value ),color="black",size=0.05) +
    taxdat::color_scale(type = "rates", use_case = "ggplot map", use_log = TRUE)+
    ggplot2::labs(fill="Incidence rate\n [cases/10'000/year]")+
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
get_data_fidelity <- function(stan_input_filenames, model_output_filenames){
  
  if (length(stan_input_filenames) != length(model_output_filenames))
    stop("Need to provide same number of stan_input and stan_output files")
  
  rc <- list()
  layer_index <- 1
  for (i in 1:length(model_output_filenames)) {
    i=1
    filename <- model_output_filenames[i]
    # corresponding_input_filename <- gsub('\\d+.csv','json',gsub("stan_output","stan_input", filename))
    # print(c(filename, corresponding_input_filename))
    model.rand <- read_file_of_type(filename, "model.rand")
    nchain <- dim(MCMCvis::MCMCchains(model.rand, params='lp__'))[1] / niter_per_chain
    
    # ####important added -- 11/12/2021
    # taxdat::read_file_of_type(stan_input_filenames[i], "stan_input")$sf_cases_resized$OC_UID
    # taxdat::read_file_of_type(stan_input_filenames[i], "stan_input")$sf_cases_resized$"attributes.fields.suspected_cases"
    # taxdat::read_file_of_type(stan_input_filenames[i], "stan_input")$stan_data$y

    stan_data <- read_file_of_type(stan_input_filenames[i], "stan_input")$stan_data
    modeled_cases <- as.array(model.rand)[, , grepl("modeled_cases", names(model.rand)), drop = FALSE]
    modeled_cases_chain_mean <- apply(modeled_cases, c(2, 3), mean)
    actual_cases <- matrix(stan_data$y, nrow(modeled_cases_chain_mean), ncol(modeled_cases_chain_mean), byrow=TRUE)
    dimnames(actual_cases) <- dimnames(modeled_cases_chain_mean)
    modeled_cases_chain_mean <- reshape2::melt(modeled_cases_chain_mean)
    actual_cases <- reshape2::melt(actual_cases)
    actual_cases$censoring <- rep(stan_data$censoring_inds, each = nchain)
    actual_cases$oc_uid <- rep(taxdat::read_file_of_type(stan_input_filenames[i], "stan_input")$sf_cases_resized$OC_UID, 
                               each = nchain) #newly added
    actual_cases$oc_year <- rep(paste0(format(taxdat::read_file_of_type(stan_input_filenames[i], "stan_input")$sf_cases_resized$TL, '%Y'),
                                       "_",
                                       format(taxdat::read_file_of_type(stan_input_filenames[i], "stan_input")$sf_cases_resized$TR, '%Y')),
                               each = nchain) #newly added

    obs_tfrac=data.frame(
      obs = initial_values_data$stan_data$map_obs_loctime_obs,
      tfrac = initial_values_data$stan_data$tfrac
    ) %>%
      dplyr::group_by(obs) %>%
      dplyr::summarize(tfrac = sum(tfrac))
    actual_cases$tfrac<-1
    actual_cases[stringr::str_detect(actual_cases$parameters,"tfrac"),]$tfrac<-rep(obs_tfrac$tfrac,each=nchain)
    
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
                                render = T,
                                by_censoring = F){
  comparison <- data_fidelity
  rate_raster <- case_raster
  if (!by_censoring) {
    plt <- ggplot2::ggplot(comparison[[1]]  %>% 
                             dplyr::filter(stringr::str_detect(parameters, 'tfrac'))) +
      ggplot2::geom_point(ggplot2::aes(y = `modeled cases`, x = `actual cases`, col = chains)) +
      ggplot2::geom_abline(intercept = 0, slope = 1) +
      ggplot2::coord_fixed(ratio = 1, xlim = c(1, max(comparison[[1]][,3:4])), ylim = c(1, max(comparison[[1]][,3:4]))) +
      ggplot2::theme_bw()
  } else {
    plt <- ggplot2::ggplot(comparison[[1]] %>% 
                             dplyr::filter(!stringr::str_detect(parameters, 'tfrac'))) +
      ggplot2::geom_point(ggplot2::aes(y = `modeled cases`, x = `actual cases`, col = chains)) +
      ggplot2::geom_abline(intercept = 0, slope = 1) +
      ggplot2::coord_fixed(ratio = 1, xlim = c(1, max(comparison[[1]][,3:4])), ylim = c(1, max(comparison[[1]][,3:4]))) +
      ggplot2::themec_bw() +
      ggplot2::facet_wrap(~censoring)
  }

 
  if (render) {
    plt
  }
}

#' @export
#' @name plot_model_fidelity_tfrac_adjusted
#' @title plot_model_fidelity_tfrac_adjusted
#' @description add
#' @param data_fidelity data_fidelity object
#' @param case_raster case_raster object
#' @param render default is TRUE
#' @return ggplot object with modeled vs actual cases by observation
plot_model_fidelity_tfrac_adjusted <- function(data_fidelity,
                                case_raster,
                                render = T){
  comparison <- data_fidelity
  rate_raster <- case_raster
  
  plt <- ggplot2::ggplot(comparison[[1]]  %>% 
                            dplyr::filter(!stringr::str_detect(parameters, 'tfrac'))) +
    ggplot2::geom_point(ggplot2::aes(y = `modeled cases`, x = `actual cases`, col = oc_uid)) +
    ggplot2::labs(x="Actual cases",y="Modeled cases")+
    ggplot2::geom_abline(intercept = 0, slope = 1) +
    ggplot2::coord_fixed(ratio = 1, xlim = c(0, max(comparison[[1]][,3:4])), ylim = c(0, max(comparison[[1]][,3:4]))) +
    ggplot2::theme_bw()
  
  
  if (render) {
    plt
  }
}


#' @export
#' @name plot_model_fidelity_tfrac_converted
#' @title plot_model_fidelity_tfrac_converted
#' @description add
#' @param data_fidelity data_fidelity object
#' @param case_raster case_raster object
#' @param render default is TRUE
#' @return ggplot object with modeled vs actual cases by observation
plot_model_fidelity_tfrac_converted <- function(data_fidelity,
                                               case_raster,
                                               render = T){
  comparison <- data_fidelity
  rate_raster <- case_raster
  
  plt <- ggplot2::ggplot(comparison[[1]]  %>% 
                           dplyr::filter(stringr::str_detect(parameters, 'tfrac'))) +
    ggplot2::geom_point(ggplot2::aes(y = `modeled cases`/tfrac, x = `actual cases`/tfrac, col = oc_uid)) +
    ggplot2::labs(x="Actual cases/tfrac",y="tfrac_modeled_cases/tfrac")+
    ggplot2::geom_abline(intercept = 0, slope = 1) +
    ggplot2::coord_fixed(ratio = 1, xlim = c(0, max(comparison[[1]][,3:4])), ylim = c(0, max(comparison[[1]][,3:4]))) +
    ggplot2::theme_bw()
  

  if (render) {
    plt
  }
}

#' @export
#' @name plot_model_fidelity_tfrac_adjusted_by_year
#' @title plot_model_fidelity_tfrac_adjusted_by_year
#' @description add
#' @param data_fidelity data_fidelity object
#' @param case_raster case_raster object
#' @param render default is TRUE
#' @return ggplot object with modeled vs actual cases by observation
plot_model_fidelity_tfrac_adjusted_by_year <- function(data_fidelity,
                                case_raster,
                                render = T){
  comparison <- data_fidelity
  rate_raster <- case_raster
  
  plt <- ggplot2::ggplot(comparison[[1]]  %>% 
                            dplyr::filter(!stringr::str_detect(parameters, 'tfrac'))) +
    ggplot2::geom_point(ggplot2::aes(y = `modeled cases`, x = `actual cases`, col = oc_uid)) +
    ggplot2::labs(x="Actual cases",y="Modeled cases")+
    ggplot2::geom_abline(intercept = 0, slope = 1) +
    ggplot2::coord_fixed(ratio = 1, xlim = c(0, max(comparison[[1]][,3:4])), ylim = c(0, max(comparison[[1]][,3:4]))) +
    ggplot2::theme_bw() +
    ggplot2::facet_wrap(~oc_year, ncol = 2)
  
  if (render) {
    plt
  }
}

#' @export
#' @name plot_model_fidelity_tfrac_unadjusted
#' @title plot_model_fidelity_tfrac_unadjusted
#' @description add
#' @param data_fidelity data_fidelity object
#' @param case_raster case_raster object
#' @param render default is TRUE
#' @return ggplot object with modeled vs actual cases by observation
plot_model_fidelity_tfrac_unadjusted <- function(data_fidelity,
                                case_raster,
                                render = T){
  comparison <- data_fidelity
  rate_raster <- case_raster

  plt <- ggplot2::ggplot(comparison[[1]] %>% 
                            dplyr::filter(!stringr::str_detect(parameters, 'tfrac'))) +
    ggplot2::geom_point(ggplot2::aes(y = `modeled cases`, x = `actual cases`, col = oc_uid)) +
    ggplot2::labs(x="Actual cases",y="modeled_cases")+
    ggplot2::geom_abline(intercept = 0, slope = 1) +
    ggplot2::coord_fixed(ratio = 1, xlim = c(0, max(comparison[[1]][,3:4])), ylim = c(0, max(comparison[[1]][,3:4]))) +
    ggplot2::theme_bw() +
    ggplot2::facet_wrap(~censoring, ncol = 2)
  
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
  if (is.character(sf_cases$OC_UID[1])) {
    source_ids$id <- as.character(source_ids$id)
  }
  
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


#' @export
#' @name get_spatial_coverage
#' @description Gets the percentage of pixels covered by location-periods at a given spatial scale
#' @param config the configuration file
#' @param cholera_directory the cholera directory where the data is stored
#' @param area_cuts area cut points for spatial scales when all location names are not available (in sqkm)
#' @return a dataframe with the percentage of pixels covered for each modeling
#' time band and each spatial scale
get_spatial_coverage <- function(config,
                                 cholera_directory,
                                 area_cuts = c(0, 1e2, 1e3, 1e4, Inf)) {
  # Get stan input and covar cube
  file_names <- get_filenames(config, cholera_directory)
  stan_input <- read_file_of_type(file_names["stan_input"], "stan_input")
  sf_cases <- stan_input$sf_cases_resized #read_file_of_type(file_names["data"], "sf_cases")
  sf_cases$location_name <- NA
  
  covar_cube_output <- read_file_of_type(file_names["covar"], "covar_cube_output")
  
  # Get unique location periods
  u_lps <- sf_cases %>% 
    dplyr::group_by(locationPeriod_id) %>% 
    dplyr::slice(1) %>% 
    dplyr::select(locationPeriod_id, location_name)
  
  # Get taxonomy database credentials
  source(paste0(cholera_directory, "/Analysis/R/database_api_key.R"))
  
  if (!exists("taxonomy_username")) {
    taxonomy_username <- NULL
    taxonomy_password <- NULL
  }
  
  if (!any(is.na(sf_cases$location_name)) | !is.null(taxonomy_username)) {
    
    # Get location names if some are NAs
    if (any(is.na(sf_cases$location_name))) {
      
      # Connect to database
      conn <- RPostgres::dbConnect(RPostgres::Postgres(),
                                   host = "db.cholera-taxonomy.middle-distance.com",
                                   dbname = "CholeraTaxonomy_production",
                                   user = taxonomy_username,
                                   password = taxonomy_password,
                                   port = "5432")
      
      locations <- DBI::dbGetQuery(
        conn = conn,  
        glue::glue_sql(
          "SELECT a.id::text as \"locationPeriod_id\", b.qualified_name as location_name
          FROM location_periods a
          JOIN locations b
          ON a.location_id = b.id
          WHERE a.id IN ({u_lps$locationPeriod_id*});", .con = conn))
      
      u_lps <- u_lps %>% 
        dplyr::select(-location_name) %>% 
        dplyr::left_join(locations)
    }
    
    # Classify areas by spatial scale based on the LP name (area_class = 0 indicates country-level observations)
    u_lps <- u_lps %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(area = sf::st_area(geom),
                    area_class = stringr::str_count(location_name, "::") - 1,
                    area_class = as.character(area_class),
                    area_class = dplyr::case_when(area_class == "0" ~ "country",
                                                  T ~ area_class))
    
  } else {
    
    # Country location name
    country <- unique(sf_cases$location_name) %>% 
      stringr::str_subset("^[A-Z]{3}::[A-Z]{3}$")
    
    u_lps <- u_lps %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(area = sf::st_area(geom),
                    area_class = cut(as.numeric(area) * 1e-6, area_cuts),
                    area_class2 = dplyr::case_when(location_name == country ~ "country",
                                                   T ~ as.character(area_class)),
                    area_class = factor(area_class2, levels = c(levels(area_class), "country"))) %>% 
      dplyr::select(-area_class2)
  }
  
  # The number of pixels for a given time band
  tot_n_pix <- stan_input$stan_data$smooth_grid_N
  
  lp_input <- tibble::tibble(
    obs = stan_input$stan_data$map_obs_loctime_obs,
    lp = stan_input$stan_data$u_loctime[stan_input$stan_data$map_obs_loctime_loc]
  ) %>% 
    dplyr::inner_join(covar_cube_output$location_periods_dict, by = c("lp" = "loctime_id")) %>% 
    dplyr::inner_join(u_lps, by = c("location_period_id" = "locationPeriod_id")) %>% 
    dplyr::group_by(t, area_class) %>% 
    summarise(n_pix = length(unique(upd_long_id)))
  
  lp_input <- lp_input %>% 
    dplyr::mutate(coverage = n_pix/tot_n_pix)
  
  return(lp_input)
}

#' @export
#' @name get_gam_values
#' @description gets the predicted GAM rate and case incidence values that are used 
#' for initializing the model
#' @param config the configuration file
#' @param cholera_directory the cholera directory where the data is stored
#' @return a dataframe based on sf_grid with a column for the log predictions of
#' cases (log_y) and of rates (log_lambda)
get_gam_values <- function(config,
                           cholera_directory) {
  
  # Get stan input and covar cube
  file_names <- get_filenames(config, cholera_directory)
  stan_input <- read_file_of_type(file_names["stan_input"], "stan_input")
  initial_values_data <- read_file_of_type(file_names["initial_values"], "initial_values_data")

  coord_frame <- tibble::as_tibble(sf::st_coordinates(stan_input$sf_grid)) %>% 
    dplyr::group_by(L2) %>% 
    dplyr::summarise(x = mean(X), 
                     y = mean(Y))
  
  # Create matrix of time
  year_df <- tibble::tibble(year = stan_input$stan_data$map_grid_time)
  year_df$year <- factor(year_df$year)
  
  ## one random effect per year
  if (length(unique(year_df$year)) == 1) {
    mat_grid_time <- matrix(1, nrow(year_df))
  } else {
    mat_grid_time <- model.matrix(as.formula("~ year - 1"), data = year_df)
  }
  
  predict_df <- tibble::tibble(sx = coord_frame$x,
                               sy = coord_frame$y) %>% 
    # Set all years to 0 to get the reference year
    cbind(mat_grid_time %>% 
            tibble::as_tibble() %>%
            magrittr::set_colnames(paste0("year_", 1:ncol(mat_grid_time)))) %>% 
    # Extract the covariates
    cbind(stan_input$stan_data$covar %>% 
            matrix(ncol = stan_input$stan_data$ncovar) %>% 
            magrittr::set_colnames(paste0("beta_", 1:stan_input$stan_data$ncovar))) %>% 
    tibble::as_tibble() %>% 
    mutate(logpop = log(stan_input$stan_data$pop),
           logoffset = logpop + log(stan_input$stan_data$meanrate))
  
  # Predict log(lambda) for the reference year with covariates
  log_y_pred_mean <- mgcv::predict.gam(initial_values_data$gam_fit_output, predict_df)
  
  y_pred_df <- stan_input$sf_grid %>% 
    dplyr::mutate(log_y = log_y_pred_mean + predict_df$logoffset,
                  y = exp(y),
                  log_lambda = log_y_pred_mean + log(stan_input$stan_data$meanrate),
                  lambda = exp(log_lambda),
                  sx=predict_df$sx,
                  sy=predict_df$sy)
  
  return(y_pred_df)
}
