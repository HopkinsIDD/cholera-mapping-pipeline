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
  if(grepl('.rds$', filename)) { # Stan generated quantities
    chol_gen <- readRDS(filename)
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
#' @name update_filename_oldruns
#' @title update_filename_oldruns
#' @description Change filenames for old runs
#' @param filename filename
#' @param old_runs if the report is for old runs (before 2021-12)
#' @return
update_filename_oldruns <- function(filename,old_runs=FALSE){
  if(old_runs){
    filename[["stan_input"]]<-stringr::str_remove(filename[["stan_input"]],"allOCs_")
    filename[["stan_output"]]<-stringr::str_remove(filename[["stan_output"]],"allOCs_")
    filename[["stan_output"]]<-stringr::str_remove(filename[["stan_output"]],"-csF-teT-teaF-weT-F")
    filename[["stan_output"]]<-stringr::str_remove(filename[["stan_output"]],"-F")
    filename[["covar"]]<-stringr::str_remove(filename[["covar"]],"allOCs_")
    filename[["data"]]<-stringr::str_remove(filename[["data"]],"allOCs_")
  }
  return(filename)
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
#' @name plot_disaggregated_modeled_cases
#' @title plot_disaggregated_modeled_cases
#' @description add
#' @param case_raster case_raster object
#' @param disaggregated_case_sf disaggregated case raster object
#' @param render default is TRUE
#' @param plot_file default is NULL
#' @param width plot width
#' @param height plot height
#' @return ggplot object with modeled cases map
plot_disaggregated_modeled_cases <- function(case_raster,
                                             disaggregated_case_sf,
                                             render = T,
                                             plot_file = NULL,
                                             width = NULL,
                                             height = NULL){
  plt_case_raster <- disaggregated_case_sf %>%
    dplyr::select(dplyr::contains("modeled cases"),id,t) %>%
    tidyr::gather(dplyr::contains("iterations: Chain"), key = "chain", value = "value") %>%
    # tidyr::pivot_longer(contains("iterations: Chain"), names_to = "chain", values_to = "value") %>%
    dplyr::mutate(chain = stringr::str_replace(chain, "modeled cases", ""))
  
  plt_2020_case_raster <- case_raster %>%
    dplyr::select(dplyr::contains("modeled cases"),id,t) %>%
    tidyr::gather(dplyr::contains("iterations: Chain"), key = "chain", value = "value") %>%
    dplyr::mutate(chain = stringr::str_replace(chain, "modeled cases", ""))
  
  plt_case_raster <- plt_case_raster %>%subset(plt_case_raster$t%in%unique(plt_2020_case_raster$t))%>%mutate(value=ifelse(value<exp(-5),exp(-5),value))
  
  plt <- ggplot2::ggplot()
  plt <-   ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = plt_case_raster,
      ggplot2::aes(fill = value),color=NA,size=0.00001)+
    taxdat::color_scale(type = "cases", use_case = "ggplot map", use_log = TRUE)+
    ggplot2::geom_sf(data=plt_2020_case_raster%>%mutate(value=NA),fill=NA,color="black",size=0.05)+
    ggplot2::labs(fill="Incidence\n [cases/year]")+
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::theme(legend.text =  ggplot2::element_text(angle = 45, vjust = 1, hjust = 1))+
    ggplot2::facet_wrap(~t,ncol = length(unique(plt_case_raster$t))) 
  
  if (!is.null(plot_file)) {
    ggplot2::ggsave(plt, plot_file, width = width , heigth = height)
  }
  if(render) {
    plt
  }
}


#' @export
#' @name plot_disaggregated_modeled_cases_time_varying
#' @title plot_disaggregated_modeled_cases_time_varying
#' @description add
#' @param config_file the actual config file 
#' @param disaggregated_case_sf disaggregated case raster object
#' @param country_iso the iso code of the country
#' @param add_shp_from_stan_input whether to add the country-level shape file from the stan input 
#' @param render default is TRUE
#' @param plot_file default is NULL
#' @param width plot width
#' @param height plot height
#' @return ggplot object with modeled cases map
plot_disaggregated_modeled_cases_time_varying <- function(config_file, 
                                                          disaggregated_case_sf,
                                                          add_shp_from_stan_input = FALSE, 
                                                          render = T,
                                                          plot_file = NULL,
                                                          width = NULL,
                                                          height = NULL, 
                                                          ...){
  iso_code <- taxdat::get_country_isocode(config_file) 

  if(any(iso_code == "ZNZ")){
    boundary_sf <- rgeoboundaries::gb_adm1("TZA")[rgeoboundaries::gb_adm1("TZA")$shapeName %in% 
      c("Zanzibar South & Central", "Zanzibar North", "Zanzibar Urban/West", "North Pemba", "South Pemba"), ]
    unionized <- sf::st_union(boundary_sf)
    boundary_sf <- boundary_sf[1, ]
    sf::st_geometry(boundary_sf) <- unionized
    boundary_sf$shapeName <- "Zanzibar"
    boundary_sf$shapeType <- "ADM0"
  }else{
    boundary_sf <- rgeoboundaries::gb_adm0(iso_code)
  }

# Get the country-level shape file from the stan input and plot it against the country-level shape file in the modeled case figure
if(add_shp_from_stan_input){
  shp_from_stan_input <- get_country_shp_from_stan_input(cache=cache, config=params$config, cholera_directory=params$cholera_directory, ...)
}

plt <- ggplot2::ggplot()
plt <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = disaggregated_case_sf,
    ggplot2::aes(fill = value),color=NA,size=0.00001)+
  taxdat::color_scale(type = "cases", use_case = "ggplot map", use_log = TRUE)+
  ggplot2::geom_sf(data=boundary_sf,fill=NA,color="black",size=0.05)+
  {if(add_shp_from_stan_input) ggplot2::geom_sf(data=shp_from_stan_input,fill=NA,color="red",size=0.05)} +
  ggplot2::labs(fill="Incidence\n [cases/year]")+
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::theme(legend.text =  ggplot2::element_text(angle = 45, vjust = 1, hjust = 1))+
  ggplot2::facet_wrap(~t,ncol = length(unique(disaggregated_case_sf$t))) 

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
#' @name plot_modeled_rates_time_varying
#' @title plot_modeled_rates_time_varying
#' @description add
#' @param config_file the actual config file 
#' @param disaggregated_rate_sf disaggregated_rate_sf object
#' @param add_shp_from_stan_input whether to add the country-level shape file from the stan input 
#' @param render default is TRUE
#' @param plot_file default is NULL
#' @param width plot width
#' @param height plot height
#' @return ggplot object with modeled rates map
plot_modeled_rates_time_varying <- function(config_file, 
                                            disaggregated_rate_sf,
                                            add_shp_from_stan_input = FALSE, 
                                            render = T,
                                            plot_file = NULL,
                                            width = NULL,
                                            height = NULL, 
                                            ...){
  iso_code <- taxdat::get_country_isocode(config_file)

  if(any(iso_code == "ZNZ")){
    boundary_sf <- rgeoboundaries::gb_adm1("TZA")[rgeoboundaries::gb_adm1("TZA")$shapeName %in% 
      c("Zanzibar South & Central", "Zanzibar North", "Zanzibar Urban/West", "North Pemba", "South Pemba"), ]
    unionized <- sf::st_union(boundary_sf)
    boundary_sf <- boundary_sf[1, ]
    sf::st_geometry(boundary_sf) <- unionized
    boundary_sf$shapeName <- "Zanzibar"
    boundary_sf$shapeType <- "ADM0"
  }else{
    boundary_sf <- rgeoboundaries::gb_adm0(iso_code)
  }

  # Get the country-level shape file from the stan input and plot it against the country-level shape file in the modeled case figure
  if(add_shp_from_stan_input){
    shp_from_stan_input <- get_country_shp_from_stan_input(cache=cache, config=params$config, cholera_directory=params$cholera_directory, ...)
  }
  
   plt <- ggplot2::ggplot()
   plt <- ggplot2::ggplot() +
   ggplot2::geom_sf(
    data = disaggregated_rate_sf,
    ggplot2::aes(fill = value),color=NA,size=0.00001)+
    taxdat::color_scale(type = "rates", use_case = "ggplot map", use_log = TRUE)+
    ggplot2::geom_sf(data=boundary_sf,fill=NA,color="black",size=0.05)+
    {if(add_shp_from_stan_input) ggplot2::geom_sf(data=shp_from_stan_input,fill=NA,color="red",size=0.05)} +
    ggplot2::labs(fill="Incidence rate\n [cases/10'000/year]")+
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
  ggplot2::theme(legend.text =  ggplot2::element_text(angle = 45, vjust = 1, hjust = 1))+
  ggplot2::facet_wrap(~t,ncol = length(unique(disaggregated_rate_sf$t)))

  if (!is.null(plot_file)) {
    ggplot2::ggsave(plt, plot_file, width = width , heigth = height)
  }
  if (render) {
    plt
  }
}


#' @export
#' @name plot_modeled_rates_by_gridtime_chain
#' @title plot_modeled_rates_by_gridtime_chain
#' @description add
#' @param config the actual config file
#' @param cholera_directory the cholera directory 
#' @param render default is TRUE
#' @return ggplot object with modeled rates map

plot_modeled_rates_by_gridtime_chain<-function(cache,config,cholera_directory, render = T){
  config_file <- yaml::read_yaml(paste0(cholera_directory,"/",config))
  get_modeled_rates(cache=cache,config=params$config,cholera_directory=params$cholera_directory,name="modeled_rates")
  cache[["modeled_rates_by_gridtime_chain"]] <- aggregate_modeled_rates_by_chain_gridtime_no_cache(cache[["modeled_rates"]])
  get_sf_grid(name="sf_grid",config = config,cache=cache,cholera_directory = cholera_directory)
  non_na_gridcells <- get_non_na_gridcells(cache=cache,config=config,cholera_directory = cholera_directory)
  case_raster_non_na<-cache[["sf_grid"]]%>%subset(long_id%in%non_na_gridcells)
  cache[["rate_raster_by_gridtime_chain"]] <- data.frame()
  for(chain_idx in 1:nrow(cache[["modeled_rates_by_gridtime_chain"]])){
    case_raster_non_na$mean<-cache[["modeled_rates_by_gridtime_chain"]][chain_idx,]
    case_raster_non_na$chain<-paste0("Chain ",chain_idx)
    cache[["rate_raster_by_gridtime_chain"]]<-rbind(cache[["rate_raster_by_gridtime_chain"]],case_raster_non_na)
  }
  cache[["rate_raster_by_gridtime_chain"]]$t <- cache[["rate_raster_by_gridtime_chain"]]$t-1+lubridate::year(config_file$start_time)
  plot <- ggplot2::ggplot() + 
    ggplot2::geom_sf(data = cache[["rate_raster_by_gridtime_chain"]],ggplot2::aes(fill = mean), lwd = 0) + 
    taxdat::color_scale(type = "rates",use_case = "ggplot map") + taxdat::map_theme() + ggplot2::facet_wrap(~t+chain,ncol=4)
  
    if (render) {
    plot
  }

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
                             dplyr::filter(stringr::str_detect(variable, 'tfrac'))) +
      ggplot2::geom_point(ggplot2::aes(y = `modeled cases`, x = `actual cases`, col = chain)) +
      ggplot2::geom_abline(intercept = 0, slope = 1) +
      ggplot2::coord_fixed(ratio = 1, xlim = c(1, max(comparison[[1]][,3:4])), ylim = c(1, max(comparison[[1]][,3:4]))) +
      ggplot2::theme_bw()
  } else {
    plt <- ggplot2::ggplot(comparison[[1]] %>% 
                             dplyr::filter(!stringr::str_detect(variable, 'tfrac'))) +
      ggplot2::geom_point(ggplot2::aes(y = `modeled cases`, x = `actual cases`, col = chain)) +
      ggplot2::geom_abline(intercept = 0, slope = 1) +
      ggplot2::coord_fixed(ratio = 1, xlim = c(1, max(comparison[[1]][,3:4])), ylim = c(1, max(comparison[[1]][,3:4]))) +
      ggplot2::theme_bw() +
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
#' @param cache 
#' @param cholera_directory
#' @param config
#' @param render default is TRUE
#' @return ggplot object with modeled vs actual cases by observation
plot_model_fidelity_tfrac_adjusted <- function(cache,cholera_directory,config,
                                              render = T){
  cache[["data_fidelity"]]<-get_data_fidelity(cache=cache,cholera_directory=cholera_directory,config=config)
  comparison <-  cache[["data_fidelity"]]
  cache[["case_raster"]] <-get_case_raster(cache=cache,config=config,cholera_directory=cholera_directory)
  rate_raster <- cache[["case_raster"]]
  
  plt <- ggplot2::ggplot(comparison %>% 
                           dplyr::filter(!stringr::str_detect(censoring, 'tfrac'))) +
    ggplot2::geom_point(ggplot2::aes(y = `modeled cases`, x = `actual cases`, col = oc_uid)) +
    ggplot2::labs(x="Actual cases",y="Modeled cases")+
    ggplot2::geom_abline(intercept = 0, slope = 1) +
    ggplot2::coord_fixed(ratio = 1, xlim = c(0, max(comparison[,3:4])), ylim = c(0, max(comparison[,3:4]))) +
    ggplot2::theme_bw()
  
  
  if (render) {
    plt
  }
}


#' @export
#' @name plot_model_fidelity_tfrac_converted
#' @title plot_model_fidelity_tfrac_converted
#' @description add
#' @param cache 
#' @param cholera_directory
#' @param config
#' @param render default is TRUE
#' @return ggplot object with modeled vs actual cases by observation
plot_model_fidelity_tfrac_converted <- function(cache,cholera_directory,config,
                                               render = T){
  cache[["data_fidelity"]]<-get_data_fidelity(cache=cache,cholera_directory=cholera_directory,config=config)
  comparison <-  cache[["data_fidelity"]]
  cache[["case_raster"]] <-get_case_raster(cache=cache,config=config,cholera_directory=cholera_directory)
  rate_raster <- cache[["case_raster"]]
  
  plt <- ggplot2::ggplot(comparison %>% 
                           dplyr::filter(stringr::str_detect(censoring, 'tfrac'))) +
    ggplot2::geom_point(ggplot2::aes(y = `modeled cases`/tfrac, x = `actual cases`/tfrac, col = oc_uid)) +
    ggplot2::labs(x="Actual cases/tfrac",y="tfrac_modeled_cases/tfrac")+
    ggplot2::geom_abline(intercept = 0, slope = 1) +
    ggplot2::coord_fixed(ratio = 1, xlim = c(0, max(comparison[,3:4])), ylim = c(0, max(comparison[,3:4]))) +
    ggplot2::theme_bw()
  
  
  if (render) {
    plt
  }
}

#' @export
#' @name plot_model_fidelity_tfrac_adjusted_by_year
#' @title plot_model_fidelity_tfrac_adjusted_by_year
#' @description add
#' @param cache 
#' @param cholera_directory
#' @param config
#' @param render default is TRUE
#' @return ggplot object with modeled vs actual cases by observation
plot_model_fidelity_tfrac_adjusted_by_year <- function(cache,cholera_directory,config,
                                                      render = T){
  cache[["data_fidelity"]]<-get_data_fidelity(cache=cache,cholera_directory=cholera_directory,config=config)
  comparison <-  cache[["data_fidelity"]]
  cache[["case_raster"]] <-get_case_raster(cache=cache,config=config,cholera_directory=cholera_directory)
  rate_raster <- cache[["case_raster"]]
  
  
  plt <- ggplot2::ggplot(comparison  %>% 
                           dplyr::filter(!stringr::str_detect(censoring, 'tfrac'))) +
    ggplot2::geom_point(ggplot2::aes(y = `modeled cases`, x = `actual cases`, col = oc_uid)) +
    ggplot2::labs(x="Actual cases",y="Modeled cases")+
    ggplot2::geom_abline(intercept = 0, slope = 1) +
    ggplot2::coord_fixed(ratio = 1, xlim = c(0, max(comparison[,3:4])), ylim = c(0, max(comparison[,3:4]))) +
    ggplot2::theme_bw() +
    ggplot2::facet_grid(oc_year~censoring)
  
  if (render) {
    plt
  }
}

#' @export
#' @name plot_model_fidelity_tfrac_unadjusted
#' @title plot_model_fidelity_tfrac_unadjusted
#' @description add
#' @param cache 
#' @param cholera_directory
#' @param config
#' @param render default is TRUE
#' @return ggplot object with modeled vs actual cases by observation
plot_model_fidelity_tfrac_unadjusted <- function(cache,cholera_directory,config,
                                                 render = T){
  cache[["data_fidelity"]]<-get_data_fidelity(cache=cache,cholera_directory=cholera_directory,config=config)
  comparison <-  cache[["data_fidelity"]]
  cache[["case_raster"]] <-get_case_raster(cache=cache,config=config,cholera_directory=cholera_directory)
  rate_raster <- cache[["case_raster"]]
  
  plt <- ggplot2::ggplot(comparison%>% 
                           dplyr::filter(!stringr::str_detect(censoring, 'tfrac'))) +
    ggplot2::geom_point(ggplot2::aes(y = `modeled cases`, x = `actual cases`, col = oc_uid)) +
    ggplot2::labs(x="Actual cases",y="modeled_cases")+
    ggplot2::geom_abline(intercept = 0, slope = 1) +
    ggplot2::coord_fixed(ratio = 1, xlim = c(0, max(comparison[,3:4])), ylim = c(0, max(comparison[,3:4]))) +
    ggplot2::theme_bw() +
    ggplot2::facet_wrap(~censoring, ncol = 2)
  
  if (render) {
    plt
  }
}

#' @export
#' @name plot_model_fidelity_by_chain
#' @title plot_model_fidelity_by_chain
#' @description add
#' @param cache 
#' @param cholera_directory
#' @param config
#' @param render default is TRUE
#' @return ggplot object with modeled vs actual cases by chain
plot_model_fidelity_by_chain <- function(cache,cholera_directory,config,
                                         render = T){
  cache[["data_fidelity"]]<-get_data_fidelity(cache=cache,cholera_directory=cholera_directory,config=config)
  data_fidelity <-  cache[["data_fidelity"]]
  
  data_fidelity$chain=as.factor(data_fidelity$chain)
  if(!is.null(data_fidelity)){
    plt <- ggplot2::ggplot(data_fidelity) +
      ggplot2::geom_point(ggplot2::aes(y = `modeled cases`, x = `actual cases`, col = chain)) +
      ggplot2::geom_abline(intercept = 0, slope = 1) +
      ggplot2::coord_fixed(ratio = 1, xlim = c(0, max(data_fidelity[,3:4])), ylim = c(0, max(data_fidelity[,3:4]))) +
      ggplot2::theme_bw()
    plt
  } else{
    warning("data_fidelity is NULL")
  }
  
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
  stan_input <- cache[["stan_input"]]
  sf_cases <- stan_input$sf_cases_resized
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
