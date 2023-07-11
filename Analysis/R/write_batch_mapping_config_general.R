# install.packages("packages/taxdat", type="source", repos=NULL)
library(readr)
library(taxdat)
#========================================================READ ME========================================================#
# This new general config writer is updated for the general pipeline run in Jan, 2023
# This new writer with the updated "automate_mapping_config" function in the config_helpers.R
# All script settings and relevant parameters should be specified to generate the configs needed
#========================================================READ ME========================================================#



####       Modify the following settings      ##############
cholera_directory <- "."
setwd(cholera_directory)
config_path <- "Analysis/configs/..." #<-----dependent on where to save the configs
dir.create(file.path(cholera_directory, config_path), FALSE)

Sys.setenv("CHOLERA_TESTING" = "FALSE")
scale <- "country" ## country or region maps
specified_countries <- c("SDN") #<-----by default NULL if running all countries
# covar_names <- c("dist_to_water", "water_access", "san_access", "open_defe", "stunting", "wasting", "access_cities")
# ^-----refer to "Layers/covariate_dictionary.yml" for covariate names if running any covariates other than the population
covar_names <- c() #<-----if running without using any covariates other than population
Sys.setenv("CHOLERA_START_TIME" = "2016-01-01")
Sys.setenv("CHOLERA_END_TIME" = "2020-12-31")


ids <- read_csv("Analysis/R/locations_todeletelater.csv") # location ids
cw <- read_csv("Analysis/R/region_country.csv")
locs <- dplyr::right_join(cw, ids, by = c("country" = "region")) ## region & country grouping


## Change the parameter values before generating
locs <- dplyr::filter(locs, !is.na(region))
if(!is.null(specified_countries)){
  locs <- locs[locs$country %in% specified_countries, ]
}

config_start_time <- Sys.getenv("CHOLERA_START_TIME","2016-01-01")
config_end_time <- Sys.getenv("CHOLERA_END_TIME","2020-12-31")

params_df <- data.frame(
    name = 'prd-2023',
    aoi = 'raw',
    res_space = 20,
    res_time = '1 years',
    grid_rand_effects_N = 1,
    case_definition = 'suspected',
    start_time = config_start_time,
    end_time = config_end_time,
    data_source = 'sql',
    ovrt_metadata_table = 'no', 
    OCs = '', 
    taxonomy = '', 
    summary_admin_levels = '[0,1,2]',
    obs_model = 3, 
    inv_od_sd_adm0 = 0.01,
    inv_od_sd_nopool = 1,
    h_mu_sd_inv_od = 0.01,
    h_sd_sd_inv_od = 0.05,
    spatial_effect = 'yes',
    mu_sd_w = 10,
    sd_sd_w = 3,
    ncpus_parallel_prep = 2,
    do_parallel_prep = 'yes',
    time_effect = 'yes',
    time_effect_autocorr = 'no',
    use_intercept = 'no',
    covariate_transformations = '',
    beta_sigma_scale = 1,
    sigma_eta_scale = 1,
    mu_alpha = 0,
    sd_alpha = 1,
    exp_prior = 'no',
    do_infer_sd_eta = 0,
    do_zerosum_cnst = 1,
    use_weights = 'no',
    covar_warmup = 'yes',
    warmup = 'yes',
    aggregate = 'yes',
    tfrac_thresh = 0,
    drop_multiyear_adm0 = 'no',
    drop_censored_adm0 = 'yes',
    drop_censored_adm0_thresh = 2,
    censoring = 'yes',
    censoring_thresh = 0.65,
    set_tfrac = 'yes',
    snap_tol =  '7/365',
    use_pop_weight = 'yes',
    sfrac_thresh_border = '0.1',
    sfrac_thresh_conn = '0.05',
    ingest_covariates = 'no',
    ingest_new_covariates = 'no',
    ncores = 4,
    model = 'mapping_model_inference',
    genquant = 'mapping_model_generate',
    iter_warmup = 1100,
    iter_sampling = 1000,
    recompile = 'yes',
    output_directory = '',
    observations_filename = '',
    covariate_filename = '',
    stan_input_filename = '',
    initial_values_filename = '',
    stan_output_filename = '',
    stan_genquant_filename = '',
    country_data_report_filename = '',
    data_comparison_report_filename = ''
    )



###################### Generating Configs ########################
rm(config_start_time, config_end_time)
par <- params_df[1,]
start_year <- lubridate::year(as.Date(par$start_time))
end_year <- lubridate::year(as.Date(par$end_time))
run_name <- par$name

if(scale == "region"){

  regions <- unique(locs$region)
  for (i in 1:length(regions)){
    ctry_names <- locs[which(locs$region == regions[i]),]$country
    ctry_ids <- locs[which(locs$region == regions[i]),]$id
    if(nrow(locs) == 1){OC_list <- params_df$OCs}else{OC_list <- NULL} #if one country, multiple OCs will be allowed
    config_path2 <- paste0(config_path)
    dir.create(config_path2, showWarnings = FALSE)

    config_fname <- paste0(config_path2, "/config_", run_name, "_", regions[i], "_", start_year, "_", end_year, ".yml")
    # sink(file = config_fname)
    automate_mapping_config(cholera_directory, par, OC_list, covar_names, ctry_names, ctry_ids, config_fname)
    # sink()

  }

} else if (scale == "country"){

  for (j in 1:nrow(locs)){
    ctry_name <- locs[j,]$country
    ctry_id <- locs[j,]$id
    if(nrow(locs) == 1){OC_list <- params_df$OCs}else{OC_list <- NULL}
    config_path2 <- paste0(config_path)
    dir.create(config_path2, showWarnings = FALSE)

    config_fname <- paste0(config_path2, "/config_", run_name, "_", ctry_name, "_", start_year, "_", end_year, ".yml")
    # sink(file = config_fname)
    automate_mapping_config(cholera_directory, par, OC_list, covar_names, ctry_name, ctry_id, config_fname)
    # sink()
  }

} else {
  stop(paste("Writing configs at", scale, "scale is not yet implemented."))
}

