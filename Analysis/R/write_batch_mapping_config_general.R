# install.packages("packages/taxdat", type="source", repos=NULL)
library(readr)
library(taxdat)
#========================================================READ ME========================================================#
# This new general config writer is updated for the general pipeline run in Jan, 2023 
# This new writer with the updated "automate_mapping_config" function in the config_helpers.R 
# All script settings and relevant parameters should be specified to generate the configs needed                                              
#========================================================READ ME========================================================#  



####       Modify the following settings      ############## 
cholera_directory <- "/home/kaiyuezou/mapping_pipeline/tmp_config/cholera-mapping-pipeline" 
setwd(cholera_directory)
config_path <- "Analysis/configs/production_tests/config_writer_test" #<-----dependent on where to save the configs
dir.create(file.path(cholera_directory, config_path), FALSE)

Sys.setenv("CHOLERA_TESTING" = "FALSE")
scale <- "country" ## country or region maps
specified_countries <- c("AGO", "KEN") ## by default empty
# covar_names <- c("dist_to_water", "water_access", "san_access", "open_defe", "stunting", "wasting", "access_cities")
covar_names <- c() ## no covar run 
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
    name = '',  
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
    obs_model = 1, 
    od_param = '', 
    time_effect = 'yes',
    time_effect_autocorr = 'no',
    use_intercept = '', 
    covariate_transformations = '', 
    beta_sigma_scale = 1,
    sigma_eta_scale = 2,
    exp_prior = 'no', 
    do_infer_sd_eta = '', 
    do_zerosum_cnst = '', 
    use_weights = '', 
    covar_warmup = 'yes',
    warmup = 'yes',
    aggregate = 'yes',
    tfrac_thresh = 0,
    censoring = 'yes',
    censoring_thresh = 0.95, 
    set_tfrac = 'yes',
    snap_tol =  '7/365', 
    use_pop_weight = 'yes', 
    sfrac_thresh = '', 
    ingest_covariates = 'no',
    ingest_new_covariates = 'no', 
    ncores = 4,
    model = 'mapping_model_inference', 
    genquant = 'mapping_model_generate', 
    niter = 2000,
    recompile = 'yes', 
    output_directory = '/home/kaiyuezou/mapping_pipeline/tmp_config/cholera-mapping-pipeline/Analysis/data/testing', 
    observations_filename = 'test_preprocess.rdata', 
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

if(scale == "region"){

  regions <- unique(locs$region)
  for (i in 1:length(regions)){
    ctry_names <- locs[which(locs$region == regions[i]),]$country
    ctry_ids <- locs[which(locs$region == regions[i]),]$id
    config_path2 <- paste0(config_path, "/", start_year, "_", end_year, "_region")
    dir.create(config_path2, showWarnings = FALSE)

    config_fname <- paste0(config_path2, "/config_", regions[i], "_", start_year, "_", end_year, ".yml")
    # sink(file = config_fname)
    automate_mapping_config(cholera_directory, par, covar_names, ctry_names, ctry_ids, config_fname)
    # sink()

  }

} else if (scale == "country"){

  for (j in 1:nrow(locs)){
    ctry_name <- locs[j,]$country
    ctry_id <- locs[j,]$id
    config_path2 <- paste0(config_path, "/", start_year, "_", end_year, "_country")
    dir.create(config_path2, showWarnings = FALSE)

    config_fname <- paste0(config_path2, "/config_", ctry_name, "_", start_year, "_", end_year, ".yml")
    # sink(file = config_fname)
    automate_mapping_config(cholera_directory, par, covar_names, ctry_name, ctry_id, config_fname)
    # sink()
  }

} else {
  stop(paste("Writing configs at", scale, "scale is not yet implemented."))
}


