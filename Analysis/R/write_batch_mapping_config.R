
# install.packages("packages/taxdat", type="source", repos=NULL)
library(readr)

#### Modify the following settings      ############## 
#### to generate one config per country ##############

scale <- "region" ## country or region maps
config_path <- "Analysis/configs"
covar_names <- c("dist_to_water", "water_access", "san_access", "open_defe", "stunting", "wasting", "access_cities")

ids <- read_csv("Analysis/R/locations_todeletelater.csv") # location ids
cw <- read_csv("Analysis/R/region_country.csv")
locs <- dplyr::right_join(cw, ids, by = c("country" = "region")) ## region & country grouping

## rm later
locs <- dplyr::filter(locs, !is.na(region))

params_df <- data.frame(
    aoi = "raw",
    res_space = 20,
    res_time = '1 years',
    smoothing_period = 1,
    case_definition = 'suspected',
    start_time = '2015-01-01',
    end_time = '2019-12-31',
    data_source = 'sql',
    ingest_covariates = 'yes',
    covar_warmup = 'yes',
    censoring = 'no',
    aggregate = 'yes',
    time_effect = 'yes',
    time_effect_autocorr = 'no',
    beta_sigma_scale = 1.0,
    ncores = 4,
    model = 'update_yearly_dagar_timevary_speedup_flexible',
    niter = 2000,
    recompile = TRUE
              )
##############################################
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
    sink(file = config_fname)
    taxdat::automate_mapping_config(par, covar_names, ctry_names, ctry_ids)
    sink()

  }

} else if (scale == "country"){

  for (j in 1:nrow(locs)){
    ctry_name <- locs[j,]$country
    ctry_id <- locs[j,]$id
    config_path2 <- paste0(config_path, "/", start_year, "_", end_year, "_country")
    dir.create(config_path2, showWarnings = FALSE)

    config_fname <- paste0(config_path2, "/config_", ctry_name, "_", start_year, "_", end_year, ".yml")
    sink(file = config_fname)
    taxdat::automate_mapping_config(par, covar_names, ctry_name, ctry_id)
    sink()
  }

} else {
  stop(paste("Writing configs at", scale, "scale is not yet implemented."))
}






