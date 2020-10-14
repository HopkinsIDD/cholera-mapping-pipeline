library(tidyverse)
source("Analysis/R/automate_mapping_config_utils.R")

#### Modify the following settings      ############## 
#### to generate one config per country ##############

config_path <- "Analysis/configs"
covar_names <- c("dist_to_water", "water_access")

locs <- read_csv("Analysis/R/locations_todeletelater.csv")

params_df <- data.frame(
    taxonomy_path = "taxonomy-working/working-entry1",
    countries = locs$id,
    countries_name = locs$region, 
    res_space = 20,
    res_time = '1 years',
    smoothing_period = 1,
    case_definition = 'suspected',
    start_time = '2015-01-01',
    end_time = '2019-12-31',
    data_source = 'sql',
    ncores = 4,
    model = 'dagar_seasonal',
    niter = 2000,
    recompile = TRUE
              )
##############################################

for (i in 1:nrow(params_df)){

  row <- params_df[i,]
  start_year <- lubridate::year(as.Date(row$start_time))
  end_year <- lubridate::year(as.Date(row$end_time))
  config_path2 <- paste0(config_path, "/", start_year, "_", end_year)

  config_fname <- paste0(config_path2, "/config_", row$countries_name, "_", start_year, "_", end_year, ".yml")

  sink(file = config_fname)
  automate_mapping_config(row, covar_names)
  sink()

}



