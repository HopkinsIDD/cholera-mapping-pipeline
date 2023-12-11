# This script post-processes results from a set of country runs


# Preamble ----------------------------------------------------------------

library(tidyverse)
library(sf)
library(lubridate)
library(rstan)
library(cmdstanr)
library(optparse)
library(foreach)
library(rmapshaper)
library(taxdat)

# User-supplied options
opt_list <- list(
  make_option(c("-d", "--config_dir"), 
              default = "./Analysis/cholera-configs/postprocessing_test_2016_2020/",
              action ="store", type = "character", help = "Directory"),
  make_option(opt_str = c("-r", "--redo"), type = "logical",
              default = T, help = "redo final outputs"),
  make_option(opt_str = c("-i", "--redo_interm"), type = "logical",
              default = F, help = "redo intermediate"),
  make_option(opt_str = c("-j", "--redo_auxilliary"), type = "logical",
              default = T, help = "redo auxilliary files"),
  make_option(opt_str = c("-v", "--verbose"), type = "logical",
              default = T, help = "Print statements"),
  make_option(opt_str = c("-p", "--prefix"), type = "character",
              default = NULL, help = "Prefix to use in output file names"),
  make_option(opt_str = c("-s", "--suffix"), type = "character",
              default = NULL, help = "Suffix to use in output file names"),
  make_option(opt_str = c("-e", "--error_handling"), type = "character",
              default = "stop", help = "Error handling"),
  make_option(opt_str = c("-x", "--data_dir"), type = "character",
              default = "./cholera-mapping-output-1/", help = "Directory with all data"),
  make_option(opt_str = c("-y", "--interm_dir"), type = "character",
              default = "./Analysis/output/interm/", help = "Intermediate outputs directory"),
  make_option(opt_str = c("-o", "--output_dir"), type = "character",
              default = "./Analysis/output/processed_outputs/", help = "Output directory"),
  make_option(opt_str = c("-c", "--cholera_dir"), type = "character",
              default = "cholera-mapping-pipeline", help = "Cholera mapping pipeline directory"),
  make_option(opt_str = c("-d", "--n_draws"), type = "numeric",
              default = 4000, help = "Number of draws to save from rate/cases grids")
)

opt <- parse_args(OptionParser(option_list = opt_list))


# Create directories if they don't exist
purrr::walk(c("interm_dir", "output_dir"), function(x) {
  if (!dir.exists(opt[[x]])){
    dir.create(opt[[x]])
  }
})

if (!dir.exists(opt$data_dir)) {
  stop("Data directory ", opt$data_dir, " does not exist")
}

suffix <- opt$config_dir %>% 
  # Remove tailing / to ensure non-empty string
  stringr::str_remove("/$") %>% 
  stringr::str_split("/") %>% 
  .[[1]] %>% 
  last()

if (!is.null(opt$suffix)) {
  suffix <- paste(suffix, opt$suffix, sep = "_")
}


# A. Shapefiles --------------------------------------------------------------

# All the country-level shapefiles for overlay
all_country_sf <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_adm0_sf,
  fun_name = "adm0_sf",
  fun_opts = NULL,
  postprocess_fun = tidy_shapefiles,
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose) 

opt$redo_auxilliary <- FALSE

# All the data shapfiles for spatial coverage
all_shapefiles <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_lp_shapefiles,
  fun_name = "shapefiles",
  fun_opts = NULL,
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose) 


# B. Number of observations --------------------------------------------------

# All the observation counts
all_obs_counts <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_lp_obs_counts,
  fun_name = "obs_counts",
  fun_opts = NULL,
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose) 

# All the observation counts
all_obs <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_observations,
  fun_name = "obs",
  fun_opts = NULL,
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose) 


# C. Mean annual incidence ---------------------------------------------------

# Get the total number of cases
overall_stats <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_adm0_cases,
  fun_name = "mai_cases_all",
  fun_opts = NULL,
  postprocess_fun = aggregate_and_summarise_draws,
  postprocess_fun_opts = list(col = "country_cases"),
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)


# Get the total number of simulated observed cases
overall_sim_stats <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_mai_adm0_simulated_cases,
  fun_name = "mai_simulated_cases_all",
  fun_opts = NULL,
  postprocess_fun = aggregate_and_summarise_draws,
  postprocess_fun_opts = list(col = "sim_country_cases"),
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)

# Get the number of cases by WHO region
mai_region_case_stats <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_adm0_cases,
  fun_name = "mai_cases_by_region",
  fun_opts = NULL,
  postprocess_fun = aggregate_and_summarise_draws_by_region,
  postprocess_fun_opts = list(col = "country_cases"),
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)

# Country-level cases by admin level
mean_cases <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_mean_annual_cases,
  fun_name = "mai_cases_adm",
  fun_opts = NULL,
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)


# D. Mean annual incidence rates ---------------------------------------------

# Get the total number of cases
overall_rate_stats <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_adm0_rates,
  fun_name = "mai_rates_all",
  fun_opts = NULL,
  postprocess_fun = aggregate_and_summarise_draws,
  postprocess_fun_opts = list(col = "country_rates",
                              weights_col = "country_pop"),
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)

mai_region_rates_stats <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_adm0_rates,
  fun_name = "mai_rates_by_region",
  fun_opts = NULL,
  postprocess_fun = aggregate_and_summarise_draws_by_region,
  postprocess_fun_opts = list(col = "country_rates",
                              weights_col = "country_pop"),
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)


# Get the MAI summary at all admin levels 
mai_stats <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_mean_annual_incidence,
  fun_name = "mai",
  fun_opts = NULL,
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)

# Get draws to compute ratio posterior quantiles
mai_draws <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_mean_annual_incidence_draws,
  fun_name = "mai_draws",
  fun_opts = NULL,
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)


# E. Coefficient of variation ------------------------------------------------

# Get the coefficient of variation summary at all admin levels 
cov_stats <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_coef_of_variation,
  fun_name = "cov",
  fun_opts = NULL,
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)


# F. Grid-level cases and rates ----------------------------------------------

# Get the MAI rates summary at space grid level 
mai_grid_rates_stats <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_grid_mai_rates,
  fun_name = "mai_grid_rates",
  postprocess_fun = collapse_grid,
  fun_opts = NULL,
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)

# Get the MAI rates draws at space grid level 
mai_grid_rates_draws <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_grid_mai_rates_draws,
  fun_name = "mai_grid_rates_draws",
  postprocess_fun = collapse_grid,
  postprocess_fun_opts = list(by_draw = TRUE),
  fun_opts = list(filter_draws = 1:opt$n_draws),
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)


# Get the MAI cases summary at space grid level 
mai_grid_cases_stats <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_grid_mai_cases,
  postprocess_fun = collapse_grid,
  fun_name = "mai_grid_cases",
  post_process_fun = collapse_grid,
  fun_opts = NULL,
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)


# Get the MAI cases draws at space grid level 
mai_grid_cases_draws <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_grid_mai_cases_draws,
  fun_name = "mai_grid_cases_draws",
  postprocess_fun = collapse_grid,
  postprocess_fun_opts = list(by_draw = TRUE),
  fun_opts = list(filter_draws = 1:opt$n_draws),
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)


# G. Risk categories ---------------------------------------------------------

# Get the risk category by location at all admin levels 
risk_categories <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_risk_category,
  fun_name = "risk_categories",
  fun_opts = list(cum_prob_thresh = 0.95),
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)

# Get the population at risk in each risk category by country 
pop_at_risk <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_pop_at_risk,
  fun_name = "pop_at_risk",
  fun_opts = NULL,
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)

# Get the population at risk overall
pop_at_risk_all <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_pop_at_risk_draws,
  fun_name = "pop_at_risk_all",
  fun_opts = NULL,
  postprocess_fun = aggregate_and_summarise_draws,
  postprocess_fun_opts = list(col = "tot_pop_risk",
                              grouping_variables = c("admin_level", "risk_cat")),
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)

# Get the population at risk by WHO region
pop_at_risk_regions <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_pop_at_risk_draws,
  fun_name = "pop_at_risk_by_region",
  fun_opts = NULL,
  postprocess_fun = aggregate_and_summarise_draws_by_region,
  postprocess_fun_opts = list(col = "tot_pop_risk",
                              grouping_variables = c("admin_level", "risk_cat")),
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)

# Get the total number of people living in high risk areas (> 1/1'000)
high_risk_pop <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_pop_at_high_risk,
  fun_name = "pop_high_risk",
  fun_opts = NULL,
  postprocess_fun = aggregate_and_summarise_draws,
  postprocess_fun_opts = list(col = "pop_high_risk"),
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)


# Get the number of cases by WHO region
high_risk_pop_regions <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_pop_at_high_risk,
  fun_name = "pop_high_risk_by_region",
  fun_opts = NULL,
  postprocess_fun = aggregate_and_summarise_draws_by_region,
  postprocess_fun_opts = list(col = "pop_high_risk"),
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)

# H. Generated observations --------------------------------------------------

# Get the generated observations to plot posterior retrodictive checks
gen_obs <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_gen_obs,
  fun_name = "gen_obs",
  fun_opts = NULL,
  prefix = opt$prefix,
  suffix = opt$suffix,
  error_handling = opt$error_handling,
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  redo_aux = opt$redo_auxilliary,
  output_dir = opt$output_dir,
  interm_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)
