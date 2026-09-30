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
              default = "./Analysis/cholera-configs/postprocessing_test_2011_2015/",
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
  make_option(opt_str = c("-n", "--n_draws"), type = "numeric",
              default = 10, help = "Number of draws to save from rate/cases grids"),
  make_option(opt_str = c("-w", "--scaling_input_dir"), type = "character",
              default = "./Analysis/scaling_input/", help = "Directory with case burden scaling inputs"),
  make_option(opt_str = c("-k", "--case_filter_draws"), type = "numeric",
              default = 1000, help = paste("Number of draws to keep per country in",
                                           "postprocess_admin_cases_draws(), set to the smallest",
                                           "draw count produced across country",
                                           "configs' Stan fits"))
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


mai_region_case_draws <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_adm0_cases,
  fun_name = "mai_cases_by_region_draws",
  fun_opts = NULL,
  postprocess_fun = aggregate_and_summarise_draws_by_region,
  postprocess_fun_opts = list(col = "country_cases",
                              do_summary = FALSE),
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

# Country-level cases by year
mean_adm0_cases_by_time <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_annual_adm0_cases,
  fun_name = "mai_adm0_cases_by_time",
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
  verbose = opt$verbose
)

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

overall_rate_draws <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_adm0_rates,
  fun_name = "mai_rates_all_draws",
  fun_opts = NULL,
  postprocess_fun = aggregate_and_summarise_draws,
  postprocess_fun_opts = list(col = "country_rates",
                              weights_col = "country_pop",
                              do_summary = FALSE),
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


mai_region_rates_draws <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_adm0_rates,
  fun_name = "mai_rates_by_region_draws",
  fun_opts = NULL,
  postprocess_fun = aggregate_and_summarise_draws_by_region,
  postprocess_fun_opts = list(col = "country_rates",
                              weights_col = "country_pop",
                              do_summary = FALSE),
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
  fun_opts = list(filter_draws = opt$n_draws),
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
  fun_opts = list(filter_draws = opt$n_draws),
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
risk_categories_95 <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_risk_category,
  fun_name = "risk_categories_95",
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


# Get the risk category by location at all admin levels
risk_categories_50 <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_risk_category,
  fun_name = "risk_categories_50",
  fun_opts = list(cum_prob_thresh = 0.50),
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

# Get the population at risk draws by WHO region
pop_at_risk_regions_draws <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_pop_at_risk_draws,
  fun_name = "pop_at_risk_by_region_draws",
  fun_opts = NULL,
  postprocess_fun = aggregate_and_summarise_draws_by_region,
  postprocess_fun_opts = list(col = "tot_pop_risk",
                              grouping_variables = c("admin_level", "risk_cat"),
                              do_summary = FALSE),
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



# I. Population -----------------------------------------------------------


# Country-level population
pop <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_mean_population,
  fun_name = "population",
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


# J. Over dispersion parameter ----

# Get the summary of overdispersion parameters by admin level
od_stat <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_od_param,
  fun_name = "od_param",
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


# K. Case burden adjustment (cCh), this is where the scaling pipeline starts

# Admin-unit-level case draws (ADM0 and all subnational levels),
# unaggregated, the input for the case burden adjustment pipeline.
# Country-level scaling factors below (positivity, p_u5, care-seeking) are
# joined on "country" only, so combine_draws() applies each country's
# factors to every admin unit of that country; reporting_ratio is a single
# global scalar applied to every row with no join at all (see
# get_config_reporting_ratio())
mai_admin_case_draws <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_admin_cases_draws,
  fun_name = "mai_cases_by_admin_unit_draws",
  fun_opts = list(filter_draws = opt$case_filter_draws),
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

base_case_draws <- mai_admin_case_draws
n_draws <- dplyr::n_distinct(base_case_draws$.draw)

sid <- opt$scaling_input_dir

# Country-specific severity-by-age-class scalars (proportion of cases that
# are moderate-to-severe, among under-5s and among 5-and-overs): two
# GLOBAL scalars (NOT per-country, see get_config_severity_scalars()),
# applied identically to every country
severity_scalars <- get_config_severity_scalars(opt$config_dir)

# Reporting-completeness ratio (ratio of reported sCh to all
# medically-attended sCh): a single global scalar, applied to every country and admin unit
reporting_ratio <- get_config_reporting_ratio(opt$config_dir)

# Test positivity and care-seeking posteriors, global (from systematic
# review), not country-specific, so these have no "country" column;
# combine_draws() applies a shared .draw value to every country.
positivity_draws <- readr::read_csv(file.path(sid, "prop_pos_adj_est.csv"), show_col_types = FALSE) %>%
  dplyr::transmute(positivity = x) %>%
  align_draws_to_reference(n_draws)
care_seek_mild <- readr::read_csv(file.path(sid, "prop_sought_general_est.csv"), show_col_types = FALSE) %>%
  dplyr::transmute(care_seeking = x) %>%
  align_draws_to_reference(n_draws)
care_seek_severe <- readr::read_csv(file.path(sid, "prop_sought_severe_est.csv"), show_col_types = FALSE) %>%
  dplyr::transmute(care_seeking = x) %>%
  align_draws_to_reference(n_draws)

# Country lookup + country-level under-5 proportion posteriors, from the
# case age-distribution model (fit separately by fit_cholera_age_model.R)
country_lookup <- readr::read_csv(file.path(sid, "country_lookup.csv"))
country_id_vec <- readr::read_csv(file.path(sid, "country_id_vec.csv"))$country_id

# Countries with case draws from the main pipeline but no
# age-split data to fit the age-distribution model on get the
# model's p_epidemic_posterior fallback instead of a country-specific value
missing_countries <- setdiff(unique(base_case_draws$country), country_lookup$country)

p_u5_draws <- dplyr::bind_rows(
    postprocess_country_p_u5(sid, country_lookup, country_id_vec = country_id_vec),
    postprocess_country_p_u5_fallback(sid, missing_countries)
  ) %>%
  align_draws_to_reference(n_draws)

# Divide: reported / ratio = all medically-attended sCh. reporting_ratio is
# a single global scalar, so every row (every country, every admin unit,
# every draw) is divided by the same value
medically_attended_sCh <- scale_by_reporting_ratio(base_case_draws, "admin_cases", reporting_ratio)

# Scale by test positivity (global posterior, broadcast to every admin unit)
medically_attended_cCh <- combine_draws(medically_attended_sCh, "admin_cases",
                                        positivity_draws, "positivity", "cCh")

# Country-specific mild vs. moderate-to-severe split: severity_u5/
# severity_o5 are global scalars, weighted per country and per draw by
# that country's posterior proportion of cases under 5 (p_u5_draws)
severity_props <- compute_severity_proportions(p_u5_draws, severity_scalars$severity_u5, severity_scalars$severity_o5)

cCh_mild <- combine_draws(medically_attended_cCh, "cCh", severity_props, "prop_mild", "cCh_mild")
cCh_severe <- combine_draws(medically_attended_cCh, "cCh", severity_props, "prop_severe", "cCh_severe")

# Divide (not multiply): care_seeking is P(a symptomatic person seeks care),
# so cCh_mild/cCh_severe (cases among those who sought care) / care_seeking
# = all cases regardless of care-seeking -- same logic as
# scale_by_reporting_ratio() dividing by reporting_ratio above.
all_cCh_mild <- combine_draws(cCh_mild, "cCh_mild", care_seek_mild, "care_seeking", "all_cCh_mild", op = `/`)
all_cCh_severe <- combine_draws(cCh_severe, "cCh_severe", care_seek_severe, "care_seeking", "all_cCh_severe", op = `/`)

# Sum mild + severe to get total confirmed cases per country
all_cCh <- combine_draws(all_cCh_mild, "all_cCh_mild", all_cCh_severe, "all_cCh_severe", "all_cCh", op = `+`)

# Save every intermediate and final output: unscaled outputs from earlier
# sections are untouched, so both scaled and unscaled results are available
purrr::iwalk(
  list(mai_admin_case_draws = mai_admin_case_draws,
       medically_attended_sCh = medically_attended_sCh,
       medically_attended_cCh = medically_attended_cCh,
       cCh_mild = cCh_mild,
       cCh_severe = cCh_severe,
       all_cCh_mild = all_cCh_mild,
       all_cCh_severe = all_cCh_severe,
       all_cCh = all_cCh),
  ~ save_file_generic(.x, make_std_output_name(opt$output_dir, fun_name = .y,
                                                prefix = opt$prefix, suffix = opt$suffix,
                                                file_type = "rds"), file_type = "rds")
)


# L. Case burden adjustment -- scaling-step validation -----------------------
#
# Confirms, per step, that the observed ratio between each pair of
# before/after draws tibbles matches the scaling factor that step was supposed to apply
# Every check compares by row POSITION rather than by re-joining on
# (country, .draw) or similar: combine_draws() preserves
# df1's exact row order and count. Reports PASS/FAIL per step but does not stop()
# on a failure: a mismatch is printed for review, not treated as fatal,
# so the saved .rds outputs above are not blocked by this section.

check <- function(desc, ok) {
  message(if (ok) "PASS -- " else "FAIL -- ", desc)
  invisible(ok)
}

check_scaling_step <- function(before, before_col, after, after_col,
                                factor_df, factor_col, factor_join_keys, label,
                                invert = FALSE) {
  if (nrow(before) != nrow(after)) {
    check(paste0(label, ": before/after row counts differ (", nrow(before), " vs ", nrow(after),
                ") -- cannot validate by row position"), FALSE)
    return(invisible(NULL))
  }
  combined <- dplyr::bind_cols(
    before %>% dplyr::select(country, .draw, dplyr::all_of(before_col)),
    after %>% dplyr::select(value_after = dplyr::all_of(after_col))
  ) %>%
    dplyr::inner_join(factor_df %>% dplyr::select(dplyr::all_of(c(factor_join_keys, factor_col))),
                       by = factor_join_keys) %>%
    dplyr::mutate(observed_ratio = value_after / !!rlang::sym(before_col),
                  # BUGFIX 30 Sep 2026 CA: when the scaling step DIVIDES by
                  # factor_col (invert = TRUE, e.g. care-seeking below) the
                  # expected ratio is 1/factor_col, not factor_col itself --
                  # comparing directly against factor_col here previously
                  # made this check FAIL on a correct division step.
                  expected = if (invert) 1 / !!rlang::sym(factor_col) else !!rlang::sym(factor_col),
                  diff = observed_ratio - expected)
  max_diff <- max(abs(combined$diff))
  check(sprintf("%-28s ratio range: [%.6f, %.6f]  |  max |diff|: %.2e",
               label, min(combined$observed_ratio), max(combined$observed_ratio), max_diff),
        max_diff <= 1e-6)
  invisible(combined)
}

r1 <- dplyr::bind_cols(
  mai_admin_case_draws %>% dplyr::select(admin_cases),
  medically_attended_sCh %>% dplyr::select(value_after = admin_cases)
) %>% dplyr::mutate(observed_ratio = value_after / admin_cases)
check(sprintf("%-28s ratio range: [%.6f, %.6f]  (expect == %.6f, from reporting_ratio = %.6f)",
             "sCh (÷ratio)", min(r1$observed_ratio), max(r1$observed_ratio),
             1 / reporting_ratio, reporting_ratio),
      max(abs(r1$observed_ratio - 1 / reporting_ratio)) <= 1e-6)

check_scaling_step(medically_attended_sCh, "admin_cases", medically_attended_cCh, "cCh",
                    positivity_draws, "positivity", ".draw", "cCh (x positivity)")
check_scaling_step(medically_attended_cCh, "cCh", cCh_mild, "cCh_mild",
                    severity_props, "prop_mild", c("country", ".draw"), "cCh_mild (x prop_mild)")
check_scaling_step(medically_attended_cCh, "cCh", cCh_severe, "cCh_severe",
                    severity_props, "prop_severe", c("country", ".draw"), "cCh_severe (x prop_severe)")
check_scaling_step(cCh_mild, "cCh_mild", all_cCh_mild, "all_cCh_mild",
                    care_seek_mild, "care_seeking", ".draw", "all_cCh_mild (÷care_seek)", invert = TRUE)
check_scaling_step(cCh_severe, "cCh_severe", all_cCh_severe, "all_cCh_severe",
                    care_seek_severe, "care_seeking", ".draw", "all_cCh_severe (÷care_seek)", invert = TRUE)

if (nrow(all_cCh_mild) != nrow(all_cCh_severe) || nrow(all_cCh_mild) != nrow(all_cCh)) {
  check("all_cCh: all_cCh_mild/all_cCh_severe/all_cCh row counts disagree -- the final combine_draws() sum did not produce a clean one-to-one result", FALSE)
} else {
  r_final <- dplyr::bind_cols(
    all_cCh_mild %>% dplyr::select(all_cCh_mild),
    all_cCh_severe %>% dplyr::select(all_cCh_severe),
    all_cCh %>% dplyr::select(all_cCh)
  ) %>% dplyr::mutate(diff = all_cCh - (all_cCh_mild + all_cCh_severe))
  max_sum_diff <- max(abs(r_final$diff))
  check(sprintf("%-28s max |all_cCh - (mild+severe)|: %.2e", "all_cCh (sum)", max_sum_diff),
        max_sum_diff <= 1e-6)
}
