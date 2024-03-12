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
              default = 10, help = "Number of draws to save from rate/cases grids")
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

# C. Mean annual incidence ---------------------------------------------------

# Get the total number of cases ****
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

# Country-level cases by admin level ****
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

# Get the total number of cases ****
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


# G. Risk categories ---------------------------------------------------------


# Get the population at risk overall ****
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

# Get the population at risk draws by WHO region ****
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

# make_final_figures_and_table.R ---------

prefix_list <- list(
  "2011-2015" = "2011_2015",
  "2016-2020" = "2016_2020"
)

# Functions ---------------------------------------------------------------

#' combine_period_output
#'
#' @param prefix_list 
#' @param output_name 
#' @param output_dir 
#'
#' @return
#' @export
#'
#' @examples
combine_period_output <- function(prefix_list,
                                  output_name,
                                  output_dir) {
  
  res <- map_df(1:length(prefix_list), function(j) {
    readRDS(str_glue("{output_dir}/{prefix_list[j]}_{output_name}.rds")) %>% 
      ungroup() %>% 
      mutate(period = names(prefix_list)[j])
  })
  
  res
}

unpack_pop_at_risk <- function(df) {
  risk_cat_map <- get_risk_cat_dict()
  names(risk_cat_map) <- janitor::make_clean_names(risk_cat_map) %>% 
    str_remove("x")
  
  
  df %>% 
    mutate(risk_cat = str_extract(variable, str_c(rev(names(risk_cat_map)), collapse = "|")),
           risk_cat = risk_cat_map[risk_cat] %>% factor(levels = risk_cat_map),
           admin_level = str_c("ADM", str_extract(variable, "(?<=adm)[0-9]+"))
    )
}

parse_AFRO_region <- function(df) {
  
  if (!("AFRO_region" %in% colnames(df))) {
    stop("AFRO_region needs to be defined to be parsed")
  }
  
  df %>% 
    mutate(AFRO_region = AFRO_region %>% 
             str_replace("_", " ") %>% 
             str_to_title(),
           AFRO_region = factor(AFRO_region, 
                                levels = get_AFRO_region_levels()))
}

# Population at risk by WHO-AFRO region
pop_at_risk_regions_draws <- combine_period_output(prefix_list = prefix_list,
                                                   output_name = "pop_at_risk_by_region_draws",
                                                   output_dir = opt$output_dir) %>% 
  # Keep only adm2 data
  select(-contains("adm0"), -contains("adm1")) %>% 
  pivot_longer(cols = contains("tot"),
               values_to = "value",
               names_to = "variable") %>% 
  mutate(AFRO_region = str_extract(variable, "(?<=tot_pop_risk_)(.)*(?=_adm)")) %>%
  parse_AFRO_region() %>% 
  unpack_pop_at_risk()

# Compute population in high risk categories (> 10/100,000 cases) at ADM2 level overall
pop_high_risk_all_stats <- pop_at_risk_regions_draws %>% 
  group_by(period, .draw, admin_level) %>% 
  summarise(n_high_risk = sum(value[!(risk_cat %in% get_risk_cat_dict()[1:2])])) %>% 
  group_by(period, admin_level) %>% 
  summarise(mean = mean(n_high_risk),
            q025 = quantile(n_high_risk, 0.025),
            q975 = quantile(n_high_risk, 0.975)) %>% 
  ungroup()

saveRDS(pop_high_risk_all_stats, file = str_glue("{opt$output_dir}/pop_high_risk_all_stats.rds"))
