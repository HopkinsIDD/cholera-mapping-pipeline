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

# User-supplied options
opt_list <- list(
  make_option(c("-d", "--config_dir"), 
              default = "./Analysis/cholera-configs/postprocessing_test",
              action ="store", type = "character", help = "Directory"),
  make_option(opt_str = c("-r", "--redo"), type = "logical",
              default = T, help = "redo final outputs"),
  make_option(opt_str = c("-i", "--redo_interm"), type = "logical",
              default = F, help = "redo intermediate"),
  make_option(opt_str = c("-v", "--verbose"), type = "logical",
              default = T, help = "Print statements"),
  make_option(opt_str = c("-s", "--suffix"), type = "character",
              default = NULL, help = "Suffix to use in output file names"),
  make_option(opt_str = c("-x", "--data_dir"), type = "character",
              default = "./cholera-mapping-output/", help = "Directory with all data"),
  make_option(opt_str = c("-y", "--interm_dir"), type = "character",
              default = "./Analysis/output/interm/", help = "Intermediate outputs directory"),
  make_option(opt_str = c("-o", "--output_dir"), type = "character",
              default = "./Analysis/output/processed_outputs/", help = "Output directory")
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

# Get spatial data for plots ----------------------------------------------

# Get lakes
lakes_sf <- get_lakes()

# All the country-level shapefiles for overlay
all_country_sf <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_adm0_sf,
  fun_name = "adm0_sf",
  fun_opts = NULL,
  prefix = "test",
  suffix = opt$suffix,
  error_handling = "remove",
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  output_dir = opt$output_dir,
  inter_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)


# Get outputs -------------------------------------------------------------

# Get the MAI summary at all admin levels 
mai_stats <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_mean_annual_incidence,
  fun_name = "mai",
  fun_opts = NULL,
  prefix = "test",
  suffix = opt$suffix,
  error_handling = "remove",
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  output_dir = opt$output_dir,
  inter_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)


# Get the population at risk all admin levels 
pop_at_risk_stats <- run_all(
  config_dir = opt$config_dir,
  fun = postprocess_pop_at_risk,
  fun_name = "mai",
  fun_opts = NULL,
  prefix = "test",
  suffix = opt$suffix,
  error_handling = "remove",
  redo = opt$redo,
  redo_interm = opt$redo_interm,
  output_dir = opt$output_dir,
  inter_dir = opt$interm_dir,
  data_dir = opt$data_dir,
  output_file_type = "rds",
  verbose = opt$verbose)

# Figure 1: Mean annual incidence ADM0 ------------------------------------

mai_adm0 <- mai_stats %>% 
  filter(admin_level == "ADM0") %>% 
  rmapshaper::ms_simplify(keep = 0.05,
                          keep_shapes = FALSE)

# Save
save_file_generic(res = mai_adm0,
                  res_file = str_glue("{opt$output_dir}/mai_adm0_{suffix}.rds"))

p_fig1 <- output_plot_map(sf_obj = mai_adm0,
                          lakes_sf = lakes_sf,
                          country_borders = all_country_sf,
                          fill_var = "mean",
                          fill_color_scale_type = "rates") +
  ggtitle(str_glue("Mean mean annual incidence rate\nat national level"))

ggsave(p_fig1,
       file = str_glue("{opt$output_dir}/figure_mai_adm0_{suffix}.png"),
       width = 10,
       height = 8, 
       dpi = 300)

# Figure 2: Mean annual incidence ADM2 ------------------------------------

mai_adm2 <- mai_stats %>% 
  filter(admin_level == "ADM2") %>% 
  rmapshaper::ms_simplify(keep = 0.05,
                          keep_shapes = FALSE)

save_file_generic(res = mai_adm2,
                  res_file = str_glue("{opt$output_dir}/mai_adm2_{suffix}.rds"))

p_fig2 <- output_plot_map(sf_obj = mai_adm2,
                          lakes_sf = lakes_sf,
                          country_borders = all_country_sf,
                          fill_var = "mean",
                          fill_color_scale_type = "rates")  +
  ggtitle(str_glue("Mean mean annual incidence rate\n at ADM2 level"))


ggsave(p_fig2,
       file = str_glue("{opt$output_dir}/figure_mai_adm2_{suffix}.png"),
       width = 10,
       height = 8, 
       dpi = 300)
