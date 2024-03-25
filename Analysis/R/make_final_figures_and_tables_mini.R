## This is mini version of make_final_figures_and_tables.R for processing outputs and making figures for a subset of country models.

# Preamble ----------------------------------------------------------------

library(tidyverse)
library(sf)
library(lubridate)
library(cmdstanr)
library(optparse)
library(rmapshaper)
library(taxdat)
library(cowplot)

# User-supplied options
opt_list <- list(
  make_option(opt_str = c("-o", "--output_dir"), type = "character",
              default = "./Analysis/output/processed_outputs/", 
              help = "Output directory with postprocessed objects"),
  make_option(opt_str = c("-x", "--prefix_p1"), type = "character",
              default = "postprocessing_test_2011_2015", 
              help = "Prefix of output files for first period"),
  make_option(opt_str = c("-y", "--prefix_p2"), type = "character",
              default = "postprocessing_test_2016_2020", 
              help = "Prefix of output files for second period"),
  make_option(opt_str = c("-p", "--out_prefix"), type = "character",
              default = "postprocessing", 
              help = "Prefix for output figures and tables"),
  make_option(opt_str = c("-d", "--out_dir"), type = "character",
              default = "./Analysis/output/figures",
              help = "Output directory for figures"),
  make_option(opt_str = c("-f", "--bundle_filename"), type = "character",
              default = "./Analysis/output/data_bundle_for_figures_test.rdata", 
              help = "Data bundle to avoid re-processing"),
  make_option(opt_str = c("-r", "--redo"), type = "logical",
              default = FALSE, 
              help = "Redo re-processing")
)

opt <- parse_args(OptionParser(option_list = opt_list))


prefix_list <- list(
  "2011-2015" = opt$prefix_p1,
  "2016-2020" = opt$prefix_p2
)

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
