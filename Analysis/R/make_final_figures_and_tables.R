# This script combines postprocessed outputs and makes final figures


# Preamble ----------------------------------------------------------------

library(tidyverse)
library(sf)
library(lubridate)
library(cmdstanr)
library(optparse)
library(rmapshaper)
library(taxdat)

# User-supplied options
opt_list <- list(
  make_option(opt_str = c("-o", "--output_dir"), type = "character",
              default = "./Analysis/output/processed_outputs/", help = "Output directory"),
  make_option(opt_str = c("-x", "--prefix_p1"), type = "character",
              default = "2011_2015", help = "Prefix of output files for first period"),
  make_option(opt_str = c("-y", "--prefix_p2"), type = "character",
              default = "2016_2020", help = "Prefix of output files for second period")
)

opt <- parse_args(OptionParser(option_list = opt_list))


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
  
  res <- map_df(1:length(prefix_list), function(x) {
    readRDS(str_glue("{output_dir}/{prefix_list[x]}_{output_name}.rds")) %>% 
      mutate(period = names(prefix_list)[x])
  })
  
  res
}

prefix_list <- list(
  "2011_2015" = "postprocessing_test_3"
)


# Load data ---------------------------------------------------------------

# Total number of cases by region
cases_by_region <- combine_period_output(prefix_list = prefix_list,
                                         output_name = "mai_cases_by_region",
                                         output_dir = opt$output_dir) %>% 
  mutate(AFRO_region = str_remove(variable, "country_cases_"))

# Mean annual incidence rates by region
rates_by_region <- combine_period_output(prefix_list = prefix_list,
                                         output_name = "mai_rates_by_region",
                                         output_dir = opt$output_dir) %>% 
  mutate(AFRO_region = str_remove(variable, "country_rates_"))


# Number of people living in different risk categories
risk_pop_adm2 <- combine_period_output(prefix_list = prefix_list,
                                       output_name = "risk_categories",
                                       output_dir = opt$output_dir) %>% 
  filter(admin_level == "ADM2") %>% 
  taxdat::get_AFRO_region(ctry_col = "country")


# Mean annual incidence rates at ADM2 level
mai_adm_all <- combine_period_output(prefix_list = prefix_list,
                                  output_name = "mai",
                                  output_dir = opt$output_dir) %>% 
  mutate(run_id = str_c(country, period, sep = "-"),
         log10_rate_per_1e5 = log10(mean * 1e5))

# Combine results with no-w runs
mai_adm <- bind_rows(
  mai_adm_all %>% filter(admin_level == "ADM0", run_id %in% get_no_w_runs()),
  mai_adm_all %>% filter(admin_level == "ADM2", !(run_id %in% get_no_w_runs()))
)

# List of all countries with intended run
# Sheet Data Entry Tracking Feb 2022 from spreadsheet that can be downloaded from
# https://docs.google.com/spreadsheets/d/17MtTdUlC2tNLk3QPYdTzgFqiPacUg4rbi8cVTYoOaZE/edit#gid=1264119518

intended_runs <- read_csv("Analysis/output/Data Entry Coordination - Data Entry Tracking - Feb 2022.csv") %>% 
  janitor::clean_names() %>% 
  filter(country != "GMB") %>% 
  mutate(isocode = case_when(str_detect(country, "TZA") ~ "TZA",
                             T ~ country))

# Spatial data
data(afr_sf, package = "taxdat")
afr_sf <- afr_sf %>% 
  janitor::clean_names() %>% 
  mutate(country_name = country,
         intended_run = country %in% intended_runs$isocode) %>% 
  st_crop(st_bbox(st_sfc(
    st_point(c(-18.8, 36.6)), 
    st_point(c(52.2, -36.5)),
    crs = 4326)))

# Lakes for plots
lakes_sf <- get_lakes()

# Fig 2: ADM2 rate maps --------------------------------------------------------

p_fig2 <- output_plot_map(sf_obj = mai_adm,
                          lakes_sf = lakes_sf,
                          all_countries_sf = afr_sf,
                          fill_var = "log10_rate_per_1e5",
                          fill_color_scale_type = "rates") +
  facet_wrap(~ period) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 15),
        legend.position = "right") +
  guides(fill = guide_colorbar("Incidence rate\n[cases/100,000 people]"))


# Fig. 3A: People per risk category ---------------------------------------
# This is at the ADM2 level per country

fig_3A <- risk_pop_adm2 %>% 
  bind_rows(risk_pop_adm2 %>% mutate(AFRO_region = "all")) %>% 
  ggplot(aes(x = risk_cat, y = pop, fill = period)) +
  geom_bar(stat = "identity") +
  facet_wrap(~AFRO_region) +
  theme_bw() +
  labs(x = "Incidence risk category", y = "ADM2 population at risk")


# Fig. 1C: cases by region and time period --------------------------------

fig_1C <- cases_by_region %>% 
  ggplot(aes(x = mean, y = period, fill = AFRO_region)) +
  geom_bar(stat = "identity")  +
  theme_bw() +
  labs(x = "Mean annual cholera incidence [cases/year]", y = "Time period")

