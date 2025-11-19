## The purpose of this script is to compare mean annual incidence rate estimates from June 2023 (2011-2015 and 2016-2020) to previously published Lancet estimates (2010-2016), estimates sent to Gavi in 2021 (interim report representing 2015-2019), and raw WHO annual reports (2015-2019)

library(tidyverse)
library(sf)
library(ggplot2)
library(colorspace)

mai0_11 <- readRDS("Analysis/output/processed_outputs/mai_adm0__2011_2015.rds")
mai2_11 <- readRDS("Analysis/output/processed_outputs/mai_adm2__2011_2015.rds")
mai0_16 <- readRDS("Analysis/output/processed_outputs/mai_adm0__2016_2020.rds")
mai2_16 <- readRDS("Analysis/output/processed_outputs/mai_adm2__2016_2020.rds")

mai0 <- dplyr::bind_rows(
  dplyr::mutate(mai0_11, data_source = "JHU_2011-15", years = "2011-2015"), 
  dplyr::mutate(mai0_16, data_source = "JHU_2016-20", years = "2016-2020")) %>%
  sf::st_drop_geometry() %>%
  dplyr::rename(country = shapeName, low = q5, high = q95) %>%
  dplyr::mutate(countrycode = stringr::str_sub(location_period_id, 1, 3), type = "rates") %>%
  dplyr::select(countrycode, type, mean, low, high, country, data_source, years)

gavi <- read_csv("Analysis/R/notebooks/gavi_report_html/report_Rmd/prelim_estimates_minireport.csv")
new <- dplyr::filter(gavi, type == "rates") %>%
  dplyr::bind_rows(mai0) %>% 
  dplyr::arrange(countrycode, data_source) %>%
  dplyr::mutate(fignum = dplyr::case_when(
    years %in% c("2010-2016", "2011-2015") ~ 1,
    years %in% c("2015-2019", "2016-2020") ~ 2)) %>%
  dplyr::mutate(data_source = dplyr::case_when(
    stringr::str_detect(country, "\\*") & data_source == "JHU_2015-19" ~ "WHO_2015-19",
    TRUE ~ as.character(data_source)
  )) %>% 
  dplyr::mutate(country = dplyr::case_when(
    country == "Central African Republic" ~ "C. African Republic",
    country == "Côte d'Ivoire" ~ "Cote D'Ivoire",
    country == "Republic of the Congo" ~ "Republic of Congo",
    country == "Guinea-Bissau" ~ "Guinea Bissau",
    country == "Swaziland" ~ "Eswatini",
    TRUE ~ as.character(country))) %>%
  dplyr::mutate(country = stringr::str_remove(country, "\\*")) %>% 
  dplyr::mutate(mean10K = mean*1E4,
                low10K = low*1E4,
                high10K = high*1E4) %>%
  dplyr::distinct() %>%
  dplyr::mutate(data_source2 = dplyr::recode(data_source, 
                                            "JHU_2010-16" = "Lancet 2010-16",
                                            "JHU_2011-15" = "2011-15 (new)",
                                            "WHO_2015-19" = "WHO 2015-19",
                                            "JHU_2015-19" = "2015-19 (interim)",
                                            "JHU_2016-20" = "2016-20 (new)"))
  
f1 <- dplyr::filter(new, fignum == 1)
f2 <- dplyr::filter(new, fignum == 2)

pal <- colorspace::qualitative_hcl(5, palette = "Dark 3")
# names(pal) <- c("JHU_2010-16", "JHU_2011-15", "WHO_2015-19", "JHU_2015-19", "JHU_2016-20")
names(pal) <- c("Lancet 2010-16", "2011-15 (new)", "WHO 2015-19", "2015-19 (interim)", "2016-20 (new)")
w=7
h=5
hcl_palettes("qualitative", n = 5, plot = TRUE)


f_p1 <- ggplot(f1, aes(y = country)) +
  geom_point(aes(x = mean10K, colour = data_source2, group = data_source), alpha = 0.8, size = 2) +
  geom_path(aes(x = mean10K), colour = "grey60") +
  scale_x_continuous("Mean Annual Incidence Rate per 10K", labels = scales::comma) +
  scale_colour_manual(values = pal) +
  theme_bw() +
  theme(legend.position = "bottom", axis.title.y = element_blank(), legend.title = element_blank())
ggsave("Analysis/output/processed_outputs/compare_mai_lancet_2011-15.png", width=w, height = h)

f_p2 <- ggplot(f2, aes(y = country)) +
  geom_point(aes(x = mean10K, colour = data_source2, group = data_source), alpha = 0.8, size = 2) +
  geom_path(aes(x = mean10K), colour = "grey60") +
  scale_x_continuous("Mean Annual Incidence Rate per 10K", labels = scales::comma) +
  scale_colour_manual(values = pal) +
  theme_bw() +
  theme(legend.position = "bottom", axis.title.y = element_blank(), legend.title = element_blank())
ggsave("Analysis/output/processed_outputs/compare_mai_gavi_2016-20.png", width=w, height = h)
