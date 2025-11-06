## code to prepare `WPP2024` dataset 

library(tidyverse)

# Download data from https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_TotalPopulationBySex.csv.gz
# For years after 2024, each country-year has multiple population estimates under different projection scenarios. The dictionary for the scenarios can be obtained here: https://population.un.org/wpp/definition-of-projection-scenarios. 
# For the WPP2024 data in taxdat, we chose the "Medium" project scenario.

# [Accessed May 6 2025]
# Fields to save:
#   - ISO3_code: ISO3 country code
#   - Location: long format name
#   - Time: year of estimate
#   - PopTotal: total population (in thousands)

WPP2024 <- read_csv("data-raw/WPP2024_TotalPopulationBySex.csv") %>% 
  filter(Variant == "Medium") %>% # Medium projection scenario was chosen
  select(ISO3_code, Location, Time, PopTotal) %>% 
  filter(!is.na(ISO3_code))

usethis::use_data(WPP2024, overwrite = TRUE)
