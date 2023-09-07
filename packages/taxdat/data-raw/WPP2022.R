## code to prepare `WPP2022` dataset 

library(wpp2019)
library(tidyverse)

# Download data from https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_TotalPopulationBySex.zip
# [Accessed Sep 4 2023]
# Fields to save:
#   - ISO3_code: ISO3 country code
#   - Location: long format name
#   - Time: year of estimate
#   - PopTotal: total population (in thousands)

WPP2022 <- read_csv("data-raw/WPP2022_TotalPopulationBySex.csv") %>% 
  select(ISO3_code, Location, Time, PopTotal) %>% 
  filter(!is.na(ISO3_code))

usethis::use_data(WPP2022, overwrite = TRUE)
