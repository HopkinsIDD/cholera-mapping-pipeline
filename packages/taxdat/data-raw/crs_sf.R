# This script saves crs_sf to data to avoid dependency on crsuggest

crs_sf <- crsuggest::crs_sf

usethis::use_data(crs_sf, 
                  overwrite = TRUE,
                  internal = TRUE)
