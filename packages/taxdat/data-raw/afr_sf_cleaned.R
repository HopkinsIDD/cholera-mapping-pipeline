# Create the cleaned set of ADM0 shapefiles for Africa

library(sf)
library(tidyverse)

# File data-raw/afr_sf.shp contains the ADM0 level shapefiles from the output_shapefiles
# table. The code below was used to create it (do not uncomment)

# afr_sf <- st_read("~/projects/cholera-mapping-pipeline/output_shapefiles.gpkg") %>% 
#   filter(admin_level == "ADM0")
# 
# st_write(afr_sf, dsn = "data-raw/afr_sf.gpkg", layer = "ADM0")

afr_sf <- sf::st_read("data-raw/afr_sf.gpkg") %>% 
  # Remove wholes 
  nngeo::st_remove_holes() %>% 
  # Remove small islands 
  rmapshaper::ms_filter_islands(min_area = 1e9) %>% 
  rmapshaper::ms_simplify(keep = 0.05,
                          keep_shapes = FALSE)

usethis::use_data(afr_sf, internal = FALSE)
