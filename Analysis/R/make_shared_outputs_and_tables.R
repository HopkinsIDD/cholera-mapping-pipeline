library(tidyverse)
library(sf)
library(raster)
library(ncdf4)
library(bigmemory)
library(stars)
 
# convert grided mean incidence and cases to TIF and ncdf
sf_object <- readRDS("2016_2020_mai_grid_rates.rds")

# Convert the sf object to a Spatial object
sp_object <- as(sf_object, "Spatial")

# Create a raster with the same extent as the Spatial object (20 km * 20 km resolution)
raster_object <- raster(extent(sp_object), res = 0.179)

# Set the CRS of the raster object
crs(raster_object) <- "+proj=longlat +datum=WGS84"
crs(sp_object) <- "+proj=longlat +datum=WGS84"

# Rasterize the Spatial object
final_raster_object <- rasterize(sp_object, raster_object, field = "mean")

crs(final_raster_object) <- crs(raster_object)

# Save the raster object as a .tif and cdf file
#writeRaster(final_raster_object, "2016_2020_mai_grid_cases.tif", format = "GTiff",overwrite = T)
#writeRaster(final_raster_object, "2016_2020_mai_grid_cases.nc", format = "CDF",overwrite = T)

# check all the nc and tif files ##############################
data_tif <- raster("2011_2015_mai_grid_rates.tif")
data_nc <- raster("2011_2015_mai_grid_rates.nc")
data_rds <- readRDS("2011_2015_mai_grid_rates.rds")
plot(data_tif)
plot(data_nc)
summary(values(data_nc))
summary(values(data_tif))
summary(data_rds$mean)
value = c(values(data_nc))
summary(value[is.na(value)==F])
summary(data_rds$mean)
value = c(values(data_tif))
summary(value[is.na(value)==F])

##############################large draw files(working in progress)##############################
# convert grided mean incidence and cases to TIF and ncdf
sf_object <- readRDS("2016_2020_mai_grid_cases_draws.rds")
colnames(sf_object)
# Convert sf object to stars (if possible)
stars_object <- st_as_stars(sf_object, dims = c("x", "y"))
 
# Write stars data to NetCDF
# write_stars(stars_object, "2016_2020_mai_grid_cases_draws.nc")
# write_stars(stars_object, "2016_2020_mai_grid_cases_draws.tif", driver = "GTiff")
##########################################################################################

## csv file: 
# MAI rate mean and 95% CI -at continent, region, country levels for each period
mai_continent <- readRDS("2011_2015_mai_rates_all.rds") %>% 
  mutate(location  = "Africa", 
         period = "2011-2015",
         mean = signif(mean,3), 
         lower_95CI = signif(q2.5,3), 
         upper_95CI = signif(q97.5,3),
         spatial_scale = "continent") %>% 
  dplyr::select(location, period, mean, lower_95CI, upper_95CI, spatial_scale) %>% 
  bind_rows(
    readRDS("2016_2020_mai_rates_all.rds") %>% 
      mutate(location  = "Africa", 
             period = "2016-2020",
             mean = signif(mean,3), 
             lower_95CI = signif(q2.5,3), 
             upper_95CI = signif(q97.5,3),
             spatial_scale = "continent") %>% 
      dplyr::select(location, period, mean, lower_95CI, upper_95CI, spatial_scale))

mai_region <- readRDS("2011_2015_mai_rates_by_region.rds") %>% 
  mutate(location = gsub("country_rates_","",variable),
         period = "2011-2015",
         mean = signif(mean,3), 
         lower_95CI = signif(q2.5,3), 
         upper_95CI = signif(q97.5,3),
         spatial_scale = "region") %>% 
  dplyr::select(location, period, mean, lower_95CI, upper_95CI, spatial_scale) %>% 
  bind_rows(
    readRDS("2016_2020_mai_rates_by_region.rds") %>% 
      mutate(location = gsub("country_rates_","",variable),
             period = "2016-2020",
             mean = signif(mean,3), 
             lower_95CI = signif(q2.5,3), 
             upper_95CI = signif(q97.5,3),
             spatial_scale = "region") %>% 
      dplyr::select(location, period, mean, lower_95CI, upper_95CI, spatial_scale))

mai_country <- readRDS("mai_adm0_2011_2015.rds") %>% 
  sf::st_drop_geometry() %>% 
  mutate(location = shapeName, 
         period = "2011-2015",
         mean = signif(mean,3), 
         lower_95CI = signif(q2.5,3), 
         upper_95CI = signif(q97.5,3),
         spatial_scale = "country") %>% 
  dplyr::select(location, period, mean, lower_95CI, upper_95CI, spatial_scale) %>% 
  bind_rows(
    readRDS("mai_adm0_2016_2020.rds") %>% 
      sf::st_drop_geometry() %>% 
      mutate(location = shapeName, 
             period = "2016-2020",
             mean = signif(mean,3), 
             lower_95CI = signif(q2.5,3), 
             upper_95CI = signif(q97.5,3),
             spatial_scale = "country") %>% 
      dplyr::select(location, period, mean, lower_95CI, upper_95CI, spatial_scale))

write.csv(mai_by_admin_level, "mai_continent_region_country.csv", row.names = FALSE)

# IRR mean and 95% CI - at continent, region, country levels
irr_continent <- readRDS("mai_Africa_ratio_stats.rds") %>% 
  mutate(location  = "Africa", 
         mean = signif(mean,3), 
         lower_95CI = signif(q2.5,3),
         upper_95CI = signif(q97.5,3),
         spatial_scale = "continent") %>% 
  dplyr::select(location, mean, lower_95CI, upper_95CI, spatial_scale)

irr_region <- readRDS("mai_region_ratio_stats.rds") %>% 
  mutate(location  = AFRO_region, 
         mean = signif(mean,3), 
         lower_95CI = signif(q2.5,3),
         upper_95CI = signif(q97.5,3),
         spatial_scale = "region") %>% 
  dplyr::select(location, mean, lower_95CI, upper_95CI, spatial_scale)

irr_country <- readRDS("mai_ratio_stats.rds") %>% 
  sf::st_drop_geometry() %>% 
  filter(admin_level == "ADM0") %>%
  mutate(mean = signif(mean,3), 
         lower_95CI = signif(q2.5,3),
         upper_95CI = signif(q97.5,3),
         spatial_scale = "country") %>% 
  left_join(readRDS("mai_adm0_2011_2015.rds") %>% 
              sf::st_drop_geometry() %>% 
              filter(admin_level == "ADM0") %>%
              distinct(country,shapeName), by ="country") %>% 
  mutate(location = shapeName) %>% 
  dplyr::select(location, mean, lower_95CI, upper_95CI, spatial_scale)

irr <- bind_rows(irr_continent,irr_region,irr_country) %>% 
  rename(`Incidence Rate Ratio` = mean)

write.csv(irr,"incidence_rate_ratio.csv",row.names = F)

# ADM2 MAI rate mean and 95% CI
mai_adm2 <- readRDS("mai_adm2_2011_2015.rds") %>% 
  sf::st_drop_geometry() %>% 
  mutate(location = shapeName, 
         period = "2011-2015",
         mean = signif(mean,3), 
         lower_95CI = signif(q2.5,3), 
         upper_95CI = signif(q97.5,3),
         spatial_scale = "admin2") %>% 
  dplyr::select(location, period, mean, lower_95CI, upper_95CI, country, spatial_scale) %>% 
  bind_rows(
    readRDS("mai_adm2_2016_2020.rds") %>% 
      sf::st_drop_geometry() %>% 
      mutate(location = shapeName, 
             period = "2016-2020",
             mean = signif(mean,3), 
             lower_95CI = signif(q2.5,3), 
             upper_95CI = signif(q97.5,3),
             spatial_scale = "admin2") %>% 
      dplyr::select(location, period, mean, lower_95CI, upper_95CI, country, spatial_scale)) %>% 
  left_join(readRDS("mai_adm0_2011_2015.rds") %>% 
              sf::st_drop_geometry() %>% 
              distinct(country, shapeName), by ="country") %>% 
  mutate(country = shapeName) %>% 
  dplyr::select(country, location, period,mean, lower_95CI, upper_95CI) %>% 
  rename(`Mean Annual Incidence Rate per 100,000 population` = mean)

write.csv(mai_adm2,"mai_adm2.csv",row.names = F)

# Number and proportion of people living in ADM2 units with different 5-year and 10-year incidence categories, by country-period (complete -- we already sent this to Gavi)

risk_pop_all_1115 <- readRDS("2011_2015_pop_at_risk.rds") %>% 
  filter(admin_level == "ADM2" | (admin_level == "ADM1" & country == "LSO")) %>% 
  dplyr::select(country,risk_cat, mean, q2.5, q97.5) %>%
  mutate(time_period = '2011-2015')

risk_pop_all_1620 <- readRDS("2016_2020_pop_at_risk.rds") %>% 
  filter(admin_level == "ADM2" | (admin_level == "ADM1" & country == "LSO")) %>% 
  dplyr::select(country,risk_cat, mean, q2.5, q97.5) %>%
  mutate(time_period = '2016-2020')

risk_pop_all <- bind_rows(risk_pop_all_1620,risk_pop_all_1115)

pop <- readRDS("2011_2015_population.rds") %>% 
  subset(admin_level == "ADM0") %>% 
  mutate(pop = mean,time_period = "2011-2015") %>% 
  dplyr::select(shapeName,country,pop,time_period) %>% 
  rbind(
    readRDS("2016_2020_population.rds") %>% 
      subset(admin_level == "ADM0") %>% 
      mutate(pop = mean,time_period = "2016-2020") %>% 
      dplyr::select(shapeName,country,pop,time_period)
  ) %>% 
  dplyr::select(!pop)

table_5year <- 
  risk_pop_all %>% 
  inner_join(pop,by=c("country","time_period")) %>% 
  group_by(country,time_period) %>% 
  mutate(pop = sum(mean)) %>% 
  ungroup()%>% 
  dplyr::select(!country) %>% 
  arrange(factor(risk_cat,levels=c(">100","50-100","20-50","10-20","1-10","<1"))) %>% 
  mutate(
    people_mn = round(mean/10^6,1),
    people_lb = round(q2.5/10^6,1),
    people_ub = round(q97.5/10^6,1),
    proportion = round(mean/pop,2)
  ) %>% 
  rename(
    period = time_period,
    country = shapeName,
    incidence_category = risk_cat
  ) %>% 
  dplyr::select(
    country,
    period,
    incidence_category,
    people_mn,
    people_lb,
    people_ub,
    proportion
  )
endemicity <- readRDS("endemicity_50.rds")
table_10year <-
  endemicity %>% 
  group_by(country,endemicity) %>%
  summarize(mean= sum(pop),time_period = "2011-2020") %>% 
  ungroup() %>%
  group_by(country,time_period) %>% 
  mutate(pop = sum(mean)) %>% 
  ungroup() %>% 
  inner_join(pop %>% subset(time_period == "2016-2020") %>% dplyr::select(!time_period),by=c("country")) %>% 
  dplyr::select(!country) %>% 
  rename(country = shapeName,incidence_category = endemicity) %>% 
  mutate(
    people_mn = round(mean/10^6,1),
    people_lb = NA,
    people_ub = NA,
    proportion = round(mean/pop,2)
  )  %>% 
  rename(
    period = time_period
  ) %>% 
  dplyr::select(
    country,
    period,
    incidence_category,
    people_mn,
    people_lb,
    people_ub,
    proportion
  )

table <- rbind(table_5year,table_10year)%>% 
  mutate(incidence_category = as.character(incidence_category)) %>% 
  mutate(
    incidence_category = ifelse(
      incidence_category == "<1", "<1 per 100K",
      ifelse(
        incidence_category == "1-10", "1-10 per 100K",
        ifelse(
          incidence_category == "10-20", "10-20 per 100K",
          ifelse(
            incidence_category == "20-50", "20-50 per 100K",
            ifelse(incidence_category == "50-100", "50-100 per 100K",
                   ifelse(
                     incidence_category == ">100", ">100 per 100K", 
                     incidence_category
                   ))
          )
        )
      )
    )
  )

write.csv(table,"pop_incidence_category.csv",row.names = F)