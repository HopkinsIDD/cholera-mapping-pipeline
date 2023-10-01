"
Script to process WASH rasters following the `Protocol for processing the WASH rasters`
 
 
"

library(terra)
library(sf)
library(ggplot2)
library(dplyr)

theme_set(theme_minimal())
theme_update(legend.position = "bottom")
options(ggplot2.continuous.colour="viridis")
options(ggplot2.continuous.fill = "viridis")


### Step 0: Get the Data 

## read and subset shapefiles
output_shapefiles<- sf::st_read('data/output_shapefiles.gpkg')
output_shapefiles<- output_shapefiles[output_shapefiles$country!='YEM', ]
# subset admin 2
adm2_output_shapefiles<- output_shapefiles[output_shapefiles$admin_level=='ADM2', ]
# get admin 1 for LSO and add with admin 2 
LSO_adm1<- output_shapefiles[output_shapefiles$admin_level=='ADM1' & output_shapefiles$country=='LSO', ]
adm2_shapefiles<- rbind(adm2_output_shapefiles, LSO_adm1)
# subset admin 0
adm0_shapefiles<- output_shapefiles[output_shapefiles$admin_level=='ADM0', ]

## Read population rasters for 2012 and 2017
pop12<- terra::rast('data/worldpop/ppp_2012_1km_Aggregated.tif')
pop17<- terra::rast('data/worldpop/ppp_2017_1km_Aggregated.tif')

## Read WASH rasters for 2012 and 2017
wash17_dir<- "data/wash_ihme/2017/"
wash12_dir<- "data/wash_ihme/2012/"
# 2017 data
wash_2017<- terra::rast()
for (r in dir(wash17_dir)){
  temp_raster<- terra::rast(paste0(wash17_dir, r))
  terra::set.names(temp_raster, gsub("IHME_LMIC_WASH_2000_2017_(.*?)_PERCENT_MEAN_2017_Y2020M06D02", "\\1", terra::varnames(temp_raster)))
  wash_2017<- c(wash_2017, temp_raster)
}

# 2012 data
wash_2012<- terra::rast()
for (r in dir(wash12_dir)){
  temp_raster<- terra::rast(paste0(wash12_dir, r))
  terra::set.names(temp_raster, gsub("IHME_LMIC_WASH_2000_2017_(.*?)_PERCENT_MEAN_2012_Y2020M06D02", "\\1", terra::varnames(temp_raster)))
  wash_2012<- c(wash_2012, temp_raster)
}


### Step 1: Disaggregate and Resample WASH rasters

# disaggregation function
disagg_wash<- function(wash_raster, pop_raster= pop17){
  temp_rast<- terra::disagg(wash_raster, fact= round(terra::res(wash_raster)/terra::res(pop_raster)))
}

# 2017
wash_2017_disagg<- terra::rast()
wash_2017_resample<- terra::rast()
for (i in 1:terra::nlyr(wash_2017)){
  temp_raster<- wash_2017[[i]]
  temp_raster<- disagg_wash(temp_raster, pop_raster = pop17)
  wash_2017_disagg<- c(wash_2017_disagg, temp_raster)
  temp_raster<- terra::resample(temp_raster, pop17, method= 'near')
  wash_2017_resample<- c(wash_2017_resample, temp_raster)
  print(paste('Layer', i, 'complete!'))
}
# terra::writeRaster(wash_2017_disagg, 'data/wash_2017_disagg.tif', gdal=c("COMPRESS=LZW"), overwrite=TRUE)
# wash_2017_disagg<- terra::rast('data/wash_2017_disagg.tif')

# 2012
wash_2012_disagg<- terra::rast()
wash_2012_resample<- terra::rast()
for (i in 1:terra::nlyr(wash_2012)){
  temp_raster<- wash_2012[[i]]
  temp_raster<- disagg_wash(temp_raster, pop_raster = pop12)
  wash_2012_disagg<- c(wash_2012_disagg, temp_raster)
  temp_raster<- terra::resample(temp_raster, pop12, method= 'near')
  wash_2012_resample<- c(wash_2012_resample, temp_raster)
  print(paste('Layer', i, 'complete!'))
}

rm(temp_raster)



### Step 3: Save resampled rasters
terra::writeRaster(wash_2017_resample, 'data/wash_2017_resample.tif', gdal=c("COMPRESS=LZW"), overwrite=TRUE)
# wash_2017_resample<- terra::rast('data/wash_2017_resample.tif')
terra::writeRaster(wash_2012_resample, 'data/wash_2012_resample.tif', gdal=c("COMPRESS=LZW"), overwrite=TRUE)

# remove other rasters
rm(wash_2012, wash_2012_disagg)
rm(wash_2017, wash_2017_disagg)


# step 4 to 10
# doing full continent together 
# 2012
temp_w<- terra::crop(wash_2012_resample, adm2_shapefiles, snap= 'out') # crop wash
temp_p<- terra::crop(pop12, adm2_shapefiles, snap= 'out') # crop population
temp_wp<- temp_w*temp_p # step 8
adm2_sf_wash<- cbind(adm2_shapefiles, exactextractr::exact_extract(temp_wp, adm2_shapefiles, fun= 'sum')) # step 10
names(adm2_sf_wash) <- gsub('sum.', 'Y2012_', names(adm2_sf_wash))
# 2017
temp_w<- terra::crop(wash_2017_resample, adm2_shapefiles, snap= 'out')
temp_p<- terra::crop(pop17, adm2_shapefiles, snap= 'out')
temp_wp<- temp_w*temp_p
adm2_sf_wash<- cbind(adm2_sf_wash, exactextractr::exact_extract(temp_wp, adm2_shapefiles, fun= 'sum'))
names(adm2_sf_wash) <- gsub('sum.', 'Y2017_', names(adm2_sf_wash))
# remove temp rasters 
rm(temp_w, temp_p, temp_wp)
# save 
sf::st_write(adm2_sf_wash, 'results/adm2_sf_wash.gpkg')
adm2_sf_wash<- sf::st_read('results/adm2_sf_wash.gpkg')
### step 11
# proportions 
adm2_df_wash<- sf::st_drop_geometry(adm2_sf_wash)
# remove Improved columns because Improved = Piped + Improved Other
imp_cols<- c("Y2012_S_IMP", "Y2012_W_IMP", "Y2017_S_IMP", "Y2017_W_IMP")
adm2_df_wash<- adm2_df_wash[, !(names(adm2_df_wash) %in% imp_cols)]

# calculates proportion from a subset of columns with same prefix
calc_proportion<- function(df, column_prefix){
  prop_df<- df%>%
    select(starts_with(column_prefix))%>%
    mutate(./rowSums(.), .keep='all')%>%
    rename_with(~paste0('prop_', .x))
  return(cbind(df, prop_df))
}

adm2_df_wash<- calc_proportion(adm2_df_wash, 'Y2012_S')
adm2_df_wash<- calc_proportion(adm2_df_wash, 'Y2012_W')
adm2_df_wash<- calc_proportion(adm2_df_wash, 'Y2017_S')
adm2_df_wash<- calc_proportion(adm2_df_wash, 'Y2017_W')

### step 12: proportion at country level 
# country level data
adm0_df_wash<-adm2_df_wash%>%group_by(country)%>%summarize(across(starts_with("Y"), ~ sum(.)))
# proportion 
adm0_df_wash<- calc_proportion(adm0_df_wash, 'Y2012_S')
adm0_df_wash<- calc_proportion(adm0_df_wash, 'Y2012_W')
adm0_df_wash<- calc_proportion(adm0_df_wash, 'Y2017_S')
adm0_df_wash<- calc_proportion(adm0_df_wash, 'Y2017_W')

# step 13: remove rasters
rm(wash_2012_resample, wash_2017_resample)
rm(pop12, pop17)


# step 14
write.csv2(adm0_df_wash, 'results/adm0_df_wash.csv')
adm0_df_wash<- read.csv2('results/adm0_df_wash.csv')
sf::st_write(adm2_sf_wash, 'results/adm2_sf_wash.gpkg')



# verification plots (step 2)
iso_name<- 'BDI'
iso_name<- 'NGA'
col_prefix<- 'prop_Y2017'
gg_list<- list()
col_names<- names(adm2_df_wash)[grepl(col_prefix, names(adm2_df_wash))]
for (i in 1:length(col_names)){
  print(col_names[i])
  gg_list[[i]]<- ggplot()+ 
    geom_density(data= adm2_df_wash%>%filter(country==iso_name), aes(x= .data[[col_names[i]]]))+ 
    geom_vline(data= adm0_df_wash%>%filter(country==iso_name), aes(xintercept=.data[[col_names[i]]]), color= '#cc4778', linetype= 1) +
    theme(legend.title = element_blank(), axis.title.y = element_blank())+ 
    labs(title= sub('prop_', '', col_names[i]), x= 'Proportion')
}

gg_all_indicators<- gg_list[[1]]+ gg_list[[2]]+ gg_list[[3]]+ gg_list[[4]]+ gg_list[[5]] + 
  gg_list[[6]]+ gg_list[[7]]+ gg_list[[8]]+ gg_list[[9]]+ gg_list[[10]] + 
  plot_layout(ncol = 5)

png(paste0('results/compare_resample_disaggregation/', iso_name, '_', col_prefix, '.png'), width= 12, height= 4, unit= 'in', res= 300)
gg_all_indicators
dev.off()


### check to make sure all indicator from the same ladder sums to 1 
options(digits = 20)
check_s_2012<- 
  adm2_df_wash$prop_Y2012_S_IMP_OTHER+
  adm2_df_wash$prop_Y2012_S_OD+
  adm2_df_wash$prop_Y2012_S_PIPED+
  adm2_df_wash$prop_Y2012_S_UNIMP
sum(round(check_s_2012, 10)!=1, na.rm = T) 

check_w_2012<- 
  adm2_df_wash$prop_Y2012_W_IMP_OTHER+
  adm2_df_wash$prop_Y2012_W_OD+
  adm2_df_wash$prop_Y2012_W_PIPED+
  adm2_df_wash$prop_Y2012_W_UNIMP
sum(round(check_w_2012, 10)!=1, na.rm = T) 

check_s_2017<- 
  adm2_df_wash$prop_Y2017_S_IMP_OTHER+
  adm2_df_wash$prop_Y2017_S_OD+
  adm2_df_wash$prop_Y2017_S_PIPED+
  adm2_df_wash$prop_Y2017_S_UNIMP
sum(round(check_s_2017, 10)!=1, na.rm = T) 

check_w_2017<- 
  adm2_df_wash$prop_Y2017_W_IMP_OTHER+
  adm2_df_wash$prop_Y2017_W_OD+
  adm2_df_wash$prop_Y2017_W_PIPED+
  adm2_df_wash$prop_Y2017_W_UNIMP
sum(round(check_w_2017, 10)!=1, na.rm = T) 


### step 15:  For each admin unit, identify which category of access at least 60% of 
#             the population has access to (open defecation / surface water, unimproved 
#             facility, no single dominant facility type (transitioning), improved facility type)

# Sanitation 2012
adm2_df_wash$S_2012_60pct<- adm2_df_wash%>%
  select(starts_with('prop_Y2012_S'))%>% 
  rowwise()%>%
  mutate(which.6= ifelse(any(c_across(everything())>.6),  which(c_across(everything())>.6)[1], 99))%>% # 
  mutate(S_2012_60pct= ifelse(which.6<5, colnames(.)[which.6],'transitioning'), .keep = 'none')%>%
  pull(S_2012_60pct)
# Water 2012
adm2_df_wash$W_2012_60pct<- adm2_df_wash%>%
  select(starts_with('prop_Y2012_W'))%>% 
  rowwise()%>%
  mutate(which.6= ifelse(any(c_across(everything())>.6),  which(c_across(everything())>.6)[1], 99))%>% # 
  mutate(W_2012_60pct= ifelse(which.6<5, colnames(.)[which.6],'transitioning'), .keep = 'none')%>%
  pull(W_2012_60pct)

# Sanitation 2017
adm2_df_wash$S_2017_60pct<- adm2_df_wash%>%
  select(starts_with('prop_Y2017_S'))%>% 
  rowwise()%>%
  mutate(which.6= ifelse(any(c_across(everything())>.6),  which(c_across(everything())>.6)[1], 99))%>% # 
  mutate(S_2017_60pct= ifelse(which.6<5, colnames(.)[which.6],'transitioning'), .keep = 'none')%>%
  pull(S_2017_60pct)
# Water 2017
adm2_df_wash$W_2017_60pct<- adm2_df_wash%>%
  select(starts_with('prop_Y2017_W'))%>% 
  rowwise()%>%
  mutate(which.6= ifelse(any(c_across(everything())>.6),  which(c_across(everything())>.6)[1], 99))%>% # 
  mutate(W_2017_60pct= ifelse(which.6<5, colnames(.)[which.6],'transitioning'), .keep = 'none')%>%
  pull(W_2017_60pct)


# save the adm2 data 
write.csv2(adm2_df_wash, 'results/adm2_df_wash.csv')
# adm2_df_wash<- read.csv2('results/adm2_df_wash.csv')


##










