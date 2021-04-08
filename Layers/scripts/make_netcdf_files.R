# Creates the netcdf files with raster temporal information
# Data produce by IHME at yearly time steps fro the period 2000-2017
# The data is replicated for each month in the year to have monthly estimates
# 
# Preamble ---------------------------------------------------------------------
library(stringr)
library(magrittr)
library(raster)
library(ncdf4)
library(foreach)
library(doParallel)


vars <- dir("Layers/final_lbd_wash/rasters/", pattern = "mean.tif") %>% 
  str_replace("_mean.tif", "")

var_dict <- c(
  "w_piped" = "Piped water (on or off premises)",
  "w_imp" = "Improved water (Piped, improved wells and springs, etc.)",
  "w_imp_other" = "Non-piped imrproved water (w_imp - w_piped)",
  "w_unimp" = "Unimproved water",
  "w_surface" = "Surface water",
  "s_piped" = "Sewer or septic sanitation",
  "s_imp" = "Improved sanitation (Sewer, septic, improved latrines, etc.)",
  "s_imp_other" = "Non-piped imrproved sanitation (s_imp - s_piped)",
  "s_unimp" = "Unimproved sanitation",
  "s_od" = "Open defecation"
)

# Specify time dimension based on CHIRPS template
r_time_info <- list(
  units = "days since 1980-1-1 0:0:0",
  standard_name = "time",
  calendar = "gergorian",
  axis = T
)
# Reference date
start_date <- as.Date(str_extract(r_time_info$units, "[0-9]{4}-[0-9]+-[0-9]+"))
# Dates in IHME data
r_dates <- seq.Date(as.Date("2000-01-01"), as.Date("2017-01-01"), by = "1 years")
r_dates_monthly <- seq.Date(as.Date("2000-01-01"), as.Date("2017-12-31"), by = "1 month")

if (!dir.exists("Layers/final_lbd_wash/rasters/netcdf/"))
  dir.create("Layers/final_lbd_wash/rasters/netcdf/")

cl <- parallel::makeCluster(4)
registerDoParallel(cl)

for(var in vars)  {
  
  cat("--- Processinfg", var, "\n")
  long_var_name <- var_dict[var]
  
  res_dir <- str_c("Layers/final_lbd_wash/rasters/netcdf/", var)
  if (!dir.exists(res_dir))
    dir.create(res_dir)
  
  # Data is yearly, so it is replicated to obtain monthly values.
  foreach(i = 1:length(r_dates),
          .packages = c("ncdf4" ,"tidyverse", "lubridate", "raster"),
          .inorder = F,
          .export = c("var")) %dopar% {
            
            for(m in 1:12)  {
              r <- raster::raster(str_c("Layers/final_lbd_wash/rasters/", var,"_mean.tif"), band = i)
              if (m == 1) {
                data <- r
              } else {
                data <- stack(data, r)
              }
            }
            
            # Load data
            res_file <- str_c("Layers/final_lbd_wash/rasters/netcdf/", var, "/", var, "-", lubridate::year(r_dates[i]), "_monthly.nc")
            cat(res_file)
            
            # From https://gis.stackexchange.com/questions/58550/r-raster-package-write-netcdf-with-time-dimension
            if (!file.exists(res_file)) {
              
              # Longitude and Latitude data
              xvals <- unique(values(init(data, "x")))
              yvals <- unique(values(init(data, "y")))
              nx <- length(xvals)
              ny <- length(yvals)
              lon <- ncdim_def("longitude", "degrees_east", xvals)
              lat <- ncdim_def("latitude", "degrees_north", yvals)
              
              # Missing value to use
              mv <- -9999
              # data[is.na(data)] <- mv
              
              # Time component
              time <- ncdim_def(name = "time", 
                                units = r_time_info$units, 
                                vals = as.numeric(difftime(r_dates_monthly[lubridate::year(r_dates_monthly) == lubridate::year(r_dates[i])], start_date, "days")), 
                                unlim = TRUE,
                                longname = "time")
              
              # Define the temperature variables
              var_wash <- ncvar_def(name = var,
                                    units = "fraction of population",
                                    dim = list(lon, lat, time),
                                    longname = str_c(long_var_name, " (Produced by IHME)."),
                                    missval = mv,
                                    compression = 9)
              
              # Add the variables to the file
              ncout <- nc_create(res_file, list(var_wash), force_v4 = TRUE)
              print(paste("The file has", ncout$nvars,"variables"))
              print(paste("The file has", ncout$ndim,"dimensions"))
              
              # add some global attributes
              ncatt_put(ncout, 0, "Title", str_c(long_var_name, " (Produced by IHME)."))
              ncatt_put(ncout, 0, "Source", "IHME estimates")
              ncatt_put(ncout, 0, "Created on", date())
              
              # Place the precip and tmax values in the file
              # need to loop through the layers to get them 
              # to match to correct time index
              for (i in 1:nlayers(data)) { 
                message("Processing layer ", i, " of ", nlayers(data))
                ncvar_put(nc = ncout, 
                          varid = var_wash, 
                          vals = values(data[[i]]), 
                          start = c(1, 1, i), 
                          count = c(-1, -1, 1))
              }
              # Close the netcdf file when finished adding variables
              nc_close(ncout)
            }
          }
}

stopCluster(cl)

# Scraps -----------------------------------------------------------------------

# # Add date
# r_nc_res <- ncdf4::nc_open(res_file, write = T)
# ncdf4::ncvar_put(r_nc_res, 
#                  varid = "time", 
#                  vals = as.numeric(difftime(r_dates_monthly[lubridate::year(r_dates_monthly) == lubridate::year(r_dates[i])], start_date, "days")))
# ncdf4::nc_close(r_nc_res)
# raster::writeRaster(data, 
#                     res_file,
#                     overwrite = TRUE,
#                     NAflag = -9999,
#                     format = "CDF",
#                     varname = var,
#                     varunit = "fraction",  
#                     longname = str_c(long_var_name, " (Produced by IHME)."),
#                     xname = "longitude",
#                     yname = "latitude",
#                     zname = "time",
#                     zunit = r_time_info$units)



