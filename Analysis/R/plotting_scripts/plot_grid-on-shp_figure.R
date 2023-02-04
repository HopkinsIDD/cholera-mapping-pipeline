### Setup
library(dplyr)
library(rgeoboundaries)
## go to the mapping pipeline directory
setwd("/home/.../cholera-mapping-pipeline")


### Load the data files (using SDN as an example)
## get the grid
country_code <- "SDN"
load("Analysis/data/SDN_allOCs_1_years_2016-01-01-2020-12-31_20km_suspected.nocovar.pc-N-agT-tft0-tfsF-ct0.95.stan_input.rdata")
grid <- stan_input$sf_grid
## get the country-level shape file from the stan input 
who_annual_cases <- stan_input$sf_cases_resized
who_annual_cases_from_db <- NULL
who_annual_cases_from_db <- taxdat::pull_output_by_source(who_annual_cases, "%WHO Annual Cholera Reports%",
                                                          database_api_key_rfile = stringr::str_c(getwd(), "/Analysis/R/database_api_key.R"))
## get the rgeoboundary shape files 
cty0 <- gb_adm0(country_code)
cty1 <- gb_adm1(country_code)
cty2 <- gb_adm2(country_code)

### Plotting
plt1 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data=who_annual_cases_from_db[1, ],fill="lightblue",color="black",size=0.05)+
  ggplot2::geom_sf(
    data = grid,
    ggplot2::aes(),fill = NA,color="black",size=0.00001) + 
  ggplot2::labs(title="Grid versus Stan Input Shape Files", 
                subtitle="Stan Input Shape File: light blue")+
  ggplot2::theme_minimal() + 
  ggplot2::theme(legend.position = "bottom")
  
plt2 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data=cty1,fill="pink",color="red",size=0.05)+
  ggplot2::geom_sf(data=who_annual_cases_from_db[1, ],fill=NA,color="black",size=0.05)+
  ggplot2::geom_sf(
    data = grid,
    ggplot2::aes(),fill = NA,color="black",size=0.00001) + 
  ggplot2::labs(title="Grid versus Stan Input Shape Files and Rgeo1", 
                subtitle="Stan Input Shape File: transparent; \nRgeo1: pink and red")+
  ggplot2::theme_minimal() + 
  ggplot2::theme(legend.position = "bottom")

plt3 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data=cty2,fill="pink",color="red",size=0.05)+
  ggplot2::geom_sf(data=who_annual_cases_from_db[1, ],fill=NA,color="black",size=0.05)+
  ggplot2::geom_sf(
    data = grid,
    ggplot2::aes(),fill = NA,color="black",size=0.00001) + 
  ggplot2::labs(title="Grid versus Stan Input Shape Files and Rgeo2", 
                subtitle="Stan Input Shape File: transparent; \nRgeo2: pink and red")+
  ggplot2::theme_minimal() + 
  ggplot2::theme(legend.position = "bottom")

plt4 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data=cty0,fill=NA,color="black",size=0.05)+
  ggplot2::geom_sf(data=cty1,fill="pink",color="red",size=0.05)+
  ggplot2::geom_sf(
    data = grid,
    ggplot2::aes(),fill = NA,color="black",size=0.00001) + 
  ggplot2::labs(title="Grid versus Rgeo0 and Rgeo1", 
                subtitle="Rgeo0: transparent; \nRgeo1: pink and red")+
  ggplot2::theme_minimal() + 
  ggplot2::theme(legend.position = "bottom")

plt5 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data=cty0,fill=NA,color="black",size=0.05)+
  ggplot2::geom_sf(data=cty2,fill="pink",color="red",size=0.05)+
  ggplot2::geom_sf(
    data = grid,
    ggplot2::aes(),fill = NA,color="black",size=0.00001) + 
  ggplot2::labs(title="Grid versus Rgeo0 and Rgeo2", 
                subtitle="Rgeo0: transparent; \nRgeo2: pink and red")+
  ggplot2::theme_minimal() + 
  ggplot2::theme(legend.position = "bottom")

pdf("Analysis/output/plot_grid_on_shapefiles.pdf")
plt1
plt2
plt3
plt4
plt5
dev.off()

