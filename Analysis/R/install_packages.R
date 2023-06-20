# Title: install required packages for running pipelin in idm cluster

package_list <- c(
  "devtools",
  "DBI",
  "digest",
  "dplyr",
  "foreach",
  "gdalUtils",
  "glue",
  "geojsonsf",
  "igraph",
  "inline",
  "itertools",
  "jsonlite",
  "lubridate",
  "magrittr",
  "mgcv",
  "ncdf4",
  "odbc",
  "optparse",
  "parallel",
  "posterior",
  "purrr",
  "RCurl",
  "R.utils",
  "raster",
  "rjson",
  "rstan",
  "rpostgis",
  "rts",
  "RPostgres",
  "sf",
  "spdep",
  "stringr",
  "tidync",
  "tibble",
  "zoo",
  "geodata",
  "cmdstanr",
  "ISOcodes",
  "rgdal",
)

chooseCRANmirror(graphics = FALSE, ind = 1)
for (package in package_list) {
  if (!require(package = package, character.only = T)) {
    install.packages(pkgs = package)
  }
}

# gdalUtils is not on CRAN, can be installed from github
library(devtools)
devtools:::install_github("gearslaboratory/gdalUtils")
