require(raster)
slogo <- stack(system.file("external/rlogo.grd", package = "raster"))
aggregate_raster_xcells(slogo, mean)
