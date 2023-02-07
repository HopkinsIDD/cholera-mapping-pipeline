require(raster)
slogo <- stack(system.file("external/rlogo.grd", package="raster")) 
aggregate_raster_xlayers(slogo,mean)

