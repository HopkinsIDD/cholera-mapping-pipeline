## testing for using rgeoboundaries shapefiles for certain country's national shapefiles

test_that("get_country_admin_units works", { 
 expect_message(get_country_admin_units(iso_code="UGA",admin_level=0),"Using the rgeoboundaries shapefiles for this country at admin level 0")
 expect_message(get_country_admin_units(iso_code="AGO",admin_level=0),"Using the gadm shapefile for this country at admin level 0.")
 expect_message(get_country_admin_units(iso_code="ZNZ",admin_level=0),"Using the aggregated gadm shapefiles for Zanzibar")
})