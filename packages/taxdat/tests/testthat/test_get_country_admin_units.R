## testing for using rgeoboundaries shapefiles for certain country's national shapefiles

test_that("get_country_admin_units works", { 
 testthat::expect_message(get_country_admin_units(iso_code="UGA",admin_level=0))
 testthat::expect_no_message(get_country_admin_units(iso_code="AGO",admin_level=0))
})
