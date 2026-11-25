## testing for using rgeoboundaries shapefiles for certain country's national shapefiles

test_that("get_country_admin_units works", { 
 expect_message(get_country_admin_units(iso_code="UGA",admin_level=0),"Using the rgeoboundaries shapefiles for all countries, for this country a admin level: 0")
})

test_that("get_geoboundaries returns boundaries with the rgeoboundaries columns", {
  uga <- get_geoboundaries(iso_code = "UGA", admin_level = 0)
  expect_s3_class(uga, "sf")
  expect_equal(nrow(uga), 1)
  expect_true(all(c("shapeName", "shapeID", "shapeGroup", "shapeType", "geometry") %in% names(uga)))
  expect_equal(uga$shapeType, "ADM0")
})
