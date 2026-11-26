## testing for using geoBoundaries shapefiles for certain country's national shapefiles

# The test downloads from geoboundaries.org; skip it (rather than fail) when the
# site does not answer, so an outage there does not break CI.
skip_if_geoboundaries_unreachable <- function(timeout = 10) {
  ok <- requireNamespace("curl", quietly = TRUE) && tryCatch({
    h <- curl::new_handle(nobody = TRUE, timeout = timeout, connecttimeout = timeout)
    curl::curl_fetch_memory("https://www.geoboundaries.org/", handle = h)$status_code < 500
  }, error = function(e) FALSE)
  if (!isTRUE(ok)) testthat::skip("www.geoboundaries.org is unreachable")
}

test_that("get_country_admin_units works", {
  skip_if_geoboundaries_unreachable()
  expect_message(get_country_admin_units(iso_code="UGA",admin_level=0),"Using the geoBoundaries shapefiles for all countries, for this country a admin level: 0")
})

test_that("get_geoboundaries returns boundaries with the expected columns", {
  skip_if_geoboundaries_unreachable()
  uga <- get_geoboundaries(iso_code = "UGA", admin_level = 0)
  expect_s3_class(uga, "sf")
  expect_equal(nrow(uga), 1)
  expect_true(all(c("shapeName", "shapeID", "shapeGroup", "shapeType", "geometry") %in% names(uga)))
  expect_equal(uga$shapeType, "ADM0")
})
