## testing for using rgeoboundaries shapefiles for certain country's national shapefiles

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
  expect_message(get_country_admin_units(iso_code="UGA",admin_level=0),"Using the rgeoboundaries shapefiles for all countries, for this country a admin level: 0")
})
