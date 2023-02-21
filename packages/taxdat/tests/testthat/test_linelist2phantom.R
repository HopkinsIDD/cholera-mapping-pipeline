## testing for linelist2phantom

## Basic test to make sure that linelist2phantom ever works
test_that("linelist2phantom works", {
  expect_error(
    {
      dat <- read.csv("gnb_linelist_test.csv")
      # dat = dplyr::rename(dat,sCh=suspected_cases,dead = deaths)
      linelist2phantom(dat, location = "location", assumed_complete_location = "GNB")
    },
    NA
  )
})
