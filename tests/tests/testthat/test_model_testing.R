context("Model testing")
testthat::test_that("Model testing is working", {
  Sys.setenv(CHOLERA_TEST_ITERATION = 200)
  expect_error(
    {
      Sys.setenv(CHOLERA_CONFIG = normalizePath("../../../Analysis/R/test_case/test_case_1.yml"))
      source("../../../Analysis/R/test_case/test_case.R")
    },
    NA
  )
  ###   expect_error(
  ###     {
  ###       source("../../../Analysis/R/test_case/test_case_2.R")
  ###     },
  ###     NA
  ###   )
  ###   expect_error(
  ###     {
  ###       source("../../../Analysis/R/test_case/test_case_3.R")
  ###     },
  ###     NA
  ###   )
  ###   expect_error(
  ###     {
  ###       source("../../../Analysis/R/test_case/test_case_4.R")
  ###     },
  ###     NA
  ###   )
  ###   expect_error(
  ###     {
  ###       source("../../../Analysis/R/test_case/test_case_5.R")
  ###     },
  ###     NA
  ###   )
  ###  expect_error(
  ###    {
  ###      source("../../../Analysis/R/test_case/test_case_6.R")
  ###    },
  ###    NA
  ###  )
  ###  expect_error(
  ###    {
  ###      source("../../../Analysis/R/test_case/test_case_7.R")
  ###    },
  ###    NA
  ###  )
  ###  expect_error(
  ###    {
  ###      source("../../../Analysis/R/test_case/test_case_8.R")
  ###    },
  ###    NA
  ###  )
})
