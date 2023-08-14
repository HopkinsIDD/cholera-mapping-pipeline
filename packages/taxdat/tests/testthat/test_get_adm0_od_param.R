
test_that("get_adm0_od_param works general", {
  
  sf_cases_resized <- tibble::tibble(
    cases = 10,
    TL = as.Date("2000-01-01"),
    TR = as.Date("2000-12-31"),
    admin_level = 0,
    censoring = "full"
  )
  
  cases_column <- "cases"
  res_time <- "1 years"
  
  od_param1 <- get_adm0_od_param(sf_cases_resized = sf_cases_resized,
                                 res_time = res_time,
                                 cases_column = cases_column)
  
  sf_cases_resized2 <- sf_cases_resized
  sf_cases_resized2$cases <- 1e5
  
  od_param2 <- get_adm0_od_param(sf_cases_resized = sf_cases_resized2,
                                 res_time = res_time,
                                 cases_column = cases_column)
  
  
  expect_equal(od_param1, 100)
  expect_equal(od_param2, 1000)
}) 