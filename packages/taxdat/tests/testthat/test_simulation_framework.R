
test_that("get or set seed works", {
  expect_error(seed <- get_or_set_seed(), NA)
  seed <- get_or_set_seed()
  expect_equal(
    {
      get_or_set_seed(seed)
      .GlobalEnv$.Random.seed
    },
    seed
  )

  expect_equal(
    {
      new_seed <- get_or_set_seed(seed)
      new_seed
    },
    seed
  )
})

test_that("get or set seed fixes next random number sequence", {
  seed <- get_or_set_seed()
  result <- rnorm(1)
  result2 <- rnorm(1)

  expect_equal(
    {
      get_or_set_seed(seed)
      rnorm(1)
    },
    result
  )
  expect_equal(
    {
      get_or_set_seed(seed)
      rnorm(1)
      rnorm(1)
    },
    result2
  )

  # test_if_seed_setting_works(rnorm, n = 1)
})

test_that("seed setting works for simulation functions", {
  test_if_seed_setting_works(create_test_extent)
  test_if_seed_setting_works(create_test_raster)
  test_if_seed_setting_works(create_test_2d_polygons)
  test_if_seed_setting_works(create_test_lines)
  test_if_seed_setting_works(create_test_points)
  test_if_seed_setting_works(create_test_polygons)
  test_if_seed_setting_works(create_test_layered_polygons)
  test_if_seed_setting_works(create_test_covariate)
  test_if_seed_setting_works(create_multiple_test_covariates)
  test_if_seed_setting_works(create_underlying_distribution)
  test_if_seed_setting_works(observe_gridcells)
  test_if_seed_setting_works(observe_polygons)
  test_if_seed_setting_works(create_standardized_test_data)
})

test_that("Simulation framework works for nlayers = 1", {
  expect_error(create_standardized_test_data(nlayers = 1), NA)
})
