## testing for setup_helpers functions

test_that("check_aoi works", {
  tmpfile <- tempfile(fileext = ".yml")

  yaml::write_yaml(data.frame(aoi = "invalid string"), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_equal(
    check_aoi(config$aoi),
    "raw"
  )

  yaml::write_yaml(data.frame(aoi = "any string"), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  Sys.setenv("CHOLERA_TESTING" = TRUE)
  expect_equal(
    check_aoi(config$aoi),
    "any string"
  )
  Sys.unsetenv("CHOLERA_TESTING")

})

test_that("check_res_space works", {
  tmpfile <- tempfile(fileext = ".yml")

  yaml::write_yaml(data.frame(res_space = NULL), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_error(
    check_res_space(config$res_space),
    "The res_space parameter should not be blank because there is no default"
  )

  yaml::write_yaml(data.frame(res_space = "any string"), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_error(
    check_res_space(config$res_space),
    "The res_space parameter is not in the numeric type"
  )

})

test_that("check_time works", {
  tmpfile <- tempfile(fileext = ".yml")

  yaml::write_yaml(data.frame(time = NULL), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_error(
    check_time(config$time),
    "The start_time/end_time parameter should not be blank because there is no default"
  )

  yaml::write_yaml(data.frame(time = NA), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_error(
    check_time(config$time),
    "The start_time/end_time parameter should not be blank because there is no default"
  )

  yaml::write_yaml(data.frame(time = "any string"), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_error(
    check_time(config$time),
    "The start_time/end_time parameter is not in the Date type"
  )

  yaml::write_yaml(data.frame(time = "200-0010-1"), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_error(
    check_time(config$time),
    "The start_time/end_time parameter is not in the Date type"
  )

  yaml::write_yaml(data.frame(time = "2000-01-01"), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_equal(
    check_time(config$time),
    "2000-01-01"
  )

})

test_that("check_data_source works", {
  tmpfile <- tempfile(fileext = ".yml")

  yaml::write_yaml(data.frame(data_source = NULL), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_equal(
    check_data_source(config$data_source),
    "sql"
  )

  yaml::write_yaml(data.frame(data_source = "SqL"), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_equal(
    check_data_source(config$data_source),
    "sql"
  )

  yaml::write_yaml(data.frame(data_source = "API"), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_warning(
    check_data_source(config$data_source),
    "The API is not currently functional for mapping pipeline purposes, use with caution. "
  )

  yaml::write_yaml(data.frame(data_source = "other string"), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_warning(
    check_data_source(config$data_source),
    "The data_source parameter can only be either api or sql, now using the default. "
  )

})

test_that("check_ovrt_metadata_table works", {
  tmpfile <- tempfile(fileext = ".yml")

  yaml::write_yaml(data.frame(ovrt_metadata_table = NULL), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_equal(
    check_ovrt_metadata_table(config$ovrt_metadata_table),
    "no"
  )

  yaml::write_yaml(data.frame(ovrt_metadata_table = "yes"), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_equal(
    check_ovrt_metadata_table(config$ovrt_metadata_table),
    "yes"
  )

  yaml::write_yaml(data.frame(ovrt_metadata_table = "true"), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_equal(
    check_ovrt_metadata_table(config$ovrt_metadata_table),
    TRUE
  )

  yaml::write_yaml(data.frame(ovrt_metadata_table = "random string"), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_error(
    check_ovrt_metadata_table(config$ovrt_metadata_table),
    "The ovrt_metadata_table parameter has to be true/false or yer/no. "
  )

})

test_that("check_taxonomy works", {
  tmpfile <- tempfile(fileext = ".yml")

  yaml::write_yaml(data.frame(taxonomy = NULL), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_equal(
    check_taxonomy(config$taxonomy),
    NULL
  )

  yaml::write_yaml(data.frame(taxonomy = "random string"), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_warning(
    check_taxonomy(config$taxonomy),
    'The taxonomy parameter can only be "taxonomy-working/working-entry1", now using this default. '
  )

  yaml::write_yaml(data.frame(taxonomy = "random string"), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  defaultW <- getOption("warn") 
  options(warn = -1) 
  expect_equal(
    check_taxonomy(config$taxonomy),
    "taxonomy-working/working-entry1"
  )
  options(warn = defaultW)

})

test_that("check_obs_model works", {
  tmpfile <- tempfile(fileext = ".yml")

  yaml::write_yaml(data.frame(obs_model = NULL), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_warning(
    check_obs_model(config$obs_model),
    "The obs_model parameter cannot be null, now returning the default value 1, meaning the poisson observation model will be used in Stan. "
  )

  yaml::write_yaml(data.frame(obs_model = NULL), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  defaultW <- getOption("warn") 
  options(warn = -1) 
  expect_equal(
    check_obs_model(config$obs_model),
    1
  )
  options(warn = defaultW)

  yaml::write_yaml(data.frame(obs_model = "5"), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_warning(
    check_obs_model(config$obs_model),
    "The obs_model parameter can only be 1, 2, or 3, now returning the default value 1, meaning the poisson observation model will be used in Stan. "
  )

  yaml::write_yaml(data.frame(obs_model = "5"), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  defaultW <- getOption("warn") 
  options(warn = -1) 
  expect_equal(
    check_obs_model(config$obs_model),
    1
  )
  options(warn = defaultW)

  yaml::write_yaml(data.frame(obs_model = "random string"), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_warning(
    check_obs_model(config$obs_model),
    "The obs_model parameter can only be 1, 2, or 3, now returning the default value 1, meaning the poisson observation model will be used in Stan. "
  )

  yaml::write_yaml(data.frame(obs_model = "random string"), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  defaultW <- getOption("warn") 
  options(warn = -1) 
  expect_equal(
    check_obs_model(config$obs_model),
    1
  )
  options(warn = defaultW)

  yaml::write_yaml(data.frame(obs_model = 2), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_equal(
    check_obs_model(config$obs_model),
    2
  )

})

test_that("check_od_param works", {
  tmpfile <- tempfile(fileext = ".yml")

  yaml::write_yaml(data.frame(od_param = NULL), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_equal(
    check_od_param(1, config$od_param),
    NULL
  )

  yaml::write_yaml(data.frame(obs_model = 2, od_param = 1), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_equal(
    check_od_param(config$obs_model, config$od_param),
    1
  )

  yaml::write_yaml(data.frame(obs_model = 2, od_param = "1"), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_equal(
    check_od_param(config$obs_model, config$od_param),
    1
  )

  yaml::write_yaml(data.frame(obs_model = 1, od_param = 1), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_equal(
    check_od_param(config$obs_model, config$od_param),
    NULL
  )

})

test_that("check_censoring_thresh works", {
  tmpfile <- tempfile(fileext = ".yml")

  yaml::write_yaml(data.frame(censoring_thresh = NULL), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_equal(
    check_censoring_thresh(config$censoring_thresh),
    0.95
  )

  yaml::write_yaml(data.frame(censoring_thresh = NA), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_equal(
    check_censoring_thresh(config$censoring_thresh),
    0.95
  )

  yaml::write_yaml(data.frame(censoring_thresh = "random string"), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  defaultW <- getOption("warn")
  options(warn = -1) 
  expect_equal(
    check_censoring_thresh(config$censoring_thresh),
    0.95
  )
  options(warn = defaultW)

  yaml::write_yaml(data.frame(censoring_thresh = "-1"), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_error(
    check_censoring_thresh(config$censoring_thresh),
    "The censoring_thresh parameter should not be under 0"
  )

  yaml::write_yaml(data.frame(censoring_thresh = "1.2"), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_warning(
    check_censoring_thresh(config$censoring_thresh),
    "The censoring_thresh parameter is bigger than 1, now returning 1. "
  )

  yaml::write_yaml(data.frame(censoring_thresh = 0.80), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_equal(
    check_censoring_thresh(config$censoring_thresh),
    0.80
  )

})

test_that("check_grid_rand_effects_N works", {
  tmpfile <- tempfile(fileext = ".yml")

  yaml::write_yaml(data.frame(grid_rand_effects_N = 1), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_equal(
    check_grid_rand_effects_N(config$grid_rand_effects_N),
    1
  )

  yaml::write_yaml(data.frame(grid_rand_effects_N = 2), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_error(
    check_grid_rand_effects_N(config$grid_rand_effects_N),
    "must be 1."
  )

  yaml::write_yaml(data.frame(grid_rand_effects_N = TRUE), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_error(
    check_grid_rand_effects_N(config$grid_rand_effects_N),
    "must be numeric."
  )

  yaml::write_yaml(data.frame(other_arg = TRUE), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_equal(
    check_grid_rand_effects_N(config$grid_rand_effects_N),
    1
  )

})

test_that("check_set_tfrac works",{
  tmpfile <- tempfile(fileext = ".yml")

  yaml::write_yaml(data.frame(set_tfrac = TRUE), tmpfile)
  config_true <- yaml::read_yaml(tmpfile)
  expect_true(
    check_set_tfrac(config_true$set_tfrac)
  )

  yaml::write_yaml(data.frame(set_tfrac = FALSE), tmpfile)
  config_false <- yaml::read_yaml(tmpfile)
  expect_false(
    check_set_tfrac(config_false$set_tfrac)
  )

  yaml::write_yaml(data.frame(other_arg = TRUE), tmpfile)
  config_null <- yaml::read_yaml(tmpfile)
  expect_false(
    check_set_tfrac(config_null$set_tfrac)
  )

})

test_that("check_tfrac_thresh works",{
  tmpfile <- tempfile(fileext = ".yml")

  yaml::write_yaml(data.frame(tfrac_thresh = 0.4), tmpfile)
  config_04 <- yaml::read_yaml(tmpfile)
  expect_equal(
    check_tfrac_thresh(config_04$tfrac_thresh),
    0.4
  )

  yaml::write_yaml(data.frame(other_arg = TRUE), tmpfile)
  config_null <- yaml::read_yaml(tmpfile)
  expect_equal(
    check_tfrac_thresh(config_null$tfrac_thresh),
    0
  )

  yaml::write_yaml(data.frame(tfrac_thresh = TRUE), tmpfile)
  config_error <- yaml::read_yaml(tmpfile)
  expect_error(
    check_tfrac_thresh(config_error$tfrac_thresh),
    "must be a numeric"
  )

})

test_that("check_aggregate works",{
  tmpfile <- tempfile(fileext = ".yml")

  yaml::write_yaml(data.frame(aggregate = TRUE), tmpfile)
  config_true <- yaml::read_yaml(tmpfile)
  expect_true(
    check_aggregate(config_true$aggregate)
  )

  yaml::write_yaml(data.frame(aggregate = FALSE), tmpfile)
  config_false <- yaml::read_yaml(tmpfile)
  expect_false(
    check_aggregate(config_false$aggregate)
  )

  yaml::write_yaml(data.frame(other_arg = TRUE), tmpfile)
  config_null <- yaml::read_yaml(tmpfile)
  expect_true(
    check_aggregate(config_null$aggregate)
  )

})