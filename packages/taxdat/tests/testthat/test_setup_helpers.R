## testing for setup_helpers functions

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