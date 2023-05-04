## testing for setup_helpers functions

test_that("check_countries_name works", {
  tmpfile <- tempfile(fileext = ".yml")

  yaml::write_yaml(data.frame(countries_name = "invalid string"), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_error(
    check_countries_name(config$countries_name),
    "not a valid ISO3"
  )

  yaml::write_yaml(data.frame(countries_name = c("KEN", "fake")), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_error(
    check_countries_name(config$countries_name),
    "fake in countries_name is not a valid"
  )

  yaml::write_yaml(data.frame(countries_name = "KEN"), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_equal(
    check_countries_name(config$countries_name),
    "KEN"
  )

  yaml::write_yaml(data.frame(countries_name = c("KEN", "SEN")), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_equal(
    check_countries_name(config$countries_name),
    c("KEN", "SEN")
  )
})

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

## check_res_time

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

## check_case_definition

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
    "not a Date type"
  )

  yaml::write_yaml(data.frame(time = "200-0010-1"), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_error(
    check_time(config$time),
    "not a Date type"
  )

  yaml::write_yaml(data.frame(time = "2000-01-01"), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_equal(
    check_time(config$time),
    "2000-01-01"
  )

})

## check_model_date_range
## modeling_time_slices

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
    "API is not currently functional "
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
    FALSE
  )

  yaml::write_yaml(data.frame(ovrt_metadata_table = TRUE), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_equal(
    check_ovrt_metadata_table(config$ovrt_metadata_table),
    TRUE
  )

  sink(file = tmpfile)
  cat(paste0("ovrt_metadata_table: yes\n"))
  sink()
  config <- yaml::read_yaml(tmpfile)
  expect_equal(
    check_ovrt_metadata_table(config$ovrt_metadata_table),
    TRUE
  )

  sink(file = tmpfile)
  cat(paste0("ovrt_metadata_table: no\n"))
  sink()
  config <- yaml::read_yaml(tmpfile)
  expect_equal(
    check_ovrt_metadata_table(config$ovrt_metadata_table),
    FALSE
  )

  yaml::write_yaml(data.frame(ovrt_metadata_table = "random string"), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_error(
    check_ovrt_metadata_table(config$ovrt_metadata_table),
    "must be logical"
  )

  yaml::write_yaml(data.frame(ovrt_metadata_table = "true"), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_error(
    check_ovrt_metadata_table(config$ovrt_metadata_table),
    "must be logical"
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

## check_covariate_choices

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
    0
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
    0
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

  yaml::write_yaml(data.frame(tfrac_thresh = "non-num"), tmpfile)
  config_error <- yaml::read_yaml(tmpfile)
  expect_error(
    check_tfrac_thresh(config_error$tfrac_thresh),
    "must be a numeric"
  )

  yaml::write_yaml(data.frame(tfrac_thresh = 1.1), tmpfile)
  config_error <- yaml::read_yaml(tmpfile)
  expect_error(
    check_tfrac_thresh(config_error$tfrac_thresh),
    "between 0 and 1"
  )

})

## check_censoring

test_that("check_censoring_thresh works", {
  tmpfile <- tempfile(fileext = ".yml")

  yaml::write_yaml(data.frame(censoring_thresh = NULL), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_equal(
    check_censoring_thresh(config$censoring_thresh),
    0.95
  )
  yaml::write_yaml(data.frame(censoring_thresh = "random string"), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_error(
    check_censoring_thresh(config$censoring_thresh),
    "Must be numeric"
  )
  yaml::write_yaml(data.frame(censoring_thresh = -1), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_warning(
    check_censoring_thresh(config$censoring_thresh),
    "cannot be less than 0"
  )
  expect_equal(
    check_censoring_thresh(config$censoring_thresh),
    0
  )
  yaml::write_yaml(data.frame(censoring_thresh = 1.2), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_warning(
    check_censoring_thresh(config$censoring_thresh),
    "cannot be bigger than 1"
  )
  expect_equal(
    check_censoring_thresh(config$censoring_thresh),
    1
  )
  yaml::write_yaml(data.frame(censoring_thresh = 0.80), tmpfile)
  config <- yaml::read_yaml(tmpfile)
  expect_equal(
    check_censoring_thresh(config$censoring_thresh),
    0.80
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

test_that("check_snap_tol works",{
  tmpfile <- tempfile(fileext = ".yml")
  
  yaml::write_yaml(data.frame(snap_tol = "7/365", res_time ="1 years"), tmpfile)
  config_equal <- yaml::read_yaml(tmpfile)
  expect_equal(
    round(check_snap_tol(snap_tol = config_equal$snap_tol, res_time = config_equal$res_time),3),
    0.019
  )
  
  yaml::write_yaml(data.frame(snap_tol = "7)365", res_time ="1 years"), tmpfile)
  config_error <- yaml::read_yaml(tmpfile)
  expect_error(
    check_snap_tol(snap_tol = config_error$snap_tol, res_time = config_error$res_time),
    "snap_tol expression is invalid."
  )

})

test_that("get_all_config_options works",{
  config_options <- get_all_config_options()

  no_check <- "no-check"
  stan_check <- "stan-check"
  file_name_list <- list(
    output_directory = "output_directory",
    data = "observations_filename",
    covar = "covariate_filename",
    stan_input = "stan_input_filename",
    initial_values = "initial_values_filename",
    stan_output = "stan_output_filename",
    stan_genquant = "stan_genquant_filename",
    country_data_report_filename = "country_data_report_filename",
    data_comparison_report_filename = "data_comparison_report_filename"
  )

  expect_equal(config_options$name, no_check)
  expect_equal(config_options$countries, no_check)
  expect_equal(config_options$countries_name, as.function(check_countries_name))
  expect_equal(config_options$aoi, as.function(check_aoi))
  expect_equal(config_options$res_time, as.function(check_res_time))
  expect_equal(config_options$grid_rand_effects_N, as.function(check_grid_rand_effects_N))
  expect_equal(config_options$case_definition, as.function(check_case_definition))
  expect_equal(config_options$start_time, as.function(check_time))
  expect_equal(config_options$end_time, as.function(check_time))
  expect_equal(config_options$data_source, as.function(check_data_source))
  expect_equal(config_options$ovrt_metadata_table, as.function(check_ovrt_metadata_table))
  expect_equal(config_options$OCs, no_check)
  expect_equal(config_options$taxonomy, as.function(check_taxonomy))
  expect_equal(config_options$covariate_choices, as.function(check_covariate_choices))
  expect_equal(config_options$ncpus_parallel_prep, as.function(check_ncpus_parallel_prep))
  expect_equal(config_options$do_parallel_prep, as.function(check_do_parallel_prep))
  expect_equal(config_options$obs_model, as.function(check_obs_model))
  expect_equal(config_options$od_param, as.function(check_od_param))
  expect_equal(config_options$time_effect, stan_check)
  expect_equal(config_options$time_effect_autocorr, stan_check)
  expect_equal(config_options$use_intercept, stan_check)
  expect_equal(config_options$covariate_transformations, no_check)
  expect_equal(config_options$beta_sigma_scale, stan_check)
  expect_equal(config_options$sigma_eta_scale, stan_check)
  expect_equal(config_options$exp_prior, stan_check)
  expect_equal(config_options$do_infer_sd_eta, stan_check)
  expect_equal(config_options$do_zerosum_cnst, stan_check)
  expect_equal(config_options$use_weights, stan_check)
  expect_equal(config_options$use_rho_prior, stan_check)
  expect_equal(config_options$covar_warmup, stan_check)
  expect_equal(config_options$warmup, stan_check)
  expect_equal(config_options$aggregate, as.function(check_aggregate))
  expect_equal(config_options$tfrac_thresh, as.function(check_tfrac_thresh))
  expect_equal(config_options$censoring, as.function(check_censoring))
  expect_equal(config_options$censoring_thresh, as.function(check_censoring_thresh))
  expect_equal(config_options$set_tfrac, as.function(check_set_tfrac))
  expect_equal(config_options$snap_tol, as.function(check_snap_tol))
  expect_equal(config_options$use_pop_weight, as.function(check_use_pop_weight))
  expect_equal(config_options$sfrac_thresh, as.function(check_sfrac_thresh))
  expect_equal(config_options$ingest_covariates, as.function(check_ingest_covariates))
  expect_equal(config_options$ingest_new_covariates, as.function(check_ingest_covariates))
  expect_equal(config_options$stan, c("ncores", "model", "genquant", "niter", "recompile"))
  expect_equal(config_options$file_names, file_name_list)
})
## check_snap_tol, check_use_pop_weight, check_sfrac_thresh, check_ingest_covariates, check_ingest_new_covariates, check_stan_debug, check_stan_model, get_all_config_options, check_update_config