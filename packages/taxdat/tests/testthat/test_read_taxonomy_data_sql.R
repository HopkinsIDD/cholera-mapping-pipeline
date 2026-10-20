# Tests for read_taxonomy_data_sql()
#
# A. Argument validation -- no database needed.
#    Invalid calls must fail with class "taxdat_arg_error" BEFORE any
#    connection attempt. Valid calls target an unresolvable host (reserved
#    ".invalid" TLD, RFC 6761): they pass validation, then fail at connection
#    time with an error of another class.
# B. Integration -- real database, read-only.
#    Skipped unless credentials are set as environment variables; never run
#    on CI. Assertions are boolean invariants only, so that a failure never
#    prints observation data in test logs.
#
# No credential is stored in this file.

# ============================================================================
# A. Argument validation (no database)
# ============================================================================

.fake_host <- "taxdat-test.invalid"

# Call the function with dummy credentials, valid defaults and the fake host.
# Arguments passed in `...` override the defaults; note that utils::modifyList
# DROPS an element set to NULL, so `x = NULL` falls back to the function's own
# default (NULL for locations/time/uids, missing for username/password).
.call_fake <- function(...) {
  args <- utils::modifyList(
    list(username = "dummy", password = "dummy", locations = 14,
         time_left = "2019-01-01", time_right = "2019-12-31", uids = 1,
         host = .fake_host),
    list(...)
  )
  do.call(read_taxonomy_data_sql, args)
}

# Expect an argument error (raised before connecting)
expect_arg_error <- function(..., regexp = NULL) {
  expect_error(suppressWarnings(.call_fake(...)),
               regexp = regexp, class = "taxdat_arg_error")
}

# Expect the call to pass validation: it must then fail at connection time
expect_passes_validation <- function(...) {
  err <- tryCatch(suppressWarnings(.call_fake(...)), error = function(e) e)
  expect_s3_class(err, "error")                    # fake host: must not connect
  expect_false(inherits(err, "taxdat_arg_error"))  # ...but not an argument error
}

# Warnings emitted before the (failing) connection attempt
.warnings_of <- function(...) {
  capture_warnings(try(.call_fake(...), silent = TRUE))
}

test_that("credentials are validated", {
  expect_error(read_taxonomy_data_sql(password = "x", locations = 14),
               class = "taxdat_arg_error")             # username missing
  expect_error(read_taxonomy_data_sql(username = "x", locations = 14),
               class = "taxdat_arg_error")             # password missing
  expect_arg_error(username = NA_character_)
  expect_arg_error(username = "")
  expect_arg_error(username = c("a", "b"))
  expect_arg_error(username = 123)
  expect_arg_error(password = NA_character_)
  expect_arg_error(password = "", regexp = "only allowed with host = 'localhost'")
  # Empty password + localhost is a valid call (local socket)
  expect_passes_validation(password = "", host = "localhost")
})

test_that("locations are validated", {
  expect_arg_error(locations = NULL, regexp = "can't be NULL")
  expect_arg_error(locations = "AFR::ETH", regexp = "not yet implemented")
  for (bad in list(NA_real_, 0, -1, 1.5, Inf, numeric(0), c(14, NA))) {
    expect_arg_error(locations = bad, regexp = "positive whole numbers")
  }
  expect_passes_validation(locations = c(14, 15))
})

test_that("uids are validated", {
  for (bad in list(31.4, -314, 0, NA_real_, "abc", "-1", TRUE, numeric(0))) {
    expect_arg_error(uids = bad, regexp = "positive whole ids")
  }
  expect_passes_validation(uids = 314)
  expect_passes_validation(uids = c(314, 21041))
  expect_passes_validation(uids = "314")
})

test_that("dates are validated", {
  for (bad in list("200-01-01", "04/03/2017", "2017-02-30", 20190101,
                   c("2019-01-01", "2019-02-01"), NA, as.Date(NA))) {
    expect_arg_error(time_left = bad)
  }
  expect_arg_error(time_left = "2000-01-01", time_right = "1999-01-01",
                   regexp = "is after")
  expect_arg_error(time_left = as.Date("2000-01-01"),
                   time_right = as.Date("1999-01-01"), regexp = "is after")
  expect_passes_validation(time_left = as.Date("2019-01-01"))
  expect_passes_validation(time_left = as.POSIXct("2019-01-01", tz = "UTC"))
  expect_passes_validation(time_left = "2019-06-01", time_right = "2019-06-01")
})

test_that("governance flags are validated", {
  for (bad in list("TRUE", NA, c(TRUE, FALSE))) {
    expect_arg_error(discard_incomplete_observation_collections = bad)
  }
  w <- .warnings_of(unified_dataset_behaviour = "Drop")
  expect_true(any(grepl("unified-dataset filter is disabled", w)))
  expect_length(.warnings_of(unified_dataset_behaviour = "keep"), 0)
})

test_that("missing filters raise the documented warnings", {
  w <- .warnings_of(time_left = NULL, time_right = NULL)
  expect_equal(sum(grepl("^No time filters", w)), 2)
  w <- .warnings_of(uids = NULL)
  expect_true(any(grepl("^No uid filters", w)))
  # All filters given: no warning at all
  expect_length(.warnings_of(), 0)
})

# ============================================================================
# B. Integration (real database, read-only)
# ============================================================================

# Names of the environment variables exported by load_cholera_env.sh
.db_user <- Sys.getenv("CHOLERA_USER")
.db_pass <- Sys.getenv("CHOLERA_PASSWORD")

skip_if_no_db <- function() {
  skip_on_ci()
  skip_on_cran()
  skip_if(!nzchar(.db_user) || !nzchar(.db_pass),
          "Taxonomy DB credentials not set in the environment")
}

# Assumption carried over from the original tests: location 14 is Ethiopia
# (qualified name "AFR::ETH") and contains collection 21136.
.expected_cols <- c("id", "observation_collection_id", "time_left", "time_right",
                    "suspected_cases", "confirmed_cases", "deaths", "phantom",
                    "primary", "location_name", "location_via_fallback",
                    "location_id", "location_period_id")

test_that("integration: rows respect location, date and uid filters", {
  skip_if_no_db()
  tl <- as.Date("2019-01-01"); tr <- as.Date("2020-08-02"); uids <- 21136
  x <- read_taxonomy_data_sql(.db_user, .db_pass, locations = 14,
                              time_left = tl, time_right = tr, uids = uids)

  # Output contract
  expect_s3_class(x, "sf")
  expect_true(all(.expected_cols %in% names(x)))
  expect_identical(attr(x, "sf_column"), "geojson")
  expect_true(sf::st_crs(x) == sf::st_crs(4326))

  # Non-empty: guards every all() below against vacuous truth
  expect_true(nrow(x) > 0)

  # Filters
  expect_true(all(x$time_left >= tl & x$time_right <= tr))
  expect_true(all(x$observation_collection_id %in% as.character(uids)))
  expect_true(all(startsWith(x$location_name, "AFR::ETH")))

  # One row per observation
  expect_false(anyNA(x$id))
  expect_identical(anyDuplicated(x$id), 0L)
})

test_that("integration: several uids, no time filter", {
  skip_if_no_db()
  uids <- c(314, 21041, 21136)
  w <- capture_warnings(
    x <- read_taxonomy_data_sql(.db_user, .db_pass, locations = 14, uids = uids)
  )
  expect_equal(sum(grepl("^No time filters", w)), 2)
  expect_true(nrow(x) > 0)
  expect_true(all(x$observation_collection_id %in% as.character(uids)))
  expect_identical(anyDuplicated(x$id), 0L)
})

test_that("integration: empty result stops with a query error, not an argument error", {
  skip_if_no_db()
  err <- tryCatch(
    read_taxonomy_data_sql(.db_user, .db_pass, locations = 14, uids = 21136,
                           time_left = "2100-01-01", time_right = "2100-12-31"),
    error = function(e) e)
  expect_match(conditionMessage(err), "No observations found")
  expect_false(inherits(err, "taxdat_arg_error"))
})
