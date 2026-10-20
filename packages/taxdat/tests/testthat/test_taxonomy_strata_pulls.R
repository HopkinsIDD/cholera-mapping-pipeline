# Tests for the stratified pulls (read_taxonomy_age_sql, read_taxonomy_sex_sql,
# read_taxonomy_age_sex_sql) and marginalize_strata().
#
# A. No database needed
#    A1. Argument validation of the three wrappers. Invalid calls must fail
#        with class "taxdat_arg_error" BEFORE any connection attempt; valid
#        calls target an unresolvable host (reserved ".invalid" TLD, RFC 6761)
#        and fail at connection time with another class. Same contract as
#        read_taxonomy_data_sql.
#    A2. marginalize_strata() on small SYNTHETIC data frames: every value is
#        invented (fictional location, ids 1, 2, 3...), no real observation.
# B. Integration -- real database, read-only.
#    Skipped unless credentials are set as environment variables AND an
#    integration scope is set below; never run on CI. Assertions are boolean
#    invariants only, so that a failure never prints observation data.
#
# No credential and no real data are stored in this file.

# ============================================================================
# A1. Argument validation of the three wrappers (no database)
# ============================================================================

.fake_host <- "taxdat-test.invalid"

.wrappers <- list(
  read_taxonomy_age_sql     = read_taxonomy_age_sql,
  read_taxonomy_sex_sql     = read_taxonomy_sex_sql,
  read_taxonomy_age_sex_sql = read_taxonomy_age_sex_sql
)

# Call `fn` with dummy credentials, valid defaults and the fake host.
# Arguments in `...` override the defaults; utils::modifyList DROPS an element
# set to NULL, so `x = NULL` falls back to the function's own default.
.call_fake <- function(fn, ...) {
  args <- utils::modifyList(
    list(username = "dummy", password = "dummy", locations = 14,
         time_left = "2019-01-01", time_right = "2019-12-31", uids = 1,
         host = .fake_host),
    list(...)
  )
  do.call(fn, args)
}

# Expect an argument error (raised before connecting)
expect_arg_error <- function(fn, ..., regexp = NULL) {
  expect_error(suppressWarnings(.call_fake(fn, ...)),
               regexp = regexp, class = "taxdat_arg_error")
}

# Expect the call to pass validation: it must then fail at connection time
expect_passes_validation <- function(fn, ...) {
  err <- tryCatch(suppressWarnings(.call_fake(fn, ...)), error = function(e) e)
  expect_s3_class(err, "error")                    # fake host: must not connect
  expect_false(inherits(err, "taxdat_arg_error"))  # ...but not an argument error
}

# Warnings emitted before the (failing) connection attempt
.warnings_of <- function(fn, ...) {
  capture_warnings(try(.call_fake(fn, ...), silent = TRUE))
}

for (nm in names(.wrappers)) {
  fn <- .wrappers[[nm]]

  test_that(paste0(nm, ": credentials are validated"), {
    expect_error(fn(password = "x", locations = 14),
                 class = "taxdat_arg_error")           # username missing
    expect_error(fn(username = "x", locations = 14),
                 class = "taxdat_arg_error")           # password missing
    expect_arg_error(fn, username = NA_character_)
    expect_arg_error(fn, username = "")
    expect_arg_error(fn, username = c("a", "b"))
    expect_arg_error(fn, username = 123)
    expect_arg_error(fn, password = NA_character_)
    expect_arg_error(fn, password = "", regexp = "only allowed with host = 'localhost'")
    # Empty password + localhost is a valid call (local socket)
    expect_passes_validation(fn, password = "", host = "localhost")
  })

  test_that(paste0(nm, ": locations are validated"), {
    expect_arg_error(fn, locations = NULL, regexp = "can't be NULL")
    expect_arg_error(fn, locations = "AFR::ETH", regexp = "not yet implemented")
    for (bad in list(NA_real_, 0, -1, 1.5, Inf, numeric(0), c(14, NA))) {
      expect_arg_error(fn, locations = bad, regexp = "positive whole numbers")
    }
    expect_passes_validation(fn, locations = c(14, 15))
  })

  test_that(paste0(nm, ": uids are validated"), {
    for (bad in list(31.4, -314, 0, NA_real_, "abc", "-1", TRUE, numeric(0))) {
      expect_arg_error(fn, uids = bad, regexp = "positive whole ids")
    }
    expect_passes_validation(fn, uids = 314)
    expect_passes_validation(fn, uids = c(314, 21041))
    expect_passes_validation(fn, uids = "314")
  })

  test_that(paste0(nm, ": dates are validated"), {
    for (bad in list("200-01-01", "04/03/2017", "2017-02-30", 20190101,
                     c("2019-01-01", "2019-02-01"), NA, as.Date(NA))) {
      expect_arg_error(fn, time_left = bad)
    }
    expect_arg_error(fn, time_left = "2000-01-01", time_right = "1999-01-01",
                     regexp = "is after")
    expect_arg_error(fn, time_left = as.Date("2000-01-01"),
                     time_right = as.Date("1999-01-01"), regexp = "is after")
    expect_passes_validation(fn, time_left = as.Date("2019-01-01"))
    expect_passes_validation(fn, time_left = as.POSIXct("2019-01-01", tz = "UTC"))
    expect_passes_validation(fn, time_left = "2019-06-01", time_right = "2019-06-01")
  })

  test_that(paste0(nm, ": flags are validated"), {
    for (bad in list("TRUE", NA, c(TRUE, FALSE))) {
      expect_arg_error(fn, discard_incomplete_observation_collections = bad)
    }
    for (bad in list("TRUE", NA, c(TRUE, FALSE), 1)) {
      expect_arg_error(fn, include_geojson = bad, regexp = "include_geojson")
    }
    expect_passes_validation(fn, include_geojson = TRUE)
    w <- .warnings_of(fn, unified_dataset_behaviour = "Drop")
    expect_true(any(grepl("unified-dataset filter is disabled", w)))
    expect_length(.warnings_of(fn, unified_dataset_behaviour = "keep"), 0)
  })

  test_that(paste0(nm, ": missing filters raise the documented warnings"), {
    w <- .warnings_of(fn, time_left = NULL, time_right = NULL)
    expect_equal(sum(grepl("^No time filters", w)), 2)
    w <- .warnings_of(fn, time_right = NULL)
    expect_equal(sum(grepl("^No time filters", w)), 1)
    w <- .warnings_of(fn, uids = NULL)
    expect_true(any(grepl("^No uid filters", w)))
    expect_length(.warnings_of(fn), 0)               # all filters given
  })
}

# ============================================================================
# A2. marginalize_strata() on synthetic data (no database)
# ============================================================================

# Columns of a stratified pull (the contract checked by marginalize_strata)
.strata_cols <- c("id", "observation_collection_id", "is_public", "unified", "status",
                  "time_left", "time_right", "suspected_cases", "confirmed_cases", "deaths",
                  "phantom", "primary", "age_l", "age_r", "age", "sex", "sex_raw",
                  "location_name", "location_via_fallback", "location_id",
                  "location_period_id", "n_identical_obs", "potential_duplicate")

# Build a synthetic stratified pull: columns given in `...` vary per row,
# every other column takes a fixed default. All values are invented.
.make_strata_df <- function(...) {
  varying <- data.frame(..., stringsAsFactors = FALSE)
  n <- nrow(varying)
  defaults <- list(
    id = as.character(seq_len(n)), observation_collection_id = "1",
    is_public = TRUE, unified = FALSE, status = "verified",
    time_left = as.Date("2020-01-01"), time_right = as.Date("2020-12-31"),
    suspected_cases = 1L, confirmed_cases = NA_integer_, deaths = 0L,
    phantom = FALSE, primary = FALSE,
    age_l = NA_character_, age_r = NA_character_, age = NA_character_,
    sex = NA_integer_, sex_raw = NA_character_,
    location_name = "TST::FICTIONAL", location_via_fallback = FALSE,
    location_id = "100", location_period_id = "1000",
    n_identical_obs = 1L, potential_duplicate = FALSE
  )
  df <- as.data.frame(lapply(defaults, rep, length.out = n), stringsAsFactors = FALSE)
  df[names(varying)] <- varying
  # Raw sex value consistent with the recoded one, unless given explicitly
  if ("sex" %in% names(varying) && !"sex_raw" %in% names(varying)) {
    df$sex_raw <- ifelse(is.na(df$sex), NA_character_, as.character(df$sex))
  }
  df
}

.marg <- function(...) suppressMessages(marginalize_strata(...))

test_that("synthetic builder matches the stratified-pull contract", {
  expect_true(all(.strata_cols %in% names(.make_strata_df(age = "5"))))
})

test_that("over = 'sex' sums the sexes within each age", {
  df  <- .make_strata_df(age = c("5", "5", "10"), sex = c(1L, 0L, 1L),
                         suspected_cases = c(2L, 3L, 1L))
  out <- .marg(df, over = "sex")
  expect_false(inherits(out, "sf"))
  expect_identical(nrow(out), 2L)
  expect_identical(out$suspected_cases[out$age == "5"], 5L)
  expect_identical(out$suspected_cases[out$age == "10"], 1L)
  expect_false("sex" %in% names(out))              # summed-over variable dropped
  expect_identical(sum(out$suspected_cases), sum(df$suspected_cases))
})

test_that("over = 'age' sums the ages within each sex", {
  df  <- .make_strata_df(age = c("5", "10", "10"), sex = c(1L, 1L, 0L),
                         suspected_cases = c(2L, 3L, 4L))
  out <- .marg(df, over = "age")
  expect_identical(nrow(out), 2L)
  expect_identical(out$suspected_cases[out$sex == 1L], 5L)
  expect_identical(out$suspected_cases[out$sex == 0L], 4L)
  expect_false(any(c("age_l", "age_r", "age") %in% names(out)))
})

test_that("over = 'none' merges only rows identical on age AND sex", {
  df  <- .make_strata_df(age = c("5", "5", "5"), sex = c(1L, 1L, 0L))
  out <- .marg(df, over = "none")
  expect_identical(nrow(out), 2L)
  expect_identical(out$suspected_cases[out$sex == 1L], 2L)
  expect_identical(out$suspected_cases[out$sex == 0L], 1L)
  expect_true(all(c("age", "sex") %in% names(out)))
})

test_that("missing kept variable: never merged; missing summed variable: merged", {
  # No age at all: each row stays alone in the age table
  df  <- .make_strata_df(age = c(NA, NA), sex = c(1L, 1L))
  expect_identical(nrow(.marg(df, over = "sex")), 2L)
  # Unknown sex still counts in its age
  df  <- .make_strata_df(age = c("5", "5"), sex = c(1L, NA))
  out <- .marg(df, over = "sex")
  expect_identical(nrow(out), 1L)
  expect_identical(out$suspected_cases, 2L)
})

test_that("na_counts: 'as_zero' vs 'strict'", {
  df <- .make_strata_df(age = c("5", "5"),
                        suspected_cases = c(2L, NA), confirmed_cases = c(NA, NA))
  z <- .marg(df, over = "sex", na_counts = "as_zero")
  s <- .marg(df, over = "sex", na_counts = "strict")
  expect_identical(z$suspected_cases, 2L)          # NA counted as 0
  expect_true(is.na(s$suspected_cases))            # any NA -> NA
  expect_true(is.na(z$confirmed_cases))            # all NA -> NA in both modes
  expect_true(is.na(s$confirmed_cases))
  zm <- .marg(df, over = "sex", metadata = TRUE)
  expect_identical(zm$na_as_zero, "suspected_cases:1")
})

test_that("drop_exact_duplicates keeps one copy of flagged duplicates", {
  df <- .make_strata_df(age = c("5", "5"), sex = c(1L, 1L),
                        suspected_cases = c(5L, 5L), n_identical_obs = c(2L, 2L),
                        potential_duplicate = c(TRUE, TRUE))
  expect_identical(.marg(df, over = "sex")$suspected_cases, 10L)
  kept <- .marg(df, over = "sex", drop_exact_duplicates = TRUE, metadata = TRUE)
  expect_identical(kept$suspected_cases, 5L)
  expect_identical(kept$n_duplicates_dropped, 1L)
})

test_that("metadata = TRUE adds traceability columns", {
  df  <- .make_strata_df(id = c("2", "10"), age = c("5", "5"), sex = c(1L, 0L))
  out <- .marg(df, over = "sex", metadata = TRUE)
  expect_true(all(c("n_source_rows", "sex_values", "na_as_zero", "potential_duplicate",
                    "n_duplicate_sources", "n_duplicates_dropped", "source_ids")
                  %in% names(out)))
  expect_false("sex_raw_values" %in% names(out))   # not kept in the age table
  expect_identical(out$n_source_rows, 2L)
  expect_identical(out$sex_values, "0,1")
  expect_identical(out$source_ids, "2,10")         # numeric order, not text order
})

test_that("invalid inputs are rejected", {
  df <- .make_strata_df(age = c("5", "5"))
  expect_error(.marg(df[, setdiff(names(df), "age")], over = "sex"), "not a stratified pull")
  dup <- df; dup$id <- c("1", "1")
  expect_error(.marg(dup, over = "sex"), "duplicated ids")
  expect_error(.marg(df, over = "both"))
})

test_that("a summary message is always emitted", {
  df <- .make_strata_df(age = "5")
  expect_message(marginalize_strata(df, over = "sex"),
                 "marginalize_strata\\(over = 'sex'\\)")
})

test_that("sf input: one geometry per location period is re-attached", {
  df <- .make_strata_df(age = c("5", "5", "5"), sex = c(1L, 0L, 1L),
                        location_period_id = c("1000", "1000", "2000"),
                        suspected_cases = c(1L, 2L, 4L))
  geoms <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(0, 0)),
                      sf::st_point(c(1, 1)), crs = 4326)
  out <- .marg(sf::st_sf(df, geojson = geoms), over = "sex")

  expect_s3_class(out, "sf")
  expect_identical(attr(out, "sf_column"), "geojson")
  expect_true(sf::st_crs(out) == sf::st_crs(4326))
  expect_identical(nrow(out), 2L)                  # location period is in the key
  xy_1000 <- sf::st_coordinates(out[which(out$location_period_id == "1000"), ])
  xy_2000 <- sf::st_coordinates(out[which(out$location_period_id == "2000"), ])
  expect_equal(unname(xy_1000[1, c("X", "Y")]), c(0, 0))
  expect_equal(unname(xy_2000[1, c("X", "Y")]), c(1, 1))
  # Same counts as the data.frame route
  out_df <- .marg(df, over = "sex")
  expect_identical(sort(out$suspected_cases), sort(out_df$suspected_cases))
})

test_that("sf input: groups without location period get an empty geometry", {
  df <- .make_strata_df(age = c("5", "10"), location_period_id = c("1000", NA))
  geoms <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 4326)
  out <- .marg(sf::st_sf(df, geojson = geoms), over = "sex")
  expect_true(all(sf::st_is_empty(out)[is.na(out$location_period_id)]))
  expect_false(any(sf::st_is_empty(out)[!is.na(out$location_period_id)]))
})

# ============================================================================
# B. Integration (real database, read-only)
# ============================================================================

# Names of the environment variables exported by load_cholera_env.sh
.db_user <- Sys.getenv("CHOLERA_USER")
.db_pass <- Sys.getenv("CHOLERA_PASSWORD")

# Integration scope: a location id and an observation collection that contain
# BOTH age and sex data (e.g. the collection used for pre-validation).
# Left as NA, the integration tests are skipped.
.int_location <- 635
.int_uids     <- 21542

skip_if_no_db <- function() {
  skip_on_ci()
  skip_on_cran()
  skip_if(!nzchar(.db_user) || !nzchar(.db_pass),
          "Taxonomy DB credentials not set in the environment")
  skip_if(is.na(.int_location) || anyNA(.int_uids),
          "Integration scope (.int_location / .int_uids) not set")
}

.pull <- function(fn, include_geojson = FALSE) {
  suppressWarnings(fn(.db_user, .db_pass, locations = .int_location,
                      uids = .int_uids, include_geojson = include_geojson))
}

for (nm in names(.wrappers)) {
  fn <- .wrappers[[nm]]

  test_that(paste0(nm, ": integration contract and row filter"), {
    skip_if_no_db()
    x <- .pull(fn)
    expect_false(inherits(x, "sf"))                # default: plain data.frame
    expect_true(nrow(x) > 0)                       # guards all() below
    expect_true(all(.strata_cols %in% names(x)))
    expect_false(anyNA(x$id))
    expect_identical(anyDuplicated(x$id), 0L)
    expect_true(all(x$observation_collection_id %in% as.character(.int_uids)))
    expect_true(all(is.na(x$sex) | x$sex %in% c(0, 1)))
    has_age <- !is.na(x$age_l) | !is.na(x$age_r) | !is.na(x$age)
    has_sex <- !is.na(x$sex_raw)
    if (nm != "read_taxonomy_sex_sql") expect_true(all(has_age))
    if (nm != "read_taxonomy_age_sql") expect_true(all(has_sex))
  })

  test_that(paste0(nm, ": include_geojson = TRUE returns the same rows as sf"), {
    skip_if_no_db()
    x  <- .pull(fn, include_geojson = FALSE)
    xg <- .pull(fn, include_geojson = TRUE)
    expect_s3_class(xg, "sf")
    expect_identical(attr(xg, "sf_column"), "geojson")
    expect_true(sf::st_crs(xg) == sf::st_crs(4326))
    expect_identical(xg$id, x$id)                  # same rows, same order
    expect_true(all(sf::st_is_empty(xg)[is.na(xg$location_period_id)]))
  })
}

test_that("integration: marginalize_strata conserves counts, with and without geometry", {
  skip_if_no_db()
  x    <- .pull(read_taxonomy_age_sql, include_geojson = FALSE)
  xg   <- .pull(read_taxonomy_age_sql, include_geojson = TRUE)
  out  <- .marg(x,  over = "sex")
  outg <- .marg(xg, over = "sex")
  # Totals preserved (compared, never printed)
  for (cc in c("suspected_cases", "confirmed_cases", "deaths")) {
    expect_true(isTRUE(all.equal(sum(out[[cc]], na.rm = TRUE),
                                 sum(x[[cc]],   na.rm = TRUE))))
  }
  # Geometry route gives the same table
  expect_s3_class(outg, "sf")
  expect_identical(nrow(outg), nrow(out))
  expect_true(isTRUE(all.equal(sf::st_drop_geometry(outg)$suspected_cases,
                               out$suspected_cases)))
})
