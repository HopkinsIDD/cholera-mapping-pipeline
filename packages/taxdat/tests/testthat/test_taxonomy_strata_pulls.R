# Tests for the stratified pulls (read_taxonomy_age_sql, read_taxonomy_sex_sql,
# read_taxonomy_age_sex_sql) and marginalize_strata().
#
# A. No database needed
#    A1. Argument validation of the three wrappers. Invalid calls must fail
#        with class "taxdat_arg_error" BEFORE any connection attempt; valid
#        calls target an unresolvable host (reserved ".invalid" TLD, RFC 6761)
#        and fail at connection time with another class. Same contract as
#        read_taxonomy_data_sql.
#    A1b. Aggregation arguments of the age and sex wrappers (aggregate,
#        na_counts, drop_exact_duplicates, drop_superseded_strata, metadata).
#    A2. marginalize_strata() on small SYNTHETIC data frames: every value is
#        invented (fictional location, ids 1, 2, 3...), no real observation.
#    A2b. Superseded strata (drop_superseded_strata) and the flags raised when
#        a full row differs from the sum of its strata, on a synthetic
#        collection.
#    A3. The age and sex wrappers with the database engine replaced by a mock
#        replaying that synthetic collection: defaults, aggregate = FALSE,
#        routing and wiring to marginalize_strata().
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
# A1b. Aggregation arguments of the age and sex wrappers (no database)
# ============================================================================
# read_taxonomy_age_sex_sql has no aggregation argument: it always returns the
# raw observations.

.agg_wrappers <- .wrappers[c("read_taxonomy_age_sql", "read_taxonomy_sex_sql")]

for (nm in names(.agg_wrappers)) {
  fn <- .agg_wrappers[[nm]]

  test_that(paste0(nm, ": aggregation flags are validated"), {
    for (arg in c("aggregate", "drop_exact_duplicates", "drop_superseded_strata", "metadata")) {
      for (bad in list("TRUE", NA, c(TRUE, FALSE), 1)) {
        args <- stats::setNames(list(bad), arg)
        expect_error(suppressWarnings(do.call(.call_fake, c(list(fn), args))),
                     regexp = arg, class = "taxdat_arg_error")
      }
    }
    # na_counts goes through match.arg(), also before any connection
    expect_error(suppressWarnings(.call_fake(fn, na_counts = "bad")), "should be one of")
    expect_passes_validation(fn, na_counts = "strict")
    expect_passes_validation(fn, aggregate = FALSE)
    expect_passes_validation(fn, drop_superseded_strata = FALSE, metadata = TRUE)
  })

  test_that(paste0(nm, ": aggregation options are ignored, visibly, when aggregate = FALSE"), {
    msg <- "ignored when aggregate = FALSE"
    expect_true(any(grepl(msg, .warnings_of(fn, aggregate = FALSE, drop_exact_duplicates = TRUE))))
    expect_true(any(grepl(msg, .warnings_of(fn, aggregate = FALSE, metadata = TRUE))))
    # drop_superseded_strata defaults to TRUE: only warn when the caller sets it
    expect_true(any(grepl(msg, .warnings_of(fn, aggregate = FALSE, drop_superseded_strata = TRUE))))
    expect_length(.warnings_of(fn, aggregate = FALSE), 0)
    expect_length(.warnings_of(fn, aggregate = FALSE, drop_superseded_strata = FALSE), 0)
    expect_length(.warnings_of(fn), 0)               # aggregate = TRUE defaults
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
# A2b. Superseded strata, on a synthetic collection "X" (no database)
# ============================================================================
# Total 95 = M 60 + F 35 = <5 15 + >5 80; cells: M<5 10, F<5 5, M>5 50, F>5 30.
# Age < 5 is stored as age_r = "5", age > 5 as age_l = "5" (the notation used by
# the rule is "compared as stored"). All values are invented.

.oc_x <- function() {
  .make_strata_df(
    suspected_cases = c(95L, 60L, 35L, 15L, 80L, 10L, 5L, 50L, 30L),
    age_l = c(NA, NA, NA, NA, "5", NA, NA, "5", "5"),
    age_r = c(NA, NA, NA, "5", NA, "5", "5", NA, NA),
    sex   = c(NA, 1L, 0L, NA, NA, 1L, 0L, 1L, 0L))
}

# Rows returned by each pull (the row filters of the wrappers, replicated in R)
.rows_of_pull <- function(df, pull = c("age", "sex", "both")) {
  pull    <- match.arg(pull)
  has_age <- !(is.na(df$age_l) & is.na(df$age_r) & is.na(df$age))
  has_sex <- !is.na(df$sex_raw)
  switch(pull, age = df[has_age, ], sex = df[has_sex, ], both = df[has_age & has_sex, ])
}
# Suspected cases of the "<5" (age_r) or ">5" (age_l) row of an age table
.band <- function(out, which = c("<5", ">5")) {
  which <- match.arg(which)
  out$suspected_cases[if (which == "<5") !is.na(out$age_r) else !is.na(out$age_l)]
}
.sex_val <- function(out, s) out$suspected_cases[!is.na(out$sex) & out$sex == s]

test_that("the synthetic collection X has the expected pulls", {
  oc <- .oc_x()
  expect_identical(nrow(.rows_of_pull(oc, "age")),  6L)
  expect_identical(nrow(.rows_of_pull(oc, "sex")),  6L)
  expect_identical(nrow(.rows_of_pull(oc, "both")), 4L)
  # the total (no age, no sex) is in none of the pulls
  expect_false("1" %in% c(.rows_of_pull(oc, "age")$id, .rows_of_pull(oc, "sex")$id))
})

test_that("drop_superseded_strata is OFF by default in marginalize_strata (double counting)", {
  oc <- .oc_x()
  a  <- .marg(.rows_of_pull(oc, "age"), over = "sex")
  s  <- .marg(.rows_of_pull(oc, "sex"), over = "age")
  expect_identical(c(.band(a, "<5"), .band(a, ">5")), c(30L, 160L))   # total + strata
  expect_identical(c(.sex_val(s, 1L), .sex_val(s, 0L)), c(120L, 70L))
})

test_that("a full row supersedes its strata: age table (summed over sex)", {
  w <- capture_warnings(out <- .marg(.rows_of_pull(.oc_x(), "age"), over = "sex",
                                     drop_superseded_strata = TRUE))
  expect_length(w, 0)
  expect_identical(nrow(out), 2L)
  expect_identical(.band(out, "<5"), 15L)
  expect_identical(.band(out, ">5"), 80L)
  expect_identical(sum(out$suspected_cases), 95L)
})

test_that("a full row supersedes its strata: sex table (summed over age)", {
  w <- capture_warnings(out <- .marg(.rows_of_pull(.oc_x(), "sex"), over = "age",
                                     drop_superseded_strata = TRUE))
  expect_length(w, 0)
  expect_identical(nrow(out), 2L)
  expect_identical(.sex_val(out, 1L), 60L)
  expect_identical(.sex_val(out, 0L), 35L)
})

test_that("age x sex table: the four cells, no duplicated group", {
  out <- .marg(.rows_of_pull(.oc_x(), "both"), over = "none")
  expect_identical(nrow(out), 4L)
  expect_identical(sort(out$suspected_cases), c(5L, 10L, 30L, 50L))
  key <- paste(out$age_l, out$age_r, out$sex)
  expect_false(anyDuplicated(key) > 0)
})

test_that("strata are superseded only by a row equal on every other field", {
  # Rows 6:9 (the strata) differ from the totals on ONE field at a time:
  # nothing is superseded, so the rule changes nothing.
  changes <- list(primary = TRUE, time_left = as.Date("2020-02-01"), location_id = "101",
                  location_period_id = "2000", observation_collection_id = "2",
                  status = "complete1", is_public = FALSE, unified = TRUE)
  for (field in names(changes)) {
    oc <- .oc_x()
    oc[6:9, field] <- changes[[field]]
    age <- .rows_of_pull(oc, "age")
    on  <- .marg(age, over = "sex", drop_superseded_strata = TRUE)
    off <- .marg(age, over = "sex")
    expect_identical(nrow(on), 4L, info = field)
    expect_identical(sum(on$suspected_cases), 190L, info = field)  # 95 * 2: nothing discarded
    expect_equal(on$suspected_cases, off$suspected_cases, info = field)
  }
  # Different age: the >5 total does not supersede strata of another age
  oc <- .oc_x(); oc$age_l[c(8, 9)] <- "10"
  on <- .marg(.rows_of_pull(oc, "age"), over = "sex", drop_superseded_strata = TRUE)
  expect_identical(nrow(on), 3L)
  expect_identical(sum(on$suspected_cases), 15L + 80L + 80L)
})

test_that("phantom is not part of the rule key and leaves no parasite group", {
  oc <- .oc_x(); oc$phantom[6:9] <- TRUE           # strata phantom, totals not
  out <- .marg(.rows_of_pull(oc, "age"), over = "sex", drop_superseded_strata = TRUE)
  expect_identical(nrow(out), 2L)
  expect_false(anyNA(out$suspected_cases))
  expect_identical(c(.band(out, "<5"), .band(out, ">5")), c(15L, 80L))
})

test_that("an unrecognised sex is never matched in the sex table", {
  oc <- rbind(.oc_x(),
              .make_strata_df(suspected_cases = c(7L, 3L), age_r = c(NA, "5"),
                              sex_raw = c("X", "X"), id = c("10", "11")))
  w <- capture_warnings(out <- .marg(.rows_of_pull(oc, "sex"), over = "age",
                                     drop_superseded_strata = TRUE))
  expect_length(w, 0)
  expect_identical(nrow(out), 4L)                  # F, M, and the two X rows kept apart
  expect_identical(sort(out$suspected_cases[is.na(out$sex)]), c(3L, 7L))
})

test_that("strata below their full row: flagged, not 'exceed', total kept", {
  oc <- .oc_x(); oc$suspected_cases[8] <- 40L      # >5 strata sum 70 < 80
  w <- capture_warnings(out <- .marg(.rows_of_pull(oc, "age"), over = "sex",
                                     drop_superseded_strata = TRUE, metadata = TRUE))
  expect_length(w, 1)
  expect_match(w, "differs from the sum of its discarded strata")
  expect_identical(.band(out, ">5"), 80L)          # the full row is kept, nothing corrected
  expect_identical(out$superseded_mismatch[!is.na(out$age_l)], TRUE)
  expect_identical(out$superseded_mismatch[!is.na(out$age_r)], FALSE)
  expect_false(any(out$superseded_exceed))
})

test_that("strata above their full row: flagged 'exceed', OC named in the warning", {
  oc <- .oc_x(); oc$suspected_cases[8] <- 60L      # >5 strata sum 90 > 80
  w <- capture_warnings(out <- .marg(.rows_of_pull(oc, "age"), over = "sex",
                                     drop_superseded_strata = TRUE, metadata = TRUE))
  expect_length(w, 1)
  expect_match(w, "OC 1: 1 group\\(s\\), 1 with strata above the full row")
  expect_identical(out$superseded_exceed[!is.na(out$age_l)], TRUE)
  expect_identical(out$superseded_exceed[!is.na(out$age_r)], FALSE)
  expect_identical(.band(out, ">5"), 80L)          # still kept
  # sex table: male 10 + 60 > 60
  w <- capture_warnings(sx <- .marg(.rows_of_pull(oc, "sex"), over = "age",
                                    drop_superseded_strata = TRUE, metadata = TRUE))
  expect_length(w, 1)
  expect_identical(sx$superseded_exceed[sx$sex == 1L], TRUE)
  expect_identical(sx$superseded_exceed[sx$sex == 0L], FALSE)
})

test_that("a full row of unknown sex next to ONE sex stratum above it is flagged", {
  df <- .make_strata_df(age_l = c("45", "45"), sex = c(NA, 0L),
                        suspected_cases = c(18L, 30L))
  w <- capture_warnings(out <- .marg(df, over = "sex", drop_superseded_strata = TRUE,
                                     metadata = TRUE))
  expect_length(w, 1)
  expect_identical(nrow(out), 1L)
  expect_identical(out$suspected_cases, 18L)       # total kept, stratum discarded
  expect_identical(out$n_superseded_dropped, 1L)   # a single stratum: one sex only
  expect_true(out$superseded_exceed)
})

test_that("a consistent collection raises no flag", {
  out <- .marg(.rows_of_pull(.oc_x(), "age"), over = "sex",
               drop_superseded_strata = TRUE, metadata = TRUE)
  expect_false(any(out$superseded_mismatch))
  expect_false(any(out$superseded_exceed))
})

test_that("exact duplicate of a total: drop_exact_duplicates keeps one copy", {
  oc <- .oc_x(); oc <- rbind(oc, oc[4, ]); oc$id[10] <- "10"
  oc$potential_duplicate[c(4, 10)] <- TRUE; oc$n_identical_obs[c(4, 10)] <- 2L
  age <- .rows_of_pull(oc, "age")
  # Not dropped: the doubled total is visible (30) and flagged, never hidden
  w <- capture_warnings(out <- .marg(age, over = "sex", drop_superseded_strata = TRUE))
  expect_identical(.band(out, "<5"), 30L)
  expect_length(w, 1)
  # Dropped: back to 15, consistent with its strata, no warning
  w <- capture_warnings(out <- .marg(age, over = "sex", drop_superseded_strata = TRUE,
                                     drop_exact_duplicates = TRUE))
  expect_identical(.band(out, "<5"), 15L)
  expect_length(w, 0)
})

test_that("superseded columns are part of the metadata output", {
  out <- .marg(.rows_of_pull(.oc_x(), "age"), over = "sex",
               drop_superseded_strata = TRUE, metadata = TRUE)
  expect_true(all(c("n_superseded_dropped", "superseded_mismatch", "superseded_exceed")
                  %in% names(out)))
  expect_identical(sum(out$n_superseded_dropped), 4L)
})

test_that("drop_superseded_strata is ignored, with a warning, for over = 'none'", {
  w <- capture_warnings(.marg(.rows_of_pull(.oc_x(), "both"), over = "none",
                              drop_superseded_strata = TRUE))
  expect_true(any(grepl("not defined for over = 'none'", w)))
})

test_that("sf input with the superseded rule: geometry is re-attached", {
  age  <- .rows_of_pull(.oc_x(), "age")
  geoms <- sf::st_sfc(lapply(seq_len(nrow(age)), function(i) sf::st_point(c(0, 0))), crs = 4326)
  out  <- .marg(sf::st_sf(age, geojson = geoms), over = "sex", drop_superseded_strata = TRUE)
  expect_s3_class(out, "sf")
  expect_identical(attr(out, "sf_column"), "geojson")
  expect_identical(nrow(out), 2L)
  expect_identical(sort(out$suspected_cases), c(15L, 80L))
})

# ============================================================================
# A3. The age and sex wrappers on a mocked engine (no database)
# ============================================================================
# .read_taxonomy_strata_sql() (database access, validation) is replaced by a
# function replaying the synthetic collection X with the row filter of each
# pull. This tests the wiring: defaults, aggregate = FALSE, routing to the right
# table, arguments passed to marginalize_strata().

test_that("age and sex wrappers aggregate by default and return raw rows on request", {
  skip_if(utils::packageVersion("testthat") < "3.1.7",
          "local_mocked_bindings() needs testthat >= 3.1.7")
  oc <- .oc_x()
  testthat::local_mocked_bindings(
    .read_taxonomy_strata_sql = function(..., label, include_geojson = FALSE) {
      .rows_of_pull(oc, c(age = "age", sex = "sex", `age and sex` = "both")[[label]])
    },
    .package = "taxdat")

  # Defaults: aggregated, superseded strata dropped
  a <- .call_fake(read_taxonomy_age_sql)
  s <- .call_fake(read_taxonomy_sex_sql)
  expect_identical(c(.band(a, "<5"), .band(a, ">5")), c(15L, 80L))
  expect_identical(c(.sex_val(s, 1L), .sex_val(s, 0L)), c(60L, 35L))
  expect_false("sex" %in% names(a))                # summed over sex
  expect_false("age_l" %in% names(s))              # summed over age

  # aggregate = FALSE: the raw rows, with the stratified-pull contract
  ra <- .call_fake(read_taxonomy_age_sql, aggregate = FALSE)
  rs <- .call_fake(read_taxonomy_sex_sql, aggregate = FALSE)
  expect_identical(c(nrow(ra), nrow(rs)), c(6L, 6L))
  expect_true(all(.strata_cols %in% names(ra)))
  expect_true(all(.strata_cols %in% names(rs)))
  # ... which feed marginalize_strata() directly
  expect_equal(.marg(ra, over = "sex", drop_superseded_strata = TRUE), a)
  expect_equal(.marg(rs, over = "age", drop_superseded_strata = TRUE), s)

  # drop_superseded_strata = FALSE: total + strata are added (double counting)
  a0 <- .call_fake(read_taxonomy_age_sql, drop_superseded_strata = FALSE)
  s0 <- .call_fake(read_taxonomy_sex_sql, drop_superseded_strata = FALSE)
  expect_identical(c(.band(a0, "<5"), .band(a0, ">5")), c(30L, 160L))
  expect_identical(c(.sex_val(s0, 1L), .sex_val(s0, 0L)), c(120L, 70L))

  # metadata is passed through
  expect_true("superseded_mismatch" %in% names(.call_fake(read_taxonomy_age_sql, metadata = TRUE)))

  # The age x sex wrapper never aggregates
  b <- .call_fake(read_taxonomy_age_sex_sql)
  expect_identical(nrow(b), 4L)
  expect_true(all(.strata_cols %in% names(b)))
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

.pull <- function(fn, include_geojson = FALSE, aggregate = FALSE) {
  args <- list(.db_user, .db_pass, locations = .int_location,
               uids = .int_uids, include_geojson = include_geojson)
  # The age and sex wrappers aggregate by default. The contract tests below need
  # the raw rows, so they are requested explicitly (read_taxonomy_age_sex_sql has
  # no such argument and always returns raw rows).
  if ("aggregate" %in% names(formals(fn))) args$aggregate <- aggregate
  suppressWarnings(do.call(fn, args))
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

for (nm in names(.agg_wrappers)) {
  fn   <- .agg_wrappers[[nm]]
  over <- if (nm == "read_taxonomy_age_sql") "sex" else "age"

  test_that(paste0(nm, ": integration, aggregated pull == marginalize_strata of the raw pull"), {
    skip_if_no_db()
    raw <- .pull(fn)                               # aggregate = FALSE
    agg <- .pull(fn, aggregate = TRUE)             # defaults of the wrapper
    ref <- suppressWarnings(.marg(raw, over = over, drop_superseded_strata = TRUE))
    expect_true(isTRUE(all.equal(agg, ref)))
    expect_true(nrow(agg) <= nrow(raw))
    # Discarding superseded strata can only lower the totals (compared, never printed)
    for (cc in c("suspected_cases", "confirmed_cases", "deaths")) {
      expect_true(sum(agg[[cc]], na.rm = TRUE) <= sum(raw[[cc]], na.rm = TRUE))
    }
  })
}