## testing for snap_to_time_period.R

test_that("compute_tfrac works", { 
  
  # Basic tfrac computations
  TL <- as.Date("2020-01-01")
  TR <- as.Date("2020-12-31")
  TR2 <- as.Date("2020-07-01")
  TR3 <- as.Date("2020-01-01")
  
  
  # One full period
  tfrac_full <- compute_tfrac(TL, TR, TL, TR)
  # Half a period
  tfrac_half <- compute_tfrac(TL, TR2, TL, TR)
  # One day of one year
  tfrac_oneday <- compute_tfrac(TL, TR3, TL, TR)
  # no overlap
  tfrac_no_overlap <- compute_tfrac(as.Date("2019-01-01"), as.Date("2019-12-31"), TL, TR)
  
  mytol <- 1e-5
  
  expect_equal(tfrac_full, 1, tolerance = mytol)
  expect_equal(tfrac_half, .5, tolerance = mytol)
  expect_equal(tfrac_oneday, 1/365, tolerance = mytol)
  expect_identical(tfrac_no_overlap, 0)
  
})



test_that("snap_to_time_period works", {
  TL <- as.Date("2020-01-01")
  TR <- as.Date("2020-12-31")
  res_time <- "1 years"
  tol_1d <- 1/365
  tol_7d <- 7/365
  
  # No change
  snap_identical <- snap_to_time_period(TL, TR, res_time, tol_1)
  expect_identical(snap_identical, c("TL" = TL, "TR" = TR))
  
  # Left snap
  TL2 <- as.Date("2019-12-26")
  TL3 <- as.Date("2019-12-25")
  
  snap_left_1d <- snap_to_time_period(TL2, TR, res_time, tol_1d)
  snap_left_7d_in <- snap_to_time_period(TL2, TR, res_time, tol_7d)
  snap_left_7d_out <- snap_to_time_period(TL3, TR, res_time, tol_7d)
  
  expect_identical(snap_left_1d, c("TL" = TL2, "TR" = TR))
  expect_identical(snap_left_7d_in, c("TL" = TL, "TR" = TR))
  expect_identical(snap_left_7d_out, c("TL" = TL3, "TR" = TR))
  
  # Right snap
  TR2 <- as.Date("2021-01-06")
  TR3 <- as.Date("2021-01-07")
  
  snap_right_1d <- snap_to_time_period(TL, TR2, res_time, tol_1d)
  snap_right_7d_in <- snap_to_time_period(TL, TR2, res_time, tol_7d)
  snap_right_7d_out <- snap_to_time_period(TL, TR3, res_time, tol_7d)
  
  expect_identical(snap_right_1d, c("TL" = TL, "TR" = TR2))
  expect_identical(snap_right_7d_in, c("TL" = TL, "TR" = TR))
  expect_identical(snap_right_7d_out, c("TL" = TL, "TR" = TR3))
  
  # No snap
  TR4 <- as.Date("2022-01-01")
  expect_warning(snap_to_time_period(TL, TR4, res_time, tol_1d))
  
  
})
