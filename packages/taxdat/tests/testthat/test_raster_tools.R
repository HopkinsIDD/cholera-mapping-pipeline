# Raster tools on synthetic data; no database needed.

make_monthly_nc <- function(dir, n_months = 24, start = as.Date("2000-01-01"), value = NULL) {
  r <- terra::rast(nrows = 20, ncols = 20, xmin = 29, xmax = 31, ymin = -5, ymax = -3,
                   crs = "EPSG:4326", nlyrs = n_months)
  vals <- if (is.null(value)) rep(seq_len(n_months), each = 400) else rep(value, 400 * n_months)
  terra::values(r) <- vals
  f <- file.path(dir, "monthly.nc")
  write_covariate_ncdf(r, f, var_name = "v", long_name = "test variable", unit = "u",
                       dates = seq(start, by = "1 month", length.out = n_months))
  f
}

ref_grid <- function(dir, res = 0.2, xmin = 29, xmax = 31, ymin = -5, ymax = -3) {
  g <- terra::rast(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                   resolution = res, crs = "EPSG:4326", vals = 1)
  f <- file.path(dir, "grid.tif")
  terra::writeRaster(g, f, overwrite = TRUE)
  f
}

test_that("NetCDF round trip keeps dates, variable name and static files have no time", {
  d <- withr::local_tempdir()
  f <- make_monthly_nc(d, n_months = 3)
  meta <- get_ncdf_metadata(f)
  expect_equal(meta$var_name, "v")
  expect_equal(meta$dates, as.Date(c("2000-01-01", "2000-02-01", "2000-03-01")))
  s <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 1, ymin = 0, ymax = 1, vals = 1:4)
  fs <- file.path(d, "static.nc")
  write_covariate_ncdf(s, fs, var_name = "s")
  expect_null(get_ncdf_metadata(fs)$dates)
  expect_equal(get_ncdf_metadata(fs)$time_info$units, "static")
})

test_that("time_aggregate sums months into years labelled by left bound", {
  d <- withr::local_tempdir()
  f <- make_monthly_nc(d, n_months = 24)
  out <- time_aggregate(f, covar_name = "v", covar_unit = "u", covar_type = "temporal",
                        res_file = file.path(d, "yearly.nc"), res_time = "1 years",
                        aggregator = "sum")
  expect_equal(out$dates, as.Date(c("2000-01-01", "2001-01-01")))
  r <- terra::rast(out$file)
  expect_equal(terra::nlyr(r), 2)
  expect_equal(unname(unlist(terra::global(r[[1]], "max"))), sum(1:12))
  expect_equal(unname(unlist(terra::global(r[[2]], "max"))), sum(13:24))
  expect_equal(get_ncdf_metadata(out$file)$dates, out$dates)
})

test_that("time_aggregate replicates a coarser source on the cropped stack", {
  d <- withr::local_tempdir()
  r <- terra::rast(nrows = 20, ncols = 20, xmin = 29, xmax = 31, ymin = -5, ymax = -3,
                   crs = "EPSG:4326", nlyrs = 2, vals = rep(c(1, 2), each = 400))
  f <- file.path(d, "yearly.nc")
  write_covariate_ncdf(r, f, var_name = "v", dates = as.Date(c("2000-01-01", "2001-01-01")))
  aoi <- list(extent = terra::ext(29.5, 30.5, -4.5, -3.5))
  out <- time_aggregate(f, covar_name = "v", covar_unit = "u", covar_type = "temporal",
                        res_file = file.path(d, "monthly.nc"), res_time = "1 months",
                        aggregator = "mean", aoi = aoi)
  ro <- terra::rast(out$file)
  expect_equal(terra::nlyr(ro), 24)
  expect_equal(as.vector(terra::ext(ro)), as.vector(aoi$extent))
  expect_equal(out$dates[13], as.Date("2001-01-01"))
  expect_equal(unname(unlist(terra::global(ro[[13]], "max"))), 2)
})

test_that("a single-layer temporal file uses the declared resolution", {
  d <- withr::local_tempdir()
  r <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 1, ymin = 0, ymax = 1,
                   crs = "EPSG:4326", vals = 5)
  f <- file.path(d, "pop_2001.nc")
  write_covariate_ncdf(r, f, var_name = "pop", dates = as.Date("2001-01-01"))
  out <- time_aggregate(f, covar_name = "pop", covar_unit = "n", covar_type = "temporal",
                        covar_res_time = "1 years", res_file = file.path(d, "o.nc"),
                        res_time = "1 years", aggregator = "mean")
  expect_equal(out$dates, as.Date("2001-01-01"))
})

test_that("space_aggregate sum conserves totals and average keeps a constant", {
  d <- withr::local_tempdir()
  f <- make_monthly_nc(d, n_months = 2, value = 3)
  spec <- gdal_grid_spec(ref_grid(d, res = 0.2))
  out <- space_aggregate(f, file.path(d, "sum.nc"), spec, covar_type = "temporal",
                         aggregator = "sum")
  r_in <- terra::rast(f)
  r_out <- terra::rast(out$file)
  expect_equal(terra::nlyr(r_out), 2)
  expect_equal(unname(unlist(terra::global(r_out[[1]], "sum", na.rm = TRUE))),
               unname(unlist(terra::global(r_in[[1]], "sum", na.rm = TRUE))), tolerance = 1e-5)
  expect_equal(unname(as.vector(terra::ext(r_out))), c(29, 31, -5, -3), tolerance = 1e-9)
  expect_equal(dim(r_out)[1:2], c(10, 10))
  expect_equal(get_ncdf_metadata(out$file)$dates, as.Date(c("2000-01-01", "2000-02-01")))

  avg <- space_aggregate(f, file.path(d, "avg.nc"), spec, covar_type = "temporal",
                         aggregator = "mean")
  expect_equal(range(terra::values(terra::rast(avg$file)), na.rm = TRUE), c(3, 3))
})

test_that("space_aggregate output takes the reference grid extent, not the source's", {
  d <- withr::local_tempdir()
  f <- make_monthly_nc(d, n_months = 1, value = 1)
  spec <- gdal_grid_spec(ref_grid(d, res = 0.5, xmin = 29.5, xmax = 30.5, ymin = -4.5, ymax = -3.5))
  out <- space_aggregate(f, file.path(d, "o.nc"), spec, covar_type = "temporal",
                         aggregator = "average")
  expect_equal(dim(terra::rast(out$file))[1:2], c(2, 2))
})

test_that("bad aggregators and failing gdal commands stop", {
  d <- withr::local_tempdir()
  f <- make_monthly_nc(d, n_months = 1)
  spec <- gdal_grid_spec(ref_grid(d))
  expect_error(space_aggregate(f, file.path(d, "o.nc"), spec, "temporal", aggregator = "bogus"),
               "not in")
  expect_error(gdal_warp(file.path(d, "missing.tif"), file.path(d, "x.tif")), "gdalwarp")
})

test_that("transform_spatraster turns log(0) into NA with a warning", {
  r <- terra::rast(nrows = 1, ncols = 3, vals = c(0, 1, exp(1)))
  expect_warning(out <- transform_spatraster(r, "log"), "non-finite")
  expect_equal(as.vector(terra::values(out)), c(NA, 0, 1))
})

test_that("generate_time_sequence fails loudly on gaps", {
  res_src <- list(dt_units = 1, dt_days = 365)
  expect_error(generate_time_sequence(as.Date(c("2000-01-01", "2002-01-01")), res_src, "1 months"),
               "Could not map")
})
