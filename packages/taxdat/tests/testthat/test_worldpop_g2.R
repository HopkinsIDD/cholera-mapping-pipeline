# WorldPop Global 2 conversion on synthetic rasters; no database or download.

g2_fixture <- function(dir, year = 2020, shift = 0) {
  land <- terra::rast(nrows = 10, ncols = 10, xmin = 29, xmax = 30, ymin = -4, ymax = -3,
                      crs = "EPSG:4326", vals = 1)
  land[1:2, ] <- NA                                   # sea
  pop <- terra::rast(nrows = 10, ncols = 10, xmin = 29 + shift, xmax = 30 + shift, ymin = -4,
                     ymax = -3, crs = "EPSG:4326", vals = NA_real_)
  pop[5:6, 3:4] <- 10                                 # settled land
  pop[2, 1] <- 3                                      # people on a pixel the land raster calls sea
  raw <- file.path(dir, "raw")
  dir.create(raw, showWarnings = FALSE)
  land_file <- file.path(raw, "x_pop_2020_UC_1km_R2024B_UA_v1.tif")
  terra::writeRaster(land, land_file, overwrite = TRUE, NAflag = -99999)
  terra::writeRaster(pop, file.path(raw, sprintf("x_pop_%d_CN_1km_R2025A_UA_v1.tif", year)),
                     overwrite = TRUE, NAflag = -99999)
  list(raw = raw, land = land_file, out = file.path(dir, "pop"))
}

test_that("Global 2 URLs follow the WorldPop layout", {
  expect_equal(worldpop_g2_url(2020),
               "https://data.worldpop.org/GIS/Population/Global_2015_2030/R2025A/2020/0_Mosaicked/v1/1km_ua/constrained/global_pop_2020_CN_1km_R2025A_UA_v1.tif")
  expect_match(worldpop_g2_url(2020, "R2024B", constrained = FALSE, iso3 = "bdi"),
               "R2024B/2020/BDI/v1/1km_ua/unconstrained/bdi_pop_2020_UC_1km_R2024B_UA_v1.tif$")
  expect_error(worldpop_g2_url(2014), "2015 to 2030")
  expect_error(worldpop_g2_url(2020, constrained = FALSE), "R2024B")
})

test_that("conversion writes 0 on unsettled land, keeps totals and dates the band 1 January", {
  d <- withr::local_tempdir()
  fx <- g2_fixture(d)
  res <- suppressWarnings(prepare_worldpop_g2(fx$raw, fx$land, fx$out, years = 2020))
  expect_equal(res$total, 43)
  expect_equal(res$settled_pixels, 5)
  expect_equal(res$pixels_off_land, 1)
  expect_equal(res$people_off_land, 3)

  f <- file.path(fx$out, "population_2020_yearly.nc")
  r <- terra::rast(f)
  expect_equal(terra::global(r, "sum", na.rm = TRUE)$sum, 43)
  expect_equal(terra::global(r == 0, "sum", na.rm = TRUE)$sum, 80 - 4)   # land without people
  expect_equal(terra::global(is.na(r), "sum")$sum, 20 - 1)               # sea, minus the kept pixel
  expect_equal(get_ncdf_metadata(f)$dates, as.Date("2020-01-01"))
  expect_equal(get_ncdf_metadata(f)$var_name, "pop")
  expect_equal(list_covariate_files(fx$out, "temporal")$first_dates, as.Date("2020-01-01"))
  expect_true(any(grepl("WorldPop Global 2", readLines(file.path(fx$out, "PROVENANCE.txt")))))
})

test_that("conversion refuses another lattice and directories with other population files", {
  d <- withr::local_tempdir()
  fx <- g2_fixture(d, shift = 0.03)
  expect_error(suppressWarnings(prepare_worldpop_g2(fx$raw, fx$land, fx$out, years = 2020)),
               "not on the lattice")

  d2 <- withr::local_tempdir()
  fx2 <- g2_fixture(d2)
  dir.create(fx2$out)
  file.create(file.path(fx2$out, "population_2020_yearly.nc"))           # e.g. a Global 1 file
  expect_error(prepare_worldpop_g2(fx2$raw, fx2$land, fx2$out, years = 2020), "empty directory")
  expect_error(find_worldpop_g2_file(fx2$raw, 2019), "found 0")
})
