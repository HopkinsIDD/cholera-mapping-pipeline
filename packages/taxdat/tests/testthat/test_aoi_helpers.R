# AOI and admin-units cache on synthetic boundaries; no network, no database.

square <- function(xmin, xmax, ymin, ymax) {
  sf::st_polygon(list(rbind(c(xmin, ymin), c(xmax, ymin), c(xmax, ymax),
                            c(xmin, ymax), c(xmin, ymin))))
}

write_fixture_cache <- function(dir, iso = "BDI") {
  adm0 <- sf::st_sf(shapeName = "Testland", country = iso,
                    location_period_id = "T-ADM0-1", shapeType = "ADM0", source = "fixture",
                    geom = sf::st_sfc(sf::st_multipolygon(list(square(29, 31, -4.5, -2.5))),
                                      crs = 4326))
  adm1 <- sf::st_sf(shapeName = c("West", "East"), country = iso,
                    location_period_id = c("T-ADM1-1", "T-ADM1-2"), shapeType = "ADM1",
                    source = "fixture",
                    geom = sf::st_sfc(sf::st_multipolygon(list(square(28.8, 30, -4.5, -2.5))),
                                      sf::st_multipolygon(list(square(30, 31, -4.5, -2.5))),
                                      crs = 4326))
  sf::st_write(adm0, file.path(dir, paste0(iso, "_adm0.gpkg")), quiet = TRUE)
  sf::st_write(adm1, file.path(dir, paste0(iso, "_adm1.gpkg")), quiet = TRUE)
  invisible(dir)
}

test_that("aoi_tag and raw AOI", {
  expect_equal(aoi_tag("BDI", 50), "bdi_b50km")
  expect_equal(aoi_tag("raw", 50), "raw")
  expect_null(get_aoi("raw"))
  expect_equal(aoi_metadata(NULL)$aoi_name, "raw")
})

test_that("get_aoi buffers the national boundary in kilometres", {
  d <- write_fixture_cache(withr::local_tempdir())
  aoi <- get_aoi("BDI", buffer_km = 50, cache_dir = d)
  expect_equal(aoi$name, "bdi_b50km")
  # 50 km is about 0.45 degrees at this latitude
  expect_equal(aoi$bbox[["xmin"]], 29 - 0.45, tolerance = 0.02)
  expect_equal(aoi$bbox[["xmax"]], 31 + 0.45, tolerance = 0.02)
  expect_equal(aoi$bbox[["ymin"]], -4.5 - 0.45, tolerance = 0.02)
  expect_equal(aoi$bbox[["ymax"]], -2.5 + 0.45, tolerance = 0.02)
  expect_s4_class(aoi$vect, "SpatVector")
  expect_equal(aoi_metadata(aoi)$aoi_buffer_km, 50)
})

test_that("get_aoi snaps the extent to a raster lattice", {
  d <- write_fixture_cache(withr::local_tempdir())
  g <- terra::rast(xmin = 20, xmax = 40, ymin = -10, ymax = 0, resolution = 1 / 120,
                   crs = "EPSG:4326", vals = 1)
  f <- file.path(d, "master.tif")
  terra::writeRaster(g, f)
  aoi <- get_aoi("BDI", buffer_km = 10, cache_dir = d, snap_to = f)
  cells <- (aoi$bbox - c(20, 20, -10, -10)) * 120
  expect_equal(unname(cells), round(unname(cells)), tolerance = 1e-6)
})

test_that("crop_mask_to_aoi masks outside the buffered polygon", {
  d <- write_fixture_cache(withr::local_tempdir())
  aoi <- get_aoi("BDI", buffer_km = 0, cache_dir = d)
  r <- terra::rast(xmin = 28, xmax = 32, ymin = -6, ymax = -1, resolution = 0.5,
                   crs = "EPSG:4326", vals = 1)
  out <- crop_mask_to_aoi(r, aoi)
  # The crop covers the area of interest and exceeds it by at most one cell
  # (how far "snap out" goes on an exact cell edge differs between terra versions)
  e <- unname(as.vector(terra::ext(out)))
  expect_true(e[1] <= 29 && e[2] >= 31 && e[3] <= -4.5 && e[4] >= -2.5)
  expect_true(all(abs(e - c(29, 31, -4.5, -2.5)) <= 0.5 + 1e-9))
  # Every cell whose centre is inside the area of interest is kept
  xy <- terra::xyFromCell(out, seq_len(terra::ncell(out)))
  inside <- xy[, 1] > 29 & xy[, 1] < 31 & xy[, 2] > -4.5 & xy[, 2] < -2.5
  v <- terra::values(out)[, 1]
  expect_equal(sum(inside), 4 * 4)
  expect_true(all(!is.na(v[inside])))
})

test_that("multi-level admin units come from the cache and are clipped to ADM0", {
  d <- write_fixture_cache(withr::local_tempdir())
  adm <- get_multi_country_admin_units("BDI", admin_levels = 0:1, cache_dir = d)
  expect_equal(sort(unique(adm$admin_level)), c("ADM0", "ADM1"))
  west <- adm[adm$shapeName == "West", ]
  expect_equal(unname(sf::st_bbox(west)[["xmin"]]), 29)
  expect_error(get_multi_country_admin_units("BDI", admin_levels = 1, cache_dir = d), "Admin level 0")
})

test_that("clip_shapefiles_to_adm0 drops outside shapes and clips the rest", {
  d <- write_fixture_cache(withr::local_tempdir())
  lps <- sf::st_sf(location_period_id = c(1, 2),
                   geom = sf::st_sfc(sf::st_multipolygon(list(square(30.5, 31.5, -3, -2))),
                                     sf::st_multipolygon(list(square(40, 41, 0, 1))),
                                     crs = 4326))
  out <- expect_message(clip_shapefiles_to_adm0("BDI", lps, cache_dir = d), "Dropping 1")
  expect_equal(out$location_period_id, 1)
  expect_equal(unname(sf::st_bbox(out)[["xmax"]]), 31)
})

test_that("check_input_crop refuses rasters cropped for another area", {
  d <- withr::local_tempdir()
  expect_true(check_input_crop(d, NULL))
  writeLines("cropped", file.path(d, "CROPPED_TO_BDI_150KM.txt"))
  expect_error(check_input_crop(d, NULL), "cropped to BDI")
  expect_error(check_input_crop(d, list(iso_code = "KEN")), "cropped to BDI")
  expect_true(check_input_crop(d, list(iso_code = "BDI")))
})
