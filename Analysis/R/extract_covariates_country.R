# Extract every covariate in the database over one country, and check the
# values against the source rasters. Works on any database, including the
# uncropped global one (aoi: raw).
#
# For each covariate table (all bands), the pixels inside the country's
# bounding box (plus a margin) are read from the database. A pixel is "in the
# country" when its centre falls inside the national boundary. The source
# files listed in covariates.metadata (src_dir) are summarised over the same
# footprint (source pixels whose centre lies in one of those database
# pixels), so the two summaries are comparable:
#   - space aggregation "sum" (population): total over the footprint, which
#     the database should reproduce closely;
#   - otherwise: mean over the footprint. The database mean weights each
#     model pixel equally, the source mean each source pixel, so expect
#     differences of a few % where the source has NoData (water, coast).
#
# Outputs in -o: <ISO>_covariate_pixels.csv.gz (long: table, band, date, x, y,
# value, in_country), <ISO>_covariate_summary.csv,
# <ISO>_covariate_vs_source.csv, and a PDF with the figures.
#
# Database connection: PGHOST, PGPORT, PGDATABASE, PGUSER, PGPASSWORD.
# Usage:
#   Rscript Analysis/R/extract_covariates_country.R -i BDI -o diagnostics/BDI

option_list <- list(
  optparse::make_option(c("-i", "--iso"), default = "BDI", type = "character",
                        help = "ISO3 code of the country"),
  optparse::make_option(c("-l", "--layers_directory"), default = Sys.getenv("CMP_LAYERS", "Layers"),
                        type = "character", help = "Layers directory (source paths in metadata are relative to it)"),
  optparse::make_option(c("-m", "--margin_deg"), default = 0.25, type = "double",
                        help = "Margin around the country's bounding box, in degrees"),
  optparse::make_option(c("-o", "--output_directory"), default = NULL, type = "character",
                        help = "Where to write the CSVs and the PDF (default: covariate_extract_<ISO>)")
)
opt <- optparse::parse_args(optparse::OptionParser(option_list = option_list))
iso <- toupper(opt$iso)
out_dir <- if (is.null(opt$output_directory)) paste0("covariate_extract_", iso) else opt$output_directory
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
layers_dir <- normalizePath(opt$layers_directory, mustWork = TRUE)
if (Sys.getenv("CHOLERA_AOI_CACHE_DIR") == "") {
  Sys.setenv(CHOLERA_AOI_CACHE_DIR = file.path(layers_dir, "admin_units"))
}

suppressPackageStartupMessages({
  library(magrittr)
  library(ggplot2)
})
suppressMessages(sf::sf_use_s2(FALSE))
if (!isTRUE(capabilities("cairo"))) {
  stop("This R has no Cairo support, which the figures need on a headless node.")
}
options(bitmapType = "cairo")

# Style (as in db_diagnostics.R)
col <- list(surface = "#fcfcfb", text = "#0b0b0b", text2 = "#52514e", grid = "#e8e7e3",
            s1 = "#2a78d6", s2 = "#eb6834", s3 = "#1baf7a",
            seq_low = "#cde2fb", seq_high = "#0d366b", neutral = "#c9c8c2",
            bad = "#d03b3b")
theme_diag <- function(map = FALSE) {
  th <- theme_minimal(base_size = 10) +
    theme(plot.background = element_rect(fill = col$surface, colour = NA),
          text = element_text(colour = col$text),
          axis.text = element_text(colour = col$text2),
          plot.subtitle = element_text(colour = col$text2),
          plot.caption = element_text(colour = col$text2, hjust = 0),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = col$grid, linewidth = 0.3),
          strip.text = element_text(colour = col$text, face = "bold"),
          legend.position = "bottom")
  if (map) {
    th <- th + theme(axis.title = element_blank(), axis.text = element_blank(),
                     panel.grid.major = element_blank(), legend.key.width = grid::unit(2, "cm"))
  }
  th
}
fmt <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)

# Country boundary --------------------------------------------------------------
adm0 <- taxdat::get_multi_country_admin_units(iso, admin_levels = 0, clip_to_adm0 = FALSE) %>%
  sf::st_transform(4326) %>%
  { suppressMessages(sf::st_union(.)) } %>%
  sf::st_sf(geometry = .)
bb <- sf::st_bbox(adm0) + c(-1, -1, 1, 1) * opt$margin_deg

conn <- taxdat::connect_to_db()
on.exit(DBI::dbDisconnect(conn), add = TRUE)
q <- function(sql) DBI::dbGetQuery(conn, sql)
db_name <- q("SELECT current_database() AS d")$d

meta <- q("SELECT * FROM covariates.metadata ORDER BY covariate")
bands <- q("SELECT covariate, band, tl, tr FROM covariates.bands ORDER BY covariate, band")
if (nrow(meta) == 0) stop("No covariates in ", db_name)
cat("Database", db_name, ":", nrow(meta), "covariate tables\n")

band_label <- function(tl) ifelse(is.na(tl), "static", format(as.Date(tl), "%Y"))

# Pixels of every band inside the bounding box ------------------------------------
# ST_Band first, then clip band 1: ST_Clip(rast, n, ...) with n > 1 crashes the
# server in PostGIS 3.6.4 (see get_pop_weights).
read_pixels <- function(table) {
  srid <- q(sprintf('SELECT ST_SRID(rast) AS s FROM covariates."%s" LIMIT 1', table))$s
  env <- sprintf("ST_MakeEnvelope(%.6f, %.6f, %.6f, %.6f, %d)", bb["xmin"], bb["ymin"],
                 bb["xmax"], bb["ymax"], srid)
  px <- q(sprintf('
    SELECT b AS band, ST_X((p).geom) AS x, ST_Y((p).geom) AS y, (p).val AS value
    FROM (SELECT b, ST_PixelAsCentroids(ST_Clip(ST_Band(r.rast, b), 1, %s, true), 1, true) AS p
          FROM covariates."%s" r, generate_series(1, ST_NumBands(r.rast)) AS b
          WHERE ST_Intersects(r.rast, %s)) s', env, table, env))
  res <- q(sprintf('SELECT ST_PixelWidth(rast) AS w, ST_PixelHeight(rast) AS h FROM covariates."%s" LIMIT 1', table))
  list(px = px, w = res$w, h = res$h)
}

in_country <- function(x, y) {
  xy <- unique(data.frame(x = x, y = y))
  pts <- sf::st_as_sf(xy, coords = c("x", "y"), crs = 4326)
  xy$inside <- lengths(suppressMessages(sf::st_intersects(pts, adm0))) > 0
  key_all <- paste(x, y)
  xy$inside[match(key_all, paste(xy$x, xy$y))]
}

summarise_values <- function(v, agg) {
  v <- v[!is.na(v)]
  if (agg == "sum") sum(v) else if (length(v)) mean(v) else NA_real_
}

# Raster on the database pixels of one table, 1 where the pixel is in the country
footprint_raster <- function(px, w, h) {
  xy <- dplyr::distinct(px, x, y, in_country)
  h <- abs(h)
  tmpl <- terra::rast(terra::ext(min(xy$x) - w / 2, max(xy$x) + w / 2, min(xy$y) - h / 2, max(xy$y) + h / 2),
                      resolution = c(w, h), crs = "EPSG:4326")
  v <- rep(NA_real_, terra::ncell(tmpl))
  v[terra::cellFromXY(tmpl, cbind(xy$x, xy$y))[xy$in_country]] <- 1
  terra::values(tmpl) <- v
  tmpl
}

pixels <- list()
pixel_size <- list()
footprints <- list()
db_summary <- list()
for (t in meta$covariate) {
  t0 <- Sys.time()
  r <- read_pixels(t)
  px <- r$px
  pixel_size[[t]] <- c(w = r$w, h = r$h)
  if (nrow(px) == 0) {
    message("-- ", t, ": no pixels in the bounding box")
    next
  }
  px$in_country <- in_country(px$x, px$y)
  footprints[[t]] <- footprint_raster(px, r$w, r$h)
  tb <- dplyr::filter(bands, covariate == t)
  px <- px %>%
    dplyr::left_join(dplyr::select(tb, band, tl), by = "band") %>%
    dplyr::mutate(table = t, label = band_label(tl))
  pixels[[t]] <- px
  agg <- meta$space_agg[meta$covariate == t]
  db_summary[[t]] <- px %>%
    dplyr::filter(in_country) %>%
    dplyr::group_by(table, band, tl, label) %>%
    dplyr::summarise(n_pixels = dplyr::n(), n_na = sum(is.na(value)),
                     min = min(value, na.rm = TRUE), max = max(value, na.rm = TRUE),
                     mean = mean(value, na.rm = TRUE), sum = sum(value, na.rm = TRUE),
                     db_value = summarise_values(value, agg), .groups = "drop") %>%
    dplyr::mutate(space_agg = agg)
  message(sprintf("-- %s: %s bands, %s pixels in %s (%.1f s)", t, nrow(tb),
                  fmt(sum(px$in_country & px$band == 1)), iso,
                  as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
db_summary <- dplyr::bind_rows(db_summary)
all_px <- dplyr::bind_rows(pixels)
utils::write.csv(db_summary, file.path(out_dir, paste0(iso, "_covariate_summary.csv")), row.names = FALSE)
gz <- gzfile(file.path(out_dir, paste0(iso, "_covariate_pixels.csv.gz")), "w")
utils::write.csv(dplyr::select(all_px, table, band, date = tl, x, y, value, in_country), gz, row.names = FALSE)
close(gz)

# Same summaries from the source rasters -----------------------------------------
# Source layers are grouped by year and combined with the covariate's time
# aggregation, as the pre-compute does for yearly bands.
source_layer_dates <- function(f, n) {
  if (!grepl("\\.nc$", f, ignore.case = TRUE)) return(rep(as.Date(NA), n))
  d <- tryCatch(taxdat::get_ncdf_metadata(f)$dates, error = function(e) NULL)
  if (length(d) == n) as.Date(d) else rep(as.Date(NA), n)
}
source_summary <- function(row) {
  src <- row$src_dir
  if (!is.na(src) && !startsWith(src, "/")) src <- file.path(layers_dir, src)
  if (is.na(src) || !file.exists(src)) {
    message("-- ", row$covariate, ": source not found (", src, ")")
    return(NULL)
  }
  files <- if (dir.exists(src)) list.files(src, pattern = "\\.nc$", full.names = TRUE) else src
  agg <- row$space_agg
  fp <- footprints[[row$covariate]]
  if (is.null(fp)) return(NULL)
  purrr::map_dfr(files, function(f) {
    r <- terra::rast(f)
    r <- terra::crop(r, terra::ext(fp), snap = "out")
    # Source pixels whose centre lies in a database pixel of the country
    sel <- r[[1]]
    terra::values(sel) <- terra::extract(fp, terra::xyFromCell(r, seq_len(terra::ncell(r))))[, 1]
    m <- terra::mask(r, sel)
    fun <- if (agg == "sum") "sum" else "mean"
    vals <- terra::global(m, fun, na.rm = TRUE)[[1]]
    data.frame(file = basename(f), layer = seq_along(vals), date = source_layer_dates(f, length(vals)),
               value = vals, src_res = terra::res(r)[1])
  }) %>%
    dplyr::mutate(covariate = row$covariate, static = row$src_res_time == "static",
                  time_agg = row$time_agg)
}
src_rows <- purrr::map_dfr(seq_len(nrow(meta)), function(i) {
  t0 <- Sys.time()
  s <- tryCatch(source_summary(meta[i, ]), error = function(e) {
    message("-- ", meta$covariate[i], ": source summary failed: ", conditionMessage(e))
    NULL
  })
  if (!is.null(s)) message(sprintf("-- source of %s: %d layers (%.1f s)", meta$covariate[i], nrow(s),
                                   as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  s
})

comparison <- NULL
if (nrow(src_rows) > 0) {
  src_by_label <- src_rows %>%
    dplyr::mutate(label = ifelse(static, "static", format(date, "%Y"))) %>%
    dplyr::group_by(covariate, label) %>%
    dplyr::summarise(src_value = if (dplyr::first(time_agg) %in% "sum") sum(value) else mean(value),
                     src_res = dplyr::first(src_res), src_files = paste(unique(file), collapse = ";"),
                     .groups = "drop")
  comparison <- db_summary %>%
    dplyr::select(table, label, tl, db_value, space_agg, n_pixels) %>%
    dplyr::left_join(src_by_label, by = c("table" = "covariate", "label")) %>%
    dplyr::mutate(rel_diff = db_value / src_value - 1)
  utils::write.csv(comparison, file.path(out_dir, paste0(iso, "_covariate_vs_source.csv")), row.names = FALSE)
}

# Figures ------------------------------------------------------------------------
figures <- list()
add_fig <- function(plot, width = 10, height = 7.5) {
  figures[[length(figures) + 1]] <<- list(plot = plot, width = width, height = height)
}

# 1. Overview: database vs source, by covariate and year
if (!is.null(comparison)) {
  cmp <- comparison %>%
    dplyr::mutate(pct = 100 * rel_diff,
                  pct_lab = ifelse(is.na(pct), "no source", sprintf("%+.1f", pct)),
                  flag = dplyr::case_when(is.na(pct) ~ "no source value",
                                          abs(pct) <= 2 ~ "within 2 %",
                                          abs(pct) <= 10 ~ "2 to 10 %",
                                          TRUE ~ "over 10 %"),
                  label = factor(label, levels = c(sort(unique(label[label != "static"])), "static")))
  p1 <- ggplot(cmp, aes(label, table, fill = flag)) +
    geom_tile(colour = col$surface, linewidth = 0.8) +
    geom_text(aes(label = pct_lab), size = 2.3, colour = col$text) +
    scale_fill_manual(name = NULL, values = c("within 2 %" = "#bfe6d6", "2 to 10 %" = "#fbd9b5",
                                              "over 10 %" = "#f2b1b1", "no source value" = col$neutral)) +
    labs(title = sprintf("%s: covariates in %s vs their source rasters", iso, db_name),
         subtitle = paste0("Each tile: difference (%) between the database and its source raster, both over the database pixels whose centre is in the country.\n",
                           "Population: total. Others: mean (the database weights model pixels equally, the source its own pixels)."),
         caption = paste0("Population should agree within about 1 %; means within a few %, more where the source has NoData (lakes, coast).\n",
                          "Source: covariates.metadata src_dir. ", format(Sys.time(), "%Y-%m-%d %H:%M")),
         x = NULL, y = NULL) +
    theme_diag() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5), panel.grid.major = element_blank())
  add_fig(p1, width = 11, height = 2.5 + 0.35 * length(unique(cmp$table)))

  # 2. Time series of the country value, database vs source
  ts <- dplyr::bind_rows(
    dplyr::transmute(comparison, table, label, value = db_value, series = "database"),
    dplyr::transmute(comparison, table, label, value = src_value, series = "source raster")) %>%
    dplyr::filter(label != "static") %>%
    dplyr::mutate(year = as.integer(label))
  if (nrow(ts) > 0) {
    p2 <- ggplot(ts, aes(year, value, colour = series, shape = series)) +
      geom_line(linewidth = 0.6) +
      geom_point(size = 1.6) +
      facet_wrap(~table, scales = "free_y", ncol = 3) +
      scale_colour_manual(name = NULL, values = c(database = col$s1, `source raster` = col$s2)) +
      scale_shape_manual(name = NULL, values = c(database = 16, `source raster` = 1)) +
      scale_y_continuous(labels = scales::label_comma()) +
      labs(title = sprintf("%s: yearly covariates over the country", iso),
           subtitle = "Population: total. Others: mean of the pixels whose centre is in the country.",
           x = NULL, y = NULL) +
      theme_diag()
    add_fig(p2, width = 11, height = 2.5 + 2.4 * ceiling(length(unique(ts$table)) / 3))
  }

  # 3. Static covariates: database vs source
  st <- dplyr::filter(comparison, label == "static")
  if (nrow(st) > 0) {
    st_long <- dplyr::bind_rows(
      dplyr::transmute(st, table, value = db_value, series = "database"),
      dplyr::transmute(st, table, value = src_value, series = "source raster"))
    p3 <- ggplot(st_long, aes(value, table, colour = series, shape = series)) +
      geom_point(size = 2.6) +
      facet_wrap(~table, scales = "free", ncol = 2) +
      # From zero, so the gap reads at its true size
      scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
      scale_colour_manual(name = NULL, values = c(database = col$s1, `source raster` = col$s2)) +
      scale_shape_manual(name = NULL, values = c(database = 16, `source raster` = 1)) +
      labs(title = sprintf("%s: static covariates, mean over the country", iso), x = NULL, y = NULL) +
      theme_diag() +
      theme(axis.text.y = element_blank(), strip.text = element_text(hjust = 0))
    add_fig(p3, width = 10, height = 2 + 1.2 * ceiling(nrow(st) / 2))
  }
}

# 4. Maps: every covariate table, its bands (population at 1 km: four years)
for (t in names(pixels)) {
  px <- pixels[[t]]
  labs_avail <- unique(px$label)
  if (grepl("_1_1$", t) && length(labs_avail) > 4) {
    keep <- labs_avail[unique(round(seq(1, length(labs_avail), length.out = 4)))]
    px <- dplyr::filter(px, label %in% keep)
  }
  is_pop <- startsWith(t, "pop_")
  ps <- pixel_size[[t]]
  s <- dplyr::filter(db_summary, table == t)
  n_lab <- length(unique(px$label))
  ncol_f <- min(n_lab, if (n_lab > 12) 6 else 4)
  if (is_pop) px$value[px$value <= 0] <- NA   # log scale: zero shown grey
  p <- ggplot(px, aes(x, y, fill = value)) +
    geom_tile(width = ps[["w"]], height = abs(ps[["h"]])) +
    geom_sf(data = adm0, inherit.aes = FALSE, fill = NA, colour = col$text, linewidth = 0.4) +
    facet_wrap(~label, ncol = ncol_f) +
    { if (is_pop) {
      scale_fill_gradient(name = "People per pixel", low = col$seq_low, high = col$seq_high,
                          trans = "log10", limits = c(1, max(1, px$value, na.rm = TRUE)),
                          oob = scales::squish, labels = scales::label_comma(), na.value = col$neutral)
    } else {
      # 1st to 99th percentile, so a few extreme pixels do not flatten the map
      scale_fill_gradient(name = t, low = col$seq_low, high = col$seq_high, na.value = col$neutral,
                          limits = stats::quantile(px$value, c(0.01, 0.99), na.rm = TRUE, names = FALSE),
                          oob = scales::squish)
    } } +
    coord_sf(expand = FALSE) +
    labs(title = sprintf("covariates.%s over %s", t, iso),
         subtitle = sprintf("%s bands; pixel %.4f deg; %s pixels per band in the country; values %s to %s%s.",
                            nrow(dplyr::filter(bands, covariate == t)), ps[["w"]],
                            fmt(s$n_pixels[1]), signif(min(s$min), 3), signif(max(s$max), 3),
                            if (is_pop) "; log scale, zero shown grey, below 1 as 1" else ""),
         caption = paste0("Black: national boundary. Grey: NoData",
                          if (is_pop) " or zero." else ". Colours span the 1st to 99th percentile.")) +
    theme_diag(map = TRUE)
  add_fig(p, width = 11, height = 2 + 2.6 * ceiling(n_lab / ncol_f))
  message("-- figure ", t)
}

pdf_file <- file.path(out_dir, sprintf("covariates_%s_%s.pdf", iso, db_name))
grDevices::cairo_pdf(pdf_file, width = 11, height = 8.5, bg = col$surface, onefile = TRUE)
for (f in figures) print(f$plot)
invisible(grDevices::dev.off())

if (!is.null(comparison)) {
  worst <- comparison %>%
    dplyr::group_by(table) %>%
    dplyr::summarise(bands = dplyr::n(), max_abs_pct = round(100 * max(abs(rel_diff), na.rm = TRUE), 2),
                     .groups = "drop")
  print(as.data.frame(worst), row.names = FALSE)
}
cat("Wrote", length(figures), "figures to", normalizePath(pdf_file), "\n")
