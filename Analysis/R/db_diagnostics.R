# Diagnostic figures for a covariates-database build and a test mapping run.
#
# Sections (one PDF page per figure, plus a PNG each and CSV summaries):
#   A. Database build: area of interest, master-grid land mask, modelling grid
#   B. Covariates: what is in the database for which years; population totals
#      at 1 km vs the model grid; first/last-year maps at the model resolution
#   C. Extraction for the test run (needs the run's .preprocess / .covar files):
#      observations by year and admin level, location periods, covariate cube,
#      population weights, population kept in the cube vs the database
#
# Database connection: PGHOST, PGPORT, PGDATABASE, PGUSER, PGPASSWORD.
# Usage:
#   Rscript Analysis/R/db_diagnostics.R -c Analysis/configs/BDI_pilot_api.yml \
#     -l Layers -d Analysis/data -o db_diagnostics

option_list <- list(
  optparse::make_option(c("-c", "--config"), type = "character", help = "Run config"),
  optparse::make_option(c("-l", "--layers_directory"), default = "Layers", type = "character",
                        help = "Layers directory (grids/, admin_units/)"),
  optparse::make_option(c("-d", "--data_directory"), default = "Analysis/data", type = "character",
                        help = "Directory with the test run's .preprocess.rdata / .covar.rdata"),
  optparse::make_option(c("-o", "--output_directory"), default = "db_diagnostics", type = "character",
                        help = "Where to write the PDF, PNGs and CSVs")
)
opt <- optparse::parse_args(optparse::OptionParser(option_list = option_list))
if (is.null(opt$config)) stop("Give the run config with -c")

suppressPackageStartupMessages({
  library(magrittr)
  library(ggplot2)
})
sf::sf_use_s2(FALSE)
`%||%` <- function(a, b) if (is.null(a)) b else a   # base R only has it from 4.4

config <- yaml::read_yaml(opt$config, eval.expr = TRUE)
layers_dir <- normalizePath(opt$layers_directory, mustWork = TRUE)
if (Sys.getenv("CHOLERA_AOI_CACHE_DIR") == "") {
  Sys.setenv(CHOLERA_AOI_CACHE_DIR = file.path(layers_dir, "admin_units"))
}
out_dir <- opt$output_directory
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
res_space <- config$res_space
model_grid <- sprintf("grid_%s_%s", res_space, res_space)

# Style (reference data-viz palette: recessive grid, one-hue sequential ramp,
# fixed categorical order) -----------------------------------------------------
col <- list(surface = "#fcfcfb", text = "#0b0b0b", text2 = "#52514e", grid = "#e8e7e3",
            s1 = "#2a78d6", s2 = "#eb6834", s3 = "#1baf7a",
            seq_low = "#cde2fb", seq_high = "#0d366b", neutral = "#c9c8c2")
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
  if (map) th <- th + theme(axis.title = element_blank(), panel.grid.major = element_blank())
  th
}
scale_fill_pop <- function(name = "Population") {
  scale_fill_gradient(name = name, low = col$seq_low, high = col$seq_high, trans = "log10",
                      labels = scales::label_comma(), na.value = col$neutral)
}

# Whole years only on year axes
year_breaks <- function(lim) {
  lo <- ceiling(lim[1]); hi <- floor(lim[2])
  seq(lo, hi, by = max(1, round((hi - lo) / 10)))
}
scale_x_years <- function() scale_x_continuous(breaks = year_breaks, labels = function(x) format(x, nsmall = 0))

figures <- list()
add_fig <- function(id, plot, width = 8, height = 6) {
  figures[[id]] <<- list(plot = plot, width = width, height = height)
  ggsave(file.path(out_dir, paste0(id, ".png")), plot, width = width, height = height,
         dpi = 150, bg = col$surface)
  message("-- figure ", id)
}
write_csv <- function(df, name) utils::write.csv(df, file.path(out_dir, name), row.names = FALSE)

conn <- taxdat::connect_to_db()
on.exit(DBI::dbDisconnect(conn), add = TRUE)
q <- function(sql) DBI::dbGetQuery(conn, sql)
db_name <- q("SELECT current_database() AS d")$d

# A. Database build --------------------------------------------------------------
aoi <- taxdat::get_aoi(config$aoi, buffer_km = config$aoi_buffer_km %||% 50)
aoi_name <- taxdat::aoi_metadata(aoi)$aoi_name
adm0 <- taxdat::get_multi_country_admin_units(config$countries_name, admin_levels = 0,
                                              clip_to_adm0 = FALSE)

grids_meta <- q("SELECT * FROM grids.metadata ORDER BY res_km DESC")
grids_meta$n_cells <- vapply(grids_meta$grid, function(g) {
  if (g == "master_grid") {
    return(NA_real_)
  }
  as.numeric(q(sprintf('SELECT count(*) AS n FROM grids."%s_centroids"', g))$n)
}, numeric(1))
write_csv(grids_meta, "grids_metadata.csv")

master <- terra::rast(taxdat::master_grid_file_path(layers_dir, aoi_name))
if (terra::ncell(master) > 2e6) {
  master <- terra::aggregate(master, fact = ceiling(sqrt(terra::ncell(master) / 2e6)), fun = "max")
}
master_df <- terra::as.data.frame(master, xy = TRUE, na.rm = TRUE)
grid_polys <- sf::st_read(conn, query = sprintf('SELECT geom FROM grids."%s_polys"', model_grid), quiet = TRUE)

p_a1 <- ggplot() +
  geom_raster(data = master_df, aes(x, y), fill = col$seq_low) +
  geom_sf(data = grid_polys, fill = NA, colour = col$s1, linewidth = 0.2) +
  geom_sf(data = adm0, fill = NA, colour = col$text, linewidth = 0.5) +
  { if (!is.null(aoi)) geom_sf(data = aoi$polygon, fill = NA, colour = col$s2, linewidth = 0.5, linetype = "22") } +
  coord_sf(expand = FALSE) +
  labs(title = sprintf("Database build: %s (%s)", db_name, aoi_name),
       subtitle = sprintf("Light blue: 1 km land mask (master grid). Blue outlines: %s km model grid, %s cells.\nBlack: national boundary. Dashed orange: area of interest (boundary + %s km).",
                          res_space, format(grids_meta$n_cells[grids_meta$grid == model_grid], big.mark = ","),
                          config$aoi_buffer_km %||% 50),
       caption = paste("Grids:", paste(sprintf("%s (%s)", grids_meta$grid,
                                               ifelse(is.na(grids_meta$n_cells), "1 km land mask",
                                                      paste(formatC(grids_meta$n_cells, big.mark = ",", format = "d"), "cells"))),
                                       collapse = "; "))) +
  theme_diag(map = TRUE)
add_fig("A1_database_build", p_a1, width = 8, height = 8)

# B. Covariates ------------------------------------------------------------------
cov_meta <- q("SELECT * FROM covariates.metadata ORDER BY covariate")
bands <- q("SELECT * FROM covariates.bands ORDER BY covariate, band")
write_csv(cov_meta, "covariates_metadata.csv")
write_csv(bands, "covariate_bands.csv")

avail <- bands %>%
  dplyr::mutate(year = as.integer(format(as.Date(tl), "%Y")))
static_cov <- cov_meta$covariate[cov_meta$src_res_time == "static"]
years <- sort(unique(stats::na.omit(avail$year)))
if (length(static_cov) > 0 && length(years) > 0) {
  avail <- dplyr::bind_rows(dplyr::filter(avail, !is.na(year)),
                            tidyr::expand_grid(covariate = static_cov, year = years, band = 1L))
}
avail <- avail %>%
  dplyr::mutate(kind = ifelse(covariate %in% static_cov, "static (same every year)", "one band per year"))
model_years <- seq(as.integer(substr(config$start_time, 1, 4)), as.integer(substr(config$end_time, 1, 4)))

p_b1 <- ggplot(avail, aes(year, covariate, fill = kind)) +
  annotate("rect", xmin = min(model_years) - 0.5, xmax = max(model_years) + 0.5,
           ymin = -Inf, ymax = Inf, fill = NA, colour = col$s2, linewidth = 0.6) +
  geom_tile(colour = col$surface, linewidth = 0.6, height = 0.7) +
  scale_fill_manual(name = NULL, values = c("one band per year" = col$s1, "static (same every year)" = col$s3)) +
  scale_x_years() +
  labs(title = "Covariates in the database, by year",
       subtitle = sprintf("Each tile is one band of a covariate table. Orange frame: model years %s-%s.",
                          min(model_years), max(model_years)),
       x = NULL, y = NULL) +
  theme_diag()
add_fig("B1_covariate_availability", p_b1, width = 8, height = 2 + 0.4 * length(unique(avail$covariate)))

band_totals <- function(table) {
  q(sprintf('SELECT b AS band, sum((ST_SummaryStats(rast, b, true)).sum) AS total
             FROM covariates."%s", generate_series(1, ST_NumBands(rast)) AS b
             GROUP BY b ORDER BY b', table))
}
pop_tables <- intersect(c("pop_1_years_1_1", sprintf("pop_1_years_%s_%s", res_space, res_space)),
                        cov_meta$covariate)
pop_totals <- purrr::map_dfr(pop_tables, function(t) {
  band_totals(t) %>%
    dplyr::left_join(dplyr::filter(bands, covariate == t), by = "band") %>%
    dplyr::mutate(table = t, year = as.integer(format(as.Date(tl), "%Y")))
})
write_csv(pop_totals, "population_totals_by_year.csv")
if (nrow(pop_totals) > 0) {
  rel <- pop_totals %>%
    dplyr::select(year, table, total) %>%
    tidyr::pivot_wider(names_from = table, values_from = total)
  max_diff <- if (ncol(rel) == 3) max(abs(rel[[3]] / rel[[2]] - 1), na.rm = TRUE) else NA
  pop_lab <- c(pop_1_years_1_1 = "1 km table")
  pop_lab[sprintf("pop_1_years_%s_%s", res_space, res_space)] <- sprintf("%s km table", res_space)
  p_b2 <- ggplot(pop_totals, aes(year, total, colour = table)) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.8) +
    scale_colour_manual(name = NULL, values = setNames(c(col$s1, col$s2), pop_tables), labels = pop_lab) +
    scale_y_continuous(labels = scales::label_comma(), limits = c(0, NA)) +
    scale_x_years() +
    labs(title = "Total population in the database, by year",
         subtitle = if (!is.na(max_diff)) sprintf("The model-grid table should match the 1 km table (sum aggregation). Largest difference: %.2f %%.", 100 * max_diff) else NULL,
         x = NULL, y = "People") +
    theme_diag()
  add_fig("B2_population_totals", p_b2)
}

pixel_values <- function(table, band) {
  q(sprintf('SELECT ST_X((p).geom) AS x, ST_Y((p).geom) AS y, (p).val AS value,
                    ST_PixelWidth(rast) AS w, ST_PixelHeight(rast) AS h
             FROM (SELECT rast, ST_PixelAsCentroids(rast, %d) AS p FROM covariates."%s") s', band, table))
}
model_tables <- cov_meta$covariate[grepl(sprintf("_%s_%s$", res_space, res_space), cov_meta$covariate)]
for (t in model_tables) {
  tb <- dplyr::filter(bands, covariate == t)
  pick <- unique(c(1L, max(tb$band)))
  vals <- purrr::map_dfr(pick, function(b) {
    lab <- if (is.na(tb$tl[tb$band == b])) "static" else format(as.Date(tb$tl[tb$band == b]), "%Y")
    dplyr::mutate(pixel_values(t, b), panel = lab)
  })
  is_pop <- startsWith(t, "pop_")
  p <- ggplot(vals, aes(x, y, fill = value)) +
    geom_tile(aes(width = w, height = h)) +
    geom_sf(data = adm0, inherit.aes = FALSE, fill = NA, colour = col$text, linewidth = 0.4) +
    facet_wrap(~panel) +
    { if (is_pop) scale_fill_pop() else scale_fill_gradient(name = t, low = col$seq_low, high = col$seq_high, na.value = col$neutral) } +
    coord_sf(expand = FALSE) +
    labs(title = sprintf("covariates.%s", t),
         subtitle = sprintf("%s pixels per band; first and last band shown. Black: national boundary.",
                            format(nrow(vals) / length(pick), big.mark = ","))) +
    theme_diag(map = TRUE)
  add_fig(paste0("B3_map_", t), p, width = 9, height = 5.5)
}

# C. Extraction for the test run ---------------------------------------------------
stem <- sprintf("%s_%s_%s_%s_", paste(config$countries_name, collapse = "-"), config$start_time,
                config$end_time, config$name)
find_run_file <- function(suffix) {
  f <- list.files(opt$data_directory, full.names = TRUE)
  f <- f[startsWith(basename(f), stem) & endsWith(f, suffix)]
  if (length(f) == 0) return(NULL)
  f[which.max(file.mtime(f))]
}
pre_file <- find_run_file(".preprocess.rdata")
cov_file <- find_run_file(".covar.rdata")

if (is.null(pre_file) || is.null(cov_file)) {
  message("-- No test-run files for ", stem, "* in ", opt$data_directory, "; skipping section C")
} else {
  message("-- Test run: ", basename(pre_file))
  e <- new.env()
  load(pre_file, envir = e)
  load(cov_file, envir = e)
  sf_cases <- e$sf_cases
  cube <- e$covar_cube_output

  obs <- sf::st_drop_geometry(sf_cases) %>%
    dplyr::mutate(year = as.integer(format(as.Date(TL), "%Y")),
                  level = factor(paste0("ADM", admin_level)))
  obs_counts <- dplyr::count(obs, year, level)
  write_csv(obs_counts, "observations_by_year_admin_level.csv")
  levels_present <- levels(obs$level)
  lvl_cols <- setNames(c(col$s1, col$s2, col$s3, "#eda100")[seq_along(levels_present)], levels_present)
  p_c1 <- ggplot(obs_counts, aes(year, n, fill = level)) +
    geom_col(width = 0.7, colour = col$surface, linewidth = 0.4) +
    scale_fill_manual(name = "Admin level", values = lvl_cols) +
    scale_y_continuous(labels = scales::label_comma()) +
    scale_x_years() +
    labs(title = "Observations used by the test run",
         subtitle = sprintf("%s observations from %s location periods (after filters and clipping to the national boundary).",
                            format(nrow(obs), big.mark = ","), length(unique(obs$attributes.location_period_id))),
         x = NULL, y = "Observations") +
    theme_diag()
  add_fig("C1_observations_by_year", p_c1)

  if (!is.null(e$shapefiles)) {
    lp_counts <- obs %>%
      dplyr::count(attributes.location_period_id, level, name = "n_obs")
    lps <- e$shapefiles %>%
      dplyr::inner_join(lp_counts, by = c("location_period_id" = "attributes.location_period_id"))
    p_c2 <- ggplot(lps) +
      geom_sf(aes(fill = n_obs), colour = col$surface, linewidth = 0.1) +
      geom_sf(data = adm0, fill = NA, colour = col$text, linewidth = 0.4) +
      facet_wrap(~level) +
      scale_fill_gradient(name = "Observations", low = col$seq_low, high = col$seq_high, trans = "log10") +
      labs(title = "Location periods with observations", subtitle = "Fill: number of observations in each location period.") +
      theme_diag(map = TRUE)
    add_fig("C2_location_periods", p_c2, width = 9, height = 7)
  }

  covar_cube <- cube$covar_cube
  n_cells <- dim(covar_cube)[1]
  grid_t <- cube$sf_grid %>%
    dplyr::mutate(pop = covar_cube[cbind(id, t, 1)],
                  kept = long_id %in% cube$non_na_gridcells,
                  year = model_years[t],
                  pop_kept = ifelse(kept, pop, NA))
  p_c3 <- ggplot(grid_t) +
    geom_sf(aes(fill = pop_kept), colour = col$surface, linewidth = 0.1) +
    geom_sf(data = adm0, fill = NA, colour = col$text, linewidth = 0.4) +
    facet_wrap(~year) +
    scale_fill_pop() +
    labs(title = "Covariate cube: population of each model cell, by year",
         subtitle = sprintf("%s cells per year intersect the location periods; grey = dropped (no population, or low population share at a border).",
                            format(n_cells, big.mark = ","))) +
    theme_diag(map = TRUE)
  add_fig("C3_covariate_cube", p_c3, width = 10, height = 2 + 3.4 * ceiling(length(unique(grid_t$year)) / 3))

  dict <- dplyr::distinct(cube$location_periods_dict, location_period_id, rid, x, y, t, pop_weight)
  p_c4 <- ggplot(dict, aes(pop_weight)) +
    geom_histogram(bins = 40, fill = col$s1, colour = col$surface, linewidth = 0.3, boundary = 0) +
    scale_y_continuous(labels = scales::label_comma()) +
    labs(title = "Population weights of cell-to-location-period links",
         subtitle = sprintf("%s links; %s %% are whole cells (weight 1). Partial weights come from the 1 km population.",
                            format(nrow(dict), big.mark = ","), round(100 * mean(dict$pop_weight >= 0.999))),
         x = "Share of the cell's population inside the location period", y = "Links") +
    theme_diag()
  add_fig("C4_population_weights", p_c4)

  cube_tot <- sf::st_drop_geometry(grid_t) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(total = sum(pop_kept, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(series = "cube, kept cells")
  db_tot <- pop_totals %>%
    dplyr::filter(year %in% model_years) %>%
    dplyr::mutate(series = ifelse(table == "pop_1_years_1_1", "database, 1 km (area of interest)",
                                  sprintf("database, %s km (area of interest)", res_space))) %>%
    dplyr::select(year, total, series)
  tot <- dplyr::bind_rows(db_tot, cube_tot)
  write_csv(tot, "population_cube_vs_database.csv")
  series_cols <- setNames(c(col$s1, col$s2, col$s3), unique(c(sort(unique(db_tot$series)), "cube, kept cells")))
  p_c5 <- ggplot(tot, aes(year, total, colour = series)) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.8) +
    scale_colour_manual(name = NULL, values = series_cols) +
    scale_y_continuous(labels = scales::label_comma(), limits = c(0, NA)) +
    scale_x_continuous(breaks = model_years) +
    labs(title = "Population covered by the model vs in the database",
         subtitle = "The cube only holds cells that intersect location periods with data;\nthe database tables cover the whole area of interest.",
         x = NULL, y = "People") +
    theme_diag()
  add_fig("C5_population_cube_vs_database", p_c5)
}

# All figures in one PDF --------------------------------------------------------------
pdf_file <- file.path(out_dir, sprintf("db_diagnostics_%s.pdf", db_name))
grDevices::pdf(pdf_file, width = 10, height = 8, bg = col$surface)
for (f in figures) print(f$plot)
grDevices::dev.off()
cat("Wrote", length(figures), "figures to", normalizePath(out_dir), "\n")
