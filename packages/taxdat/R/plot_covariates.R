#' @export
#' 
#' @name plot_population_map_in_scale_20_for_one_year
#' @title plot_population_map_in_scale_20_for_one_year
#' @description plot pop covariates for one year in scale 20km*20km
#' @param con cholera_covariates database connection
#' @param band_number integer
#' @return ggplot object with covariate raster
plot_population_map_in_scale_20_for_one_year <- function(con, band_number) {
  # Load required packages
  require(raster)
  require(terra)
  require(ggplot2)
  require(DBI)
  require(RPostgreSQL)
  
  # construct query
  query <- sprintf("
    SELECT 
      rid,
      ST_DumpValues(ST_Band(rast, %d)) AS vals,
      ST_UpperLeftX(rast) AS ulx,
      ST_UpperLeftY(rast) AS uly,
      ST_ScaleX(rast) AS scalex,
      ST_ScaleY(rast) AS scaley,
      ST_SRID(rast) AS srid
    FROM covariates.pop_1_years_20_20;", 
    band_number)
result <- dbGetQuery(con, query)  
# process vals
vals_clean <- gsub("NULL", "-9999", result$vals, ignore.case = TRUE) |>
  gsub("[{}]", "", x = _) |>                  
  gsub(",+", ",", x = _) |>                    
  gsub("^,|,$", "", x = _)                   

# create vals list and check the length
vals_list <- strsplit(vals_clean, ",") |>
  lapply(\(x) {
    if (length(x) < 199*199) {
      length(x) <- 199*199
      x[is.na(x)] <- "-9999"                  
    }
    x[1:(199*199)]                             
  })

# change to numeric
vals_numeric <- lapply(vals_list, as.numeric)


# raster layout（4 rows 10 cols）
n_cols <- 10  
n_rows <- 4  

# get coordinate
base_ulx <- result$ulx[1]
base_uly <- result$uly[1]
scalex <- result$scalex[1]
scaley <- result$scaley[1]  
srid <- result$srid[1]


all_rasters_terra <- lapply(1:nrow(result), \(i) {

  row_idx <- (i - 1) %/% n_cols
  col_idx <- (i - 1) %% n_cols
  
  
  current_ulx <- base_ulx + col_idx * 199 * scalex
  current_uly <- base_uly + row_idx * 199 * scaley  
 
  matrix_vals <- matrix(
  vals_numeric[[i]],
  nrow = 199,
  ncol = 199,
  byrow = TRUE
)
  matrix_vals[matrix_vals == -9999] <- NA 
  # creat raster
  terra::rast(
    matrix_vals,
    extent = terra::ext(
      current_ulx,
      current_ulx + 199 * scalex,
      current_uly + 199 * scaley,
      current_uly
    ),
    crs = paste0("EPSG:", srid)
  )
})
  class(all_rasters_terra[[1]])
  # merge rasters --------------------------------------------------------
  merged_raster <- all_rasters_terra[[1]]
  for (i in 2:length(all_rasters_terra)) {
    merged_raster <- terra::merge(merged_raster, all_rasters_terra[[i]])
  }
  
  class(merged_raster)

raster_df <- as.data.frame(merged_raster, xy = TRUE, na.rm = TRUE)
colnames(raster_df) <- c("x", "y", "population")


breaks <- c(0, 10, 50, 100, 500, 1000, Inf)
labels <- c("0-10", "10-50", "50-100", "100-500", "500-1000", ">1000")


population_plot <- ggplot() +
  geom_raster(
    data = raster_df,
    aes(x = x, y = y, fill = cut(population, breaks, labels = labels))
  ) +
  scale_fill_viridis_d(
    name = "Population Density\n(persons/km²)",
    option = "magma",
    na.value = "gray90"
  ) +
  labs(
    title = sprintf("Population Distribution (Band %d)", band_number),
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "right",
    legend.key.height = unit(2, "cm"),  
    plot.margin = margin(2, 2, 2, 2, "cm")  
  )
  
  # safe figure
  output_file <- sprintf("population_map_band%d.pdf", band_number)
  ggsave(
    filename = output_file,
    plot = population_plot,
    width = 16,
    height = 12,
    dpi = 300
  ) 
  # return object
  return(merged_raster)
}