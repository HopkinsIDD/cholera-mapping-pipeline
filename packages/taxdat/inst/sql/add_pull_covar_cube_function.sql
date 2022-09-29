CREATE FUNCTION pull_covar_cube(location_name text, start_date date, end_date date, width_in_km int, height_in_km int, time_scale text)
RETURNS TABLE(covariate_name text, t bigint, id bigint, rid int, x int, y int, value double precision, geometry geometry)AS $$
SELECT
  raster_covariate_collections.name,
  temporal_grid.id as t,
  grid_pixels.id as grid_id,
  grid_pixels.rid as grid_rid,
  grid_pixels.x,
  grid_pixels.y,
  resized_covariates.sum as value,
  grid_pixels.polygon as geometry
FROM
  filter_resized_spatial_grid_pixels_to_location(location_name, width_in_km, height_in_km) as grid_pixels
INNER JOIN
  resized_covariates
    ON
      grid_pixels.id = resized_covariates.grid_id
      AND grid_pixels.rid = resized_covariates.grid_rid
INNER JOIN
  resize_temporal_grid(time_scale) as temporal_grid
    ON
      resized_covariates.time_left <= temporal_grid.time_midpoint AND
      resized_covariates.time_right >= temporal_grid.time_midpoint
INNER
  JOIN public.raster_covariate_collections
    ON
      resized_covariates.raster_covariate_collection_id = raster_covariate_collections.id
WHERE
    temporal_grid.time_midpoint >= start_date AND
    temporal_grid.time_midpoint <= end_date
$$ LANGUAGE SQL SECURITY DEFINER;
