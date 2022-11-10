CREATE MATERIALIZED VIEW resized_covariates as (
select
  resized_spatial_grid_pixels.rid as grid_rid,
  resized_spatial_grid_pixels.id as grid_id,
  all_covariates.raster_covariate_collection_id,
  all_covariates.filename,
  all_covariates.time_left,
  all_covariates.time_right,
  all_covariates.rid,
  (st_summarystats(st_clip(all_covariates.rast, resized_spatial_grid_pixels.polygon))).sum
from
  grids.resized_spatial_grid_pixels
inner join
  covariate_grid_map
    on
      resized_spatial_grid_pixels.rid = covariate_grid_map.grid_rid
      AND resized_spatial_grid_pixels.height = covariate_grid_map.height
      AND resized_spatial_grid_pixels.width = covariate_grid_map.width
inner join
  covariates.all_covariates
    on
      all_covariates.filename = covariate_grid_map.filename
      and covariate_grid_map.time_left = all_covariates.time_left
      and covariate_grid_map.time_right = all_covariates.time_right
      and covariate_grid_map.covar_rid = all_covariates.rid
      and st_intersects(st_envelope(all_covariates.rast), resized_spatial_grid_pixels.polygon)
);
