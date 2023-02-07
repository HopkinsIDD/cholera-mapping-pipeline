CREATE MATERIALIZED VIEW IF NOT EXISTS covariate_grid_map AS
SELECT
  all_covariates.covariate_name,
  all_covariates.time_left,
  all_covariates.time_right,
  all_covariates.rid as covar_rid,
  resized_spatial_grids.rid as grid_rid,
  resized_spatial_grids.width,
  resized_spatial_grids.height
FROM
  grids.resized_spatial_grids
    LEFT JOIN
  covariates.all_covariates
    ON
      st_intersects(all_covariates.rast, resized_spatial_grids.rast);
