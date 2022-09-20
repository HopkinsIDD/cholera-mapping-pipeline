CREATE MATERIALIZED VIEW IF NOT EXISTS shape_resized_spatial_grid_populations as (select
  shape_resized_spatial_grid_map_view.location_period_id,
  shape_resized_spatial_grid_map_view.shape_id,
  shape_resized_spatial_grid_map_view.grid_id,
  all_covariates.time_left,
  all_covariates.time_right,
  all_covariates.covariate_name,
  (st_summarystats(st_clip(rast,intersection_geom))).sum as intersection_population,
  (st_summarystats(st_clip(rast,grid_geom))).sum as grid_population
FROM
  shape_resized_spatial_grid_map_view
INNER JOIN
  covariates.all_covariates
    on
      st_intersects(shape_resized_spatial_grid_map_view.intersection_geom, st_envelope(all_covariates.rast))
WHERE
  covariate_name = 'population');
