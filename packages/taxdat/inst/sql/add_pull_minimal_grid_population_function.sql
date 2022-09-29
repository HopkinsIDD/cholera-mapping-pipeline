CREATE FUNCTION pull_minimal_grid_population(location_name text, start_date date, end_date date, time_scale text)
  returns table(
    rid int,
    temporal_grid_id bigint,
    rast raster
  ) AS $$
SELECT
  rid,
  temporal_grid.id as t,
  rast
FROM
  locations
INNER JOIN
  location_periods
    ON
      locations.id = location_periods.location_id
INNER JOIN
  shapes
    ON
      location_periods.id = shapes.location_period_id
INNER JOIN
  covariates.all_covariates
    ON
      st_intersects(shapes.shape, st_envelope(rast))
INNER JOIN
  resize_temporal_grid(time_scale) as temporal_grid
    ON
      covariates.all_covariates.time_left <= temporal_grid.time_midpoint
      AND covariates.all_covariates.time_right >= temporal_grid.time_midpoint
INNER
  JOIN public.raster_covariate_collections
    ON
      all_covariates.raster_covariate_collection_id = raster_covariate_collections.id
WHERE
  time_midpoint >=start_date
  AND time_midpoint <= end_date
  AND raster_covariate_collections.name = 'population'
  AND locations.qualified_name = location_name
$$ LANGUAGE SQL SECURITY DEFINER;
