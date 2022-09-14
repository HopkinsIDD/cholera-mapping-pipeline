create or replace function filter_resized_spatial_grid_pixels_to_location(location_name text, width_in_km int, height_in_km int)
  returns table(id bigint, rid int, x int, y int, centroid geometry, polygon geometry) AS $$
 SELECT
    DISTINCT
    spatial_grid.id,
    spatial_grid.rid,
    spatial_grid.x,
    spatial_grid.y,
    spatial_grid.centroid,
    spatial_grid.polygon
  FROM
    filter_location_periods(location_name) as location_periods
  LEFT JOIN
    shapes
      on
        location_periods.location_period_id = shapes.location_period_id
  LEFT JOIN
    shape_resized_spatial_grid_populations
      ON
        shape_resized_spatial_grid_populations.shape_id = shapes.id
  LEFT JOIN
    grids.resized_spatial_grid_pixels as spatial_grid
      ON
        shape_resized_spatial_grid_populations.grid_id = spatial_grid.id
  WHERE
    spatial_grid.width = width_in_km AND
    spatial_grid.height = height_in_km AND
    (
      (shape_resized_spatial_grid_populations.grid_population > 0) OR
      (shape_resized_spatial_grid_populations.intersection_population IS NULL)
    )
  $$ LANGUAGE SQL SECURITY DEFINER;
