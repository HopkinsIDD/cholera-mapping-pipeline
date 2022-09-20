create or replace function pull_location_period_grid_map_interior(location_name text, start_date date, end_date date, width_in_km int, height_in_km int, time_scale text)
RETURNS TABLE(qualified_name text, location_id bigint, location_period_id bigint, shape_id bigint, spatial_grid_id bigint, rid int, x int, y int, t bigint, sfrac double precision) AS $$
  SELECT
    location_periods.qualified_name as qualified_name,
    location_periods.location_id as location_id,
    location_periods.location_period_id as location_period_id,
    shapes.id as shape_id,
    spatial_grid.id as spatial_grid_id,
    spatial_grid.rid,
    spatial_grid.x,
    spatial_grid.y,
    temporal_grid.id as t,
    1.::double precision as sfrac
  FROM
    filter_location_periods(location_name) as location_periods
  LEFT JOIN
    shapes
      on
        location_periods.location_period_id = shapes.location_period_id
  LEFT JOIN
    grids.resized_spatial_grid_pixels as spatial_grid
      ON
        st_intersects(shapes.shape,spatial_grid.centroid)
  FULL JOIN
    resize_temporal_grid(time_scale) as temporal_grid
      ON
        1=1
  WHERE
    temporal_grid.time_midpoint <= end_date
    AND temporal_grid.time_midpoint >= start_date
    AND spatial_grid.width = width_in_km
    AND spatial_grid.height = height_in_km
  $$ LANGUAGE SQL;