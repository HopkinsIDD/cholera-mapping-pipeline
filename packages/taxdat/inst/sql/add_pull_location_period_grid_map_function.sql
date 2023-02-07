create or replace function pull_location_period_grid_map(location_name text, start_date date, end_date date, width_in_km int, height_in_km int, time_scale text)
RETURNS TABLE(qualified_name text, location_id bigint, location_period_id bigint, shape_id bigint, spatial_grid_id bigint, rid int, x int, y int, t bigint, sfrac double precision) AS $$
SELECT
  qualified_name, location_id, location_period_id, shape_id, spatial_grid_id, rid, x, y, t, min(sfrac)
FROM (
  SELECT * FROM pull_location_period_grid_map_interior(location_name, start_date, end_date, width_in_km, height_in_km, time_scale)
    UNION ALL
  SELECT * FROM pull_location_period_grid_map_boundary(location_name, start_date, end_date, width_in_km, height_in_km, time_scale)
) AS tmp
GROUP BY
  qualified_name, location_id, location_period_id, shape_id, spatial_grid_id, rid, x, y, t
;
  $$ LANGUAGE SQL;