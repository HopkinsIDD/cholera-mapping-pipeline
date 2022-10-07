CREATE VIEW grids.master_temporal_grid AS
SELECT
  generate_series(
    time_bounds.time_left::timestamp,
    time_bounds.time_right::timestamp,
    '1 day'::interval
  )::date as time_midpoint
FROM grids.time_bounds;
