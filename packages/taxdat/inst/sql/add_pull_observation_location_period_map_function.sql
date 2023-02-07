create or replace function pull_observation_location_period_map(location_name text, start_date date, end_date date, time_scale text)
  returns table(
    observation_id bigint,
    location_period_id bigint,
    t bigint,
    tfrac double precision
  ) AS $$
WITH
  observation_data AS
  (SELECT * FROM pull_observation_data(location_name, start_date, end_date))
SELECT
  observation_data.observation_id,
  location_periods.id as location_period_id,
  temporal_grid.id as temporal_grid_id,
  ( 1 + least(observation_data.time_right, temporal_grid.time_max) - greatest(observation_data.time_left, temporal_grid.time_min)) * 1.::double precision / ( 1 + temporal_grid.time_max - temporal_grid.time_min) as tfrac
FROM
  observation_data
INNER JOIN
  location_periods
    ON
      observation_data.location_period_id = location_periods.id
INNER JOIN
  resize_temporal_grid(time_scale) AS temporal_grid
    ON
      observation_data.time_left < temporal_grid.time_max AND
      observation_data.time_right >= temporal_grid.time_min
$$ LANGUAGE SQL SECURITY DEFINER;
