CREATE FUNCTION pull_observation_data(location_name text, start_date date, end_date date)
  returns table(
    observation_collection_id bigint,
    observation_id bigint,
    location_name text,
    location_id bigint,
    location_period_id bigint,
    suspected_cases int,
    confirmed_cases int,
    deaths int,
    time_left date,
    time_right date,
    is_primary bool,
    is_phantom bool,
    shape geometry
  ) AS $$
  SELECT
    observations.observation_collection_id,
    observations.id as observation_id,
    filtered_location_periods.qualified_name as location_name,
    filtered_location_periods.location_id,
    observations.location_period_id,
    observations.suspected_cases,
    observations.confirmed_cases,
    observations.deaths,
    observations.time_left,
    observations.time_right,
    observations.primary,
    observations.phantom,
    shapes.shape
  FROM
    observations
      INNER JOIN
        filter_location_periods(location_name) as filtered_location_periods
          ON
    observations.location_period_id = filtered_location_periods.location_period_id
  LEFT JOIN
    shapes
      ON
        observations.location_period_id = shapes.location_period_id
  WHERE
    observations.time_left >= start_date AND
    observations.time_right <= end_date;
  $$ LANGUAGE SQL SECURITY DEFINER;
