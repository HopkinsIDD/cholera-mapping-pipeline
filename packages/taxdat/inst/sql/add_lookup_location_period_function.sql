create or replace function lookup_location_period(target_location_id bigint, target_start_date date, target_end_date date)
  returns bigint
  as $$
  SELECT
    location_periods.id
  FROM
    location_periods
  WHERE
    location_periods.location_id = target_location_id AND
    location_periods.start_date <= target_start_date AND
    location_periods.end_date >= target_end_date
  $$ LANGUAGE SQL SECURITY DEFINER;
