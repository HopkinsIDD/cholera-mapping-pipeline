create or replace function pull_boundary_polygon(location_name text)
  returns table(
    shape geometry
  ) AS $$
SELECT shapes.shape
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
WHERE
  locations.qualified_name = location_name
$$ LANGUAGE SQL SECURITY DEFINER;
