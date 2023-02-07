CREATE FUNCTION filter_location_periods(location_name text)
  returns table(id bigint, location_id bigint, qualified_name text, location_period_id bigint) AS $$
  SELECT
    ROW_NUMBER() OVER (ORDER BY 1),
    descendants.id as location_id,
    descendants.qualified_name as qualified_name,
    location_periods.id as location_period_id
  FROM
    locations as descendants
      LEFT JOIN
        location_hierarchies
           ON descendants.id = location_hierarchies.descendant_id
      LEFT JOIN
        locations as ancestors
          ON ancestors.id = location_hierarchies.ancestor_id
      LEFT JOIN
        location_periods
          ON descendants.id = location_periods.location_id
  WHERE ancestors.qualified_name = location_name;
  $$ LANGUAGE SQL SECURITY DEFINER;
