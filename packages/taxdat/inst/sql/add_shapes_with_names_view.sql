CREATE MATERIALIZED VIEW IF NOT EXISTS shapes_with_names AS
SELECT locations.qualified_name, locations.id as location_id, location_periods.id as location_period_id, shapes.id as shape_id, shapes.shape as geom
FROM locations inner join location_periods on locations.id = location_periods.location_id inner join shapes on location_periods.id = shapes.location_period_id;
