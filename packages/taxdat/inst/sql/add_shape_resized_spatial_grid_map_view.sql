CREATE MATERIALIZED VIEW shape_resized_spatial_grid_map_view AS(
SELECT  l.qualified_name, l.location_period_id as location_period_id, l.shape_id, p.id as grid_id, ST_Intersection(p.polygon, l.geom) as intersection_geom, p.polygon as grid_geom
FROM
  grids.resized_spatial_grid_pixels p,
  shapes_with_names l
WHERE
l.valid = true
AND ST_Intersects(p.polygon, l.boundary) );
