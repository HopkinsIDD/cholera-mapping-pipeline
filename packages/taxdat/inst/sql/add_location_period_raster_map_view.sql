CREATE MATERIALIZED VIEW location_period_raster_map AS
  SELECT
    shapes.id as shape_id,
    location_period_id,
    rid,
    width,
    height
  FROM
    shapes
      LEFT JOIN
    grids.resized_spatial_grids
      on
        ST_INTERSECTS(shapes.box, resized_spatial_grids.rast);
