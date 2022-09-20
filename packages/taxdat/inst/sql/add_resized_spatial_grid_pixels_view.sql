CREATE MATERIALIZED VIEW IF NOT EXISTS grids.resized_spatial_grid_pixels
AS
  SELECT
    ROW_NUMBER() OVER (ORDER BY 1) as id,
    rid,
    width,
    height,
    dp.x,
    dp.y,
    dp.geom as polygon,
    st_centroid(dp.geom) as centroid
  FROM
    grids.resized_spatial_grids, LATERAL ST_PixelAsPolygons(rast, 1) AS dp;
