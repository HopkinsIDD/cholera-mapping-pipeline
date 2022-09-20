CREATE MATERIALIZED VIEW IF NOT EXISTS grids.master_spatial_grid_centroids
AS
  SELECT
    ROW_NUMBER() OVER (ORDER BY 1) as id,
    rid,
    dp.x,
    dp.y,
    dp.geom
  FROM
    grids.master_spatial_grid, LATERAL ST_PixelAsCentroids(rast, 1) AS dp;
