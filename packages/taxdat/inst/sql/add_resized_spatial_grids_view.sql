CREATE MATERIALIZED VIEW IF NOT EXISTS grids.resized_spatial_grids AS
   SELECT
     spatial_resolutions.width_in_km as width,
     spatial_resolutions.height_in_km as height,
     resized_grid.*
   FROM
     grids.spatial_resolutions,
   LATERAL
     resize_spatial_grid(width_in_km, height_in_km) as resized_grid;
