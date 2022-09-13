create or replace function resize_spatial_grid(width_in_km int, height_in_km int)
  returns table(rid integer, rast raster)
  as $$
  SELECT
    master_spatial_grid.rid,
    st_resample(
      master_spatial_grid.rast,
      (st_metadata(master_spatial_grid.rast)).width / width_in_km,
      (st_metadata(master_spatial_grid.rast)).height / height_in_km
    ) as new_rast
  FROM
    grids.master_spatial_grid;
  $$ LANGUAGE SQL SECURITY DEFINER;
