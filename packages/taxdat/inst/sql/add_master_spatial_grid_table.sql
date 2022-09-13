CREATE MATERIALIZED VIEW grids.master_spatial_grid as
  select 1 as rid, st_asraster(shape, 10, 10, '32BF') as rast
  from (SELECT shape from shapes where id = 1) as local_shapes;
