DROP TABLE IF EXISTS observations CASCADE;
CREATE TABLE observations(
  id bigint,
  observation_collection_id bigint,
  time_left date,
  time_right date,
  location_period_id bigint,
  location_id bigint,
  "primary" boolean,
  phantom boolean,
  suspected_cases integer,
  confirmed_cases integer,
  deaths integer
);

DROP TABLE IF EXISTS locations CASCADE;
CREATE TABLE locations(id bigint, qualified_name text);

DROP TABLE IF EXISTS location_periods CASCADE;
CREATE TABLE location_periods(id bigint, location_id bigint, start_date date, end_date date);

DROP TABLE IF EXISTS shapes CASCADE;
CREATE TABLE shapes(id bigint, location_period_id bigint, shape GEOMETRY, box GEOMETRY);
INSERT INTO shapes VALUES(1,1,ST_SETSRID('POLYGON((0 0,0 1,1 1,1 0,0 0))'::geometry,4326), ST_SETSRID('POLYGON((0 0,0 1,1 1,1 0,0 0))'::geometry,4326));

DROP TABLE IF EXISTS location_hierarchies CASCADE;
CREATE TABLE location_hierarchies(ancestor_id bigint, descendant_id bigint, generations integer);

DROP TABLE IF EXISTS grids.master_spatial_grid;
CREATE TABLE grids.master_spatial_grid as
  select 1 as rid, st_asraster(shape, 10, 10, '32BF') as rast
  from (SELECT shape from shapes where id = 1) as local_shapes;


DROP TABLE IF EXISTS covariates.population_2000;
CREATE TABLE covariates.population_2000 as SELECT * from grids.master_spatial_grid;

drop table if exists grids.resized_spatial_grids cascade;
\i ../../Analysis/sql/setup_database.sql

INSERT INTO locations VALUES (1,'somelocation');
INSERT INTO location_hierarchies VALUES(1,1,0);
INSERT INTO location_periods VALUES (1,1,'2000-01-01', '2000-12-31');
INSERT INTO observations VALUES (1,1,'2000-01-01', '2000-12-31', 1, 1, TRUE, TRUE, 100, 10, 1);

select ingest_resized_spatial_grid(1,1);
select ingest_covariate('population','population_2000','2000-01-01','2000-12-31');
REFRESH MATERIALIZED VIEW grids.resized_spatial_grid_polygons;
REFRESH MATERIALIZED VIEW grids.resized_spatial_grid_centroids;
REFRESH MATERIALIZED VIEW location_period_raster_map;
REFRESH MATERIALIZED VIEW location_period_raster_grid_map;
REFRESH MATERIALIZED VIEW covariate_grid_map;
