-- Create master grid:
-- Relies on shapes.shape and shapes.box having srid 4326 (same as grid) edan gnivah xob.s shapee
-- Download ftp://ftp.worldpop.org.uk/GIS/Population/Global_2000_2020/2020/0_Mosaicked/ppp_2020_1km_Aggregated.tif
-- raster2pgsql -s EPSGS:4326 -I -t auto -d {master_grid_filename}  grids.master_grid

-- Create Raw Covariate Tables

-- Relies on covariates.<covariate_name>_<time-range> already existing
-- Get shapefiles from github
-- raster2pgsql -s EPSGS:4326 -I -t auto -d {covariate_for_year}  covariates.{covar_name}_{covar_year}

-- Set SRID for shapes table in dan's grid

CREATE MATERIALIZED VIEW
  grids.master_grid_centroids
AS
  SELECT
    rid,
    dp.*
  FROM
    grids.master_grid, LATERAL ST_PixelAsCentroids(rast, 1) AS dp;

CREATE INDEX master_grid_centroids_gidx ON grids.master_grid_centroids USING GIST(geom);
CREATE INDEX master_grid_centroids_idx ON grids.master_grid_centroids (rid,x,y);
VACUUM ANALYZE grids.master_grid_centroids;

CREATE TABLE grids.resized_grids(rid integer, rast raster, width int, height int);
CREATE INDEX grids_resized_grids_size_idx ON grids.resized_grids (width, height, rid);

CREATE TABLE covariates.all_covariates(covariate_name text, time_left date, time_right date, rid integer, rast raster);
CREATE INDEX covariate_name_size_idx ON covariates.all_covariates(covariate_name, time_left, time_right, rid);

CREATE MATERIALIZED VIEW grids.resized_grid_centroids
AS
  SELECT
    rid,
    width,
    height,
    dp.*
  FROM
    grids.resized_grids, LATERAL ST_PixelAsCentroids(rast, 1) AS dp;

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
    grids.resized_grids
      on
        ST_INTERSECTS(shapes.box, resized_grids.rast);

-- Create functions for extracting from the grid

create or replace function resize_grid(width_in_km int, height_in_km int)
  returns table(rid integer, rast raster)
  as $$
  SELECT
    master_grid.rid,
    st_resample(
      master_grid.rast,
      (st_metadata(master_grid.rast)).width / width_in_km,
      (st_metadata(master_grid.rast)).height / height_in_km
    ) as rast
  FROM
    grids.master_grid;
  $$ LANGUAGE SQL;

create or replace function ingest_resized_grid(width_in_km int, height_in_km int)
  returns void
  LANGUAGE plpgsql
  as $$
  begin
  PERFORM * FROM grids.resized_grids where resized_grids.width = width_in_km AND resized_grids.height = height_in_km;
  if not found then
     INSERT INTO grids.resized_grids SELECT rid, rast, width_in_km as width, height_in_km as height FROM resize_grid(width_in_km, height_in_km);
  end if;
  end;
  $$ SECURITY DEFINER;

create or replace function ingest_covariate(name text, table_name text, ingest_time_left date, ingest_time_right date)
  returns void
  LANGUAGE plpgsql
  as $$
  begin
  PERFORM * FROM covariates.all_covariates where all_covariates.covariate_name = name AND all_covariates.time_left = ingest_time_left and all_covariates.time_right = ingest_time_right;
  if not found then
  EXECUTE FORMAT ('INSERT INTO covariates.all_covariates SELECT %L as name, %L as time_left, %L as time_right, rid, rast FROM covariates.%I', name, ingest_time_left, ingest_time_right, table_name);
  end if;
  end;
  $$ SECURITY DEFINER;

create or replace function filter_location_periods(location_name text)
  returns table(location_id bigint, qualified_name text, location_period_id bigint) AS $$
  SELECT
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
  $$ LANGUAGE SQL;

create or replace function location_period_raster_centroid_map(location_name text, width_in_km int, height_in_km int)
  returns table(location_period_id bigint, rid int, x int, y int) AS $$
  SELECT
    filtered_location_periods.location_period_id,
    resized_grid_centroids.rid,
    resized_grid_centroids.x,
    resized_grid_centroids.y
  FROM
    filter_location_periods(location_name) as filtered_location_periods
      LEFT JOIN
        shapes
          ON
            filtered_location_periods.location_period_id = shapes.location_period_id
      LEFT JOIN
        location_period_raster_map
	  ON
	    location_period_raster_map.location_period_id = filtered_location_periods.location_period_id AND
	    location_period_raster_map.shape_id = shapes.id
      LEFT JOIN
        grids.resized_grid_centroids
          ON
	    location_period_raster_map.rid = grids.resized_grid_centroids.rid AND
            st_within(resized_grid_centroids.geom, shapes.shape)
  WHERE
    resized_grid_centroids.width = width_in_km AND
    resized_grid_centroids.height = height_in_km
  $$ LANGUAGE SQL;

create or replace function filter_resized_grid_centroids_to_location(location_name text, width_in_km int, height_in_km int)
  returns table(rid int,x int,y int, geom geometry) AS $$
  SELECT
    resized_grid_centroids.rid,
    resized_grid_centroids.x,
    resized_grid_centroids.y,
    resized_grid_centroids.y,
    resized_grid_centroids.geom
  FROM
    locations
      LEFT JOIN
    location_periods
      ON
        locations.id = location_periods.location_id
      LEFT JOIN
    location_period_raster_map
      ON
        location_period_raster_map.location_period_id = location_periods.id
      LEFT JOIN
    grids.resized_grid_centroids
      ON
        location_period_raster_map.rid = resized_grid_centroids.rid AND
        location_period_raster_map.width = resized_grid_centroids.width AND
        location_period_raster_map.height = resized_grid_centroids.height
  WHERE
    locations.qualified_name = location_name;
  $$ LANGUAGE SQL;

CREATE MATERIALIZED VIEW covariate_grid_map AS
SELECT
  all_covariates.covariate_name,
  all_covariates.time_left,
  all_covariates.time_right,
  all_covariates.rid as covar_rid,
  resized_grids.rid as grid_rid,
  resized_grids.width,
  resized_grids.height
FROM
  grids.resized_grids
    LEFT JOIN
  covariates.all_covariates
    ON
      st_intersects(all_covariates.rast, resized_grids.rast);

-- Pipeline Specific Functionality

create or replace function pull_observation_data(location_name text, start_date date, end_date date)
  returns table(location_name text, location_id bigint, location_period_id bigint, suspected_cases int, confirmed_cases int, deaths int, time_left date, time_right date, shape geometry) AS $$
  SELECT
    filtered_location_periods.qualified_name as location_name,
    filtered_location_periods.location_id,
    observations.location_period_id,
    observations.suspected_cases,
    observations.confirmed_cases,
    observations.deaths,
    observations.time_left,
    observations.time_right,
    shapes.shape
  FROM
    filter_location_periods(location_name) as filtered_location_periods
      LEFT JOIN
        observations
          ON
	    observations.location_period_id = filtered_location_periods.location_period_id
      LEFT JOIN
        shapes
	  ON
	    observations.location_period_id = shapes.location_period_id;
  $$ LANGUAGE SQL;

create or replace function pull_covar_cube(location_name text, start_date date, end_date date, width_in_km int, height_in_km int)
RETURNS TABLE(covariate_name text, time_left date, time_right date, rid int, x int, y int, value double precision)AS $$
  SELECT
    all_covariates.covariate_name,
    all_covariates.time_left,
    all_covariates.time_right,
    grid_centroids.rid,
    grid_centroids.x,
    grid_centroids.y,
    ST_VALUE(all_covariates.rast, grid_centroids.geom) as value
  FROM filter_resized_grid_centroids_to_location(location_name, width_in_km, height_in_km) as grid_centroids
    INNER JOIN
      covariate_grid_map
        ON
          grid_centroids.rid = covariate_grid_map.grid_rid
    INNER JOIN
      covariates.all_covariates
        on
	  covariate_grid_map.covar_rid = all_covariates.rid AND
	  covariate_grid_map.time_left = all_covariates.time_left AND
	  covariate_grid_map.time_right = all_covariates.time_right AND
	  covariate_grid_map.covariate_name = all_covariates.covariate_name AND
          st_intersects(all_covariates.rast, grid_centroids.geom)
  WHERE
    all_covariates.time_left >= start_date AND all_covariates.time_right <= end_date;
  $$ LANGUAGE SQL;


-------------- OLD CODE -----------------
create or replace function filter_grid_to_location(location_name text, width_in_km int, height_in_km int)
  returns table(rid integer, rast raster)
  language plpgsql
  as $$
  begin
  return QUERY
  SELECT
    grid.rid, grid.rast
  from
    locations
      left join
    location_periods
      on
        locations.id = location_periods.location_id
      left join
    shapes
      on
        location_periods.id = shapes.location_period_id
      left join
    resize_grid(width_in_km, height_in_km) grid
      on
        st_intersects(grid.rast::geometry, shape)
  where
    locations.qualified_name = location_name;
  end;
  $$;

create or replace function get_resized_centroids(width_in_km int, height_in_km int)
  returns table(rid int, x int, y int, geom geometry)
  language plpgsql
  as $$
  declare
    my_query text;
    my_record table(int,int,int,geometry);
  begin
  EXECUTE format('
    CREATE MATERIALIZED VIEW IF NOT EXISTS grids.%I AS
    SELECT rid, dp.x, dp.y, dp.geom
    FROM resize_grid(%L, %L), LATERAL ST_PixelAsCentroids(rast) as dp',
    'resized_grid_' || width_in_km || '_' || height_in_km || '_centroids', width_in_km, height_in_km
  );

  EXECUTE format(
    'CREATE INDEX IF NOT EXISTS %I on grids.%I USING GIST(geom)',
      'grids_resized_grid_' || width_in_km || '_' || height_in_km || '_gidx',
      'resized_grid_' || width_in_km || '_' || height_in_km || '_centroids'
  );

  EXECUTE format(
    'CREATE INDEX IF NOT EXISTS %I on grids.%I (rid,x,y)',
      'grids_resized_grid_' || width_in_km || '_' || height_in_km || '_idx',
      'resized_grid_' || width_in_km || '_' || height_in_km || '_centroids'
  );

  -- EXECUTE format('VACUUM ANALYZE grids.%I', 'resized_grid_' || width_in_km || '_' || height_in_km || '_centroids');

  my_query = format('
    SELECT
      %1$I.rid,
      %1$I.x,
      %1$I.y,
      %1$I.geom
    FROM
      grids.%1$I',
    'resized_grid_' || width_in_km || '_' || height_in_km || '_centroids');
    my_record = EXECUTE my_query
    RETURN my_record
  end;
  $$;

create or replace function filter_location_periods(location_name text)
  returns table(location_id bigint, qualified_name text, location_period_id bigint)
  language plpgsql
  as $$
  begin
  EXECUTE FORMAT('
    CREATE MATERIALIZED VIEW if not exists %I
      AS
        SELECT
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
        WHERE ancestors.qualified_name = %L;',
    'location_periods_' || location_name, location_name);

  RETURN QUERY
  EXECUTE FORMAT('
    SELECT
      %1$I.location_id,
      %1$I.qualified_name::text,
      %1$I.location_period_id
    FROM
      %1$I',
    'location_periods_' || location_name
  );
  end;
  $$;

create or replace function location_period_raster_centroid_map(location_name text, width_in_km int, height_in_km int)
  returns table(location_period_id bigint, rid int, x int, y int) AS $$
  SELECT
    filtered_location_periods.location_period_id,
    grid_centroids.rid,
    grid_centroids.x,
    grid_centroids.y
  FROM
    filter_location_periods(location_name) as filtered_location_periods
      LEFT JOIN
        shapes
          ON
            filtered_location_periods.location_period_id = shapes.location_period_id
      LEFT JOIN
        get_resized_centroids(width_in_km, height_in_km) as grid_centroids
          ON
            st_within(grid_centroids.geom, shapes.shape)
    WHERE
      filtered_location_periods.location_period_id is not null AND
      grid_centroids.rid is not null
  $$ LANGUAGE SQL;

-- Create location period gridcell link :
CREATE MATERIALIZED VIEW
  location_period_raster_map
AS
  SELECT
    location_periods.id as location_period_id,
    grids.master_grid_centroids.rid,
    grids.master_grid_centroids.x,
    grids.master_grid_centroids.y
  FROM
    location_periods
      LEFT JOIN
        shapes
          ON
            location_periods.id = shapes.location_period_id
      LEFT JOIN
        grids.master_grid_centroids
          ON
            st_within(grids.master_grid_centroids.geom, shapes.shape);


SELECT
  observation_collections.id as observation_collection_id,
  observations.id as observation_id,
  locations.id as location_id,
  location_periods.id as location_period_id,
  observations.suspected_cases as suspected_cases,
  location_period_raster_map.rid as rid,
  location_period_raster_map.x as x,
  location_period_raster_map.y as y
FROM
  observations
    LEFT JOIN
  observation_collections
    ON
      observations.observation_collection_id = observation_collections.id
    LEFT JOIN
  locations
    ON
      locations.id = observations.location_id
    LEFT JOIN
  location_periods
    ON
      location_periods.id = observations.location_period_id
    LEFT JOIN
  location_hierarchies
    ON
      location_hierarchies.descendant_id = observations.location_id
    LEFT JOIN
  locations as parent_locations
    ON
      location_hierarchies.ancestor_id = parent_locations.id
    LEFT JOIN
  location_period_raster_map
    ON
      location_periods.id = location_period_raster_map.location_period_id
  WHERE
    parent_locations.qualified_name = 'AFR::KEN::Central'
    AND observations.time_left >= '2020-01-01'
    AND observations.time_right <= '2020-12-31'
;









CREATE MATERIALIZED VIEW grids.ken_grid as
select
  rid,
  rast
from
  locations
    left join
  location_periods
    on
      locations.id = location_periods.location_id
    left join
  shapes
    on
      location_periods.id = shapes.location_period_id
    left join
  grids.master_grid
    on
      st_intersects(rast::geometry, shape)
where
  locations.qualified_name = 'AFR::KEN';

CREATE MATERIALIZED VIEW
  grids.ken_grid_centroids
AS
  SELECT
    rid,
    dp.*
  FROM
    grids.ken_grid, LATERAL ST_PixelAsCentroids(rast, 1) AS dp;

CREATE INDEX ken_grid_centroids_gidx ON grids.ken_grid_centroids USING GIST(geom);
CREATE INDEX ken_grid_centroids_idx ON grids.ken_grid_centroids (rid,x,y);
VACUUM ANALYZE grids.ken_grid_centroids;

CREATE MATERIALIZED VIEW location_periods_BFA AS
  SELECT
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
  WHERE ancestors.qualified_name = 'AFR::BFA';
-- Create location period gridcell link :
CREATE MATERIALIZED VIEW
  location_period_raster_map_BFA
AS
  SELECT
    location_periods_bfa.location_period_id,
    resized_grid_20_20_centroids.rid,
    resized_grid_20_20_centroids.x,
    resized_grid_20_20_centroids.y
  FROM
    location_periods_bfa
      LEFT JOIN
        shapes
          ON
            location_periods_bfa.location_period_id = shapes.location_period_id
      LEFT JOIN
        grids.resized_grid_20_20_centroids
          ON
            st_within(resized_grid_20_20_centroids.geom, shapes.shape);


SELECT
  observation_collections.id as observation_collection_id,
  observations.id as observation_id,
  locations.id as location_id,
  location_periods_ken.id as location_period_id,
  observations.suspected_cases as suspected_cases,
  location_period_raster_map_ken.rid as rid,
  location_period_raster_map_ken.x as x,
  location_period_raster_map_ken.y as y
FROM
  observations
    LEFT JOIN
  observation_collections
    ON
      observations.observation_collection_id = observation_collections.id
    LEFT JOIN
  locations
    ON
      locations.id = observations.location_id
    LEFT JOIN
  location_periods_ken
    ON
      location_periods_ken.id = observations.location_period_id
    LEFT JOIN
  location_hierarchies
    ON
      location_hierarchies.descendant_id = observations.location_id
    LEFT JOIN
  locations as parent_locations
    ON
      location_hierarchies.ancestor_id = parent_locations.id
    LEFT JOIN
  location_period_raster_map_ken
    ON
      location_periods_ken.id = location_period_raster_map_ken.location_period_id
  WHERE
    parent_locations.qualified_name = 'AFR::KEN::Central'
    AND observations.time_left >= '2020-01-01'
    AND observations.time_right <= '2020-12-31'
;


  SELECT
    filtered_location_periods.qualified_name as location_name,
    filtered_location_periods.location_period_id as location_period_id,
    shapes.id as shape_id,
    location_period_raster_map.*
  FROM
    filter_location_periods('AFR::BFA') as filtered_location_periods
      LEFT JOIN
        shapes
          ON
            filtered_location_periods.location_period_id = shapes.location_period_id
      LEFT JOIN
        location_period_raster_map
	  ON
	    location_period_raster_map.location_period_id = filtered_location_periods.location_period_id AND
	    location_period_raster_map.shape_id = shapes.id
      LEFT JOIN
        grids.resized_grid_centroids
          ON
	    location_period_raster_map.rid = grids.resized_grid_centroids.rid AND
            st_within(resized_grid_centroids.geom, shapes.shape)
  WHERE
    resized_grid_centroids.width = 20 AND
    resized_grid_centroids.height = 20;
