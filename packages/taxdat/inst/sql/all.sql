CREATE SCHEMA IF NOT EXISTS grids;
   CREATE MATERIALIZED VIEW grids.master_spatial_grid as
   select 1 as rid, st_asraster(shape, 10, 10, ‘32BF’) as rast
   from (SELECT shape from shapes where id = 1) as local_shapes;

   CREATE TABLE grids.resized_spatial_grids(rid integer, rast raster, width int, height int);
   CREATE INDEX grids_resized_spatial_grids_size_idx ON grids.resized_spatial_grids (width, height, rid);

   CREATE TABLE grids.time_bounds(time_left date, time_right date);
   INSERT INTO grids.time_bounds VALUES(‘2000-01-01’, ‘2020-12-31’);

   CREATE OR REPLACE VIEW grids.master_temporal_grid AS
   SELECT generate_series(time_bounds.time_left::timestamp, time_bounds.time_right::timestamp, ‘1 day’::interval)::date as time_midpoint FROM grids.time_bounds;

   CREATE MATERIALIZED VIEW IF NOT EXISTS grids.resized_spatial_grid_polygons
   AS SELECT rid, width, height, dp.*
    FROM grids.resized_spatial_grids, LATERAL ST_PixelAsPolygons(rast, 1) AS dp;

  CREATE MATERIALIZED VIEW
IF
	NOT EXISTS grids.resized_spatial_grid_centroids AS SELECT
	rid,
	width,
	height,
	dp.*
FROM
	grids.resized_spatial_grids,
	LATERAL ST_PixelAsCentroids ( rast, 1 ) AS dp;

CREATE MATERIALIZED VIEW
IF
	NOT EXISTS location_period_raster_map AS SELECT
	shapes.ID AS shape_id,
	location_period_id,
	rid,
	width,
	height
FROM
	shapes
	LEFT JOIN grids.resized_spatial_grids ON ST_INTERSECTS ( shapes.box, resized_spatial_grids.rast );

CREATE MATERIALIZED VIEW
IF
	NOT EXISTS covariate_grid_map AS SELECT
	all_covariates.filename AS covariate_name,
	all_covariates.time_left,
	all_covariates.time_right,
	all_covariates.rid AS covar_rid,
	resized_spatial_grids.rid AS grid_rid,
	resized_spatial_grids.width,
	resized_spatial_grids.height
FROM
	grids.resized_spatial_grids
	LEFT JOIN covariates.all_covariates ON st_intersects ( all_covariates.rast, resized_spatial_grids.rast );

CREATE OR REPLACE FUNCTION resize_spatial_grid ( width_in_km INT, height_in_km INT ) RETURNS TABLE ( rid INTEGER, rast raster ) AS $$ SELECT
master_spatial_grid.rid,
st_resample (
	master_spatial_grid.rast,
	( st_metadata ( master_spatial_grid.rast ) ).width / width_in_km,
	( st_metadata ( master_spatial_grid.rast ) ).height / height_in_km
) AS rast
FROM
	grids.master_spatial_grid;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION lookup_location_period ( target_location_id BIGINT, target_start_date DATE, target_end_date DATE ) RETURNS BIGINT AS $$ SELECT
location_periods.ID
FROM
	location_periods
WHERE
	location_periods.location_id = target_location_id
	AND location_periods.start_date <= target_start_date
	AND location_periods.end_date >= target_end_date
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION trigger_lookup_missing_location_period_for_observations ( ) RETURNS TRIGGER LANGUAGE plpgsql AS $func$ BEGIN
	NEW.location_period_id := lookup_location_period (
		NEW.location_id,
		NEW.time_left,
		NEW.time_right
	);
	RETURN NEW;
END $func$;

CREATE OR REPLACE FUNCTION ingest_resized_spatial_grid ( width_in_km INT, height_in_km INT ) RETURNS void LANGUAGE plpgsql AS $$ BEGIN
	PERFORM *
	FROM
		grids.resized_spatial_grids
	WHERE
		resized_spatial_grids.width = width_in_km
		AND resized_spatial_grids.height = height_in_km;
	IF
	NOT FOUND THEN
			INSERT INTO grids.resized_spatial_grids SELECT
			rid,
			rast,
			width_in_km AS width,
			height_in_km AS height
		FROM
			resize_spatial_grid ( width_in_km, height_in_km );

	END IF;
	END;
$$ SECURITY DEFINER;

CREATE OR REPLACE FUNCTION ingest_covariate (
	NAME TEXT,
	TABLE_NAME TEXT,
	ingest_time_left DATE,
	ingest_time_right DATE
	) RETURNS void LANGUAGE plpgsql AS $$ BEGIN
	PERFORM *
	FROM
		covariates.all_covariates
	WHERE
		all_covariates.filename = NAME
		AND all_covariates.time_left = ingest_time_left
		AND all_covariates.time_right = ingest_time_right;
	IF
	NOT FOUND THEN
			EXECUTE FORMAT ( ‘INSERT INTO covariates.all_covariates SELECT %L as name, %L as time_left, %L as time_right, rid, rast FROM covariates.%I’, NAME, ingest_time_left, ingest_time_right, TABLE_NAME );

	END IF;

END;
$$ SECURITY DEFINER;

CREATE OR REPLACE FUNCTION filter_resized_spatial_grid_centroids_to_location ( location_name TEXT, width_in_km INT, height_in_km INT ) RETURNS TABLE (
	ID BIGINT,
	rid INT,
	x INT,
	y INT,
	geom geometry
) AS $$ SELECT ROW_NUMBER
( ) OVER ( ORDER BY 1 ) AS ID,
resized_spatial_grid_centroids.rid,
resized_spatial_grid_centroids.x,
resized_spatial_grid_centroids.y,
resized_spatial_grid_centroids.geom
FROM
	locations
	LEFT JOIN location_periods ON locations.ID = location_periods.location_id
	LEFT JOIN shapes ON location_periods.ID = shapes.location_period_id
	LEFT JOIN location_period_raster_map ON location_period_raster_map.shape_id = shapes.
	ID LEFT JOIN grids.resized_spatial_grid_centroids ON location_period_raster_map.rid = resized_spatial_grid_centroids.rid
	AND resized_spatial_grid_centroids.width = location_period_raster_map.width
	AND resized_spatial_grid_centroids.height = location_period_raster_map.height
	AND st_contains ( shapes.shape, resized_spatial_grid_centroids.geom )
WHERE
	locations.qualified_name = location_name
	AND resized_spatial_grid_centroids.width = width_in_km
	AND resized_spatial_grid_centroids.height = height_in_km$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION pull_grid_adjacency ( location_name TEXT, width_in_km INT, height_in_km INT ) RETURNS TABLE (
	id_1 BIGINT,
	rid_1 INT,
	x_1 INT,
	y_1 INT,
	id_2 BIGINT,
	rid_2 INT,
	x_2 INT,
	y_2 INT
) AS $$ SELECT
lhs.ID AS id_1,
lhs.rid AS rid_1,
lhs.x AS x_1,
lhs.y AS y_1,
rhs.ID AS id_2,
rhs.rid AS rid_2,
rhs.x AS x_2,
rhs.y AS y_2
FROM
	filter_resized_spatial_grid_centroids_to_location ( location_name, width_in_km, height_in_km ) AS lhs
	INNER JOIN grids.resized_spatial_grid_polygons AS lhs_poly ON lhs.rid = lhs_poly.rid
	AND lhs.x = lhs_poly.x
	AND lhs.y = lhs_poly.y
	INNER JOIN grids.resized_spatial_grid_polygons AS rhs_poly ON ST_INTERSECTS (
		st_buffer (
			lhs_poly.geom,
			SQRT ( st_area ( lhs_poly.geom ) ) *.01
		),
		rhs_poly.geom
	)
	INNER JOIN filter_resized_spatial_grid_centroids_to_location ( location_name, width_in_km, height_in_km ) AS rhs ON rhs.rid = rhs_poly.rid
	AND rhs.x = rhs_poly.x
	AND rhs.y = rhs_poly.y
WHERE
	lhs.ID < rhs.ID
	AND lhs_poly.width = width_in_km
	AND lhs_poly.height = height_in_km
	AND rhs_poly.width = width_in_km
	AND rhs_poly.height = height_in_km
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION pull_symmetric_grid_adjacency ( location_name TEXT, width_in_km INT, height_in_km INT ) RETURNS TABLE (id_1 BIGINT, rid_1 INT, x_1 INT, y_1 INT, id_2 BIGINT, rid_2 INT, x_2 INT, y_2 INT) AS $$ SELECT
lhs.ID AS id_1,
lhs.rid AS rid_1,
lhs.x AS x_1,
lhs.y AS y_1,
rhs.ID AS id_2,
rhs.rid AS rid_2,
rhs.x AS x_2,
rhs.y AS y_2
FROM
	filter_resized_spatial_grid_centroids_to_location ( location_name, width_in_km, height_in_km ) AS lhs
	INNER JOIN grids.resized_spatial_grid_polygons AS lhs_poly ON lhs.rid = lhs_poly.rid
	AND lhs.x = lhs_poly.x
	AND lhs.y = lhs_poly.y
	INNER JOIN grids.resized_spatial_grid_polygons AS rhs_poly ON ST_INTERSECTS (
		st_buffer (
			lhs_poly.geom,
			SQRT ( st_area ( lhs_poly.geom ) ) *.01
		),
		rhs_poly.geom
	)
	INNER JOIN filter_resized_spatial_grid_centroids_to_location ( location_name, width_in_km, height_in_km ) AS rhs ON rhs.rid = rhs_poly.rid
	AND rhs.x = rhs_poly.x
	AND rhs.y = rhs_poly.y
WHERE
	lhs.ID != rhs.ID
	AND lhs_poly.width = width_in_km
	AND lhs_poly.height = height_in_km
	AND rhs_poly.width = width_in_km
	AND rhs_poly.height = height_in_km
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION filter_resized_spatial_grid_centroids_to_location ( location_name TEXT, width_in_km INT, height_in_km INT ) RETURNS TABLE (
	ID BIGINT,
	rid INT,
	x INT,
	y INT,
	geom geometry
) AS $$ SELECT ROW_NUMBER
( ) OVER ( ORDER BY 1 ) AS ID,
resized_spatial_grid_centroids.rid,
resized_spatial_grid_centroids.x,
resized_spatial_grid_centroids.y,
resized_spatial_grid_centroids.geom
FROM
	locations
	LEFT JOIN location_periods ON locations.ID = location_periods.location_id
	LEFT JOIN shapes ON location_periods.ID = shapes.location_period_id
	LEFT JOIN location_period_raster_map ON location_period_raster_map.shape_id = shapes.
	ID LEFT JOIN grids.resized_spatial_grid_centroids ON location_period_raster_map.rid = resized_spatial_grid_centroids.rid
	AND resized_spatial_grid_centroids.width = location_period_raster_map.width
	AND resized_spatial_grid_centroids.height = location_period_raster_map.height
	AND st_contains ( shapes.shape, resized_spatial_grid_centroids.geom )
WHERE
	locations.qualified_name = location_name
	AND resized_spatial_grid_centroids.width = width_in_km
	AND resized_spatial_grid_centroids.height = height_in_km
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION filter_location_periods ( location_name TEXT ) RETURNS TABLE (
	ID BIGINT,
	location_id BIGINT,
	qualified_name TEXT,
	location_period_id BIGINT
) AS $$ SELECT ROW_NUMBER
( ) OVER ( ORDER BY 1 ),
descendants.ID AS location_id,
descendants.qualified_name AS qualified_name,
location_periods.ID AS location_period_id
FROM
	locations AS descendants
	LEFT JOIN location_hierarchies ON descendants.ID = location_hierarchies.descendant_id
	LEFT JOIN locations AS ancestors ON ancestors.ID = location_hierarchies.ancestor_id
	LEFT JOIN location_periods ON descendants.ID = location_periods.location_id
WHERE
	ancestors.qualified_name = location_name;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION pull_observation_data ( location_name TEXT, start_date DATE, end_date DATE ) RETURNS TABLE (
	ID BIGINT,
	observation_collection_id BIGINT,
	observation_id BIGINT,
	location_name TEXT,
	location_id BIGINT,
	location_period_id BIGINT,
	suspected_cases INT,
	confirmed_cases INT,
	deaths INT,
	time_left DATE,
	time_right DATE,
	is_primary bool,
	is_phantom bool,
	shape geometry
) AS $$ SELECT ROW_NUMBER
( ) OVER ( ORDER BY observation_collection_id, observations.ID ),
observations.observation_collection_id,
observations.ID AS observation_id,
filtered_location_periods.qualified_name AS location_name,
filtered_location_periods.location_id,
observations.location_period_id,
observations.suspected_cases,
observations.confirmed_cases,
observations.deaths,
observations.time_left,
observations.time_right,
observations.PRIMARY,
observations.phantom,
shapes.shape
FROM
	observations
	INNER JOIN filter_location_periods ( location_name ) AS filtered_location_periods ON observations.location_period_id = filtered_location_periods.location_period_id
	LEFT JOIN shapes ON observations.location_period_id = shapes.location_period_id
WHERE
	observations.time_left >= start_date
	AND observations.time_right <= end_date;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION resize_temporal_grid ( time_unit TEXT ) RETURNS TABLE (
	ID BIGINT,
	time_midpoint DATE,
	time_min DATE,
	time_max DATE
	) AS $$ BEGIN
	IF
		time_unit = ‘year’ THEN
			RETURN QUERY SELECT ROW_NUMBER
			( ) OVER ( ORDER BY EXTRACT ( YEAR FROM tbl.time_midpoint ) ),
			(
				TIMESTAMP WITHOUT TIME ZONE'1970-01-01' + CAST (
					AVG ( EXTRACT ( epoch FROM tbl.time_midpoint ) ) :: TEXT AS INTERVAL
				)
			) :: DATE,
			(
				TIMESTAMP WITHOUT TIME ZONE'1970-01-01' + CAST (
					MIN ( EXTRACT ( epoch FROM tbl.time_midpoint ) ) :: TEXT AS INTERVAL
				)
			) :: DATE,
			(
				TIMESTAMP WITHOUT TIME ZONE'1970-01-01' + CAST (
					MAX ( EXTRACT ( epoch FROM tbl.time_midpoint ) ) :: TEXT AS INTERVAL
				)
			) :: DATE
		FROM
			grids.master_temporal_grid AS tbl
		GROUP BY
			EXTRACT ( YEAR FROM tbl.time_midpoint );

	END IF;
	IF
		time_unit = ‘month’ THEN
			RETURN QUERY SELECT ROW_NUMBER
			( ) OVER (

			ORDER BY
				EXTRACT ( YEAR FROM tbl.time_midpoint ),
				EXTRACT ( MONTH FROM tbl.time_midpoint )
			),
			(
				TIMESTAMP WITHOUT TIME ZONE'1970-01-01' + CAST (
					AVG ( EXTRACT ( epoch FROM tbl.time_midpoint ) ) :: TEXT AS INTERVAL
				)
			) :: DATE,
			(
				TIMESTAMP WITHOUT TIME ZONE'1970-01-01' + CAST (
					MIN ( EXTRACT ( epoch FROM tbl.time_midpoint ) ) :: TEXT AS INTERVAL
				)
			) :: DATE,
			(
				TIMESTAMP WITHOUT TIME ZONE'1970-01-01' + CAST (
					MAX ( EXTRACT ( epoch FROM tbl.time_midpoint ) ) :: TEXT AS INTERVAL
				)
			) :: DATE
		FROM
			grids.master_temporal_grid AS tbl
		GROUP BY
			EXTRACT ( YEAR FROM tbl.time_midpoint ),
			EXTRACT ( MONTH FROM tbl.time_midpoint );

	END IF;

END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION pull_observation_location_period_map (
	location_name TEXT,
	start_date DATE,
	end_date DATE,
	time_scale TEXT
	) RETURNS TABLE (
	observation_id BIGINT,
	location_period_id BIGINT,
	T BIGINT,
	temporal_location_id BIGINT
) AS $$ SELECT
observation_data.ID AS observation_id,
location_periods.location_period_id,
temporal_grid.ID AS T,
DENSE_RANK ( ) OVER ( ORDER BY location_periods.location_period_id, temporal_grid.ID ) AS temporal_location_id
FROM
	pull_observation_data ( location_name, start_date, end_date ) AS observation_data
	LEFT JOIN filter_location_periods ( location_name ) AS location_periods ON observation_data.location_period_id = location_periods.location_period_id
	LEFT JOIN resize_temporal_grid ( time_scale ) AS temporal_grid ON observation_data.time_left < temporal_grid.time_midpoint
	AND observation_data.time_right >= temporal_grid.time_midpoint;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION pull_location_period_grid_map (
	location_name TEXT,
	start_date DATE,
	end_date DATE,
	width_in_km INT,
	height_in_km INT,
	time_scale TEXT
	) RETURNS TABLE (
	qualified_name TEXT,
	location_id BIGINT,
	location_period_id BIGINT,
	temporal_location_id BIGINT,
	shape_id BIGINT,
	spatial_grid_id BIGINT,
	rid INT,
	x INT,
	y INT,
	T BIGINT
	) AS $$ SELECT
	location_periods.qualified_name AS qualified_name,
	location_periods.location_id AS location_id,
	location_periods.location_period_id AS location_period_id,
	temporal_locations.temporal_location_id AS temporal_location_id,
	shapes.ID AS shape_id,
	spatial_grid.ID AS spatial_grid_id,
	spatial_grid.rid,
	spatial_grid.x,
	spatial_grid.y,
	temporal_locations.T
FROM
	filter_location_periods ( location_name ) AS location_periods
	LEFT JOIN shapes ON location_periods.location_period_id = shapes.location_period_id
	LEFT JOIN filter_resized_spatial_grid_centroids_to_location ( location_name, width_in_km, height_in_km ) AS spatial_grid ON ST_CONTAINS ( shapes.shape, spatial_grid.geom )
	LEFT JOIN pull_observation_location_period_map ( location_name, start_date, end_date, time_scale ) AS temporal_locations ON location_periods.location_period_id = temporal_locations.location_period_id;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION pull_covar_cube (
	location_name TEXT,
	start_date DATE,
	end_date DATE,
	width_in_km INT,
	height_in_km INT,
	time_scale TEXT) RETURNS TABLE (
	covariate_name TEXT,
	T BIGINT,
	ID BIGINT,
	rid INT,
	x INT,
	y INT,
	VALUE
	DOUBLE PRECISION) AS $$ SELECT
	all_covariates.filename,
	temporal_grid.ID AS T,
	grid_centroids.ID,
	grid_centroids.rid,
	grid_centroids.x,
	grid_centroids.y,
	ST_VALUE ( all_covariates.rast, grid_centroids.geom ) AS VALUE FROM
	filter_resized_spatial_grid_centroids_to_location ( location_name, width_in_km, height_in_km ) AS grid_centroids
	INNER JOIN covariate_grid_map ON grid_centroids.rid = covariate_grid_map.grid_rid
	FULL JOIN resize_temporal_grid ( time_scale ) AS temporal_grid ON 1 = 1
	LEFT JOIN covariates.all_covariates ON covariate_grid_map.covar_rid = all_covariates.rid
	AND covariate_grid_map.time_left = all_covariates.time_left
	AND covariate_grid_map.time_right = all_covariates.time_right
	AND covariate_grid_map.width = width_in_km
	AND covariate_grid_map.height = height_in_km
	AND all_covariates.time_left <= temporal_grid.time_midpoint
	AND all_covariates.time_right >= temporal_grid.time_midpoint
	AND covariate_grid_map.covariate_name = all_covariates.filename
	AND st_intersects ( all_covariates.rast, grid_centroids.geom ) WHERE
	temporal_grid.time_midpoint >= start_date
	AND temporal_grid.time_midpoint <= end_date
	AND covariate_grid_map.time_left <= temporal_grid.time_midpoint
	AND covariate_grid_map.time_right >= temporal_grid.time_midpoint
