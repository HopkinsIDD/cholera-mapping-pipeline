CREATE OR REPLACE FUNCTION pull_grid_adjacency(location_name text, width_in_km int, height_in_km int)
RETURNS TABLE(id_1 BIGINT, rid_1 INT, x_1 INT, y_1 INT, id_2 BIGINT, rid_2 INT, x_2 INT, y_2 INT)AS $$
  SELECT
    lhs.id AS id_1,
    lhs.rid AS rid_1,
    lhs.x AS x_1,
    lhs.y AS y_1,
    rhs.id AS id_2,
    rhs.rid AS rid_2,
    rhs.x AS x_2,
    rhs.y AS y_2
  FROM filter_resized_spatial_grid_pixels_to_location(location_name, width_in_km, height_in_km) AS lhs
    INNER JOIN
      filter_resized_spatial_grid_pixels_to_location(location_name, width_in_km, height_in_km) AS rhs
        ON
          ST_INTERSECTS(ST_BUFFER(lhs.polygon, SQRT(ST_AREA(lhs.polygon))*.01), rhs.polygon)
  WHERE
    lhs.id < rhs.id
  $$ LANGUAGE SQL SECURITY DEFINER;
