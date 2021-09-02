CREATE OR REPLACE FUNCTION classify_date(duration_in_days int) returns text AS $$
BEGIN
  IF duration_in_days <= 2 THEN RETURN 'day'; END IF;
  IF duration_in_days >= 6 AND duration_in_days <= 8 THEN RETURN 'week'; END IF;
  IF duration_in_days >= 12 AND duration_in_days <= 16 THEN RETURN 'biweek'; END IF;
  IF duration_in_days >= 26 AND duration_in_days <= 32 THEN RETURN 'month'; END IF;
  IF duration_in_days >= 56 AND duration_in_days <= 64 THEN RETURN 'bimonth'; END IF;
  IF duration_in_days >= 360 AND duration_in_days <= 370 THEN RETURN 'year'; END IF;
  IF duration_in_days >= 720 AND duration_in_days <= 740 THEN RETURN 'biyear'; END IF;
  RETURN null;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION date_class_length(date_class text) returns int AS $$
BEGIN
  IF date_class = 'day' THEN RETURN 1; END IF;
  IF date_class = 'week' THEN RETURN 7; END IF;
  IF date_class = 'biweek' THEN RETURN 14; END IF;
  IF date_class = 'month' THEN RETURN 30; END IF;
  IF date_class = 'bimonth' THEN RETURN 61; END IF;
  IF date_class = 'year' THEN RETURN 365; END IF;
  IF date_class = 'biyear' THEN RETURN 730; END IF;
  RETURN null;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION count_siblings(location_id bigint) returns bigint AS $$
  SELECT
    count(rhs.descendant_id)
  FROM
    location_hierarchies AS lhs
  LEFT JOIN
    location_hierarchies as rhs
      ON
        lhs.ancestor_id = rhs.ancestor_id
  WHERE
    lhs.generations = 1 AND
    rhs.generations = 1 AND
    lhs.descendant_id = location_id
  GROUP BY
    lhs.descendant_id;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION get_spatial_scale(location_id bigint) returns int AS $$
  SELECT MAX(generations) as spatial_scale FROM location_hierarchies WHERE descendant_id = location_id
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION guess_date_type(oc_id bigint) RETURNS text AS $$
  SELECT classify_date(time_right - time_left) as date_class
  FROM observations
  WHERE
    observation_collection_id = oc_id AND
    observations.primary
  GROUP BY date_class
  ORDER BY -count(*) * sqrt(date_class_length(classify_date(time_right - time_left)))
  LIMIT 1;
$$ LANGUAGE SQL COST 1000;

CREATE OR REPLACE FUNCTION guess_spatial_scale(oc_id bigint) RETURNS int AS $$
  SELECT
    spatial_scale
  FROM (
    SELECT
      location_id,
      get_spatial_scale(location_id) as spatial_scale,
      count_siblings(location_id) as sibling_count,
      count(*) as my_count
    FROM observations
    WHERE
      observation_collection_id = oc_id AND
      observations.primary
    GROUP BY location_id
  ) as tmp
  GROUP BY spatial_scale
  ORDER BY -sum(my_count / sqrt(sibling_count))
  LIMIT 1;
$$ LANGUAGE SQL COST 1000;

CREATE OR REPLACE FUNCTION clean_observation_collection(
  oc_id bigint
) RETURNS
  TABLE(location_id bigint, time_left date, time_right date, is_primary boolean, is_phantom boolean, suspected_cases int, deaths int)
AS $$
  SELECT
    observations.location_id,
    observations.time_left,
    observations.time_right,
    observations.primary,
    observations.phantom,
    observations.suspected_cases,
    observations.deaths
  FROM observations
  WHERE
    observation_collection_id = oc_id AND
    observations.primary AND
    get_spatial_scale(location_id) = guess_spatial_scale(oc_id) AND
    classify_date(time_right - time_left) = guess_date_type(oc_id)
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION clean_observation_collection(
  oc_id bigint,
  spatial_scale int
) RETURNS
  TABLE(location_id bigint, time_left date, time_right date, is_primary boolean, is_phantom boolean, suspected_cases int, deaths int)
AS $$
  SELECT
    observations.location_id,
    observations.time_left,
    observations.time_right,
    observations.primary,
    observations.phantom,
    observations.suspected_cases,
    observations.deaths
  FROM observations
  WHERE
    observation_collection_id = oc_id AND
    observations.primary AND
    get_spatial_scale(location_id) = spatial_scale AND
    classify_date(time_right - time_left) = guess_date_type(oc_id)
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION clean_observation_collection(
  oc_id bigint,
  date_type text
) RETURNS
  TABLE(location_id bigint, time_left date, time_right date, is_primary boolean, is_phantom boolean, suspected_cases int, deaths int)
AS $$
  SELECT
    observations.location_id,
    observations.time_left,
    observations.time_right,
    observations.primary,
    observations.phantom,
    observations.suspected_cases,
    observations.deaths
  FROM observations
  WHERE
    observation_collection_id = oc_id AND
    observations.primary AND
    get_spatial_scale(location_id) = guess_spatial_scale(oc_id) AND
    classify_date(time_right - time_left) = date_type
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION clean_observation_collection(
  oc_id bigint,
  date_type text,
  spatial_scale int
) RETURNS
  TABLE(location_id bigint, time_left date, time_right date, is_primary boolean, is_phantom boolean, suspected_cases int, deaths int)
AS $$
  SELECT
    observations.location_id,
    observations.time_left,
    observations.time_right,
    observations.primary,
    observations.phantom,
    observations.suspected_cases,
    observations.deaths
  FROM observations
  WHERE
    observation_collection_id = oc_id AND
    observations.primary AND
    get_spatial_scale(location_id) = spatial_scale AND
    classify_date(time_right - time_left) = date_type 
$$ LANGUAGE SQL;
