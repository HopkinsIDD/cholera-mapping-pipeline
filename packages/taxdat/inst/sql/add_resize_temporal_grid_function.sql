create or replace function resize_temporal_grid(time_unit text)
RETURNS table(id bigint, time_midpoint date, time_min date, time_max date) AS
$$
  BEGIN
  IF time_unit = 'year'
  THEN
  RETURN QUERY
  SELECT
    ROW_NUMBER() OVER (ORDER BY EXTRACT(year from tbl.time_midpoint)),
    (timestamp without time zone '1970-01-01' + cast(avg(EXTRACT(epoch from tbl.time_midpoint))::text as interval))::date,
    (timestamp without time zone '1970-01-01' + cast(min(EXTRACT(epoch from tbl.time_midpoint))::text as interval))::date,
    (timestamp without time zone '1970-01-01' + cast(max(EXTRACT(epoch from tbl.time_midpoint))::text as interval))::date
  FROM
    grids.master_temporal_grid as tbl
  GROUP BY
    extract(year from tbl.time_midpoint);
  END IF;
  IF time_unit = 'month'
  THEN
  RETURN QUERY
  SELECT
    ROW_NUMBER() OVER (ORDER BY EXTRACT(year from tbl.time_midpoint), EXTRACT(month from tbl.time_midpoint)),
    (timestamp without time zone '1970-01-01' + cast(avg(EXTRACT(epoch from tbl.time_midpoint))::text as interval))::date,
    (timestamp without time zone '1970-01-01' + cast(min(EXTRACT(epoch from tbl.time_midpoint))::text as interval))::date,
    (timestamp without time zone '1970-01-01' + cast(max(EXTRACT(epoch from tbl.time_midpoint))::text as interval))::date
  FROM
    grids.master_temporal_grid as tbl
  GROUP BY
    extract(year from tbl.time_midpoint),
    extract(month from tbl.time_midpoint);
  END IF;
  END;
$$ LANGUAGE plpgsql SECURITY DEFINER;