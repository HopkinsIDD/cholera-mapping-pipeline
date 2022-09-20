CREATE TABLE IF NOT EXISTS shapes(
  id BIGSERIAL PRIMARY KEY,
  location_period_id bigint REFERENCES location_periods(id) UNIQUE,
  shape GEOMETRY,
  box GEOMETRY
);
