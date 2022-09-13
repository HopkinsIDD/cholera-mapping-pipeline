CREATE TABLE location_periods(
  id BIGSERIAL PRIMARY KEY,
  location_id bigint REFERENCES locations(id),
  start_date date,
  end_date date
);

