CREATE TABLE observations(
  id BIGSERIAL PRIMARY KEY,
  observation_collection_id bigint,
  time_left date,
  time_right date,
  location_id bigint REFERENCES locations(id),
  location_period_id bigint REFERENCES location_periods(id),
  "primary" boolean,
  phantom boolean,
  suspected_cases integer,
  confirmed_cases integer,
  deaths integer
);
