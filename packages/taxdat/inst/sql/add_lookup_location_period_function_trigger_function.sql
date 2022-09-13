CREATE OR REPLACE FUNCTION trigger_lookup_missing_location_period_for_observations()
  RETURNS trigger
  LANGUAGE plpgsql AS
$func$
BEGIN
   NEW.location_period_id := lookup_location_period(NEW.location_id, NEW.time_left, NEW.time_right);
   RETURN NEW;
END
$func$;
