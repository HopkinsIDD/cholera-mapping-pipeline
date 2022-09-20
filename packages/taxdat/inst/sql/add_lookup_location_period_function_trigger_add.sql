CREATE TRIGGER observations_location_period_id_default
BEFORE INSERT ON observations
FOR EACH ROW
WHEN (
  NEW.location_period_id IS NULL AND
  NEW.location_id IS NOT NULL AND
  NEW.time_left IS NOT NULL AND
  NEW.time_right IS NOT NULL
) EXECUTE PROCEDURE trigger_lookup_missing_location_period_for_observations();
