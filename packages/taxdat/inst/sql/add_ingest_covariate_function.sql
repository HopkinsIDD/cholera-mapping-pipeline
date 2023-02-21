CREATE FUNCTION ingest_covariate(collection_id bigint, table_name text, ingest_time_left date, ingest_time_right date)
  returns void
  LANGUAGE plpgsql
  as $$
  begin
  PERFORM * FROM covariates.all_covariates where all_covariates.raster_covariate_collection_id = collection_id AND all_covariates.filename = table_name AND all_covariates.time_left = ingest_time_left and all_covariates.time_right = ingest_time_right;
  if not found then
  EXECUTE FORMAT ('INSERT INTO covariates.all_covariates SELECT %L as raster_covariate_collection_id, %L as filename, %L as time_left, %L as time_right, rid, rast FROM covariates.%I', collection_id, table_name, ingest_time_left, ingest_time_right, table_name);
  end if;
  end;
  $$ SECURITY DEFINER;
