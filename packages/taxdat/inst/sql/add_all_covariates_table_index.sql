CREATE INDEX IF NOT EXISTS covariate_name_size_idx ON covariates.all_covariates(raster_covariate_collection_id, time_left, time_right, rid);
