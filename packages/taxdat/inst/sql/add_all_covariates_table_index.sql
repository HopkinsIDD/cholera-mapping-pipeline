CREATE INDEX IF NOT EXISTS covariate_name_size_idx ON covariates.all_covariates(covariate_name, time_left, time_right, rid);
