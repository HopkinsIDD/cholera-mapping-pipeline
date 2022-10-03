CREATE MATERIALIZED VIEW grids.master_spatial_grid AS
SELECT rid,rast FROM (
  SELECT
    rank() OVER (ORDER BY all_covariates.filename) AS rnk,
    rid,
    rast
  FROM
    covariates.all_covariates
  INNER JOIN
    raster_covariate_collections ON raster_covariate_collection_id = raster_covariate_collections.id
  WHERE raster_covariate_collections.name = 'population'
) as tmp
WHERE
  rnk = 1;
