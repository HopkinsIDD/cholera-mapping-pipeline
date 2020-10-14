# install.packages('~/svn/cholera-taxonomy/trunk/packages/taxdat',type='source',repos=NULL)
library(taxdat)

all_data <- list()
seed <- 12345

nc <- 2 ## Number of covariates
#### testing.1 ####
all_data[["full observation single covariate"]] <- function(){taxdat:::create_standardized_test_data(
  ## Extent parameters:,
  ### NONE,
  ## Raster parameters:,
  nrows = 20,
  ncols = 20,
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1,
  n_layers = 2,
  factor = 4,
  snap = FALSE,
  randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,FALSE),
  nontemporal = c(FALSE,FALSE),
  spatially_smooth = c(FALSE,FALSE),
  temporally_smooth = c(FALSE,FALSE),
  polygonal = c(FALSE,FALSE),
  radiating = c(FALSE,TRUE),
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,st_union(st_sfc(
    st_point(c(0.25,0.25)),
    st_point(c(0.75,0.75)),
    crs=st_crs(create_test_polygons())
  ))),
  radiation_function = rep(list(function(x,mu){mu*RandomFieldsUtils::matern(x/1000,3/4)}),nc),
  radiating_means = list(NA,c(1)),
  smoothing_function = rep(
    list(
      function(n, mu, covariance, centers) {
        return(scale(MASS::mvrnorm(n = n, mu = mu, Matrix::solve(covariance))))
      }
    ),
    nc
  ),
  family = "Gaussian",
  magnitude = c(6,1),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1,
  number_draws = 1,
  grid_spatial_observation_bias = FALSE,
  grid_temporal_observation_bias = FALSE,
  grid_value_observation_bias = FALSE,
  noise = FALSE,
  polygon_proportion_observed = 1,
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)),
  polygon_observation_idx = c(1,2,3,4,5),
  polygon_size_bias = FALSE,
  nonlinear_covariates = FALSE,
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = seed
)}

#### testing.2 ####
all_data[["partial observation single covariate mode 1"]] <- function(){taxdat:::create_standardized_test_data(
  ## Extent parameters:,
  ### NONE,
  ## Raster parameters:,
  nrows = 20,
  ncols = 20,
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1,
  n_layers = 2,
  factor = 4,
  snap = FALSE,
  randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,FALSE),
  nontemporal = c(FALSE,FALSE),
  spatially_smooth = c(FALSE,FALSE),
  temporally_smooth = c(FALSE,FALSE),
  polygonal = c(FALSE,FALSE),
  radiating = c(FALSE,TRUE),
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,st_union(st_sfc(
    st_point(c(0.25,0.25)),
    st_point(c(0.75,0.75)),
    crs=st_crs(create_test_polygons())
  ))),
  radiating_means = list(NA,c(1)),
  smoothing_function = rep(
    list(
      function(n, mu, covariance, centers) {
        return(scale(MASS::mvrnorm(n = n, mu = mu, Matrix::solve(covariance))))
      }
    ),
    nc
  ),
  family = "Gaussian",
  magnitude = c(6,1),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1,
  number_draws = 1,
  grid_spatial_observation_bias = FALSE,
  grid_temporal_observation_bias = FALSE,
  grid_value_observation_bias = FALSE,
  noise = FALSE,
  polygon_proportion_observed = 1,
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)),
  polygon_observation_idx = c(1,2,3),
  polygon_size_bias = FALSE,
  nonlinear_covariates = FALSE,
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = seed
)}

#### testing.3 ####
all_data[["partial observation single covariate mode 2"]] <- function(){create_standardized_test_data(
  ## Extent parameters:,
  ### NONE,
  ## Raster parameters:,
  nrows = 20,
  ncols = 20,
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1,
  n_layers = 2,
  factor = 4,
  snap = FALSE,
  randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,FALSE),
  nontemporal = c(FALSE,FALSE),
  spatially_smooth = c(FALSE,FALSE),
  temporally_smooth = c(FALSE,FALSE),
  polygonal = c(FALSE,FALSE),
  radiating = c(FALSE,TRUE),
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,st_union(st_sfc(
    st_point(c(0.25,0.25)),
    st_point(c(0.75,0.75)),
    crs=st_crs(create_test_polygons())
  ))),
  radiating_means = list(NA,c(1)),
  smoothing_function = rep(
    list(
      function(n, mu, covariance, centers) {
        return(scale(MASS::mvrnorm(n = n, mu = mu, Matrix::solve(covariance))))
      }
    ),
    nc
  ),
  family = "Gaussian",
  magnitude = c(6,1),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1,
  number_draws = 1,
  grid_spatial_observation_bias = FALSE,
  grid_temporal_observation_bias = FALSE,
  grid_value_observation_bias = FALSE,
  noise = FALSE,
  polygon_proportion_observed = 1,
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)),
  polygon_observation_idx = c(1,2,5),
  polygon_size_bias = FALSE,
  nonlinear_covariates = FALSE,
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = seed
)}

#### testing.4 ####
all_data[["partial observation single covariate mode 3"]] <- function(){create_standardized_test_data(
  ## Extent parameters:,
  ### NONE,
  ## Raster parameters:,
  nrows = 20,
  ncols = 20,
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1,
  n_layers = 2,
  factor = 4,
  snap = FALSE,
  randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,FALSE),
  nontemporal = c(FALSE,FALSE),
  spatially_smooth = c(FALSE,FALSE),
  temporally_smooth = c(FALSE,FALSE),
  polygonal = c(FALSE,FALSE),
  radiating = c(FALSE,TRUE),
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,st_union(st_sfc(
    st_point(c(0.25,0.25)),
    st_point(c(0.75,0.75)),
    crs=st_crs(create_test_polygons())
  ))),
  radiating_means = list(NA,c(1)),
  smoothing_function = rep(
    list(
      function(n, mu, covariance, centers) {
        return(scale(MASS::mvrnorm(n = n, mu = mu, Matrix::solve(covariance))))
      }
    ),
    nc
  ),
  family = "Gaussian",
  magnitude = c(6,1),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1,
  number_draws = 1,
  grid_spatial_observation_bias = FALSE,
  grid_temporal_observation_bias = FALSE,
  grid_value_observation_bias = FALSE,
  noise = FALSE,
  polygon_proportion_observed = 1,
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)),
  polygon_observation_idx = c(1,3,4),
  polygon_size_bias = FALSE,
  nonlinear_covariates = FALSE,
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = seed
)}

#### testing.5 ####
all_data[["full observation single polygonal covariate"]] <- function(){create_standardized_test_data(
  ## Extent parameters:,
  ### NONE,
  ## Raster parameters:,
  nrows = 20,
  ncols = 20,
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1,
  n_layers = 2,
  factor = 4,
  snap = FALSE,
  randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,FALSE),
  nontemporal = c(FALSE,FALSE),
  spatially_smooth = c(FALSE,FALSE),
  temporally_smooth = c(FALSE,FALSE),
  polygonal = c(FALSE,TRUE),
  radiating = c(FALSE,FALSE),
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,NA),
  radiating_means = list(NA,NA),
  smoothing_function = rep(
    list(
      function(n, mu, covariance, centers) {
        return(scale(MASS::mvrnorm(n = n, mu = mu, Matrix::solve(covariance))))
      }
    ),
    nc
  ),
  family = "Gaussian",
  magnitude = c(6,1),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1,
  number_draws = 1,
  grid_spatial_observation_bias = FALSE,
  grid_temporal_observation_bias = FALSE,
  grid_value_observation_bias = FALSE,
  noise = FALSE,
  polygon_proportion_observed = 1,
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)),
  polygon_observation_idx = c(1,2,3,4,5),
  polygon_size_bias = FALSE,
  nonlinear_covariates = FALSE,
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = seed
)}

#### testing.6 ####
all_data[["partial observation single polygonal covariate mode 0"]] <- function(){create_standardized_test_data(
  ## Extent parameters:,
  ### NONE,
  ## Raster parameters:,
  nrows = 20,
  ncols = 20,
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1,
  n_layers = 2,
  factor = 4,
  snap = FALSE,
  randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,FALSE),
  nontemporal = c(FALSE,FALSE),
  spatially_smooth = c(FALSE,FALSE),
  temporally_smooth = c(FALSE,FALSE),
  polygonal = c(FALSE,TRUE),
  radiating = c(FALSE,FALSE),
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,NA),
  radiating_means = list(NA,NA),
  smoothing_function = rep(
    list(
      function(n, mu, covariance, centers) {
        return(scale(MASS::mvrnorm(n = n, mu = mu, Matrix::solve(covariance))))
      }
    ),
    nc
  ),
  family = "Gaussian",
  magnitude = c(6,1),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1,
  number_draws = 1,
  grid_spatial_observation_bias = FALSE,
  grid_temporal_observation_bias = FALSE,
  grid_value_observation_bias = FALSE,
  noise = FALSE,
  polygon_proportion_observed = 1,
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)),
  polygon_observation_idx = c(1),
  polygon_size_bias = FALSE,
  nonlinear_covariates = FALSE,
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = seed
)}

#### testing.7 ####
all_data[["partial observation single polygonal covariate mode 1"]] <- function(){create_standardized_test_data(
  ## Extent parameters:,
  ### NONE,
  ## Raster parameters:,
  nrows = 20,
  ncols = 20,
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1,
  n_layers = 2,
  factor = 4,
  snap = FALSE,
  randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,FALSE),
  nontemporal = c(FALSE,FALSE),
  spatially_smooth = c(FALSE,FALSE),
  temporally_smooth = c(FALSE,FALSE),
  polygonal = c(FALSE,TRUE),
  radiating = c(FALSE,FALSE),
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,NA),
  radiating_means = list(NA,NA),
  smoothing_function = rep(
    list(
      function(n, mu, covariance, centers) {
        return(scale(MASS::mvrnorm(n = n, mu = mu, Matrix::solve(covariance))))
      }
    ),
    nc
  ),
  family = "Gaussian",
  magnitude = c(6,1),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1,
  number_draws = 1,
  grid_spatial_observation_bias = FALSE,
  grid_temporal_observation_bias = FALSE,
  grid_value_observation_bias = FALSE,
  noise = FALSE,
  polygon_proportion_observed = 1,
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)),
  polygon_observation_idx = c(1,2),
  polygon_size_bias = FALSE,
  nonlinear_covariates = FALSE,
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = seed
)}

#### testing.8 ####
all_data[["partial observation single polygonal covariate mode 2"]] <- function(){create_standardized_test_data(
  ## Extent parameters:,
  ### NONE,
  ## Raster parameters:,
  nrows = 20,
  ncols = 20,
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1,
  n_layers = 2,
  factor = 4,
  snap = FALSE,
  randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,FALSE),
  nontemporal = c(FALSE,FALSE),
  spatially_smooth = c(FALSE,FALSE),
  temporally_smooth = c(FALSE,FALSE),
  polygonal = c(FALSE,TRUE),
  radiating = c(FALSE,FALSE),
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,NA),
  radiating_means = list(NA,NA),
  smoothing_function = rep(
    list(
      function(n, mu, covariance, centers) {
        return(scale(MASS::mvrnorm(n = n, mu = mu, Matrix::solve(covariance))))
      }
    ),
    nc
  ),
  family = "Gaussian",
  magnitude = c(6,1),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1,
  number_draws = 1,
  grid_spatial_observation_bias = FALSE,
  grid_temporal_observation_bias = FALSE,
  grid_value_observation_bias = FALSE,
  noise = FALSE,
  polygon_proportion_observed = 1,
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)),
  polygon_observation_idx = c(1,2,3),
  polygon_size_bias = FALSE,
  nonlinear_covariates = FALSE,
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = seed
)}

#### testing.9 ####
all_data[["partial observation single polygonal covariate mode 3"]] <- function(){create_standardized_test_data(
  ## Extent parameters:,
  ### NONE,
  ## Raster parameters:,
  nrows = 20,
  ncols = 20,
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1,
  n_layers = 2,
  factor = 4,
  snap = FALSE,
  randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,FALSE),
  nontemporal = c(FALSE,FALSE),
  spatially_smooth = c(FALSE,FALSE),
  temporally_smooth = c(FALSE,FALSE),
  polygonal = c(FALSE,TRUE),
  radiating = c(FALSE,FALSE),
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,NA),
  radiating_means = list(NA,NA),
  smoothing_function = rep(
    list(
      function(n, mu, covariance, centers) {
        return(scale(MASS::mvrnorm(n = n, mu = mu, Matrix::solve(covariance))))
      }
    ),
    nc
  ),
  family = "Gaussian",
  magnitude = c(6,1),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1,
  number_draws = 1,
  grid_spatial_observation_bias = FALSE,
  grid_temporal_observation_bias = FALSE,
  grid_value_observation_bias = FALSE,
  noise = FALSE,
  polygon_proportion_observed = 1,
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)),
  polygon_observation_idx = c(1,3,4),
  polygon_size_bias = FALSE,
  nonlinear_covariates = FALSE,
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = seed
)}

#### testing.10 ####
all_data[["partial observation single polygonal covariate mode 4"]] <- function(){create_standardized_test_data(
  ## Extent parameters:,
  ### NONE,
  ## Raster parameters:,
  nrows = 20,
  ncols = 20,
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1,
  n_layers = 2,
  factor = 4,
  snap = FALSE,
  randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,FALSE),
  nontemporal = c(FALSE,FALSE),
  spatially_smooth = c(FALSE,FALSE),
  temporally_smooth = c(FALSE,FALSE),
  polygonal = c(FALSE,TRUE),
  radiating = c(FALSE,FALSE),
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,NA),
  radiating_means = list(NA,NA),
  smoothing_function = rep(
    list(
      function(n, mu, covariance, centers) {
        return(scale(MASS::mvrnorm(n = n, mu = mu, Matrix::solve(covariance))))
      }
    ),
    nc
  ),
  family = "Gaussian",
  magnitude = c(6,1),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1,
  number_draws = 1,
  grid_spatial_observation_bias = FALSE,
  grid_temporal_observation_bias = FALSE,
  grid_value_observation_bias = FALSE,
  noise = FALSE,
  polygon_proportion_observed = 1,
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)),
  polygon_observation_idx = c(1,3,5),
  polygon_size_bias = FALSE,
  nonlinear_covariates = FALSE,
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = seed
)}

#### testing.11 ####
all_data[["full observation single matern smoothed covariate"]] <- function(){create_standardized_test_data(
  ## Extent parameters:,
  ## Raster parameters:,
  nrows = 20, 
  ncols = 20, 
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1, 
  n_layers = 2, 
  factor = 4, 
  snap = FALSE, 
  randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,FALSE), 
  nontemporal = c(FALSE,FALSE), 
  spatially_smooth = c(FALSE,TRUE), 
  temporally_smooth = c(FALSE,FALSE), 
  polygonal = c(FALSE,FALSE), 
  radiating = c(FALSE,FALSE), 
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,NA), 
  radiating_means = list(NA,NA), 
  family = "Gaussian", 
  magnitude = c(6,1),
  smoothing_function = list(NA,function(n,mu,covar,centers){ # Need to pass centroids to this function
    distances <- matrix(NA,nrow(centers),nrow(centers))
    for(i in 1:nrow(distances)){
      partial <- t(as.matrix(centers)[i,] - t(as.matrix(centers)))
      distances[i,] <- sqrt(apply(partial * partial,1,sum))
    }
    covar[] <- RandomFieldsUtils::matern(distances,3/2)
    return(scale(MASS::mvrnorm(n=n,mu=mu,covar)))
  }),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1, 
  number_draws = 1, 
  grid_spatial_observation_bias = FALSE, 
  grid_temporal_observation_bias = FALSE, 
  grid_value_observation_bias = FALSE, 
  noise = FALSE, 
  polygon_proportion_observed = 1, 
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)), 
  polygon_observation_idx = c(1,2,3,4,5), 
  polygon_size_bias = FALSE, 
  nonlinear_covariates = FALSE, 
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = seed
)}

#### testing.12 ####
all_data[["full observation single matern smoothed covariate finer observation scale"]] <- function(){create_standardized_test_data(
  ## Extent parameters:,
  ## Raster parameters:,
  nrows = 20, 
  ncols = 20, 
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1, n_layers = 3, factor = 4, snap = FALSE, randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,FALSE), 
  nontemporal = c(FALSE,FALSE), 
  spatially_smooth = c(FALSE,TRUE), 
  temporally_smooth = c(FALSE,FALSE), 
  polygonal = c(FALSE,FALSE), 
  radiating = c(FALSE,FALSE), 
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,NA), 
  radiating_means = list(NA,NA), 
  family = "Gaussian", 
  magnitude = c(6,1),
  smoothing_function = list(NA,function(n,mu,covar,centers){ # Need to pass centroids to this function
    distances <- matrix(NA,nrow(centers),nrow(centers))
    for(i in 1:nrow(distances)){
      partial <- t(as.matrix(centers)[i,] - t(as.matrix(centers)))
      distances[i,] <- sqrt(apply(partial * partial,1,sum))
    }
    covar[] <- RandomFieldsUtils::matern(distances,3/2)
    return(scale(MASS::mvrnorm(n=n,mu=mu,covar)))
  }),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1, 
  number_draws = 1, 
  grid_spatial_observation_bias = FALSE, 
  grid_temporal_observation_bias = FALSE, 
  grid_value_observation_bias = FALSE, 
  noise = FALSE, 
  polygon_proportion_observed = 1, 
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)), 
  polygon_observation_idx = 1:(sum(1 * 4 ^ (0:2))), 
  polygon_size_bias = FALSE, 
  nonlinear_covariates = FALSE, 
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = seed
)}


#### testing.13 ####
all_data[["single matern smooth spacetime covariate"]] <- function(){create_standardized_test_data(
  ## Extent parameters:,
  ## Raster parameters:,
  nrows = 20, 
  ncols = 20, 
  nlayers = 12,
  ## Polygon Parameters,
  base_number = 1, 
  n_layers = 2, 
  factor = 4, 
  snap = FALSE, 
  randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,FALSE), 
  nontemporal = c(FALSE,FALSE), 
  spatially_smooth = c(FALSE,TRUE), 
  temporally_smooth = c(FALSE,TRUE), 
  polygonal = c(FALSE,FALSE), 
  radiating = c(FALSE,FALSE), 
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,NA), 
  radiating_means = list(NA,NA), 
  family = "Gaussian", 
  magnitude = c(6,1),
  smoothing_function = list(NA,function(n,mu,covar,centers){ # Need to pass centroids to this function
    distances <- matrix(NA,nrow(centers),nrow(centers))
    for(i in 1:nrow(distances)){
      partial <- t(as.matrix(centers)[i,] - t(as.matrix(centers)))
      distances[i,] <- sqrt(apply(partial * partial,1,sum))
    }
    covar[] <- RandomFieldsUtils::matern(distances,3/2)
    return(scale(MASS::mvrnorm(n=n,mu=mu,covar)))
  }),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1, 
  number_draws = 1, 
  grid_spatial_observation_bias = FALSE, 
  grid_temporal_observation_bias = FALSE, 
  grid_value_observation_bias = FALSE, 
  noise = FALSE, 
  polygon_proportion_observed = 1, 
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)), 
  polygon_observation_idx = 1:(sum(1 * 4 ^ (0:2))), 
  polygon_size_bias = FALSE, 
  nonlinear_covariates = FALSE, 
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = seed
)}

#### testing.14 ####
all_data[["grid sampled observation single covariate"]] <- function(){taxdat:::create_standardized_test_data(
  ## Extent parameters:,
  ### NONE,
  ## Raster parameters:,
  nrows = 20,
  ncols = 20,
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1,
  n_layers = 2,
  factor = 4,
  snap = FALSE,
  randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,FALSE),
  nontemporal = c(FALSE,FALSE),
  spatially_smooth = c(FALSE,FALSE),
  temporally_smooth = c(FALSE,FALSE),
  polygonal = c(FALSE,FALSE),
  radiating = c(FALSE,TRUE),
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,st_union(st_sfc(
    st_point(c(0.25,0.25)),
    st_point(c(0.75,0.75)),
    crs=st_crs(create_test_polygons())
  ))),
  radiation_function = rep(list(function(x,mu){mu*RandomFieldsUtils::matern(x/1000,3/4)}),nc),
  radiating_means = list(NA,c(1)),
  smoothing_function = rep(
    list(
      function(n, mu, covariance, centers) {
        return(scale(MASS::mvrnorm(n = n, mu = mu, Matrix::solve(covariance))))
      }
    ),
    nc
  ),
  family = "Gaussian",
  magnitude = c(6,1),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = .8,
  number_draws = 1,
  grid_spatial_observation_bias = FALSE,
  grid_temporal_observation_bias = FALSE,
  grid_value_observation_bias = FALSE,
  noise = FALSE,
  polygon_proportion_observed = 1,
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)),
  polygon_observation_idx = c(2,3,4),
  polygon_size_bias = FALSE,
  nonlinear_covariates = FALSE,
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = seed
)}

#### testing.15 ####
all_data[["matern smoothed population with simple covariate."]] <- function(){create_standardized_test_data(
  ## Extent parameters:,
  ## Raster parameters:,
  nrows = 20, 
  ncols = 20, 
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1, n_layers = 3, factor = 4, snap = FALSE, randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,FALSE), 
  nontemporal = c(FALSE,FALSE), 
  spatially_smooth = c(TRUE,FALSE), 
  temporally_smooth = c(FALSE,FALSE), 
  polygonal = c(FALSE,FALSE), 
  radiating = c(FALSE,TRUE), 
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,st_union(st_sfc(
    st_point(c(0.25,0.25)),
    st_point(c(0.75,0.75)),
    crs=st_crs(create_test_polygons())
  ))),
  radiation_function = rep(list(function(x,mu){mu*RandomFieldsUtils::matern(x/1000,3/4)}),nc),
  radiating_means = list(NA,c(1)),
  family = "Gaussian", 
  magnitude = c(6,1),
  smoothing_function = list(function(n,mu,covar,centers){ # Need to pass centroids to this function
    distances <- matrix(NA,nrow(centers),nrow(centers))
    for(i in 1:nrow(distances)){
      partial <- t(as.matrix(centers)[i,] - t(as.matrix(centers)))
      distances[i,] <- sqrt(apply(partial * partial,1,sum))
    }
    covar[] <- RandomFieldsUtils::matern(distances,3/2)
    return(scale(MASS::mvrnorm(n=n,mu=mu,covar)))
  },NA),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1, 
  number_draws = 1, 
  grid_spatial_observation_bias = FALSE, 
  grid_temporal_observation_bias = FALSE, 
  grid_value_observation_bias = FALSE, 
  noise = FALSE, 
  polygon_proportion_observed = 1, 
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)), 
  polygon_observation_idx = 1:(sum(1 * 4 ^ (0:2))), 
  polygon_size_bias = FALSE, 
  nonlinear_covariates = FALSE, 
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = seed
)}

#### testing.16 ####
all_data[["oversampled observation single covariate"]] <- function(){taxdat:::create_standardized_test_data(
  ## Extent parameters:,
  ### NONE,
  ## Raster parameters:,
  nrows = 20,
  ncols = 20,
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1,
  n_layers = 2,
  factor = 4,
  snap = FALSE,
  randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,FALSE),
  nontemporal = c(FALSE,FALSE),
  spatially_smooth = c(FALSE,FALSE),
  temporally_smooth = c(FALSE,FALSE),
  polygonal = c(FALSE,FALSE),
  radiating = c(FALSE,TRUE),
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,st_union(st_sfc(
    st_point(c(0.25,0.25)),
    st_point(c(0.75,0.75)),
    crs=st_crs(create_test_polygons())
  ))),
  radiation_function = rep(list(function(x,mu){mu*RandomFieldsUtils::matern(x/1000,3/4)}),nc),
  radiating_means = list(NA,c(1)),
  smoothing_function = rep(
    list(
      function(n, mu, covariance, centers) {
        return(scale(MASS::mvrnorm(n = n, mu = mu, Matrix::solve(covariance))))
      }
    ),
    nc
  ),
  family = "Gaussian",
  magnitude = c(6,1),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1,
  number_draws = 10,
  grid_spatial_observation_bias = FALSE,
  grid_temporal_observation_bias = FALSE,
  grid_value_observation_bias = FALSE,
  noise = FALSE,
  polygon_proportion_observed = 1,
  nonlinear_covariates = FALSE,
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = seed
)}

#### testing.17 ####
all_data[["full observation single matern smoothed covariate finer observation scale with small population"]] <- function(){create_standardized_test_data(
  ## Extent parameters:,
  ## Raster parameters:,
  nrows = 20, 
  ncols = 20, 
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1, n_layers = 3, factor = 4, snap = FALSE, randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,FALSE), 
  nontemporal = c(FALSE,FALSE), 
  spatially_smooth = c(FALSE,TRUE), 
  temporally_smooth = c(FALSE,FALSE), 
  polygonal = c(FALSE,FALSE), 
  radiating = c(FALSE,FALSE), 
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,NA), 
  radiating_means = list(NA,NA), 
  family = "Gaussian", 
  magnitude = c(4,1),
  smoothing_function = list(NA,function(n,mu,covar,centers){ # Need to pass centroids to this function
    distances <- matrix(NA,nrow(centers),nrow(centers))
    for(i in 1:nrow(distances)){
      partial <- t(as.matrix(centers)[i,] - t(as.matrix(centers)))
      distances[i,] <- sqrt(apply(partial * partial,1,sum))
    }
    covar[] <- RandomFieldsUtils::matern(distances,3/2)
    return(scale(MASS::mvrnorm(n=n,mu=mu,covar)))
  }),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1, 
  number_draws = 1, 
  grid_spatial_observation_bias = FALSE, 
  grid_temporal_observation_bias = FALSE, 
  grid_value_observation_bias = FALSE, 
  noise = FALSE, 
  polygon_proportion_observed = 1, 
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)), 
  polygon_observation_idx = 1:(sum(1 * 4 ^ (0:2))), 
  polygon_size_bias = FALSE, 
  nonlinear_covariates = FALSE, 
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = seed
)}

#### testing.18 ####
all_data[["10 % sampled observation single covariate"]] <- function(){taxdat:::create_standardized_test_data(
  ## Extent parameters:,
  ### NONE,
  ## Raster parameters:,
  nrows = 20,
  ncols = 20,
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1,
  n_layers = 2,
  factor = 100,
  snap = FALSE,
  randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,FALSE),
  nontemporal = c(FALSE,FALSE),
  spatially_smooth = c(FALSE,FALSE),
  temporally_smooth = c(FALSE,FALSE),
  polygonal = c(FALSE,FALSE),
  radiating = c(FALSE,TRUE),
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,st_union(st_sfc(
    st_point(c(0.25,0.25)),
    st_point(c(0.75,0.75)),
    crs=st_crs(create_test_polygons())
  ))),
  radiation_function = rep(list(function(x,mu){mu*RandomFieldsUtils::matern(x/1000,3/4)}),nc),
  radiating_means = list(NA,c(1)),
  smoothing_function = rep(
    list(
      function(n, mu, covariance, centers) {
        return(scale(MASS::mvrnorm(n = n, mu = mu, Matrix::solve(covariance))))
      }
    ),
    nc
  ),
  family = "Gaussian",
  magnitude = c(6,1),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1,
  number_draws = 1,
  grid_spatial_observation_bias = FALSE,
  grid_temporal_observation_bias = FALSE,
  grid_value_observation_bias = FALSE,
  noise = FALSE,
  polygon_proportion_observed = 1,
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)),
  polygon_observation_idx = c(1,sample(100,10) + 1),
  polygon_size_bias = FALSE,
  nonlinear_covariates = FALSE,
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = seed
)}

#### testing.19 ####
all_data[["30 % sampled observation single covariate"]] <- function(){taxdat:::create_standardized_test_data(
  ## Extent parameters:,
  ### NONE,
  ## Raster parameters:,
  nrows = 20,
  ncols = 20,
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1,
  n_layers = 2,
  factor = 100,
  snap = FALSE,
  randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,FALSE),
  nontemporal = c(FALSE,FALSE),
  spatially_smooth = c(FALSE,FALSE),
  temporally_smooth = c(FALSE,FALSE),
  polygonal = c(FALSE,FALSE),
  radiating = c(FALSE,TRUE),
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,st_union(st_sfc(
    st_point(c(0.25,0.25)),
    st_point(c(0.75,0.75)),
    crs=st_crs(create_test_polygons())
  ))),
  radiation_function = rep(list(function(x,mu){mu*RandomFieldsUtils::matern(x/1000,3/4)}),nc),
  radiating_means = list(NA,c(1)),
  smoothing_function = rep(
    list(
      function(n, mu, covariance, centers) {
        return(scale(MASS::mvrnorm(n = n, mu = mu, Matrix::solve(covariance))))
      }
    ),
    nc
  ),
  family = "Gaussian",
  magnitude = c(6,1),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1,
  number_draws = 1,
  grid_spatial_observation_bias = FALSE,
  grid_temporal_observation_bias = FALSE,
  grid_value_observation_bias = FALSE,
  noise = FALSE,
  polygon_proportion_observed = 1,
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)),
  polygon_observation_idx = c(1,sample(100,30) + 1),
  polygon_size_bias = FALSE,
  nonlinear_covariates = FALSE,
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = seed
)}

#### testing.20 ####
all_data[["50 % sampled observation single covariate"]] <- function(){taxdat:::create_standardized_test_data(
  ## Extent parameters:,
  ### NONE,
  ## Raster parameters:,
  nrows = 20,
  ncols = 20,
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1,
  n_layers = 2,
  factor = 100,
  snap = FALSE,
  randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,FALSE),
  nontemporal = c(FALSE,FALSE),
  spatially_smooth = c(FALSE,FALSE),
  temporally_smooth = c(FALSE,FALSE),
  polygonal = c(FALSE,FALSE),
  radiating = c(FALSE,TRUE),
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,st_union(st_sfc(
    st_point(c(0.25,0.25)),
    st_point(c(0.75,0.75)),
    crs=st_crs(create_test_polygons())
  ))),
  radiation_function = rep(list(function(x,mu){mu*RandomFieldsUtils::matern(x/1000,3/4)}),nc),
  radiating_means = list(NA,c(1)),
  smoothing_function = rep(
    list(
      function(n, mu, covariance, centers) {
        return(scale(MASS::mvrnorm(n = n, mu = mu, Matrix::solve(covariance))))
      }
    ),
    nc
  ),
  family = "Gaussian",
  magnitude = c(6,1),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1,
  number_draws = 1,
  grid_spatial_observation_bias = FALSE,
  grid_temporal_observation_bias = FALSE,
  grid_value_observation_bias = FALSE,
  noise = FALSE,
  polygon_proportion_observed = 1,
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)),
  polygon_observation_idx = c(1,sample(100,50) + 1),
  polygon_size_bias = FALSE,
  nonlinear_covariates = FALSE,
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = seed
)}

#### testing.21 ####
all_data[["70 % sampled observation single covariate"]] <- function(){taxdat:::create_standardized_test_data(
  ## Extent parameters:,
  ### NONE,
  ## Raster parameters:,
  nrows = 20,
  ncols = 20,
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1,
  n_layers = 2,
  factor = 100,
  snap = FALSE,
  randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,FALSE),
  nontemporal = c(FALSE,FALSE),
  spatially_smooth = c(FALSE,FALSE),
  temporally_smooth = c(FALSE,FALSE),
  polygonal = c(FALSE,FALSE),
  radiating = c(FALSE,TRUE),
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,st_union(st_sfc(
    st_point(c(0.25,0.25)),
    st_point(c(0.75,0.75)),
    crs=st_crs(create_test_polygons())
  ))),
  radiation_function = rep(list(function(x,mu){mu*RandomFieldsUtils::matern(x/1000,3/4)}),nc),
  radiating_means = list(NA,c(1)),
  smoothing_function = rep(
    list(
      function(n, mu, covariance, centers) {
        return(scale(MASS::mvrnorm(n = n, mu = mu, Matrix::solve(covariance))))
      }
    ),
    nc
  ),
  family = "Gaussian",
  magnitude = c(6,1),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1,
  number_draws = 1,
  grid_spatial_observation_bias = FALSE,
  grid_temporal_observation_bias = FALSE,
  grid_value_observation_bias = FALSE,
  noise = FALSE,
  polygon_proportion_observed = 1,
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)),
  polygon_observation_idx = c(1,sample(100,70) + 1),
  polygon_size_bias = FALSE,
  nonlinear_covariates = FALSE,
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = seed
)}

#### testing.22 ####
all_data[["80 % sampled observation single covariate"]] <- function(){taxdat:::create_standardized_test_data(
  ## Extent parameters:,
  ### NONE,
  ## Raster parameters:,
  nrows = 20,
  ncols = 20,
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1,
  n_layers = 2,
  factor = 100,
  snap = FALSE,
  randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,FALSE),
  nontemporal = c(FALSE,FALSE),
  spatially_smooth = c(FALSE,FALSE),
  temporally_smooth = c(FALSE,FALSE),
  polygonal = c(FALSE,FALSE),
  radiating = c(FALSE,TRUE),
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,st_union(st_sfc(
    st_point(c(0.25,0.25)),
    st_point(c(0.75,0.75)),
    crs=st_crs(create_test_polygons())
  ))),
  radiation_function = rep(list(function(x,mu){mu*RandomFieldsUtils::matern(x/1000,3/4)}),nc),
  radiating_means = list(NA,c(1)),
  smoothing_function = rep(
    list(
      function(n, mu, covariance, centers) {
        return(scale(MASS::mvrnorm(n = n, mu = mu, Matrix::solve(covariance))))
      }
    ),
    nc
  ),
  family = "Gaussian",
  magnitude = c(6,1),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1,
  number_draws = 1,
  grid_spatial_observation_bias = FALSE,
  grid_temporal_observation_bias = FALSE,
  grid_value_observation_bias = FALSE,
  noise = FALSE,
  polygon_proportion_observed = 1,
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)),
  polygon_observation_idx = c(1,sample(100,80) + 1),
  polygon_size_bias = FALSE,
  nonlinear_covariates = FALSE,
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = seed
)}

#### testing.23 ####
all_data[["90 % sampled observation single covariate"]] <- function(){taxdat:::create_standardized_test_data(
  ## Extent parameters:,
  ### NONE,
  ## Raster parameters:,
  nrows = 20,
  ncols = 20,
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1,
  n_layers = 2,
  factor = 100,
  snap = FALSE,
  randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,FALSE),
  nontemporal = c(FALSE,FALSE),
  spatially_smooth = c(FALSE,FALSE),
  temporally_smooth = c(FALSE,FALSE),
  polygonal = c(FALSE,FALSE),
  radiating = c(FALSE,TRUE),
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,st_union(st_sfc(
    st_point(c(0.25,0.25)),
    st_point(c(0.75,0.75)),
    crs=st_crs(create_test_polygons())
  ))),
  radiation_function = rep(list(function(x,mu){mu*RandomFieldsUtils::matern(x/1000,3/4)}),nc),
  radiating_means = list(NA,c(1)),
  smoothing_function = rep(
    list(
      function(n, mu, covariance, centers) {
        return(scale(MASS::mvrnorm(n = n, mu = mu, Matrix::solve(covariance))))
      }
    ),
    nc
  ),
  family = "Gaussian",
  magnitude = c(6,1),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1,
  number_draws = 1,
  grid_spatial_observation_bias = FALSE,
  grid_temporal_observation_bias = FALSE,
  grid_value_observation_bias = FALSE,
  noise = FALSE,
  polygon_proportion_observed = 1,
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)),
  polygon_observation_idx = c(1,sample(100,90) + 1),
  polygon_size_bias = FALSE,
  nonlinear_covariates = FALSE,
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = seed
)}

#### testing.24 ####
all_data[["95 % sampled observation single covariate"]] <- function(){taxdat:::create_standardized_test_data(
  ## Extent parameters:,
  ### NONE,
  ## Raster parameters:,
  nrows = 20,
  ncols = 20,
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1,
  n_layers = 2,
  factor = 100,
  snap = FALSE,
  randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,FALSE),
  nontemporal = c(FALSE,FALSE),
  spatially_smooth = c(FALSE,FALSE),
  temporally_smooth = c(FALSE,FALSE),
  polygonal = c(FALSE,FALSE),
  radiating = c(FALSE,TRUE),
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,st_union(st_sfc(
    st_point(c(0.25,0.25)),
    st_point(c(0.75,0.75)),
    crs=st_crs(create_test_polygons())
  ))),
  radiation_function = rep(list(function(x,mu){mu*RandomFieldsUtils::matern(x/1000,3/4)}),nc),
  radiating_means = list(NA,c(1)),
  smoothing_function = rep(
    list(
      function(n, mu, covariance, centers) {
        return(scale(MASS::mvrnorm(n = n, mu = mu, Matrix::solve(covariance))))
      }
    ),
    nc
  ),
  family = "Gaussian",
  magnitude = c(6,1),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1,
  number_draws = 1,
  grid_spatial_observation_bias = FALSE,
  grid_temporal_observation_bias = FALSE,
  grid_value_observation_bias = FALSE,
  noise = FALSE,
  polygon_proportion_observed = 1,
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)),
  polygon_observation_idx = c(1,sample(100,95) + 1),
  polygon_size_bias = FALSE,
  nonlinear_covariates = FALSE,
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = seed
)}

#### testing.25 ####
all_data[["99 % sampled observation single covariate"]] <- function(){taxdat:::create_standardized_test_data(
  ## Extent parameters:,
  ### NONE,
  ## Raster parameters:,
  nrows = 20,
  ncols = 20,
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1,
  n_layers = 2,
  factor = 100,
  snap = FALSE,
  randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,FALSE),
  nontemporal = c(FALSE,FALSE),
  spatially_smooth = c(FALSE,FALSE),
  temporally_smooth = c(FALSE,FALSE),
  polygonal = c(FALSE,FALSE),
  radiating = c(FALSE,TRUE),
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,st_union(st_sfc(
    st_point(c(0.25,0.25)),
    st_point(c(0.75,0.75)),
    crs=st_crs(create_test_polygons())
  ))),
  radiation_function = rep(list(function(x,mu){mu*RandomFieldsUtils::matern(x/1000,3/4)}),nc),
  radiating_means = list(NA,c(1)),
  smoothing_function = rep(
    list(
      function(n, mu, covariance, centers) {
        return(scale(MASS::mvrnorm(n = n, mu = mu, Matrix::solve(covariance))))
      }
    ),
    nc
  ),
  family = "Gaussian",
  magnitude = c(6,1),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1,
  number_draws = 1,
  grid_spatial_observation_bias = FALSE,
  grid_temporal_observation_bias = FALSE,
  grid_value_observation_bias = FALSE,
  noise = FALSE,
  polygon_proportion_observed = 1,
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)),
  polygon_observation_idx = c(1,sample(100,99) + 1),
  polygon_size_bias = FALSE,
  nonlinear_covariates = FALSE,
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = seed
)}

#### testing.26 ####
all_data[["try1 causing orig model to escape prior"]] <- function(){create_standardized_test_data(
  ## Extent parameters:,
  ## Raster parameters:,
  nrows = 20, 
  ncols = 20, 
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1, n_layers = 3, factor = 4, snap = FALSE, randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,TRUE), 
  nontemporal = c(FALSE,FALSE), 
  spatially_smooth = c(FALSE,TRUE), 
  temporally_smooth = c(FALSE,FALSE), 
  polygonal = c(FALSE,FALSE), 
  radiating = c(FALSE,FALSE), 
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,NA), 
  radiating_means = list(NA,NA), 
  family = "Gaussian", 
  magnitude = c(8,1),
  smoothing_function = list(NA,function(n,mu,covar,centers){ # Need to pass centroids to this function
    distances <- matrix(NA,nrow(centers),nrow(centers))
    for(i in 1:nrow(distances)){
      partial <- t(as.matrix(centers)[i,] - t(as.matrix(centers)))
      distances[i,] <- sqrt(apply(partial * partial,1,sum))
    }
    covar[] <- RandomFieldsUtils::matern(distances,3/2)
    return(scale(MASS::mvrnorm(n=n,mu=mu,covar)))
  }),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1, 
  number_draws = 1, 
  grid_spatial_observation_bias = FALSE, 
  grid_temporal_observation_bias = FALSE, 
  grid_value_observation_bias = FALSE, 
  noise = FALSE, 
  polygon_proportion_observed = 1, 
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)), 
  polygon_observation_idx = 1:(sum(1 * 4 ^ (0:2))), 
  polygon_size_bias = FALSE, 
  nonlinear_covariates = FALSE, 
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = 64650
)}

#### testing.27 ####
all_data[["try2 causing orig model to escape prior"]] <- function(){create_standardized_test_data(
  ## Extent parameters:,
  ## Raster parameters:,
  nrows = 20, 
  ncols = 20, 
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1, n_layers = 3, factor = 4, snap = FALSE, randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,TRUE), 
  nontemporal = c(FALSE,FALSE), 
  spatially_smooth = c(FALSE,TRUE), 
  temporally_smooth = c(FALSE,FALSE), 
  polygonal = c(FALSE,FALSE), 
  radiating = c(FALSE,TRUE), 
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,st_union(st_sfc(
    st_point(c(0.35,0.75)),
    crs=st_crs(create_test_polygons())
  ))), 
  family = "Gaussian", 
  magnitude = c(8,1),
  smoothing_function = list(NA,function(n,mu,covar,centers){ # Need to pass centroids to this function
    distances <- matrix(NA,nrow(centers),nrow(centers))
    for(i in 1:nrow(distances)){
      partial <- t(as.matrix(centers)[i,] - t(as.matrix(centers)))
      distances[i,] <- sqrt(apply(partial * partial,1,sum))
    }
    covar[] <- RandomFieldsUtils::matern(distances,3/2)
    return(scale(MASS::mvrnorm(n=n,mu=mu,covar)))
  }),
  radiation_function = rep(list(function(x,mu){mu*RandomFieldsUtils::matern(x/1000,3/4)}),nc),
  radiating_means = list(NA,c(1)),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1, 
  number_draws = 1, 
  grid_spatial_observation_bias = FALSE, 
  grid_temporal_observation_bias = FALSE, 
  grid_value_observation_bias = FALSE, 
  noise = FALSE, 
  polygon_proportion_observed = 1, 
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)), 
  polygon_observation_idx = 1:(sum(1 * 4 ^ (0:2))), 
  polygon_size_bias = FALSE, 
  nonlinear_covariates = FALSE, 
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = 45222
)}

#### testing.28 ####
all_data[["try3 causing orig model to escape prior"]] <- function(){create_standardized_test_data(
  ## Extent parameters:,
  ## Raster parameters:,
  nrows = 20, 
  ncols = 20, 
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1, n_layers = 3, factor = 4, snap = FALSE, randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,TRUE), 
  nontemporal = c(FALSE,FALSE), 
  spatially_smooth = c(FALSE,TRUE), 
  temporally_smooth = c(FALSE,FALSE), 
  polygonal = c(FALSE,FALSE), 
  radiating = c(FALSE,TRUE), 
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,st_union(st_sfc(
    st_point(c(0.35,0.85)),
    crs=st_crs(create_test_polygons())
  ))), 
  family = "Gaussian", 
  magnitude = c(8,1),
  smoothing_function = list(NA,function(n,mu,covar,centers){ # Need to pass centroids to this function
    distances <- matrix(NA,nrow(centers),nrow(centers))
    for(i in 1:nrow(distances)){
      partial <- t(as.matrix(centers)[i,] - t(as.matrix(centers)))
      distances[i,] <- sqrt(apply(partial * partial,1,sum))
    }
    covar[] <- RandomFieldsUtils::matern(distances,3/2)
    return(scale(MASS::mvrnorm(n=n,mu=mu,covar)))
  }),
  radiation_function = rep(list(function(x,mu){mu*RandomFieldsUtils::matern(x/1000,3/4)}),nc),
  radiating_means = list(NA,c(1)),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1, 
  number_draws = 1, 
  grid_spatial_observation_bias = FALSE, 
  grid_temporal_observation_bias = FALSE, 
  grid_value_observation_bias = FALSE, 
  noise = FALSE, 
  polygon_proportion_observed = 1, 
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)), 
  polygon_observation_idx = 1:(sum(1 * 4 ^ (0:2))), 
  polygon_size_bias = FALSE, 
  nonlinear_covariates = FALSE, 
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = 64650
)}

#### testing.29 ####
all_data[["try4 causing orig model to escape prior"]] <- function(){create_standardized_test_data(
  ## Extent parameters:,
  ## Raster parameters:,
  nrows = 20, 
  ncols = 20, 
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1, n_layers = 3, factor = 4, snap = FALSE, randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,TRUE), 
  nontemporal = c(FALSE,FALSE), 
  spatially_smooth = c(FALSE,TRUE), 
  temporally_smooth = c(FALSE,FALSE), 
  polygonal = c(FALSE,FALSE), 
  radiating = c(FALSE,FALSE), 
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  family = "Gaussian", 
  magnitude = c(8,1),
  smoothing_function = list(NA,function(n,mu,covar,centers){ # Need to pass centroids to this function
    distances <- matrix(NA,nrow(centers),nrow(centers))
    for(i in 1:nrow(distances)){
      partial <- t(as.matrix(centers)[i,] - t(as.matrix(centers)))
      distances[i,] <- sqrt(apply(partial * partial,1,sum))
    }
    covar[] <- RandomFieldsUtils::matern(distances,3/2)
    return(scale(MASS::mvrnorm(n=n,mu=mu,covar)))
  }),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1, 
  number_draws = 1, 
  grid_spatial_observation_bias = FALSE, 
  grid_temporal_observation_bias = FALSE, 
  grid_value_observation_bias = FALSE, 
  noise = FALSE, 
  polygon_proportion_observed = 1, 
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)), 
  polygon_observation_idx = 1:(sum(1 * 4 ^ (0:2))), 
  polygon_size_bias = FALSE, 
  nonlinear_covariates = FALSE, 
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = 80887
)}

#### testing.30 ####
all_data[["try5 causing orig model to escape prior"]] <- function(){create_standardized_test_data(
  ## Extent parameters:,
  ## Raster parameters:,
  nrows = 20, 
  ncols = 20, 
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1, n_layers = 3, factor = 4, snap = FALSE, randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,TRUE), 
  nontemporal = c(FALSE,FALSE), 
  spatially_smooth = c(FALSE,FALSE), 
  temporally_smooth = c(FALSE,FALSE), 
  polygonal = c(FALSE,FALSE), 
  radiating = c(FALSE,TRUE), 
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,st_union(st_sfc(
    st_point(c(0.35,0.85),
             c(0.55,0.10)),
    crs=st_crs(create_test_polygons())
  ))), 
  family = "Gaussian", 
  magnitude = c(8,1),
  smoothing_function = list(NA,function(n,mu,covar,centers){ # Need to pass centroids to this function
    distances <- matrix(NA,nrow(centers),nrow(centers))
    for(i in 1:nrow(distances)){
      partial <- t(as.matrix(centers)[i,] - t(as.matrix(centers)))
      distances[i,] <- sqrt(apply(partial * partial,1,sum))
    }
    covar[] <- RandomFieldsUtils::matern(distances,3/2)
    return(scale(MASS::mvrnorm(n=n,mu=mu,covar)))
  }),
  radiation_function = rep(list(function(x,mu){mu*RandomFieldsUtils::matern(x/1000,3/4)}),nc),
  radiating_means = list(NA,c(1,2)),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1, 
  number_draws = 1, 
  grid_spatial_observation_bias = FALSE, 
  grid_temporal_observation_bias = FALSE, 
  grid_value_observation_bias = FALSE, 
  noise = FALSE, 
  polygon_proportion_observed = 1, 
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)), 
  polygon_observation_idx = 1:(sum(1 * 4 ^ (0:2))), 
  polygon_size_bias = FALSE, 
  nonlinear_covariates = FALSE, 
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = 300022
)}

#### testing.31 ####
all_data[["profiling baseline"]] <- function(){taxdat:::create_standardized_test_data(
  ## Extent parameters:,
  ### NONE,
  ## Raster parameters:,
  nrows = 20,
  ncols = 20,
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1,
  n_layers = 2,
  factor = 4,
  snap = FALSE,
  randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,FALSE),
  nontemporal = c(FALSE,FALSE),
  spatially_smooth = c(FALSE,FALSE),
  temporally_smooth = c(FALSE,FALSE),
  polygonal = c(FALSE,FALSE),
  radiating = c(FALSE,TRUE),
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,st_union(st_sfc(
    st_point(c(0.25,0.25)),
    st_point(c(0.75,0.75)),
    crs=st_crs(create_test_polygons())
  ))),
  radiation_function = rep(list(function(x,mu){mu*RandomFieldsUtils::matern(x/1000,3/4)}),nc),
  radiating_means = list(NA,c(1)),
  smoothing_function = rep(
    list(
      function(n, mu, covariance, centers) {
        return(scale(MASS::mvrnorm(n = n, mu = mu, Matrix::solve(covariance))))
      }
    ),
    nc
  ),
  family = "Gaussian",
  magnitude = c(6,1),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1,
  number_draws = 1,
  grid_spatial_observation_bias = FALSE,
  grid_temporal_observation_bias = FALSE,
  grid_value_observation_bias = FALSE,
  noise = FALSE,
  polygon_proportion_observed = 1,
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)),
  polygon_size_bias = FALSE,
  nonlinear_covariates = FALSE,
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = seed
)}

#### testing.32 ####
all_data[["profiling gridsize 4x"]] <- function(){taxdat:::create_standardized_test_data(
  ## Extent parameters:,
  ### NONE,
  ## Raster parameters:,
  nrows = 40,
  ncols = 40,
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1,
  n_layers = 2,
  factor = 4,
  snap = FALSE,
  randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,FALSE),
  nontemporal = c(FALSE,FALSE),
  spatially_smooth = c(FALSE,FALSE),
  temporally_smooth = c(FALSE,FALSE),
  polygonal = c(FALSE,FALSE),
  radiating = c(FALSE,TRUE),
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,st_union(st_sfc(
    st_point(c(0.25,0.25)),
    st_point(c(0.75,0.75)),
    crs=st_crs(create_test_polygons())
  ))),
  radiation_function = rep(list(function(x,mu){mu*RandomFieldsUtils::matern(x/1000,3/4)}),nc),
  radiating_means = list(NA,c(1)),
  smoothing_function = rep(
    list(
      function(n, mu, covariance, centers) {
        return(scale(MASS::mvrnorm(n = n, mu = mu, Matrix::solve(covariance))))
      }
    ),
    nc
  ),
  family = "Gaussian",
  magnitude = c(6,1),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1,
  number_draws = 1,
  grid_spatial_observation_bias = FALSE,
  grid_temporal_observation_bias = FALSE,
  grid_value_observation_bias = FALSE,
  noise = FALSE,
  polygon_proportion_observed = 1,
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)),
  polygon_observation_idx = c(1,2,3,4,5),
  polygon_size_bias = FALSE,
  nonlinear_covariates = FALSE,
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = seed
)}

#### testing.33 ####
all_data[["profiling observations 4x"]] <- function(){taxdat:::create_standardized_test_data(
  ## Extent parameters:,
  ### NONE,
  ## Raster parameters:,
  nrows = 20,
  ncols = 20,
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1,
  n_layers = 2,
  factor = 4,
  snap = FALSE,
  randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,FALSE),
  nontemporal = c(FALSE,FALSE),
  spatially_smooth = c(FALSE,FALSE),
  temporally_smooth = c(FALSE,FALSE),
  polygonal = c(FALSE,FALSE),
  radiating = c(FALSE,TRUE),
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,st_union(st_sfc(
    st_point(c(0.25,0.25)),
    st_point(c(0.75,0.75)),
    crs=st_crs(create_test_polygons())
  ))),
  radiation_function = rep(list(function(x,mu){mu*RandomFieldsUtils::matern(x/1000,3/4)}),nc),
  radiating_means = list(NA,c(1)),
  smoothing_function = rep(
    list(
      function(n, mu, covariance, centers) {
        return(scale(MASS::mvrnorm(n = n, mu = mu, Matrix::solve(covariance))))
      }
    ),
    nc
  ),
  family = "Gaussian",
  magnitude = c(6,1),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1,
  number_draws = 4,
  grid_spatial_observation_bias = FALSE,
  grid_temporal_observation_bias = FALSE,
  grid_value_observation_bias = FALSE,
  noise = FALSE,
  polygon_proportion_observed = 1,
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)),
  polygon_size_bias = FALSE,
  nonlinear_covariates = FALSE,
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = seed
)}

#### testing.34 ####
all_data[["profiling both 4x"]] <- function(){taxdat:::create_standardized_test_data(
  ## Extent parameters:,
  ### NONE,
  ## Raster parameters:,
  nrows = 40,
  ncols = 40,
  nlayers = 2,
  ## Polygon Parameters,
  base_number = 1,
  n_layers = 2,
  factor = 4,
  snap = FALSE,
  randomize = FALSE,
  ## Covariate Parameters,
  ncovariates = nc,
  nonspatial = c(FALSE,FALSE),
  nontemporal = c(FALSE,FALSE),
  spatially_smooth = c(FALSE,FALSE),
  temporally_smooth = c(FALSE,FALSE),
  polygonal = c(FALSE,FALSE),
  radiating = c(FALSE,TRUE),
  constant = c(TRUE, FALSE),
  rho = rep(.999999, times = nc),
  radiating_polygons = list(NA,st_union(st_sfc(
    st_point(c(0.25,0.25)),
    st_point(c(0.75,0.75)),
    crs=st_crs(create_test_polygons())
  ))),
  radiation_function = rep(list(function(x,mu){mu*RandomFieldsUtils::matern(x/1000,3/4)}),nc),
  radiating_means = list(NA,c(1)),
  smoothing_function = rep(
    list(
      function(n, mu, covariance, centers) {
        return(scale(MASS::mvrnorm(n = n, mu = mu, Matrix::solve(covariance))))
      }
    ),
    nc
  ),
  family = "Gaussian",
  magnitude = c(6,1),
  normalization = function(x){
    if(length(unique(x[])) == 1){
      return(x * 0 + 1)
    }
    x <- exp(x)
    x[] <- .1 * ((x[] - min(x[])) / (max(x[]) - min(x[])))
    return(x)
  },
  ## Observation Parameters,
  grid_proportion_observed = 1,
  number_draws = 4,
  grid_spatial_observation_bias = FALSE,
  grid_temporal_observation_bias = FALSE,
  grid_value_observation_bias = FALSE,
  noise = FALSE,
  polygon_proportion_observed = 1,
  polygon_observation_rates = exp(rnorm(nrow(test_polygons), -1)),
  polygon_size_bias = FALSE,
  nonlinear_covariates = FALSE,
  min_time_left = lubridate::ymd("2000-01-01"),
  max_time_right = lubridate::ymd("2000-12-31"),
  seed = seed
)}

test_data <- all_data[[test_idx]]()
sf_grid <- test_data[['raster']]
covar_cube <- array(NA,c(max(sf_grid$id),max(sf_grid$t),length(test_data[['covariates']])))
for(i in seq_len(length(test_data[['covariates']]))){
  covar_cube[,,i] <- test_data[['covariates']][[i]][['covariate']]
}
covar_cube[,,1] <- 1 + 10^covar_cube[,,1]

all_locations <- unique(test_data[[1]]$location)
location_ids <- setNames(1:length(all_locations),all_locations)
test_data[[1]]$attributes.location_period_id <- location_ids[test_data[[1]]$location]

sf_grid$rid <- 1
sf_grid$id
sf_grid$long_id <- seq_len(nrow(sf_grid))
sf_cases <- test_data[[1]]

names(sf_cases)[names(sf_cases) == 'cases'] <- cases_column
sf_cases[[cases_column]] <- floor(sf_cases[[cases_column]])
sf_cases$TL <- sf_cases[['time_left']]
sf_cases$TR <- sf_cases[['time_right']]

full_shapefile <- st_union(sf_cases)

non_na_gridcells <- which(apply(covar_cube,c(1,2),function(x){(x[[1]] > 1) & !any(is.na(x))}))
gridchanger <- setNames(seq_len(length(non_na_gridcells)),non_na_gridcells)
sf_grid$upd_long_id <- gridchanger[as.character(sf_grid$long_id)]

location_periods_dict <- sf::st_drop_geometry(sf::st_intersection(sf_cases,sf_grid)[c('attributes.location_period_id', 'rid','col','row','id','t','long_id','upd_long_id')])
names(location_periods_dict) <-                                                     c('location_period_id','rid','x',  'y',  'id','t','long_id','upd_long_id')

# Determine which cells in the raster intersect which shapefiles
sf_cases <- sf_cases[st_dimension(sf_cases) == 2,]
sf_cases$id <- seq_len(nrow(sf_cases))
sf_cases$valid <- TRUE

min_time_left <- min(sf_cases$time_left)
max_time_right <- max(sf_cases$time_right)
model_date_range <- (max_time_right +lubridate::days(1) - min_time_left) * (seq_len(max(sf_grid$t)) - 1) + min_time_left

save(test_data, sf_cases, location_periods_dict, file = preprocessed_data_fname)
save(covar_cube, model_date_range, sf_grid, non_na_gridcells, location_periods_dict, file = preprocessed_covar_fname)
