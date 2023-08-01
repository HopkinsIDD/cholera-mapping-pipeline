## testing for get_loctime_combs function

# single year obs, single lp
test_that("get_loctime_combs works", { 
  stan_data <-list() 
  stan_data$map_obs_loctime_obs <- array(1:10)
  names(stan_data$map_obs_loctime_obs) <- 1:10
  
  stan_data$map_obs_loctime_loc <- array(rep(1:10,1))
  names(stan_data$map_obs_loctime_loc) <- 1:10
  
  loctime_combs <- taxdat::get_loctime_combs(stan_data)
  
  testthat::expect_equal(length(stan_data$map_obs_loctime_obs),length(loctime_combs))
  testthat::expect_equal(length(stan_data$map_obs_loctime_loc),length(loctime_combs))
  
})


# single year obs, multiple lps
test_that("get_loctime_combs works", { 
  stan_data <-list() 
  stan_data$map_obs_loctime_obs <- array(1:10)
  names(stan_data$map_obs_loctime_obs) <- 1:10
  
  stan_data$map_obs_loctime_loc <- array(c(rep(1:8,1),rep(10,2)))
  names(stan_data$map_obs_loctime_loc) <- 1:10
  
  loctime_combs <- taxdat::get_loctime_combs(stan_data)
  
  testthat::expect_true(length(stan_data$map_obs_loctime_obs) > length(unique(loctime_combs)))
  testthat::expect_true(length(stan_data$map_obs_loctime_loc) > length(unique(loctime_combs)))
  
})

# multi year obs, single lp
test_that("get_loctime_combs works", { 
  stan_data <-list() 
  stan_data$map_obs_loctime_obs <- array(c(1:8,9,9))
  names(stan_data$map_obs_loctime_obs) <- 1:10
  
  stan_data$map_obs_loctime_loc <- array(rep(1:10,1))
  names(stan_data$map_obs_loctime_loc) <- 1:10
  
  loctime_combs <- taxdat::get_loctime_combs(stan_data)
  
  testthat::expect_true(length(unique(stan_data$map_obs_loctime_obs)) < length(unique(loctime_combs)))
  testthat::expect_equal(length(stan_data$map_obs_loctime_loc), length(unique(loctime_combs)))
  
})

# multi year obs, single lp
test_that("get_loctime_combs works", { 
  stan_data <-list() 
  stan_data$map_obs_loctime_obs <- array(c(1:8,9,9))
  names(stan_data$map_obs_loctime_obs) <- 1:10
  
  stan_data$map_obs_loctime_loc <- array(c(rep(1:8,1),rep(10,2)))
  names(stan_data$map_obs_loctime_loc) <- 1:10
  
  loctime_combs <- taxdat::get_loctime_combs(stan_data)
  
  testthat::expect_true(length(stan_data$map_obs_loctime_obs) > length(unique(loctime_combs)))
  testthat::expect_true(length(stan_data$map_obs_loctime_loc) > length(unique(loctime_combs)))
  
})