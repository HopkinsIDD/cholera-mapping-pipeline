## testing for get_loctime_combs_mappings function

# single year obs, single lp
test_that("get_loctime_combs_mappings works", { 
    stan_data <-list() 
    stan_data$map_obs_loctime_obs <- array(1:10)
    names(stan_data$map_obs_loctime_obs) <- 1:10
    
    stan_data$map_obs_loctime_loc <- array(rep(1:10,1))
    names(stan_data$map_obs_loctime_loc) <- 1:10
    
    stan_data$map_obs_admin_lev <- rep(1:5,2)
    
    stan_data <- taxdat::get_loctime_combs_mappings(stan_data)

    testthat::expect_true(all(sapply(stan_data$u_loctime_combs,length)==1))

})


# single year obs, multiple lp
test_that("get_loctime_combs_mappings works", { 
  stan_data <-list() 
  stan_data$map_obs_loctime_obs <- array(1:10)
  names(stan_data$map_obs_loctime_obs) <- 1:10
  
  stan_data$map_obs_loctime_loc <- array(c(rep(1:8,1),rep(10,2)))
  names(stan_data$map_obs_loctime_loc) <- 1:10
  
  stan_data$map_obs_admin_lev <- rep(1:5,2)
  
  stan_data <- taxdat::get_loctime_combs_mappings(stan_data)
  
  testthat::expect_true(all(sapply(stan_data$u_loctime_combs,length)==1))
  
})

# multi year obs, single lp
test_that("get_loctime_combs_mappings works", { 
  stan_data <-list() 
  stan_data$map_obs_loctime_obs <- array(c(1:8,9,9))
  names(stan_data$map_obs_loctime_obs) <- 1:10
  
  stan_data$map_obs_loctime_loc <- array(rep(1:10,1))
  names(stan_data$map_obs_loctime_loc) <- 1:10
  
  stan_data$map_obs_admin_lev <- rep(1:5,2)
  
  stan_data <- taxdat::get_loctime_combs_mappings(stan_data)
  
  testthat::expect_true(all(sapply(stan_data$u_loctime_combs,length)==1))
  testthat::expect_false(all(table(names(stan_data$map_obs_loctime_combs))==1))
  
})

# multi year obs, multi lps
test_that("get_loctime_combs_mappings works", { 
  stan_data <-list() 
  stan_data$map_obs_loctime_obs <- array(c(1:8,9,9))
  names(stan_data$map_obs_loctime_obs) <- 1:10
  
  stan_data$map_obs_loctime_loc <- array(c(rep(1:8,1),rep(10,2)))
  names(stan_data$map_obs_loctime_loc) <- 1:10
  
  stan_data$map_obs_admin_lev <- rep(1:5,2)
  
  stan_data <- taxdat::get_loctime_combs_mappings(stan_data)
  
  testthat::expect_true(all(sapply(stan_data$u_loctime_combs,length)==1))
  testthat::expect_false(all(table(names(stan_data$map_obs_loctime_combs))==1))
  
})