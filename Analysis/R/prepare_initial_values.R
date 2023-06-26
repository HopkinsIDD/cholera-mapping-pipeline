#!/usr/bin/Rscript
##  This scripts purpose is to collect all data required to model cholera incidence, and store it in a form usable by stan.
##  This script mainly uses functions from the package taxdat, stored in trunk/packages/taxdat.
# Libraries
### Control Variables:

### Construct some additional parameters based on the above
# Define relevent directories
# Name the output file

# Stan modeling section
print("*** STARTING STAN MODEL ***")

library(rstan)

# GAM for warm start of spatial random effects ----------------------------

# Create coordinates of cell centroids
# Avoid using st_cendroid which fails on MARCC for some unkown reason
coord_frame <- tibble::as_tibble(sf::st_coordinates(sf_grid)) %>% 
  dplyr::group_by(L2) %>% 
  dplyr::summarise(x = mean(X), 
                   y = mean(Y))

# Create matrix of time
year_df <- tibble::tibble(year = stan_data$map_grid_time)
year_df$year <- factor(year_df$year)

## one random effect per year
if (length(unique(year_df$year)) == 1) {
  mat_grid_time <- matrix(1, nrow(year_df))
} else {
  mat_grid_time <- model.matrix(as.formula("~ year - 1"), data = year_df)
}

# Create dataframe for GAM model
old_percent <- 0
df <- purrr::map_dfr(
  1:length(stan_data$y),
  function(i){
    # Print progress
    new_percent <- floor(100*i/stan_data$M)
    if(new_percent != old_percent){
      print(paste(i,"/", stan_data$M))
      old_percent <<- new_percent
    }
    # Get the location period x time slice covered by observation
    ind_obs <- which(stan_data$map_obs_loctime_obs == i)
    ind_lp <- stan_data$map_obs_loctime_loc[ind_obs]
    # ind_lp <- stan_data$map_obs_loctime_loc[which(stan_data$map_obs_loctime_obs == stan_data$ind_full[i])]
    # Get the corresponding grid indices
    ind <- stan_data$map_loc_grid_grid[which(stan_data$map_loc_grid_loc %in% ind_lp)]
    # Setup the data
    pop <- stan_data$pop[ind]
    # Cases are assumed to be proportional to population in the covered grid cells
    # y <- round(rep(stan_data$y[stan_data$ind_full[i]], length(ind))*pop/sum(pop))
    obs_year <- stan_data$map_grid_time[ind]
    u_obs_years <- unique(obs_year)
    tfrac <- stan_data$tfrac[ind_obs]
    tfrac_tot <- sum(unique(tfrac))
    
    # Expand observations to space x time gridcells
    y_grid <- purrr::map(seq_along(ind_obs), function(x) {
      # Get grid indices of loctime
      sub_ind <- stan_data$map_loc_grid_grid[which(stan_data$map_loc_grid_loc %in% stan_data$map_obs_loctime_loc[ind_obs[x]])] 
      # Get the time slice
      ts <- stan_data$map_grid_time[sub_ind] %>% unique()
      # Get the population in the time slice
      pop_ts <- sum(pop[obs_year == ts])
      
      # Disaggregate
      stan_data$y[i] * tfrac[x]/tfrac_tot * stan_data$pop[sub_ind]/pop_ts
      
    }) %>%
      unlist() %>% 
      round()
    
    tfrac_vec <- purrr::map(seq_along(ind_obs), function(x)
      rep(tfrac[x], sum(obs_year == u_obs_years[x]))) %>% 
      unlist()
    
    sx <- coord_frame$x[ind]
    sy <- coord_frame$y[ind]
    
    if (stan_data$ncovar > 0) {
      beta_mat <- stan_data$covar[ind, ] %>% 
        matrix(ncol = stan_data$ncovar) %>% 
        magrittr::set_colnames(paste0("beta_", 1:stan_data$ncovar))
    }
    
    year_mat <- mat_grid_time[ind, ] %>%
      matrix(ncol = ncol(mat_grid_time)) %>%
      magrittr::set_colnames(paste0("year_", 1:ncol(mat_grid_time)))
    
    return(
      tibble::tibble(obs = i,
                     raw_y = stan_data$y[i],
                     y = y_grid,
                     sx = sx,
                     sy = sy,
                     ind = ind,
                     pop = pop,
                     obs_year = obs_year,
                     meanrate = stan_data$meanrate,
                     ey = pop*stan_data$meanrate,
                     tfrac = tfrac_vec,
                     censored = stan_data$censoring_inds[i]) %>%
        {
          if (stan_data$ncovar > 0) {
            cbind(., beta_mat) 
          } else {
            .
          }
        } %>% 
        cbind(year_mat)
    )
  }
) %>% 
  tibble::as_tibble() %>% 
  dplyr::mutate(obs_year = factor(obs_year),
                log_ey = log(ey),
                log_tfrac = log(tfrac),
                gam_offset = log_ey + log_tfrac * (1-config$censoring),
                # To apply censoring
                right_threshold = dplyr::case_when(
                  censored == "right-censored" ~ y,
                  T ~ Inf)
  )


# Initial values ----------------------------------------------------------

# Use warmup?
warmup <- config$warmup

# Specifiy whether covariates are included in the warmup
covar_warmup <- config$covar_warmup

if (warmup) {
  
  # Create gam frml
  if (stan_data$use_intercept) {
    frml <- "y ~ s(sx,sy)"
  } else {
    frml <- "y ~ s(sx,sy) - 1"
  }
  
  if (stan_data$ncovar >= 1 & covar_warmup) {
    frml <- paste(c(frml, paste0("beta_", 1:stan_data$ncovar)), collapse = " + ")
  }
  
  # Is the model one with a time-specific random effect?
  if (config$time_effect) {
    frml <- paste(c(frml, colnames(df %>% dplyr::select(dplyr::contains("year_")))), collapse = " + ")
  }
  
  # Formula for gam model
  gam_frml <- as.formula(frml)
  
  if (config$censoring) {
    # Removed censored data for which cases are 0
    df <- df %>% dplyr::filter(!(y == 0 & right_threshold == 0))
  }
  
  # Fit the GAM
  gam_fit <- mgcv::gam(gam_frml,
                       offset = gam_offset,
                       family = "poisson", 
                       data = df)
  
  # GAM estimates
  indall <- sf_grid$upd_id[sf_grid$t == 1]
  
  # Predict to get new terms
  predict_df <- tibble::tibble(sx = coord_frame$x[indall],
                               sy = coord_frame$y[indall]) %>% 
    # Set all years to 0 to get the reference year
    cbind(mat_grid_time[indall, ] %>% 
            tibble::as_tibble() %>%
            magrittr::set_colnames(paste0("year_", 1:ncol(mat_grid_time)))) %>% 
    { 
      if(stan_data$ncovar > 0) {
        # Extract the covariates
        cbind(., stan_data$covar[indall, ] %>% 
                matrix(ncol = stan_data$ncovar) %>% 
                magrittr::set_colnames(paste0("beta_", 1:stan_data$ncovar)))
      } else {
        .
      }
    }
  
  # Predict log(lambda) for the reference year with covariates
  y_pred_mean <- mgcv::predict.gam(gam_fit, predict_df)
  
  if (stan_data$ncovar >= 1 & covar_warmup) {
    # Remove the effect of the betas
    beta_effect <- as.matrix(dplyr::select(predict_df, dplyr::contains("beta"))) %*% matrix(coef(gam_fit)[stringr::str_detect(names(coef(gam_fit)), "beta")], ncol = 1)
    w.init <- y_pred_mean - as.vector(beta_effect)
  } else {
    w.init <- y_pred_mean
  }
  
  # Initial parameter values
  if (config$time_effect | config$grid_rand_effects_N != 1) {
    sd_w <- sd(w.init)
    
    if (config$time_effect & config$grid_rand_effects_N != 1) {
      stop("Current code does not allow grid_rand_effects_N != 1 and time_effect = true")
    }
    
    if (config$grid_rand_effects_N != 1) {
      init.list <- lapply(1:nchain, 
                          function(i) {
                            list(
                              # Perturbation of spatial random effects
                              w = rnorm(length(w.init) * config$grid_rand_effects_N,
                                        rep(w.init, config$grid_rand_effects_N), .1)
                            )})
    }
    
    if (config$time_effect) {
      stan_data$mat_grid_time <- mat_grid_time %>% as.matrix()
      eta <- coef(gam_fit) %>% .[stringr::str_detect(names(.), "year")]
      init.list <- lapply(1:nchain, 
                          function(i) {
                            list(
                              # Perturbation of spatial random effects
                              w = rnorm(length(w.init), w.init, .1),
                              # Perturbation of fitted etas
                              eta_tilde = as.array(rnorm(length(eta), eta/stan_data$sigma_eta_scale, .05)),
                              sigma_eta_tilde = as.array(1)
                            )})
    }
  } else {
    init.list <- lapply(1:nchain, function(i) list(w = rnorm(length(w.init), w.init, .1)))
  }
  
  if (stan_data$ncovar >= 1 & covar_warmup) {
    betas <- coef(gam_fit) %>% .[stringr::str_detect(names(.), "beta")]
    for (i in 1:length(init.list)) {
      init.list[[i]] <- append(init.list[[i]],
                               # Perturbation of fitted betas
                               list(betas = rnorm(length(betas), betas, .1) %>% array()))
    }
  }
  
} else {
  # Set to random initial draws if no covar warmup
  init.list <- NULL #QZ: change from "random" to list()
  
  if (config$time_effect) {
    stan_data$mat_grid_time <- mat_grid_time %>% as.matrix()
  }
}

if (!(config$time_effect)) {
  stan_data$mat_grid_time <- as.array(matrix(0,2,2))
}

# Set censoring and time effect and autocorrelation
stan_data$do_censoring <- ifelse(config$censoring, 1, 0)
stan_data$do_time_slice_effect <- ifelse(config$time_effect, 1, 0)
stan_data$do_time_slice_effect_autocor <- ifelse(config$time_effect_autocor, 1, 0)
stan_data$do_spatial_effect <- ifelse(config$spatial_effect, 1, 0)
stan_data$use_weights <- ifelse(config$use_weights, 1, 0)
stan_data$use_rho_prior<- ifelse(config$use_rho_prior, 1, 0)

if (config$use_rho_prior) {
  if (length(init.list) !=1) {
    for (i in 1:nchain) {
      init.list[[i]] <- append(init.list[[i]], list(rho = runif(1, .6, 1)))
    }
  }
}

if (config$time_effect) {
  # Extract number of observations per year
  obs_per_year <- df %>% dplyr::count(obs_year) %>% 
    dplyr::mutate(obs_year = as.numeric(as.character(obs_year)))
  # For each time slice check if there is data informing it
  # If there is no data in a given year, the model will ignore the yearly random effect
  has_data_year <- purrr::map_dbl(stan_data$map_grid_time, ~ . %in% obs_per_year$obs_year)
  stan_data$has_data_year <- has_data_year
} else {
  stan_data$has_data_year <- array(dim = c(0))
}


# For negbinom likelihood overwrite init.list to have more sensible
# initial values for the overdispersion parameter
if (config$obs_model == 3) {
  
  # For now use random .1 initialization
  # init.list <- .1
  
  init.list <- purrr::map(
    1:nchain, function(x) {
      init <- list(
        std_dev_w = runif(1, 5, 8),
        rho = runif(1, .7, .8),
        lambda = runif(1, .6, .8),
        sigma_std_dev_w = abs(rnorm(2, .5, .1)),
        # This assumes we have a fixed overdispersion parameter at admin level 1
        inv_od_param = abs(rnorm(stan_data$N_admin_lev - 1, 1, 1e-1))
      )
      
      if (config$time_effect) {
        if (config$do_zerosum_cnst) {
          n_eta <- stan_data$`T` - 1
        } else {
          n_eta <- stan_data$`T`
        }
        
        init <- append(
          init,
          list(eta_tilde = rnorm(n_eta, 0, .1))
        )
      }
      
      if (config$spatial_effect) {
        if (!config$use_intercept) {
          warning("!! Runing a model with no space effect and no intercept.")
        }
        
        # Small values of spatial random effects
        init <- append(
          init,
          list(w = rnorm(stan_data$smooth_grid_N, 0, .1))
        )
      } else {
        # Small values of spatial random effects
        init <- append(
          init,
          list(w = array(dim = 0),
               alpha = array(rnorm(1, 0, .1), dim = 1)
          )
        )
      }
      
      if (config$use_intercept) {
        init <- append(
          init,
          list(alpha = rnorm(1, -3, .5))
        )
      }
      
      init
    })
  
} else if (config$obs_model == 2) {
  
  N_w <- length(w.init)
  N_eta <- length(eta)
  if (config$do_zerosum_cnst) {
    N_eta <- N_eta - 1
  }
  N_admin_lev <- stan_data$N_admin_lev
  
  init.list <- lapply(
    1:nchain, 
    function(i) {
      list(
        w = rnorm(N_w, 0, .1),
        eta_tilde = as.array(rnorm(N_eta, 0, .1)),
        alpha = rnorm(1, -5, .1),
        inv_od_param = abs(rnorm(N_admin_lev, 3, 1)),
        log_std_dev_w = rnorm(1, 0, .1)
      )})
}


# Set value of negative binomial models with fixed overdispersion parameter
# Javier 17-01-2023: This section is deprecated because we are not using this typoe of function anymore
# if (stringr::str_detect(stan_model, "fixedphi")) {
#   if (is.null(config$overdispersion)) {
#     stop("Please provid the value for negative binomial models with fixed overdispersion parameter")
#   } else if (is.na(config$overdispersion)) {
#     stop("Please provid the value for negative binomial models with fixed overdispersion parameter")
#   } else {
#     stan_data$phi <- config$overdispersion
#   }
# }

initial_values_data <- list(
  stan_data = stan_data,
  init.list = init.list,
  gam_fit_input = df
)
if (warmup) {
  initial_values_data$gam_fit_output = gam_fit
}
save(initial_values_data, file=file_names[["initial_values"]])
