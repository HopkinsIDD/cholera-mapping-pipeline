# Stan modeling section
print("*** STARTING STAN MODEL ***")

# GAM for warm start of spatial random effects ----------------------------

# Create coordinates of cell centroids
# Avoid using st_cendroid which fails on MARCC for some unkown reason
coord_frame <- as_tibble(sf::st_coordinates(sf_grid)) %>% 
  group_by(L2) %>% 
  summarise(x = mean(X), 
            y = mean(Y))

# coord_frame = data.frame(
#   x = unlist(lapply(st_geometry(st_centroid(sf_grid)),function(x){x[[2]]})),
#   y = unlist(lapply(st_geometry(st_centroid(sf_grid)),function(x){x[[1]]}))
# )

# Create dataframe for GAM model
old_percent <- 0
df <- purrr::map_dfr(
  1:stan_data$M_full,
  function(i){
    # Print progress
    new_percent <- floor(100*i/stan_data$M_full)
    if(new_percent != old_percent){
      print(paste(i,"/", stan_data$M_full))
      old_percent <<- new_percent
    }
    # Get the location period covered by observation
    ind_lp <- stan_data$map_obs_loctime_loc[which(stan_data$map_obs_loctime_obs == stan_data$ind_full[i])]
    # Get the corresponding grid indices
    ind <- stan_data$map_loc_grid_grid[which(stan_data$map_loc_grid_loc %in% ind_lp)]
    # Setup the data
    pop <- stan_data$pop[ind]
    # Cases are assumed to be proportional to population in the covered grid cells
    y <- round(rep(stan_data$y[stan_data$ind_full[i]], length(ind))*pop/sum(pop))
    obs_year <- stan_data$map_grid_time[ind]
    sx <- coord_frame$x[ind]
    sy <- coord_frame$y[ind]
    
    return(
      tibble(y = y,
             sx = sx,
             sy = sy,
             pop = pop,
             obs_year = obs_year,
             meanrate = stan_data$meanrate,
             ey = pop*stan_data$meanrate)
    )
  }
) %>% 
  mutate(obs_year = factor(obs_year),
         log_ey = log(ey))

# Check if the stan model has yearly random effects 
# TODO this assumes that the modeling time resolution is 1 year!! Need to change
# it to extract the year
yearly_effect <- any(str_detect(readLines(paste0(stan_dir, config$stan$model)), "eta"))

if(yearly_effect | res_time != "1 years")
  stop("Yearly random effects for modeling time resolutions other than 1 year are not yet implemented.")

if (yearly_effect) {
  gam_frml <- y ~ s(sx,sy) + obs_year - 1
} else {
  gam_frml <- y ~ s(sx,sy) - 1
}

# Fit the GAM
gam_fit <- mgcv::gam(gam_frml, 
                     offset = log_ey,
                     family = poisson, 
                     data = df)
# GAM estimates
gam_coef <- coef(gam_fit)
# Indexes of smoothing grid
indall <- seq_len(stan_data$smooth_grid_N)

# Extract smoothing
if (yearly_effect) {
  # Extract yearly effects
  eta <- gam_coef[str_detect(names(gam_coef), "year")]
  # Remove mean to sum to 0
  mean_eta <- mean(eta)
  eta <- eta - mean(eta)
  w.init <- log(gam_fit$fitted.values[indall]) - log(df$ey[indall]) - mean_eta
} else {
  w.init <- log(gam_fit$fitted.values[indall]) - log(df$ey[indall])
}

# Initial parameter values
if (yearly_effect) {
  stan_data$sigma_eta_scale <- 10^round(log10(sd(eta)))
  
  init.list <- lapply(1:nchain, 
                      function(i) {
                        list(w = w.init,
                             eta_tilde = eta/sd(eta),
                             sigma_eta_tilde = sd(eta)/stan_data$sigma_eta_scale)
                      })
} else {
  init.list <- lapply(1:nchain, function(i) list(w = w.init))
}

# Run model ---------------------------------------------------------------
model.rand <- stan(
  file = stan_model_path,
  data = stan_data,
  chains = nchain,
  iter = niter,
  thin = max(1,floor(niter/1000)),
  control = list(
    max_treedepth=15
  ),
  init = init.list
)

# Save output
save(model.rand,file=stan_output_fname)


# split_file <- strsplit(file,'.',fixed=TRUE)[[1]]
# # print(split_file)
# samples <- as.numeric(split_file[[5]]) / 2
# if(is.na(samples)){
#   samples <- as.numeric(split_file[[6]]) / 2
# }
# model_file <- gsub('.stan$','',stan_model_path)
# if(recompile){
#   print("recompiling stan model")
#   system(paste0("compile_stan_model ",model_file))
#   recompile <- FALSE
# }
# foreach(chain = seq_len(nchain)) %dopar% {
# # for(chain in seq_len(nchain)) {
#   run_model <- TRUE
#   outfile <- gsub('json',paste0(chain,'.csv'),gsub('input','output',file))
#   random_seed = sample.int(.Machine$integer.max, 1)
#   if(file.exists(outfile)){
#     tmp <- R.utils::countLines(outfile) - 48
#     if(tmp >= samples){
#       run_model <- FALSE
#     }
#   }
#   if(run_model){
#     cat(paste0(model_file," sample num_samples=",samples," num_warmup=",samples," data file='",file,"'"," random seed=",random_seed," output file='",outfile,"'"))
#     cat("\n")
#     err <- system(paste0(model_file," sample num_samples=",samples," num_warmup=",samples," data file='",file,"'"," random seed=",random_seed," output file='",outfile,"'"))
#     if(err >= 2){stop("Stan error")}
#   }
# }
# 
# parallel::stopCluster(cl)
