# Stan modeling section
print("*** STARTING STAN MODEL ***")

# GAM for warm start of spatial random effects ----------------------------

# Create coordinates of cell centroids
# Avoid using st_cendroid which fails on MARCC for some unkown reason
coord_frame <- as_tibble(sf::st_coordinates(sf_grid)) %>% 
  group_by(L2) %>% 
  summarise(x = mean(X), 
            y = mean(Y))

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
    # Get the location period covered by observation
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
    # Expand observations to account for multiple tfracs
    y_new <- map(seq_along(ind_obs), function(x)
                       rep(stan_data$y[i] * tfrac[x]/sum(tfrac), 
                           sum(obs_year == u_obs_years[x]))) %>% 
      unlist()
    
    tfrac_vec <- map(seq_along(ind_obs), function(x)
      rep(tfrac[x], sum(obs_year == u_obs_years[x]))) %>% 
      unlist()
    
    y <- round(y_new * pop/sum(pop))
    
    sx <- coord_frame$x[ind]
    sy <- coord_frame$y[ind]
    beta_mat <- stan_data$covar[ind, ] %>% 
      as.matrix() %>% 
      set_colnames(paste0("beta_", 1:stan_data$ncovar))
    
    return(
      tibble(y = y,
             sx = sx,
             sy = sy,
             pop = pop,
             obs_year = obs_year,
             meanrate = stan_data$meanrate,
             ey = pop*stan_data$meanrate,
             tfrac = tfrac_vec,
             censored = stan_data$censoring_inds[i])
    ) %>% 
      cbind(beta_mat)
  }
) %>% 
  mutate(obs_year = factor(obs_year),
         log_ey = log(ey),
         log_tfrac = log(tfrac),
         gam_offset = log_ey + log_tfrac,
         # To apply censoring
         right_threshold = case_when(
           censored == "right-censored" ~ y,
           T ~ Inf),
         # Transform to fit with normal approximation
         y_sqrt = sqrt(y)
  )

# Create matrix of time
year_df <- data.frame(year = stan_data$map_grid_time)
year_df$year <- factor(year_df$year)
# Set reference year as the one with the largest number of cases
ref_year <- which.max(df %>% group_by(obs_year) %>% summarise(x = sum(y)) %>% .[["x"]])
year_df$year <- relevel(year_df$year, ref = ref_year)

mat_grid_time <- model.matrix(as.formula("~ year - 1"), data = year_df)
mat_grid_time <- mat_grid_time[, -1] # the reference year is always first

# Formula for gam model
gam_frml <- as.formula("y ~ s(sx,sy) - 1")

# Removed censored data for which cases are 0
df <- df %>% filter(!(y == 0 & right_threshold == 0))

# Fit the GAM
gam_fit <- mgcv::gam(gam_frml,
                     offset = gam_offset,
                     family = "poisson", 
                     data = df)

# GAM estimates
indall <- seq_len(stan_data$smooth_grid_N)

# Predict to get new terms
predict_df <- tibble(sx = coord_frame$x[indall],
                     sy = coord_frame$y[indall])

w.init <- predict.gam(gam_fit, predict_df)

# Does the model have a yearly effect
yearly_effect <- any(str_detect(readLines(stan_model_path), "eta"))

# Initial parameter values
if (yearly_effect) {
  stan_data$sigma_eta_scale <- 10
  stan_data$map_grid_time <- mat_grid_time
  init.list <- lapply(1:nchain, 
                      function(i) {
                        list(w = w.init)})
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
  pars = c("b", "t_rowsum", "vec_var"),
  include = F,
  # control = list(
  #   max_treedepth = 15
  # ),
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
