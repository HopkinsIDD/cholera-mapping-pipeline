#!/usr/bin/Rscript
##  This scripts purpose is to collect all data required to model cholera incidence, and store it in a form usable by stan.
##  This script mainly uses functions from the package taxdat, stored in trunk/packages/taxdat.
# Libraries
### Control Variables:

### Construct some additional parameters based on the above
# Define relevent directories
# Name the output file

### gam for warm start
library(mgcv)
coord_frame = data.frame(
  x = unlist(lapply(sf::st_geometry(sf::st_centroid(sf_grid)),function(x){x[[2]]})),
  y = unlist(lapply(sf::st_geometry(sf::st_centroid(sf_grid)),function(x){x[[1]]}))
)
old_percent <- 0
df <- purrr::map_dfr(
  1:length(stan_data$y),
  function(i){
    new_percent <- floor(100*i/length(stan_data$y))
    if(new_percent != old_percent){
      print(paste(i,"/",length(stan_data$y)))
      old_percent <<- new_percent
    }
    ind_lp = stan_data$map_obs_loctime_loc[which(stan_data$map_obs_loctime_obs==i)]
    ind=stan_data$map_loc_grid_grid[which(stan_data$map_loc_grid_loc %in% ind_lp)]
    #y=log(rep(max(stan_data$y[i],0.5),length(ind)))-log(stan_data$pop[ind])
    y=round(rep(stan_data$y[i],length(ind))/length(ind))
    sx= coord_frame[ind,'x']
    sy= coord_frame[ind,'y']
    pop=stan_data$pop[ind]
    return(data.frame(y=y,sx=sx,sy=sy,pop=pop,meanrate=stan_data$meanrate,ey=pop*stan_data$meanrate))
  }
)
g=gam(y ~ log(ey) + s(sx,sy),family=poisson,data=df)

#indall=which(stan_data$map_obs==1)
indall=seq_len(stan_data$smooth_grid_N)
w.init=log(g$fitted.values[indall])-log(df$ey[indall])
#image(matrix(w.init,nrow=20))
w.list=lapply(1:nchain,function(i) list(w=w.init))

model.rand<- stan(
  file=stan_model_path,
  data=stan_data,
  chains=nchain,
  iter=niter,
  # refresh= 100,
  thin = max(1,floor(niter/1000)),
  #thin=1,
  # refresh=1# ,
  # sample_file = paste0(stan_output_fname,'.tmp'),
  # control = list(
  #   ### july 5, 2020 runs: delta=0.99, tree=15
  #   ### increasing it will force Stan to take smaller steps
  #   #adapt_delta=.99,
  #   #max_treedepth=15
  # ),
  init=w.list
)
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
