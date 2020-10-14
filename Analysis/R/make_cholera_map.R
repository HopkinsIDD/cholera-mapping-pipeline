pdf(map_output_fname)

non_na_cells = function(raster_layer){
  if('population' %in% names(raster_layer)){
    non_na_gridcells = which(
      (taxdat::aggregate_raster_xlayers(raster_layer,function(x){!any(is.na(x))})[]) &
      ((raster_layer[['population']])[] >= 1)
    )
  } else {
    non_na_gridcells = which(taxdat::aggregate_raster_xlayers(raster_layer,function(x){!any(is.na(x))})[])
  }
  if(length(non_na_gridcells)==0){
    stop("All gridcells are NA")
  }
  gridcell_converter = setNames(1:length(non_na_gridcells),non_na_gridcells)
  names(non_na_gridcells) = gridcell_converter
  return(list(non_na_gridcells,gridcell_converter))
}

get_log_scores <- function(predicted,actual,lookup,probs){
  predicted = MCMCvis::MCMCchains(predicted)
  samples = abind::abind(predicted[,grep(lookup,colnames(predicted))],along=1)
  qsamp = apply(samples,2,quantile,probs=probs)
  logscores = log(dpois(actual,qsamp))
  logscores[is.infinite(logscores)] <- -10000
  errors <- abs(qsamp - actual)
  # plot(sort(log(errors)))
  return(mean(logscores))
}

get_expectation <- function(predicted,lookup,probs){
  predicted = MCMCvis::MCMCchains(predicted)
  samples = abind::abind(predicted[,grep(lookup,colnames(predicted))],along=1)
  qsamp = apply(samples,2,mean)
  return(qsamp)
}

get_quantile <- function(predicted,lookup,probs){
  predicted = MCMCvis::MCMCchains(predicted)
  samples = abind::abind(predicted[,grep(lookup,colnames(predicted))],along=1)
  qsamp = apply(samples,2,quantile,probs=probs)
  return(qsamp)
}

get_standard_deviation <- function(predicted,lookup){
  predicted = MCMCvis::MCMCchains(predicted)
  samples = abind::abind(predicted[,grep(lookup,colnames(predicted))],along=1)
  qsamp = apply(samples,2,sd)
  return(qsamp)
}

non_na_gridcells <- non_na_cells(basic_raster)[[1]]

basic_raster$cases = NA
basic_raster[['cases']][non_na_gridcells] = get_expectation(model.rand,'grid_cases')
basic_raster$cases.sd = NA
basic_raster[['cases.sd']][non_na_gridcells] = get_standard_deviation(model.rand,'grid_cases')
basic_raster$log.rate = NA
basic_raster[['log.rate']][non_na_gridcells] = get_expectation(model.rand,'log_lambda')
basic_raster$rate.sd = NA
basic_raster[['rate.sd']][non_na_gridcells] = get_standard_deviation(model.rand,'log_lambda')
plot(basic_raster[[c('cases')]])
plot(basic_raster[[c('log.rate')]])

plot(basic_raster[[c('cases','cases.sd','log.rate','rate.sd')]])

plot(population_raster[['population']])
# if(stan_data$ncovar > 0){
#   plot(basic_raster[[covariate_choices]])
#   print(max(basic_raster[['population']][],na.rm=T))
# }

## define resol
resol <- (dim(population_raster) / dim(basic_raster))[1:2]
beautiful_raster <- basic_raster$log.rate
beautiful_raster$rate <- exp(beautiful_raster$log.rate)
beautiful_raster$rate[][
  log10(beautiful_raster$rate[]) < -7
] <- 10^-7
beautiful_raster <- beautiful_raster[['rate']]
beautiful_raster <- raster::disaggregate(beautiful_raster,resol)
beautiful_raster$cases <- beautiful_raster[['rate']] * population_raster[['population']]
beautiful_raster$cases[][
  log10(beautiful_raster$cases[]) < -2
] <- 10^-2

library(RColorBrewer)
WtOrBr = c("#FFFFFF", "#FED98E", "#FE9929", "#D95F0E", "#993404")
case_palette <- colorRampPalette(WtOrBr,space="Lab")
rate_palette <- brewer.pal(9, name="RdBu")
rate_palette <-colorRampPalette(rate_palette, space = "Lab")

cases_max <- max(beautiful_raster$cases[])
plot(log10(beautiful_raster$cases),zlim = c(-5,log10(cases_max)),col=case_palette(255))

plot(log10(100000*beautiful_raster$rate),zlim = c(-4,4),col=rev(rate_palette(255)))

# for(page in 1:(nrow(unique(data.frame(sf_cases$OC_UID, sf_cases$TL, sf_cases$TR)))/4)){
#   plt <- ggplot2::ggplot(sf_cases) + 
#     ggplot2::geom_sf(ggplot2::aes(fill=log(1+attributes.fields.suspected_cases))) +
#     ggforce::facet_wrap_paginate(OC_UID + TL + TR~.,nrow=2,ncol=2,page=page)
#   print(plt)
# }

# MCMCvis::MCMCplot(model.rand,params=stan_params)
# pairs(model.rand,pars = stan_params)
MCMCvis::MCMCtrace(model.rand,pdf=FALSE)
while(dev.off() != 1){}
