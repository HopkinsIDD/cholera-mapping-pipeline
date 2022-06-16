---
title: "Country Data Report"
output: html_document
params: 
  cholera_directory: "~/cholera-mapping-pipeline"
  config: "/Analysis/configs/config.yml"
  drop_nodata_years: TRUE
  admin_level_for_summary_table: '1'
  args: 'myarg'
  old_runs: FALSE
---
```{r setup, include=FALSE, dev="CairoPNG",message=FALSE}
library(knitr)
knitr::opts_chunk$set(
  cache=TRUE,
  cache.lazy = FALSE,
  echo = FALSE,
  dev="CairoPNG",
  error = FALSE,
  fig.align = "center",
  message = TRUE,
  warning = TRUE
  )
library(stringr)
library(dplyr)
library(magrittr)
library(purrr)
library(readr)
library(ggplot2)
library(kableExtra)
library(taxdat)
library(sf)
library(raster)
library(stars)
library(taxdat)
library(gridExtra)
sf::sf_use_s2(FALSE)

### other new packages (mainly for "rgeoboundaries")
chooseCRANmirror(ind = 77)

package_list <- c(
  "fasterize", 
  "remotes",
  "rgeoboundaries"
)

for (package in package_list) {
  if (!require(package = package, character.only = T)) {
    if (package == "rgeoboundaries"){
      try({
        remotes::install_gitlab("dickoa/rgeoboundaries")
        remotes::install_github("wmgeolab/rgeoboundaries")
      })
    }else{
      install.packages(pkgs = package)
      library(package = package, character.only = T)
    }
  }
}

#Figure Caption Numbering,
capFigNo = 1
#Function to add the Figure Number
capFig = function(x){
    x = paste0("Figure ",capFigNo,". ",x)
    capFigNo <<- capFigNo + 1
    x
}

```


```{r include=FALSE, message=FALSE}
# Parameters to use for development purposes ************************************************************************************************************************
params <- new.env() 
params$cholera_directory <- "/home/kaiyuezou/mapping_pipeline/New_Dev/cholera-mapping-pipeline"
params$config1 <- "/Analysis/configs/production_tests/country_data_report_comparison_test/BEN/config_BEN_2015_2019_covar.yml"
params$config2 <- "/Analysis/configs/production_tests/country_data_report_comparison_test/BEN/config_BEN_2015_2019_nocovar.yml"
params$drop_nodata_years <- TRUE
params$admin_level_for_summary_table <- '1'
params$args <- 'myarg'
params$old_runs <- FALSE

# Plan
# 1. function that can stitch and cache two configs
# 2. function that can read in the needed output for comparison purposes 
# 3. function that can stitch and cache two output from two different runs 
# 4. function that can take stitched output and call plotting functions to generate one single plot 

```


# Mapping results summary for `r stringr::str_extract(params$config, "[A-Z]{3}")` and period `r stringr::str_extract(params$config, "[0-9]{4}_[0-9]{4}")`


```{r include=FALSE, message=FALSE}
# 1. functions that can cache and stitch two configs
cache1 <- new.env()
cache2 <- new.env()
comparison_cache <- new.env()

get_config(cache = cache1, config = params$config1, cholera_directory = params$cholera_directory)
get_config(cache = cache2, config = params$config2, cholera_directory = params$cholera_directory)

stitch_configs(output_cache = comparison_cache, input_caches = list(cache1, cache2))

# 2. function that can read in and stitch the needed output for comparison purposes 
#GAM input and output √
#table 1 and table 2 √
#covariate 
#table 4 
#more...(including comparing two configs)
#needs to change the scale of the figs to original (is it possible?)

## GAM input and output
taxdat::get_initial_values(name = "initial_values_data", cache = cache1, 
  config = params$config1, cholera_directory = params$cholera_directory)
taxdat::get_initial_values(name = "initial_values_data", cache = cache2, 
  config = params$config2, cholera_directory = params$cholera_directory)

cache1[["gam_output_df"]] <- get_gam_values_no_cache(cache = cache1, config = params$config1, cholera_directory = params$cholera_directory)
cache2[["gam_output_df"]] <- get_gam_values_no_cache(cache = cache2, config = params$config2, cholera_directory = params$cholera_directory)

stitch_gam_input(output_cache = comparison_cache, input_caches = list(cache1, cache2))
stitch_gam_output(output_cache = comparison_cache, input_caches = list(cache1, cache2))

## table 1 and table 2
cache1$used_data_table <- plot_ObservationSummary_table(config = params$config1, cache = cache1, cholera_directory = params$cholera_directory, aesthetic = FALSE)
cache2$used_data_table <- plot_ObservationSummary_table(config = params$config2, cache = cache2, cholera_directory = params$cholera_directory, aesthetic = FALSE)

cache1$dropped_data_table <- plot_DroppedData_table(config = params$config1, cache = cache1, cholera_directory = params$cholera_directory, aesthetic = FALSE)
cache2$dropped_data_table <- plot_DroppedData_table(config = params$config2, cache = cache2, cholera_directory = params$cholera_directory, aesthetic = FALSE)

stitch_used_data_table(output_cache = comparison_cache, input_caches = list(cache1, cache2))
stitch_dropped_data_table(output_cache = comparison_cache, input_caches = list(cache1, cache2))

```


**Observations input for the GAM model:**
```{r gam input of cases, fig.height=5, fig.width=10, fig.cap=capFig("Observations input for the GAM model (cases)")}
plot_gam_fit_input_cases_stitched(cache = comparison_cache)
```
```{r gam input of rates, fig.height=5, fig.width=10, fig.cap=capFig("Observations input for the GAM model (rates)")}
plot_gam_fit_input_rates_stitched(cache = comparison_cache)
```

**Observations output of the GAM model:**
```{r gam output of cases, fig.height=25, fig.width=10, fig.cap=capFig("Observations input for the GAM model (cases)")}
plot_gam_fit_output_cases_stitched(cache = comparison_cache)
```
```{r gam output of rates, fig.height=25, fig.width=10, fig.cap=capFig("Observations input for the GAM model (rates)")}
plot_gam_fit_output_rates_stitched(cache = comparison_cache)
```


**Table 1. Data used within the model by comparison between two runs:**
```{r echo = FALSE}
comparison_cache$used_data_table
```

**Table 2. Data dropped from the model by comparison between two runs:**
```{r echo=FALSE}
comparison_cache$dropped_data_table
```


```{r include=FALSE, message=FALSE}
# Try looking at the plot
pdf("/home/kaiyuezou/mapping_pipeline/New_Dev/cholera-mapping-pipeline/Analysis/output/gam_output_rates.pdf", height = 25, width = 10)
plot1
dev.off()

```


```{r include=FALSE, message=FALSE}
# 2. function that can read in and stitch the needed output for comparison purposes 
#GAM input and output √
#table 1 and table 2
#covariate 
#table 4 
#more...(including comparing two configs)
#needs to change the scale of the figs to original 
stitch_GAM_input(output_cache = comparison_cache, input_caches = list(list(params$config1, params$config2)), cholera_directory = params$cholera_directory)
stitch_GAM_output(output_cache = comparison_cache, input_caches = list(list(params$config1, params$config2)), cholera_directory = params$cholera_directory)

stitch_GAM_input_figure(output_cache = comparison_cache)
stitch_GAM_output_figure(output_cache = comparison_cache)

stitch_used_data_table(output_cache = comparison_cache, input_caches = list(list(params$config1, params$config2)), cholera_directory = params$cholera_directory)
# gridExtra::grid.arrange(comparison_cache$GAM_input_figure$fig_cases1, comparison_cache$GAM_input_figure$fig_cases2, ncol = 2) #this step plots 
# gridExtra::grid.arrange(comparison_cache$GAM_input_figure$fig_rates1, comparison_cache$GAM_input_figure$fig_rates2, ncol = 2) #this step plots

```


### GAM Input
**Observations input comparison for the GAM model:**
```{r gam input of cases,fig.height=5, fig.width=10, fig.cap=capFig("Observations input comparison for the GAM model (cases)")}
  gridExtra::grid.arrange(comparison_cache$GAM_input_figure$fig_cases1, comparison_cache$GAM_input_figure$fig_cases2, ncol = 2) #this step plots
```

```{r gam input of rates,fig.height=5, fig.width=10, fig.cap=capFig("Observations input comparison of the GAM model (rates)")}
  gridExtra::grid.arrange(comparison_cache$GAM_input_figure$fig_rates1, comparison_cache$GAM_input_figure$fig_rates2, ncol = 2) #this step plots
```

### GAM Output
**Observations output comparison of the GAM model:**
```{r gam output of cases,fig.height=25, fig.width=10, fig.cap=capFig("Observations output for the GAM model (cases)")}
  comparison_cache$GAM_output_figure$fig_cases1$facet$params$ncol <- 1
  comparison_cache$GAM_output_figure$fig_cases2$facet$params$ncol <- 1
  gridExtra::grid.arrange(comparison_cache$GAM_output_figure$fig_cases1, comparison_cache$GAM_output_figure$fig_cases2, ncol = 2) #this step plots
```

```{r gam output of rates,fig.height=25, fig.width=10, fig.cap=capFig("Observations output for the GAM model (rates)")}
  comparison_cache$GAM_output_figure$fig_rates1$facet$params$ncol <- 1
  comparison_cache$GAM_output_figure$fig_rates2$facet$params$ncol <- 1
  gridExtra::grid.arrange(comparison_cache$GAM_output_figure$fig_rates1, comparison_cache$GAM_output_figure$fig_rates2, ncol = 2) #this step plots
```


```{r include=FALSE, message=FALSE}
# Try looking at the plot
pdf("/home/kaiyuezou/mapping_pipeline/New_Dev/cholera-mapping-pipeline/Analysis/output/gam_figures.pdf")
gridExtra::grid.arrange(comparison_cache$GAM_input_figure$fig_cases1, comparison_cache$GAM_input_figure$fig_cases2, ncol = 2)
gridExtra::grid.arrange(comparison_cache$GAM_input_figure$fig_rates1, comparison_cache$GAM_input_figure$fig_rates2, ncol = 2)
gridExtra::grid.arrange(comparison_cache$GAM_output_figure$fig_cases1, comparison_cache$GAM_output_figure$fig_cases2, ncol = 2)
gridExtra::grid.arrange(comparison_cache$GAM_output_figure$fig_rates1, comparison_cache$GAM_output_figure$fig_rates2, ncol = 2)
dev.off()

```





## Cholera input data
**Data input:**
```{r load data,message=FALSE}
# Load data
cache<-new.env()
get_stan_input(name="stan_input",cache=cache,config = params$config,cholera_directory = params$cholera_directory)
get_sf_cases_resized(name="sf_cases_resized",cache=cache,config = params$config,cholera_directory = params$cholera_directory)
get_sf_cases(name="sf_cases",cache=cache,config=params$config,cholera_directory=params$cholera_directory)

get_genquant(name="genquant",cache=cache,config=params$config,cholera_directory = params$cholera_directory)
get_model_rand(name="model.rand",cache=cache,config = params$config,cholera_directory = params$cholera_directory)
get_modeled_cases(name="modeled_cases",cache=cache,config=params$config,cholera_directory = params$cholera_directory)
get_modeled_rates(name="modeled_rates",cache=cache,config = params$config,cholera_directory = params$cholera_directory)
cache[["modeled_cases_by_chain"]]<-aggregate_modeled_cases_by_chain_no_cache(cache=cache,config = params$config,cholera_directory = params$cholera_directory)
cache[["cases_chains"]]<-aggregate_modeled_cases_by_chain_gridtime_no_cache(cache=cache,config = params$config,cholera_directory = params$cholera_directory)
cache[["niter_per_chain"]]<-get_stan_model_niter_per_chain_no_cache(cache=cache,config = params$config,cholera_directory = params$cholera_directory)
cache[["nchain"]]<-get_stan_model_nchain_no_cache(cache=cache,config = params$config,cholera_directory = params$cholera_directory)
cache[["stan_output"]]<-get_stan_output_no_cache(cache=cache,config = params$config,cholera_directory = params$cholera_directory)

get_initial_values(name="initial_values_data",cache=cache,config = params$config,cholera_directory = params$cholera_directory)
cache[["gam_output_df"]]<-get_gam_values_no_cache(cache=cache,config = params$config,cholera_directory = params$cholera_directory)

get_covar_cube (name="covar_cube", config = params$config,cache=cache,cholera_directory = params$cholera_directory)
get_sf_grid(name="sf_grid",config = params$config,cache=cache,cholera_directory = params$cholera_directory)
```

### GAM Input
**Observations input for the GAM model:**
```{r gam input of cases,fig.height=5, fig.width=10, fig.cap=capFig("Observations input for the GAM model (cases)")}
  plot_gam_fit_input_cases (name="initial_values_data",cache=cache)
```

```{r gam input of rates,fig.height=5, fig.width=10, fig.cap=capFig("Observations output of the GAM model (rates)")}
  plot_gam_fit_input_rates (name="initial_values_data",cache=cache)
```

```{r include=FALSE, message=FALSE}
# Try looking at the plot
pdf("/home/kaiyuezou/mapping_pipeline/New_Dev/cholera-mapping-pipeline/Analysis/output/gam_input_case.pdf")
gridExtra::grid.arrange(comparison_cache$GAM_input_figure$fig_cases1, comparison_cache$GAM_input_figure$fig_cases1, ncol = 2) #this step plots 
dev.off()

```

### GAM Output
**Observations output of the GAM model:**
```{r gam output of cases,fig.height=5, fig.width=10, fig.cap=capFig("Observations input for the GAM model (cases)")}
  plot_gam_fit_output_cases(name="gam_output_df",cache=cache)
```

```{r gam output of rates,fig.height=5, fig.width=10, fig.cap=capFig("Observations input for the GAM model (rates)")}
  plot_gam_fit_output_rates(name="gam_output_df",cache=cache)
```

**Table 1. Data used within the model:**
```{r echo = FALSE}
  plot_ObservationSummary_table(config=params$config, cache=cache, cholera_directory=params$cholera_directory)
```

**Table 2. Data dropped from the model:**
```{r echo=FALSE}
  plot_DroppedData_table (config=params$config, cache=cache, cholera_directory=params$cholera_directory)
```

```{r disjoint set sf cases, message=FALSE}
aggregate_observed_polygon_cases_disjoint_aggregated(name="observed_polygon_cases_disjoint_aggregated",config=params$config,cholera_directory=params$cholera_directory,cache=cache)
disjoint_set_sf_cases <- cache[["observed_polygon_cases_disjoint_aggregated"]]
```

**Cases by time:**
```{r rawobsmap, fig.height=5, fig.width=10, fig.cap=capFig("Observed case counts by time adjusted for time fraction cases/year")}
plot_observed_cases_polygon_raw(config=params$config,cache=cache,cholera_directory=params$cholera_directory) 
```

**Area adjusted cases by time:**
```{r areadjustedmap, fig.height=5, fig.width=10, fig.cap=capFig("Area adjusted average observed case counts by time adjusted for time fraction cases/(year * kilometer)")}
  plot_area_adjusted_observed_cases(config=params$config,cache=cache,cholera_directory=params$cholera_directory)
```

**Cases unique location periods:**
```{r obsmap, fig.height=5, fig.width=10, fig.cap=capFig("Number of observations by unique observed location periods.")}
plot_raw_observations(config=params$config,cache=cache,cholera_directory=params$cholera_directory)
```
### Covariates

#### Population
**Population raster by time slices:**
```{r pop, fig.height=5, fig.width=10, fig.cap=capFig("Population density")}
plot_time_varying_pop_raster(cache=cache,config=params$config,cholera_directory=params$cholera_directory)
```

**Table 3. Population by admin level:**
```{r echo = FALSE}
plot_pop_by_admin(cache=cache,cholera_directory=params$cholera_directory,config=params$config)
```

#### Other covariates
**Covariate rasters:**
```{r covars, fig.height=5, fig.width=10, fig.cap=capFig("Covariate rasters")}
plot_raster_covariates(cache=cache,cholera_directory=params$cholera_directory,config=params$config)
```

### Output maps
```{r analysisyears}
obs_years <- min(lubridate::year(cache[["stan_input"]]$sf_cases_resized$TL)):max(lubridate::year(cache[["stan_input"]]$sf_cases_resized$TR))
rstan::get_elapsed_time(cache[["model.rand"]])
```


```{r importcase}
cache[["case_raster"]] <-get_case_raster(cache=cache,config=config,cholera_directory=cholera_directory)
cache[["disaggregated_rate_sf"]]<-get_disaggregated_rates_sf(
                                                cache=cache,config=params$config,
                                                cholera_directory=params$cholera_directory)

cache[["disaggregated_case_sf"]]<-get_disaggregated_cases_sf (
                                                cache=cache,config=params$config,
                                                cholera_directory=params$cholera_directory)

config_file<-yaml::read_yaml(paste0(params$cholera_directory,params$config))
analysis_years <- lubridate::year(config_file$start_time):lubridate::year(config_file$end_time)
obs_years <- min(lubridate::year(cache[["sf_cases_resized"]]$TL)):max(lubridate::year(cache[["sf_cases_resized"]]$TR))

  if(all(analysis_years %in% obs_years)){
    message("All analysis years are represented by OCs.")
  } else{
    drop_year_ix <- which(!analysis_years %in% obs_years)
    message(paste(paste(analysis_years[drop_year_ix], collapse = ", "), "are not represented in OCs"))
    if(params$drop_nodata_years){
      message(paste("Dropping", paste(analysis_years[drop_year_ix], collapse = ", "), "from case_raster"))
      case_raster <- dplyr::filter(case_raster, !(t %in% drop_year_ix))
    }
  }
```

#### Modeled cases
**Modeled cases raster by time slices:**
```{r caserast, fig.cap=capFig("Modeled cases"), fig.height=5, fig.width=10}
plot_disaggregated_modeled_cases_time_varying(disaggregated_case_sf=cache[["disaggregated_case_sf"]])
```

**Table 4. Modeled cases by admin level and time:**
```{r echo = FALSE}
plot_cases_by_admin(cache=cache,config=params$config,cholera_directory=params$cholera_directory)
```

#### Modeled incidence rates
**Modeled incidence rates raster by time slices:**
```{r raterast, fig.cap=capFig("Modeled rates by time"), fig.height=5, fig.width=10}
plot_modeled_rates_time_varying(disaggregated_rate_sf=cache[["disaggregated_rate_sf"]], render = T)
```

**Table 5. Modeled population-weighted incidence rates by admin level and time:**
```{r echo = FALSE}
plot_incidence_by_admin(cache=cache,config=params$config,cholera_directory=params$cholera_directory)
```

### Validation
```{r import_modelfidel}
cache[["data_fidelity"]]<-get_data_fidelity(cache=cache,cholera_directory=params$cholera_directory,config=params$config)
```

**Actual observations versus modeled cases scatter plot:**
```{r datafidelity1, fig.cap=capFig("Observations vs. modeled cases"), fig.height=10, fig.width=10}
plot_model_fidelity_tfrac_adjusted(data_fidelity = cache[["data_fidelity"]],case_raster = cache[["case_raster"]],render = T)
```

```{r datafidelity3, fig.cap=capFig("Observations vs. modeled cases (tfrac adjusted)"), fig.height=15, fig.width=10}
plot_model_fidelity_tfrac_adjusted_by_year(data_fidelity = cache[["data_fidelity"]],case_raster = cache[["case_raster"]],render = T)
```

```{r datafidelity4, fig.cap=capFig("Observations vs. modeled cases"), fig.height=8, fig.width=10}
plot_model_fidelity_tfrac_unadjusted(data_fidelity = cache[["data_fidelity"]],case_raster = cache[["case_raster"]],render = T)
```

**Stan trace plots:**
```{r traceplots, fig.height=8, fig.width=10, fig.cap=capFig("Stan trace plots")}
plot_chain_convergence(cache=cache,name="model.rand")
```

**Parameter posteriors:**
```{r params, fig.height=6, fig.width=6, fig.cap=capFig("Parameter posteriors")}
plot_MCMCpars(name="model.rand",cache=cache)
```

**Gelman-Rubin Rhat:**
```{r rhat, fig.height=5, fig.width=10, fig.cap=capFig("Gelman-Rubin Rhat")}
plot_Rhat(name="model.rand",cache)
```

**Table 6. Observed and estimates of WHO annual cholera reports:**
```{r WHO output, fig.height=510, fig.width=10, fig.cap = "comparison with WHO Output"}
plot_WHOcomparison_table(config=params$config, cache=cache, cholera_directory=params$cholera_directory)
```

**Table 7. Estimated cases by chain and time slice:**
```{r cases_chain_table, fig.width=10, fig.cap = "Sum of grid cases by chain and year"}
plot_modeled_cases_by_chain_time (config=params$config, cache=cache, cholera_directory=params$cholera_directory)
```

```{r full country modeled cases, fig.cap = "comparison with WHO Output"}
  total_cases <- cache[["modeled_cases"]] %>% apply(3, mean) %>% sum()
  print(paste("There are",format(round(total_cases), big.mark=","),"total cases"))
```