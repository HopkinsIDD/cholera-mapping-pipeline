#' @name run_test_case
#' @title run_test_case
#' @description Help run different kinds of test cases 
#' @param type which types of test cases you want to run: full_grid, coarse_grid,overlapping_grid_like,single_oc, multiple_ocs, positive_observations,partial_grid_coverage,partial_polygon_coverage,full_polygon_coverage,multiple_tested_observations
#' @param randomize 
#' @param ncols number of grids per column
#' @param nrows number of grids per row
#' @param grid_proportion_observed proportion of gridcells that are observed.
#' @param polygon_proportion_observed proportion of polygons that are observed.
#' @return country data report (html)
run_test_case = function(
  type=c("coarse_grid","overlapping_grid_like","single_oc","multiple_oc","positive_observation","partial_grid_coverage","partial_polygon_coverage","full_grid_coverage","full_polygon_coverage","multiple_tested_observations"),
  randomize=NULL,
  ncols=NULL,
  nrows=NULL,
  grid_proportion_observed=NULL,
  polygon_proportion_observed=NULL
  ){
  library(sf)
  library(tidyverse)

  ## ------------------------------------------------------------------------------------------------------------------------
  ## based on the type of test case, update the arguments of functions
  if(missing(type)){
    stop("A specific type of test case must be provided.")
  }
  
  if(type%in%"coarse_grid"){
    ncols=5
    nrows=5
    print("Here, we're testing the scenario with coarse grids. Only 5*5 gridcells are applied here.")
  }else{
    ncols=10
    nrows=10
  }
  
  if(type%in%"overlapping_grid_like"){
    randomize=FALSE
    print("Here, we're testing hte scenario with overlapping grid. Generating polygons uniformly for multiple layers.")
  }else{
    randomize=TRUE
  }
  
  if(type%in%"partial_grid_coverage"){
    grid_proportion_observed=0.4
    print("Here, we're testing the scenario with only a proportion of grids are observed. Only 40% of the gridcells are observed.")
  }else if(type%in%"full_grid_coverage"){
    grid_proportion_observed=1
  }else{
    grid_proportion_observed=0.9
  }
  
  if(type%in%"partial_polygon_coverage"){
    polygon_proportion_observed=0.4
    print("Here, we're testing the scenario with only a proportion of polygons are observed. Only 40% of the ploygons are observed for each draw.")
  }else if(type%in%"full_polygon_coverage"){
    polygon_proportion_observed=1
  }else{
    polygon_proportion_observed=0.9
  }
  
  ## ------------------------------------------------------------------------------------------------------------------------
  ## load test dataset

  #dbuser <- Sys.getenv("USER", "app")
  #dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")
  
  #conn_pg <- taxdat::connect_to_db(dbuser, dbname)
  #DBI::dbClearResult(DBI::dbSendQuery(conn = conn_pg, "SET client_min_messages TO WARNING;"))
  
  query_time_left <- lubridate::ymd("2000-01-01")
  query_time_right <- lubridate::ymd("2000-12-31")
  ## Pull data frames needed to create testing database from the api This doesn't
  ## pull covariates, but does pull everything else tryCatch({ all_dfs <-
  ## taxdat::create_testing_dfs_from_api(username =
  ## Sys.getenv('CHOLERA_API_USERNAME'), api_key = Sys.getenv('CHOLERA_API_KEY'),
  ## locations = 'AFR::KEN', time_left = query_time_left, time_right =
  ## query_time_right, uids = NULL, website =
  ## 'https://api.cholera-taxonomy.middle-distance.com/') }, error = function(e) {
  ## })
  load(rprojroot::find_root_file(criterion = ".choldir", "Analysis", "all_dfs_object.rdata"))
  
  if(type%in%"singe_oc"){
    all_dfs$observations_df=all_dfs$observations_df%>%mutate(attributes.source_documents=as.character(attributes.source_documents))
    observations_df=data.frame(OC=all_dfs$observations_df$relationships.observation_collection.data.id[1],
                               number_observations=nrow(all_dfs$observations_df[all_dfs$observations_df$relationships.observation_collection.data.id==all_dfs$observations_df$relationships.observation_collection.data.id[1],]))
    selected_OC=observations_df$number_observations[1]
    selected_geom=all_dfs$shapes_df$geom[1:selected_OC]
    all_dfs=list(location_df=all_dfs$location_df[1:selected_OC,],
                 location_period_df=all_dfs$location_period_df[1:selected_OC,],
                 shapes_df=all_dfs$shapes_df[1:selected_OC,],
                 observations_df=all_dfs$observations_df[1:selected_OC,])
    print(paste("Here, we're testing the scenario with only one observation collection file. Test dataset only include OC",selected_OC))
  }else if(type%in%"positive_observation"){
    #select nonZero observations
    nonZero_rowname=rownames(all_dfs$observations_df[which(all_dfs$observations_df$attributes.fields.suspected_cases>0&is.na(all_dfs$observations_df$attributes.fields.suspected_cases)==F&is.na(all_dfs$observations_df$attributes.location_period_id)==F),])
    all_dfs=list(location_df=all_dfs$location_df[nonZero_rowname,],
                 location_period_df=all_dfs$location_period_df[nonZero_rowname,],
                 shapes_df=all_dfs$shapes_df[nonZero_rowname,],
                 observations_df=all_dfs$observations_df[nonZero_rowname,])
    print("Here, we're testing the scenario with only positive. Test dataset only include positive observations.")
  }

  ## ------------------------------------------------------------------------------------------------------------------------
  ## Change polygons
  test_extent <- sf::st_bbox(all_dfs$shapes_df)
  test_raster <- create_test_raster(nrows = nrows, ncols = ncols, nlayers = 2, test_extent)
  # Create 3 layers of testing polygons starting with a single country, and
  # splitting each polygon into 4 sub-polygons
  test_polygons <- sf::st_make_valid(create_test_layered_polygons(test_raster = test_raster, 
                                                                  base_number = 1, n_layers = 2, factor = nrows * ncols, snap = FALSE, randomize = randomize))
  
  all_dfs$shapes_df <- test_polygons %>%
    dplyr::mutate(qualified_name = location, start_date = min(all_dfs$shapes_df$start_date), 
                  end_date = max(all_dfs$shapes_df$end_date))
  names(all_dfs$shapes_df)[names(all_dfs$shapes_df) == "geometry"] <- "geom"
  sf::st_geometry(all_dfs$shapes_df) <- "geom"
  
  all_dfs$location_period_df <- all_dfs$shapes_df %>%
    sf::st_drop_geometry()
  all_dfs$location_df <- all_dfs$shapes_df %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(qualified_name) %>%
    dplyr::summarize()
  
  ## ------------------------------------------------------------------------------------------------------------------------
  ## Change covariates
  test_extent <- sf::st_bbox(all_dfs$shapes_df)
  test_raster <- create_test_raster(nrows = nrows, ncols = ncols, nlayers = 2, test_extent)
  test_covariates <- create_multiple_test_covariates(test_raster = test_raster, ncovariates = 2, 
                                                     nonspatial = c(FALSE, FALSE), 
                                                     nontemporal = c(FALSE, FALSE), 
                                                     spatially_smooth = c(TRUE,FALSE), 
                                                     temporally_smooth = c(FALSE, FALSE), 
                                                     polygonal = c(TRUE, TRUE), 
                                                     radiating = c(FALSE,FALSE))
  min_time_left <- query_time_left
  max_time_right <- query_time_right
  covariate_raster_funs <- taxdat:::convert_simulated_covariates_to_test_covariate_funs(test_covariates, min_time_left, max_time_right)
  
  ## ------------------------------------------------------------------------------------------------------------------------
  ## Change observations
  raster_df <- taxdat::convert_test_covariate_funs_to_simulation_covariates(covariate_raster_funs)
  test_polygons <- dplyr::mutate(all_dfs$shapes_df, location = qualified_name, geometry = geom)
  sf::st_crs(test_polygons)<-sf::st_crs(raster_df[[1]])
  
  test_underlying_distribution <- create_underlying_distribution(covariates = raster_df)
  test_observations <- observe_polygons(test_polygons = test_polygons, test_covariates = raster_df$covar, 
                                        underlying_distribution = test_underlying_distribution, noise = FALSE, number_draws = 1, 
                                        grid_proportion_observed = 1, polygon_proportion_observed = 1, min_time_left = query_time_left, 
                                        max_time_right = query_time_right)
  
  ## ------------------------------------------------------------------------------------------------------------------------
  ## test two observations from different underlying distributions
  if(type%in%"two_tested_observations"){
    test_covariates2 <- create_multiple_test_covariates(test_raster = test_raster, ncovariates = 2, 
                                                        nonspatial = c(TRUE, TRUE), 
                                                        nontemporal = c(TRUE,TRUE), 
                                                        spatially_smooth = c(FALSE,TRUE), 
                                                        temporally_smooth = c(TRUE, TRUE), 
                                                        polygonal = c(FALSE, FALSE), 
                                                        radiating = c(FALSE,FALSE))
    covariate_raster_funs2 <- taxdat:::convert_simulated_covariates_to_test_covariate_funs(test_covariates2, min_time_left, max_time_right)
    
    raster_df2 <- taxdat::convert_test_covariate_funs_to_simulation_covariates(covariate_raster_funs2)
    sf::st_crs(raster_df2)<-sf::st_crs(raster_df[[1]])
  
    test_underlying_distribution2 <- create_underlying_distribution(covariates = raster_df2)
    test_observations2 <- observe_polygons(test_polygons = test_polygons, test_covariates = raster_df2$covar, 
                                           underlying_distribution = test_underlying_distribution2, noise = FALSE, number_draws = 100, 
                                           grid_proportion_observed = 1, polygon_proportion_observed = 1, min_time_left = query_time_left, 
                                           max_time_right = query_time_right)
    test_observations=dplyr::bind_rows(test_observations,test_observations2)
    print("Two tested observations from two different underlying distributions are applied here (different test covariates are applied to create the underlying distributions).")
  }
  
  all_dfs$observations_df <- test_observations %>%
    dplyr::mutate(observation_collection_id = draw, time_left = time_left, time_right = time_right, 
                  qualified_name = location, primary = TRUE, phantom = FALSE, suspected_cases = cases, 
                  deaths = NA, confirmed_cases = NA)
  
  ## ------------------------------------------------------------------------------------------------------------------------
  ## Create Database
  setup_testing_database(conn_pg, drop = TRUE)
  taxdat::setup_testing_database_from_dataframes(conn_pg, all_dfs, covariate_raster_funs)
  
  ## NOTE: Change me if you want to run the report locally config_filename <-
  ## paste(tempfile(), 'yml', sep = '.')
  config_filename <- paste0("/home/app/cmp/Analysis/R/",paste0(type,collapse = "_"),"_test_config.yml")
  
  ## Put your config stuff in here
  config <- list(general = list(location_name = all_dfs$location_df$qualified_name[[1]], 
                                start_date = as.character(min_time_left), end_date = as.character(max_time_right), 
                                width_in_km = 1, height_in_km = 1, time_scale = "month"), stan = list(directory = rprojroot::find_root_file(criterion = ".choldir", 
                                                                                                                                            "Analysis", "Stan"), ncores = 1, model = "dagar_seasonal.stan", niter = 1000, 
                                                                                                      recompile = TRUE), name = "test_???", taxonomy = "taxonomy-working/working-entry1", 
                 smoothing_period = 1, case_definition = "suspected", covariate_choices = raster_df$name, 
                 data_source = "sql", file_names = list(stan_output = rprojroot::find_root_file(criterion = ".choldir", 
                                                                                                "Analysis", "output", "test.stan_output.rdata"), stan_input = rprojroot::find_root_file(criterion = ".choldir", 
                                                                                                                                                                                        "Analysis", "output", "test.stan_input.rdata")))
  yaml::write_yaml(x = config, file = config_filename)
  
  Sys.setenv(CHOLERA_CONFIG = config_filename)
  source(rprojroot::find_root_file(criterion = ".choldir", "Analysis", "R", "execute_pipeline.R"))
  rmarkdown::render(rprojroot::find_root_file(criterion = ".choldir", "Analysis", "output", 
                                              "country_data_report.Rmd"), params = list(config_filename = config_filename, 
                                                                                        cholera_directory = "~/cmp/", drop_nodata_years = TRUE))
}
run_test_case(type="coase_grid")