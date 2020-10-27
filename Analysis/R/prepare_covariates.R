# Title: prepare covariates for stan model
# Description: prepare covariates for stan model using a reference raster

# Steps:
# 1. check if reference grid exists
# 2. create reference grid if not
# 3. aggregate all covariates to reference grid
# 5. return an [n x m x k] x l matrix (n:# of long, m:# of lat, k:# of timepoints, l:# of covariates)

prepare_covariates <- function(
  dbuser,
  cholera_directory,
  res_space,
  res_time,
  ingest,
  do_parallel,
  n_cores,
  ovrt_covar,
  ovrt_metadata_table = F,
  redo_metadata = F,
  covar,
  full_grid_name,
  aoi_name = "raw"
) {
  
  # Preamble ---------------------------------------------------------------------
  
  
  # print(str(list(
  #   dbuser =  dbuser,
  #   cholera_directory = cholera_directory,
  #   res_space = res_space,
  #   res_time = res_time,
  #   ingest = ingest,
  #   do_parallel = do_parallel,
  #   n_cores = n_cores,
  #   ovrt_covar = ovrt_covar,
  #   ovrt_metadata = ovrt_metadata,
  #   covar = covar,
  #   full_grid_name = full_grid_name
  # )))
  
  
  # Inputs ------------------------------------------------------------------
  
  # User-defined parameters
  # Spatial resolution
  res_x <- res_space    # longitude resolution in km
  res_y <- res_space    # latitude resolution in km
  
  # Temporal resolution 
  res_time_list <- taxdat::parse_time_res(res_time)
  
  # Other parameters
  km_to_deg <- 1/110.57    # how many degrees is a km at the equator
  
  # Other objects
  conn_pg <- taxdat::connect_to_db(dbuser)
  
  # Dictionary of aliases and aggregators for covariates
  covar_dict <- yaml::read_yaml(paste0(cholera_directory, "/Layers/covariate_dictionary.yml"))
  
  # Covariates to include in the model
  covar_abbr_list <- stringr::str_split(covar, ",")[[1]]
  covar_toingest_list <- covar_dict[unlist(lapply(covar_dict, function (x) x$abbr %in% covar_abbr_list))]
  
  # Check if all covariates were found
  for (abb in covar_abbr_list) {
    if (sum(unlist(lapply(covar_toingest_list, function(x) x$abbr == abb))) == 0)
      stop("Covariate ", abb, " not specified in dictionary")
  }
  
  # Ingest covariates -----------------------------------------------
  
  cat("**** INGESTING COVARIATES \n")
  
  covar_list <- c()
  
  # Define area of interest for covariate processing
  # This is mostly for testing purposes
  aois <- list(
    list(name = "raw", extent = NULL),
    list(name = "SSD", extent = raster::extent(23, 37, 3, 13)),   # SSD
    list(name = "KEN", extent = raster::extent(33, 42, -5.2, 5)),   # Kenya
    list(name = "SSA", extent = raster::extent(-18.8, 52.6, -35.4, 28))   # SSA
  )
  aoi_names <- purrr::map_chr(aois, "name")
  
  if (!(aoi_name %in% aoi_names)) {
    stop("Area of interest ", aoi_name, " not among pre-defined areas (", stringr::str_c(aoi_names, collapse = ","), ")")
  } else {
    aoi <- aois[[which(aoi_name == aoi_names)]]
  }
  
  if (ovrt_metadata_table) {
    # Recreate the metadata table
    DBI::dbSendStatement(conn_pg, "DROP TABLE IF EXISTS covariates.metadata;")
    DBI::dbSendStatement(conn_pg, 
                    "CREATE TABLE covariates.metadata(
                  covariate TEXT PRIMARY KEY,
                  src_res_x DOUBLE PRECISION,
                  src_res_y DOUBLE PRECISION,
                  src_res_time TEXT,
                  first_TL DATE,
                  last_TL DATE,
                  src_dir TEXT,
                  res_x DOUBLE PRECISION,
                  res_y DOUBLE PRECISION,
                  res_time text,
                  space_agg text,
                  time_agg text
                  );")
    
  }
  
  for (covarit in covar_toingest_list) {
    
    covar_alias <- taxdat::make_covar_alias(alias = covarit$alias,
                                  type = covarit$type,
                                  res_time = res_time,
                                  res_space = c(res_x, res_y))
    
    # Check whether table exists in the public or covariates schemas
    covar_in_db <- taxdat::db_exists_table_multi(conn_pg, c("public", "covariates"), covar_alias)
    covar_schema <- "covariates"
    
    if (!any(covar_in_db) & !ingest) 
      stop(glue::glue("Couldn't find {covarit$type} covariate '{covarit$alias}' at temporal resolution of: {res_time}, and spatial resolution of: {res_x}x{res_y}km. The covariate needs to be preprocessed and ingested by an authorized users."))
    
    if(!any(covar_in_db) | ovrt_covar) {
      taxdat::ingest_covariate(conn = conn_pg,
                      covar_name = covarit$name,
                      covar_dir = covarit$dir,
                      covar_alias = covar_alias,
                      covar_unit = covarit$unit,
                      covar_type =  covarit$type,
                      covar_res_time = covarit$res_time,
                      covar_schema = covar_schema,
                      aoi_extent = aoi$extent,
                      aoi_name = aoi$name,
                      res_time = res_time,
                      ref_grid = full_grid_name,
                      time_aggregator = covarit$time_aggregator,
                      res_x = res_x,
                      res_y = res_y,
                      space_aggregator = covarit$space_aggregator,
                      transform = covarit$transform,
                      path_to_trunk = cholera_directory,
                      write_to_db = !do_parallel,
                      do_parallel = do_parallel,
                      n_cpus = n_cores,
                      dbuser = dbuser)
      
    } else {
      covar_schema <- names(covar_in_db)[covar_in_db]
      cat("---- Found pre-computed ", covar_alias, " in schema '", covar_schema, "'\n", sep = "")
    }
    
    
    if (redo_metadata) {
      taxdat::write_metadata(conn = conn_pg,
                    covar_dir = covarit$dir,
                    covar_type = covarit$type,
                    covar_alias = covar_alias,
                    res_x = res_x, 
                    res_y = res_y,
                    res_time = res_time,
                    space_aggregator = covarit$space_aggregator,
                    time_aggregator = covarit$time_aggregator,
                    dbuser = dbuser)
    }
    
    covar_list <- c(covar_list, stringr::str_c(covar_schema, covar_alias, sep = "."))
  } 
  
  # Write covariate names to file
  covar_list_file <- paste0("covar_list_", Sys.getenv("USER"), ".txt")
  write(x = covar_list, file = covar_list_file, append = F)
  
  cat("Processed:", covar_list, "\n")
  cat("**** DONE COVARIATES ****\n")
  
  # close database
  DBI::dbDisconnect(conn_pg)
  
  return(covar_list)
}
