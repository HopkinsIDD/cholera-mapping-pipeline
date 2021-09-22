#!/usr/bin/Rscript
# This scripts purpose is to collect all data required to model cholera
# incidence, and store it in a form usable by stan.  This script mainly uses
# functions from the package taxdat, stored in trunk/packages/taxdat. Libraries
# Control Variables: Preamble
# ------------------------------------------------------------------------------------------------------------



### Set Error Handling
interactive_run <- Sys.getenv("INTERACTIVE_RUN", "FALSE")
if (interactive_run == "TRUE") {
    options(warn = 1, error = recover)
} else if (interactive_run == "FALSE") {
    options(warn = 1, error = function(...) {
        quit(..., status = 2)
    })
}

### Libraries
if (Sys.getenv("CHOLERA_CHECK_LIBRARIES", TRUE)) {
    base_search <- search()

    ## This is the list of packages required by this script: libxt-dev
    package_list <- c("optparse", "DBI", "RPostgres", "sf", "magrittr", "dplyr", 
        "rstan", "xfun", "kableExtra", "Cairo", "MCMCvis")

    for (package in package_list) {
        if (!require(package = package, character.only = T)) {
            chooseCRANMirror(ind = 1)
            install.packages(pkgs = package)
            library(package = package, character.only = T)
        }
    }
}

# Set working directory to access taxdat properly

if (Sys.getenv("CHOLERA_CHECK_LIBRARIES", TRUE)) {
    if (!require(taxdat)) {
        previous_wd <- getwd()
        try({
            setwd(utils::getSrcDirectory())
        }, silent = TRUE)
        try({
            setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
        }, silent = TRUE)

        install.packages("packages/taxdat", type = "source", repos = NULL)
        library(taxdat)

        try({
            setwd(utils::getSrcDirectory())
        }, silent = TRUE)
        try({
            setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
        }, silent = TRUE)
        setwd(previous_wd)
    }

    for (x in rev(sort(which(!(search() %in% base_search))))) {
        detach(pos = x, force = T)
    }
}

library(magrittr)


## Functions to move to taxdat later
## --------------------------------------------------------------------------------------------------------------


reindex <- function(df, index_column, new_index_column = index_column) {
    index_column <- rlang::sym(index_column)
    new_index_column <- rlang::sym(new_index_column)
    df %>%
        dplyr::mutate(`:=`(!!new_index_column, dplyr::group_indices(., !!index_column))) %>%
        return
}

cast_to_int32 <- function(x) {
    if (!is.integer(x)) {
        rc <- as.integer(x)
        if (all(rc == x)) {
            return(rc)
        }
        stop(paste("Conversion failed", x[rc != x], "converted to", rc[rc != x]))
    }
    return(x)
}

plot_energy <- function(stan_model, par = "all") {
    energy <- sapply(rstan::get_sampler_params(stan_model), function(x) {
        x[, "energy__"]
    })
    leapfrog_iterations <- rstan::get_num_leapfrog_per_iteration(stan_model)

    nchain <- stan_model@sim$chains
    kept_per_chain <- sapply(stan_model@sim$permutation, length)
    if (!length(unique(kept_per_chain)) == 1) {
        stop("This function assumes the same number of iterations are saved for each chain")
    }
    kept_per_chain <- unique(kept_per_chain)

    if (!(isTRUE(par == "all") || all(par %in% c(stan_model@sim$pars_oi, stan_model@sim$fnames_oi)))) {
        stop("Not all parameters are approporiate")
    }
    if (isTRUE(par == "all")) {
        par <- stan_model@sim$fnames_oi
    }
    short_par <- par[par %in% stan_model@sim$pars_oi]
    if (length(short_par) > 0) {
        longform_pars <- stan_model@sim$pars_oi[!(stan_model@sim$pars_oi %in% stan_model@sim$fnames_oi)]
        short_par <- short_par[short_par %in% longform_pars]
        par <- par[!(par %in% short_par)]
        longform_pars <- stan_model@sim$fnames_oi[!(stan_model@sim$fnames_oi %in% 
            stan_model@sim$pars_oi)]
        short_names <- gsub("\\[.*\\]", "", longform_pars)
        short_par <- unlist(lapply(short_par, function(x) {
            return(longform_pars[x == short_names])
        }))
        par <- c(par, short_par)
    }

    rc <- array(NA, c(kept_per_chain, nchain, length(par) + 2))
    dimnames(rc) <- list(NULL, paste("chain", seq_len(nchain)), c(par, "energy__", 
        "leapfrog_iterations__"))


    indices_to_pull <- lapply(stan_model@sim$permutation, function(x) {
        x + stan_model@sim$warmup
    })

    counter <- 0
    for (i in seq_len(nchain)) {
        rc[, paste("chain", i), "energy__"] <- energy[indices_to_pull[[i]], i]
        rc[, paste("chain", i), "leapfrog_iterations__"] <- leapfrog_iterations[counter + 
            seq_len(kept_per_chain)]
        counter <- counter + kept_per_chain
    }
    params <- rstan::extract(stan_model, pars = par)
    for (param in par) {
        counter <- 0
        for (i in seq_len(nchain)) {
            rc[, paste("chain", i), param] <- params[[param]][counter + seq_len(kept_per_chain)]
            counter <- counter + kept_per_chain
        }
    }

    return(pairs(apply(rc, 3, c)))
}

## Inputs
## --------------------------------------------------------------------------------------------------------------


### Command Line Options
option_list <- list(optparse::make_option(c("-c", "--config"), action = "store", 
    default = Sys.getenv("CHOLERA_CONFIG", "config.yml"), type = "character", help = "Model run configuration file"), 
    optparse::make_option(c("-d", "--cholera_covariates_directory"), action = "store", 
        default = Sys.getenv("CHOLERA_COVARIATES_DIRECTORY", "Layers"), type = "character", 
        help = "Covariate directory"), optparse::make_option(c("-d", "--cholera_pipeline_directory"), 
        action = "store", default = Sys.getenv("CHOLERA_PIPELINE_DIRECTORY", "."), 
        type = "character", help = "Pipeline directory"), optparse::make_option(c("-d", 
        "--cholera_output_directory"), action = "store", default = Sys.getenv("CHOLERA_OUTPUT_DIRECTORY", 
        "Analysis/data"), type = "character", help = "Output directory"), optparse::make_option(c("-p", 
        "--postgres_database_name"), action = "store", default = Sys.getenv("CHOLERA_POSTGRES_DATABASE", 
        "cholera_covariates"), type = "character", help = "Postgres database name"), 
    optparse::make_option(c("--postgres_database_user"), action = "store", default = Sys.getenv("USER", 
        "app"), type = "character", help = "Postgres database user"))

opt <- optparse::parse_args((optparse::OptionParser(option_list = option_list)))


### Config Options
config <- yaml::read_yaml(opt$config)
config <- taxdat::complete_config(config)
if (!taxdat::check_config(config)) {
    warning("Could not validate the config")
}


##### Config Defaults


### Setup postgres
conn_pg <- taxdat::connect_to_db(dbname = opt$postgres_database_name, dbuser = opt$postgres_database_user)

cases_column <- "suspected_cases"
## Observations
observation_data <- DBI::dbGetQuery(conn = conn_pg, statement = glue::glue_sql(.con = conn_pg, 
    "SELECT *
     FROM pull_observation_data(
       {config[[\"general\"]][[\"location_name\"]]},
       {config[[\"general\"]][[\"start_date\"]]},
       {config[[\"general\"]][[\"end_date\"]]}
    )")) %>%
    dplyr::mutate(shape = sf::st_as_sfc(shape)) %>%
    dplyr::filter(!is.na(!!rlang::sym(cases_column))) %>%
    sf::st_as_sf()

## Covariates
covar_cube <- DBI::dbGetQuery(conn = conn_pg, glue::glue_sql(.con = conn_pg, "SELECT *
     FROM pull_covar_cube(
       {config[[\"general\"]][[\"location_name\"]]},
       {config[[\"general\"]][[\"start_date\"]]},
       {config[[\"general\"]][[\"end_date\"]]},
       {config[[\"general\"]][[\"width_in_km\"]]},
       {config[[\"general\"]][[\"height_in_km\"]]},
       {config[[\"general\"]][[\"time_scale\"]]}
    )")) %>%
    tidyr::pivot_wider(names_from = covariate_name, values_from = value)



grid_adjacency <- DBI::dbGetQuery(conn = conn_pg, statement = glue::glue_sql(.con = conn_pg, 
    "SELECT * FROM pull_grid_adjacency(
      {config[[\"general\"]][[\"location_name\"]]},
       {config[[\"general\"]][[\"width_in_km\"]]},
       {config[[\"general\"]][[\"height_in_km\"]]}
    )"))

observation_location_period_mapping <- DBI::dbGetQuery(conn = conn_pg, statement = glue::glue_sql(.con = conn_pg, 
    "SELECT * FROM pull_observation_location_period_map(
      {config[[\"general\"]][[\"location_name\"]]},
       {config[[\"general\"]][[\"start_date\"]]},
       {config[[\"general\"]][[\"end_date\"]]}
    )"))

unique_location_period_ids <- sort(unique(observation_data[["location_period_id"]]))
unique_location_ids <- sort(unique(observation_data[["location_id"]]))

location_period_grid_mapping <- DBI::dbGetQuery(conn = conn_pg, statement = glue::glue_sql(.con = conn_pg, 
    "SELECT * FROM pull_location_period_grid_map(
      {config[[\"general\"]][[\"location_name\"]]},
       {config[[\"general\"]][[\"width_in_km\"]]},
       {config[[\"general\"]][[\"height_in_km\"]]}
    )"))
# location_period_grid_mapping <- location_period_grid_mapping %>%
# dplyr::filter(location_id %in% unique_location_ids)

## Compute Missingness and remove partial observations

covariate_covered_grid_ids <- covar_cube %>%
    dplyr::filter(!is.na(id), !is.na(t), population > 0) %>%
    dplyr::group_by(id) %>%
    dplyr::summarize(count = length(t)) %>%
    dplyr::filter(count == max(count)) %>%
    .$id

non_na_location_period_grid_mapping <- dplyr::filter(location_period_grid_mapping, 
    !is.na(location_period_id), !is.na(spatial_grid_id))

shapefile_covered_grid_ids <- unique(non_na_location_period_grid_mapping$spatial_grid_id)
shapefile_covered_grid_ids <- shapefile_covered_grid_ids[!is.na(shapefile_covered_grid_ids)]
fully_covered_grid_ids <- covariate_covered_grid_ids[covariate_covered_grid_ids %in% 
    shapefile_covered_grid_ids]

grid_covered_location_period_grid_mapping <- non_na_location_period_grid_mapping %>%
    dplyr::filter(spatial_grid_id %in% fully_covered_grid_ids)

grid_covered_location_period_ids <- unique(grid_covered_location_period_grid_mapping$location_period_id)
fully_covered_location_period_ids <- grid_covered_location_period_ids[grid_covered_location_period_ids %in% 
    observation_data$location_period_id]

location_period_covered_observation_ids <- observation_location_period_mapping %>%
    dplyr::filter(!is.na(observation_id), !is.na(location_period_id), location_period_id %in% 
        grid_covered_location_period_ids) %>%
    .$observation_id
fully_covered_observation_ids <- location_period_covered_observation_ids

## Actually doing filtering here

covar_cube <- covar_cube %>%
    dplyr::filter(id %in% fully_covered_grid_ids) %>%
    reindex("id", "updated_id")

grid_changer <- setNames(sort(unique(covar_cube$updated_id)), sort(unique(covar_cube$id)))
if (!all(grid_changer[as.character(covar_cube$id)] == covar_cube$updated_id)) {
    stop("There is a problem with computing the updated grid indices")
}

grid_adjacency <- grid_adjacency %>%
    dplyr::mutate(updated_id_1 = grid_changer[as.character(id_1)], updated_id_2 = grid_changer[as.character(id_2)]) %>%
    dplyr::filter(!is.na(updated_id_1), !is.na(updated_id_2))

location_period_changer <- setNames(seq_len(length(fully_covered_location_period_ids)), 
    fully_covered_location_period_ids)

# location_period_grid_mapping <- location_period_grid_mapping %>%
# dplyr::mutate(updated_spatial_grid_id =
# grid_changer[as.character(spatial_grid_id)])

nneighbors <- grid_adjacency %>%
    dplyr::group_by(id_1) %>%
    dplyr::summarize(nneighbors = length(unique(id_2))) %>%
    dplyr::right_join(tibble::tibble(id = unique(covar_cube[["id"]])), by = c(id_1 = "id")) %>%
    dplyr::arrange(id_1) %>%
    dplyr::select(nneighbors)
nneighbors[is.na(nneighbors)] <- 0

observation_data <- observation_data %>%
    dplyr::filter(observation_id %in% fully_covered_observation_ids) %>%
    reindex("observation_id", "updated_observation_id") %>%
    dplyr::mutate(updated_location_period_id = location_period_changer[as.character(location_period_id)])

observation_changer <- setNames(sort(unique(observation_data$updated_observation_id)), 
    sort(unique(observation_data$observation_id)))
if (!all(observation_changer[as.character(observation_data$observation_id)] == observation_data$updated_observation_id)) {
    stop("There is a problem with computing the updated observation indices")
}

observation_location_period_mapping <- observation_location_period_mapping %>%
    dplyr::mutate(updated_observation_id = observation_changer[as.character(observation_id)], 
        updated_location_period_id = location_period_changer[as.character(location_period_id)], 
        ) %>%
    dplyr::filter(!is.na(updated_observation_id), !is.na(updated_location_period_id))

location_period_grid_mapping <- location_period_grid_mapping %>%
    dplyr::mutate(updated_spatial_grid_id = grid_changer[as.character(spatial_grid_id)], 
        updated_location_period_id = location_period_changer[as.character(location_period_id)]) %>%
    dplyr::filter(!is.na(updated_spatial_grid_id), !is.na(updated_location_period_id))

### Construct some additional parameters based on the above Define relevent
### directories Name the output file

# Stan modeling section
print("*** STARTING STAN MODEL ***")

library(rstan)

# Initial Values ----------------------------------------------------------

## INITIAL VALUES

if (config$initial_values$warmup) {
    covariate_names <- colnames(covar_cube[, -c(1:5, ncol(covar_cube))])
    initial_values_df <- observation_data %>%
        inner_join(observation_location_period_mapping) %>%
        inner_join(location_period_grid_mapping, by = c(location_period_id = "location_period_id")) %>%
        inner_join(covar_cube, by = c(spatial_grid_id = "id", rid = "rid", x = "x", 
            y = "y")) %>%
        group_by(observation_id) %>%
        group_modify(function(.x, .y) {
            .x[[paste("raw", cases_column, sep = "_")]] <- .x[[cases_column]]
            .x[[cases_column]] <- .x[[cases_column]] * .x$population/sum(.x$population)
            return(.x)
        }) %>%
        dplyr::mutate(log_y = log(y), gam_offset = log_y)

    gam_formula <- taxdat::get_gam_formula(cases_column_name = cases_column, covariate_names = covariate_names)

    gam_fit <- mgcv::gam(gam_formula, family = "gaussian", data = initial_values_df)
    gam_predict <- mgcv::predict.gam(gam_fit, covar_cube)
    covariate_effect <- as.vector(as.matrix(covar_cube[covariate_names]) %*% coef(gam_fit)[covariate_names])

    initial_betas <- coef(gam_fit)[covariate_names]
    initial_eta <- coef(gam_fit)["obs_year"]

    initial_values_list <- lapply(seq_len(config[["stan"]][["nchain"]]), function(chain) {
        return(list(betas = rnorm(length(coef(gam_fit)[covariate_names]), coef(gam_fit)[covariate_names]), 
            eta = rnorm(length(coef(gam_fit)["obs_year"]), coef(gam_fit)["obs_year"]), 
            w = rnorm(nrow(covar_cube), gam_predict - covariate_effect)))
    })

    covar_cube[["covariate_contribution"]] <- covariate_effect
    covar_cube[["spatial_smoothing_term"]] <- gam_predict - covariate_effect
    covar_cube[["gam_output"]] <- gam_predict
} else {
    initial_values_list <- "random"
    initial_values_df <- "no warmup performed"
    covar_cube[["covariate_contribution"]] <- NA
    covar_cube[["spatial_smoothing_term"]] <- NA
    covar_cube[["gam_output"]] <- NA
}

## END INITIAL VALUES

# Run model ---------------------------------------------------------------
# model.rand <- rstan::stan(file = stan_model_path, data =
# initial_values_data$stan_data, chains = nchain, iter = niter, pars = c('b',
# 't_rowsum', 'vec_var'), include = FALSE, control = list(max_treedepth = 15),
# init = initial_values_data$init.list)
stan_dir <- config[["stan"]][["directory"]]
stan_model_path <- taxdat::check_stan_model(stan_model_path = paste(stan_dir, config[["stan"]][["model"]], 
    sep = "/"), stan_dir = stan_dir)

options(mc.cores = config[["stan"]][["ncores"]])

stan_data <- list(N = nrow(covar_cube), N_edges = nrow(grid_adjacency), smooth_grid_N = length(unique(covar_cube$updated_id)), 
    node1 = as.integer(grid_adjacency$updated_id_1), node2 = as.integer(grid_adjacency$updated_id_2), 
    diag = nneighbors$nneighbors, pop = covar_cube$population, meanrate = 1, M = nrow(observation_data), 
    y = as.array(observation_data[[cases_column]]), L = length(unique(observation_location_period_mapping$updated_location_period_id)), 
    K1 = nrow(observation_location_period_mapping), K2 = nrow(location_period_grid_mapping), 
    map_obs_loctime_obs = as.array(cast_to_int32(observation_location_period_mapping$updated_observation_id)), 
    map_obs_loctime_loc = as.array(cast_to_int32(observation_location_period_mapping$updated_location_period_id)), 
    tfrac = as.array(rep(1, times = nrow(observation_location_period_mapping))), 
    map_loc_grid_loc = as.array(cast_to_int32(location_period_grid_mapping$updated_location_period_id)), 
    map_loc_grid_grid = as.array(cast_to_int32(location_period_grid_mapping$updated_spatial_grid_id)), 
    map_smooth_grid = covar_cube$updated_id, rho = 0.999, covar = as.matrix(covar_cube[, 
        covariate_names]), ncovar = length(covariate_names))

start_time <- Sys.time()
model.rand <- rstan::stan(file = stan_model_path, data = stan_data, chains = config[["stan"]][["nchain"]], 
    iter = config[["stan"]][["niter"]], pars = c("b", "t_rowsum", "vec_var"), include = FALSE, 
    control = list(max_treedepth = 15), refresh = 0, init = initial_values_list)
end_time <- Sys.time()

elapsed_time <- end_time - start_time

# Save output
save(model.rand, elapsed_time, file = config[["file_names"]][["stan_output"]])

stan_input <- list(stan_data = stan_data, covar_cube = covar_cube, observation_data = observation_data, 
    grid_adjacency = grid_adjacency, observation_location_period_mapping = observation_location_period_mapping, 
    location_period_grid_mapping = location_period_grid_mapping, initial_values_list = initial_values_list, 
    initial_values_df = initial_values_df)
save(stan_input, file = config[["file_names"]][["stan_input"]])
