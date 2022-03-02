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
        "rstan", "xfun", "kableExtra", "MCMCvis")

    for (package in package_list) {
        if (!require(package = package, character.only = T)) {
            utils::chooseCRANmirror(ind = 1)
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
library(bit64)


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
    dplyr::mutate(shape = sf::st_as_sfc(shape), suspected_cases_L = as.numeric(NA),
        suspected_cases_R = as.numeric(NA), confirmed_cases_L = as.numeric(NA), confirmed_cases_R = as.numeric(NA),
        deaths_L = as.numeric(NA), deaths_R = as.numeric(NA)) %>%
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
    dplyr::filter(!is.na(value)) %>%
    tidyr::pivot_wider(names_from = covariate_name, values_from = value)



grid_adjacency <- DBI::dbGetQuery(conn = conn_pg, statement = glue::glue_sql(.con = conn_pg,
    "SELECT * FROM pull_grid_adjacency(
      {config[[\"general\"]][[\"location_name\"]]},
       {config[[\"general\"]][[\"width_in_km\"]]},
       {config[[\"general\"]][[\"height_in_km\"]]}
    )"))

observation_temporal_location_mapping <- DBI::dbGetQuery(conn = conn_pg, statement = glue::glue_sql(.con = conn_pg,
    "SELECT * FROM pull_observation_location_period_map(
      {config[[\"general\"]][[\"location_name\"]]},
       {config[[\"general\"]][[\"start_date\"]]},
       {config[[\"general\"]][[\"end_date\"]]},
       {config[[\"general\"]][[\"time_scale\"]]}
    )"))

unique_temporal_location_ids <- sort(unique(observation_data[["temporal_location_id"]]))
unique_location_ids <- sort(unique(observation_data[["location_id"]]))

temporal_location_grid_mapping <- DBI::dbGetQuery(conn = conn_pg, statement = glue::glue_sql(.con = conn_pg,
    "SELECT * FROM pull_location_period_grid_map(
      {config[[\"general\"]][[\"location_name\"]]},
       {config[[\"general\"]][[\"start_date\"]]},
       {config[[\"general\"]][[\"end_date\"]]},
       {config[[\"general\"]][[\"width_in_km\"]]},
       {config[[\"general\"]][[\"height_in_km\"]]},
       {config[[\"general\"]][[\"time_scale\"]]}
    )"))

# Intermediate operations like aggregation and overlap removal

observation_data.bak <- observation_data
observation_temporal_location_mapping.bak <- observation_temporal_location_mapping
if (config[["processing"]][["aggregate"]]) {
    temporally_linked_observations <- observation_data %>%
        dplyr::inner_join(observation_temporal_location_mapping)

    observation_data_aggregated <- taxdat::aggregate_case_data(temporally_linked_observations,
        taxdat::get_unique_columns_by_group(temporally_linked_observations, grouping_columns = c("observation_collection_id",
            "temporal_location_id"), skip_columns = c("observation_collection_id",
            "location_period_id", "shape", cases_column)), columns_to_sum_over = c("tfrac",
            cases_column))


    observation_data <- sf::st_as_sf(taxdat::project_to_groups(observation_data_aggregated,
        "observation_id", observation_data))
    observation_temporal_location_mapping <- taxdat::project_to_groups(observation_data_aggregated,
        c("observation_id", "temporal_location_id"), observation_temporal_location_mapping)

}


if (config[["processing"]][["remove_overlaps"]]) {
    observation_data_with_t <- observation_data %>%
        dplyr::inner_join(observation_temporal_location_mapping) %>%
        dplyr::group_by(!!!rlang::syms(taxdat::get_unique_columns_by_group(observation_data,
            "observation_id", skip_columns = c("shape")))) %>%
        dplyr::summarize(t = list(unique(t)))
    observation_data <- taxdat::remove_overlapping_observations(observation_data_with_t,
        unique_column_names = "t") %>%
        dplyr::ungroup() %>%
        dplyr::select(-t) %>%
        sf::st_as_sf()

}


if (config[["processing"]][["censor_incomplete_observations"]][["perform"]]) {
    temporally_linked_observations <- observation_data %>%
        dplyr::inner_join(observation_temporal_location_mapping)


    observation_data_censored <- taxdat::do_censoring(temporally_linked_observations,
        unique_column_names = taxdat::get_unique_columns_by_group(temporally_linked_observations,
            grouping_columns = c("observation_id"), skip_columns = c("observation_id",
                "shape", cases_column, paste0(cases_column, "_R"), paste0(cases_column,
                  "_L"))), colnames = cases_column, threshold = config[["processing"]][["censor_incomplete_observations"]][["threshold"]])


    observation_data <- sf::st_as_sf(taxdat::project_to_groups(observation_data_censored,
        "observation_id", observation_data))
    observation_temporal_location_mapping <- taxdat::project_to_groups(observation_data_censored,
        c("observation_id", "temporal_location_id"), observation_temporal_location_mapping)
} else {
    observation_data <- dplyr::filter(observation_data, !is.na(!!rlang::sym(cases_column)))
    observation_data[[paste0(cases_column, "_L")]] <- as.numeric(NA)
    observation_data[[paste0(cases_column, "_R")]] <- as.numeric(NA)
}

if (!all(is.na(observation_data[[paste0(cases_column, "_R")]]) | is.na(observation_data[[cases_column]])) &&
    !all(is.na(observation_data[[cases_column]]) | is.na(observation_data[[paste0(cases_column,
        "_L")]])) && !all(is.na(observation_data[[paste0(cases_column, "_R")]]) |
    is.na(observation_data[[paste0(cases_column, "_L")]])) | any(is.na(observation_data[[paste0(cases_column,
    "_R")]]) & is.na(observation_data[[cases_column]]) & is.na(observation_data[[paste0(cases_column,
    "_L")]]))) {
    stop(paste0("All observations should have exactly one observation from ", cases_column,
        "_R ", cases_column, " ", cases_column, "_L by this point in processing."))
}


covariate_covered_grid_ids <- covar_cube %>%
    dplyr::filter(!is.na(id), !is.na(t), population > 0) %>%
    dplyr::group_by(id) %>%
    dplyr::summarize(count = length(t)) %>%
    dplyr::filter(count == max(count)) %>%
    .$id

non_na_temporal_location_grid_mapping <- dplyr::filter(temporal_location_grid_mapping,
    !is.na(temporal_location_id), !is.na(spatial_grid_id))

shapefile_covered_grid_ids <- unique(non_na_temporal_location_grid_mapping$spatial_grid_id)
shapefile_covered_grid_ids <- shapefile_covered_grid_ids[!is.na(shapefile_covered_grid_ids)]
fully_covered_grid_ids <- covariate_covered_grid_ids[covariate_covered_grid_ids %in%
    shapefile_covered_grid_ids]

grid_covered_temporal_location_grid_mapping <- non_na_temporal_location_grid_mapping %>%
    dplyr::filter(spatial_grid_id %in% fully_covered_grid_ids)

grid_covered_temporal_location_ids <- unique(grid_covered_temporal_location_grid_mapping$temporal_location_id)

temporal_location_covered_observation_temporal_location_mapping <- observation_temporal_location_mapping %>%
    dplyr::filter(temporal_location_id %in% grid_covered_temporal_location_ids)
fully_covered_temporal_location_ids <- grid_covered_temporal_location_ids

fully_covered_observation_ids <- unique(temporal_location_covered_observation_temporal_location_mapping$observation_id)

## Actually doing filtering here

covar_cube <- covar_cube %>%
    dplyr::filter(id %in% fully_covered_grid_ids) %>%
    reindex("id", "updated_id") %>%
    dplyr::add_rownames() %>%
    dplyr::mutate(spacetime_grid_id = as.numeric(rowname)) %>%
    dplyr::select(-rowname)

spatial_grid_and_time_to_spacetime_grid_changer <- setNames(covar_cube$spacetime_grid_id,
    paste(covar_cube$updated_id, covar_cube$t, sep = "_"))

grid_changer <- setNames(sort(unique(covar_cube$updated_id)), sort(unique(covar_cube$id)))
if (!all(grid_changer[as.character(covar_cube$id)] == covar_cube$updated_id)) {
    stop("There is a problem with computing the updated grid indices")
}

grid_adjacency <- grid_adjacency %>%
    dplyr::mutate(updated_id_1 = grid_changer[as.character(id_1)], updated_id_2 = grid_changer[as.character(id_2)]) %>%
    dplyr::filter(!is.na(updated_id_1), !is.na(updated_id_2))

temporal_location_changer <- setNames(seq_len(length(fully_covered_temporal_location_ids)),
    fully_covered_temporal_location_ids)

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
    reindex("observation_id", "updated_observation_id")

observation_changer <- setNames(sort(unique(observation_data$updated_observation_id)),
    sort(unique(observation_data$observation_id)))
if (!all(observation_changer[as.character(observation_data$observation_id)] == observation_data$updated_observation_id)) {
    stop("There is a problem with computing the updated observation indices")
}

observation_temporal_location_mapping <- observation_temporal_location_mapping %>%
    dplyr::mutate(updated_observation_id = observation_changer[as.character(observation_id)],
        updated_temporal_location_id = temporal_location_changer[as.character(temporal_location_id)],
        ) %>%
    dplyr::filter(!is.na(updated_observation_id), !is.na(updated_temporal_location_id))

temporal_location_grid_mapping <- temporal_location_grid_mapping %>%
    dplyr::mutate(updated_spatial_grid_id = grid_changer[as.character(spatial_grid_id)],
        updated_temporal_location_id = temporal_location_changer[as.character(temporal_location_id)],
        spacetime_grid_id = spatial_grid_and_time_to_spacetime_grid_changer[paste(updated_spatial_grid_id,
            t, sep = "_")]) %>%
    dplyr::filter(!is.na(updated_spatial_grid_id), !is.na(updated_temporal_location_id),
        !is.na(spacetime_grid_id))

if (config[["processing"]][["reorder_adjacency_matrix"]][["perform"]]) {
    bias <- covar_cube %>%
        dplyr::filter(t == min(t)) %>%
        dplyr::mutate(bias = x - y) %>%
        .[["bias"]]
    grid_adjacency <- taxdat::reorder_adjacency_matrix(grid_adjacency, bias, c("updated_id_1",
        "updated_id_2"))
}
### Construct some additional parameters based on the above Define relevent
### directories Name the output file

## Stan modeling section

print("*** STARTING STAN MODEL ***")

library(rstan)

# Initial Values ----------------------------------------------------------

## INITIAL VALUES

if (config$initial_values$warmup) {
    covariate_names <- colnames(covar_cube[, -c(1:5, ncol(covar_cube))])
    initial_values_df <- observation_data %>%
        dplyr::inner_join(observation_temporal_location_mapping) %>%
        dplyr::inner_join(temporal_location_grid_mapping, by = c(temporal_location_id = "temporal_location_id",
            "t")) %>%
        dplyr::inner_join(covar_cube, by = c("x", "y", "t", "spacetime_grid_id")) %>%
        dplyr::group_by(updated_observation_id) %>%
        dplyr::group_modify(function(.x, .y) {
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
        w_df <- dplyr::tibble(value = gam_predict - covariate_effect, spatial_id = covar_cube$updated_id) %>%
            dplyr::group_by(spatial_id) %>%
            dplyr::summarize(value = mean(value)) %>%
            dplyr::arrange(spatial_id)

        return(list(betas = rnorm(length(coef(gam_fit)[covariate_names]), coef(gam_fit)[covariate_names]),
            eta = rnorm(length(coef(gam_fit)["obs_year"]), coef(gam_fit)["obs_year"]),
            w = rnorm(nrow(w_df), w_df$value)))
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

standardize = function(x) (x - mean(x))/sd(x)
standardize_covar = function(M) cbind(M[, 1], apply(M[, -1, drop = F], 2, standardize))

stan_data <- list(N = nrow(covar_cube), N_edges = nrow(grid_adjacency), smooth_grid_N = length(unique(covar_cube$updated_id)),
    node1 = as.integer(grid_adjacency$updated_id_1), node2 = as.integer(grid_adjacency$updated_id_2),
    diag = nneighbors$nneighbors, pop = covar_cube$population, meanrate = 1, M = nrow(observation_data),
    M_right = sum(!is.na(observation_data[[paste0(cases_column, "_R")]])), M_full = sum(!is.na(observation_data[[cases_column]])),
    M_left = sum(!is.na(observation_data[[paste0(cases_column, "_L")]])), ind_right = as.array(which(!is.na(observation_data[[paste0(cases_column,
        "_R")]]))), ind_full = as.array(which(!is.na(observation_data[[cases_column]]))),
    ind_left = as.array(which(!is.na(observation_data[[paste0(cases_column, "_L")]]))),
    T = cast_to_int32(max(observation_temporal_location_mapping$t)), y = as.array(pmax(pmin(observation_data[[cases_column]],
        observation_data[[paste0(cases_column, "_R")]], na.rm = TRUE), observation_data[[paste0(cases_column,
        "_L")]], na.rm = TRUE)), L = length(unique(observation_temporal_location_mapping$updated_temporal_location_id)),
    K1 = nrow(observation_temporal_location_mapping), K2 = nrow(temporal_location_grid_mapping),
    map_obs_loctime_obs = as.array(cast_to_int32(observation_temporal_location_mapping$updated_observation_id)),
    map_obs_loctime_loc = as.array(cast_to_int32(observation_temporal_location_mapping$updated_temporal_location_id)),
    tfrac = as.array(rep(1, times = nrow(observation_temporal_location_mapping))),
    map_loc_grid_loc = as.array(cast_to_int32(temporal_location_grid_mapping$updated_temporal_location_id)),
    map_loc_grid_grid = as.array(cast_to_int32(temporal_location_grid_mapping$spacetime_grid_id)),
    as.array(cast_to_int32(temporal_location_grid_mapping$updated_spatial_grid_id)),
    map_smooth_grid = as.array(cast_to_int32(covar_cube$updated_id)), rho = 0.999,
    covar = standardize_covar(as.matrix(covar_cube[, covariate_names])), ncovar = length(covariate_names),
    beta_sigma_scale = config[["stan"]][["beta_sigma_scale"]], sigma_eta_scale = config[["stan"]][["sigma_eta_scale"]],
    use_weights = FALSE, use_rho_prior = TRUE, do_censoring = (0 != (sum(!is.na(observation_data[[paste0(cases_column,
        "_L")]])) + sum(!is.na(observation_data[[paste0(cases_column, "_R")]])))))


# Save input
stan_input <- list(stan_data = stan_data, covar_cube = covar_cube, observation_data = observation_data,
    grid_adjacency = grid_adjacency, observation_temporal_location_mapping = observation_temporal_location_mapping,
    temporal_location_grid_mapping = temporal_location_grid_mapping, initial_values_list = initial_values_list,
    initial_values_df = initial_values_df)
save(stan_input, file = config[["file_names"]][["stan_input"]])

start_time <- Sys.time()
model.rand <- rstan::stan(file = stan_model_path, data = stan_data, chains = config[["stan"]][["nchain"]],
    iter = config[["stan"]][["niter"]], pars = c("b", "t_rowsum", "vec_var"), include = FALSE,
    control = list(max_treedepth = 15), refresh = 10, init = initial_values_list)
end_time <- Sys.time()

elapsed_time <- end_time - start_time

# Save output
save(model.rand, elapsed_time, file = config[["file_names"]][["stan_output"]])
