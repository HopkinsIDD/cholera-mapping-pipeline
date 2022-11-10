#!/usr/bin/Rscript
# This scripts purpose is to collect all data required to model cholera
# incidence, and store it in a form usable by stan.  This script mainly uses
# functions from the package taxdat, stored in trunk/packages/taxdat. Libraries
# Control Variables: Preamble
# ------------------------------------------------------------------------------------------------------------



### Set Error Handling
taxdat::set_error_handling(is_interactive = Sys.getenv("INTERACTIVE_RUN", "FALSE"))

### Libraries TODO : Update this list
taxdat::update_libraries(perform = Sys.getenv("CHOLERA_CHECK_LIBRARIES", TRUE), package_list = c(
  "optparse", "DBI", "RPostgres", "sf", "magrittr", "dplyr",
  "rstan", "xfun", "kableExtra", "MCMCvis"
))

library(magrittr)
library(bit64)
# s2 has different ideas about geometry validity than postgis does
sf::sf_use_s2(FALSE)



## Inputs
## --------------------------------------------------------------------------------------------------------------


### Command Line Options
option_list <- list(
  optparse::make_option(c("-c", "--config"),
    action = "store",
    default = Sys.getenv("CHOLERA_CONFIG", "config.yml"), type = "character", help = "Model run configuration file"
  ),
  optparse::make_option(c("-d", "--cholera_pipeline_directory"),
    action = "store",
    default = Sys.getenv("CHOLERA_PIPELINE_DIRECTORY", "."), type = "character",
    help = "Pipeline directory"
  ), optparse::make_option(c("-o", "--cholera_output_directory"),
    action = "store", default = Sys.getenv("CHOLERA_OUTPUT_DIRECTORY", "Analysis/data"),
    type = "character", help = "Output directory"
  ), optparse::make_option(c(
    "-D",
    "--postgres_database_name"
  ), action = "store", default = Sys.getenv(
    "CHOLERA_POSTGRES_DATABASE",
    "cholera_covariates"
  ), type = "character", help = "Postgres database name"),
  optparse::make_option(c("-p", "--postgres_database_port"),
    action = "store",
    default = Sys.getenv("CHOLERA_POSTGRES_PORT", 5435), type = "character",
    help = "Postgres database port"
  ), optparse::make_option(c("-u", "--postgres_database_user"),
    action = "store", default = Sys.getenv("USER", "app"), type = "character",
    help = "Postgres database user"
  ), optparse::make_option(c("--testing_run"),
    action = "store", default = FALSE, type = "logical", help = "Is this run a testing run or a production run"
  ), optparse::make_option(c("-H", "--postgres_database_host"),
    action = "store", default = "localhost", type = "character", help = "Postgres hostname"
  )
)


opt <- optparse::parse_args((optparse::OptionParser(option_list = option_list)))


### Config Options
config <- yaml::read_yaml(opt[["config"]], eval.expr = TRUE)
config <- taxdat::complete_config(config)
if (!is.na(as.integer(Sys.getenv("CHOLERA_TEST_ITERATION", NA)))) {
  config[["stan"]][["niter"]] <- as.integer(Sys.getenv("CHOLERA_TEST_ITERATION", NA))
}
if (!opt[["testing_run"]]) {
  yaml::write_yaml(config, file = paste0(opt[["config"]], ".complete"))
}
if (!taxdat::check_config(config)) {
  stop("Could not validate the config")
}


##### Config Defaults


### Setup postgres
conn_pg <- taxdat::connect_to_db(
  dbname = opt[["postgres_database_name"]], dbuser = opt[["postgres_database_user"]],
  port = opt[["postgres_database_port"]], host = opt[["host"]]
)

cases_column <- "suspected_cases"
## Observations
print("Starting")
observation_data <- DBI::dbGetQuery(conn = conn_pg, statement = glue::glue_sql(
  .con = conn_pg,
  "SELECT *
     FROM pull_observation_data(
       {config[[\"general\"]][[\"location_name\"]]},
       {config[[\"general\"]][[\"start_date\"]]},
       {config[[\"general\"]][[\"end_date\"]]}
    )"
)) %>%
  dplyr::mutate(
    shape = sf::st_as_sfc(shape), suspected_cases_L = as.numeric(NA),
    suspected_cases_R = as.numeric(NA), confirmed_cases_L = as.numeric(NA), confirmed_cases_R = as.numeric(NA),
    deaths_L = as.numeric(NA), deaths_R = as.numeric(NA)
  ) %>%
  dplyr::filter(!is.na(!!rlang::sym(cases_column))) %>%
  sf::st_as_sf()
print("Pulled observation data")


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
  tidyr::pivot_wider(names_from = covariate_name, values_from = value, values_fn = sum)
covar_cube[["geometry"]] <- sf::st_as_sfc(covar_cube[["geometry"]])
covar_cube <- sf::st_as_sf(covar_cube)
print("Pulled covariates")



grid_adjacency <- DBI::dbGetQuery(conn = conn_pg, statement = glue::glue_sql(
  .con = conn_pg,
  "SELECT * FROM pull_grid_adjacency(
      {config[[\"general\"]][[\"location_name\"]]},
       {config[[\"general\"]][[\"width_in_km\"]]},
       {config[[\"general\"]][[\"height_in_km\"]]}
    )"
))
print("Pulled adjacency matrix")

observation_temporal_location_mapping <- DBI::dbGetQuery(conn = conn_pg, statement = glue::glue_sql(
  .con = conn_pg,
  "SELECT * FROM pull_observation_location_period_map(
      {config[[\"general\"]][[\"location_name\"]]},
       {config[[\"general\"]][[\"start_date\"]]},
       {config[[\"general\"]][[\"end_date\"]]},
       {config[[\"general\"]][[\"time_scale\"]]}
    )"
)) %>%
  dplyr::mutate(temporal_location_id = paste(location_period_id, t, sep = "_"))

print("Pulled observation-location map")

temporal_location_grid_mapping <- DBI::dbGetQuery(conn = conn_pg, statement = glue::glue_sql(
  .con = conn_pg,
  "SELECT * FROM pull_location_period_grid_map(
      {config[[\"general\"]][[\"location_name\"]]},
       {config[[\"general\"]][[\"start_date\"]]},
       {config[[\"general\"]][[\"end_date\"]]},
       {config[[\"general\"]][[\"width_in_km\"]]},
       {config[[\"general\"]][[\"height_in_km\"]]},
       {config[[\"general\"]][[\"time_scale\"]]}
   )"
)) %>%
  dplyr::mutate(temporal_location_id = paste(location_period_id, t, sep = "_"))

print("Pulled location-grid map")

boundary_polygon <- sf::st_sf(geometry = sf::st_as_sfc(DBI::dbGetQuery(conn = conn_pg, statement = glue::glue_sql(
  .con = conn_pg,
  "SELECT * FROM pull_boundary_polygon(
       {config[[\"general\"]][[\"location_name\"]]}
   );"
))[["shape"]]))
print("Pulled boundary polygon")


start_time <- Sys.time()
minimal_grid_population <- DBI::dbGetQuery(conn = conn_pg, statement = glue::glue_sql(
  .con = conn_pg,
  "SELECT * FROM pull_minimal_grid_population(
       {config[[\"general\"]][[\"location_name\"]]},
       {config[[\"general\"]][[\"start_date\"]]},
       {config[[\"general\"]][[\"end_date\"]]},
       {config[[\"general\"]][[\"time_scale\"]]}
   )"
))
end_time <- Sys.time()
elapsed_time <- end_time - start_time
print(paste("SQL Pull for minimal grid population ran in", elapsed_time))

start_time <- Sys.time()
minimal_grid_population <- minimal_grid_population %>% dplyr::mutate(
  rast = lapply(rast, function(x) {
    class(x) <- "pq_raster"
    stars::st_as_stars(taxdat:::as.raster.pq_raster(x))
  })
)
end_time <- Sys.time()
elapsed_time <- end_time - start_time
print(paste("conversion for minimal grid population ran in", elapsed_time))

print("Pulled minimal-grid population")

unique_temporal_location_ids <- unique(c(
  observation_temporal_location_mapping[["temporal_location_id"]],
  temporal_location_grid_mapping[["temporal_location_id"]]
))

observation_temporal_location_mapping[["temporal_location_id"]] <- bit64::as.integer64(match(
  observation_temporal_location_mapping[["temporal_location_id"]],
  unique_temporal_location_ids
))
temporal_location_grid_mapping[["temporal_location_id"]] <- bit64::as.integer64(match(
  temporal_location_grid_mapping[["temporal_location_id"]],
  unique_temporal_location_ids
))

taxdat::check_data(observation_data)

print("Starting processing")
# Intermediate operations like aggregation and overlap removal

observation_data.bak <- observation_data
observation_temporal_location_mapping.bak <- observation_temporal_location_mapping

if (config[["processing"]][["average_inconsistent_duplicates"]]) {
  print("Dealing with inconsistent duplicate observations")

  local_unique_column_names <- taxdat::suppress_warning(
    taxdat::get_unique_columns_by_group(dplyr::mutate(
      observation_data,
      time_left = as.character(time_left), time_right = as.character(time_right)
    ), grouping_columns = c(
      "observation_collection_id",
      "location_period_id",
      "time_left",
      "time_right"
    ), skip_columns = c(
      "observation_collection_id",
      "location_period_id",
      "time_left",
      "time_right",
      cases_column,
      "shape"
    )), "coercing argument of type 'character' to logical"
  )

  observation_data_deduplicated <- taxdat::remove_inconsistent_duplicates(observation_data,
    columns_to_mean_over = c(cases_column), local_unique_column_names
  )


  observation_data <- observation_data_deduplicated

  print("Finished resolving inconsistent duplicates")
}

if (config[["processing"]][["aggregate"]]) {
  print("Aggregating")
  temporally_linked_observations <- observation_data %>%
    dplyr::inner_join(observation_temporal_location_mapping, by = c(
      "observation_id",
      "location_period_id"
    ))

  observation_data_aggregated <- taxdat::aggregate_case_data(temporally_linked_observations,
    taxdat::get_unique_columns_by_group(temporally_linked_observations, grouping_columns = c(
      "observation_collection_id",
      "temporal_location_id"
    ), skip_columns = c(
      "observation_collection_id",
      "location_period_id", "shape", cases_column, "time_left", "time_right",
      "tfrac", "observation_id", "unique_observation_ids"
    )),
    columns_to_sum_over = c("tfrac", cases_column)
  )

  observation_data <- sf::st_as_sf(taxdat::project_to_groups(
    observation_data_aggregated,
    "observation_id", observation_data
  ))

  observation_temporal_location_mapping <- taxdat::project_to_groups(
    observation_data_aggregated,
    c("observation_id", "temporal_location_id"), observation_temporal_location_mapping
  )

  print("Finished aggregating")
}


if (config[["processing"]][["remove_overlaps"]][["perform"]]) {
  print("Removing overlaps")
  observation_data_with_t <- observation_data %>%
    dplyr::inner_join(observation_temporal_location_mapping, by = c(
      "observation_id",
      "location_period_id"
    )) %>%
    dplyr::group_by(!!!rlang::syms(taxdat::get_unique_columns_by_group(observation_data,
      "observation_id",
      skip_columns = c("shape")
    ))) %>%
    dplyr::summarize(t = list(unique(t)), .groups = "drop")
  observation_data <- taxdat::remove_overlapping_observations(observation_data_with_t,
    unique_column_names = "t"
  ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-t) %>%
    sf::st_as_sf()

  print("Finished removing overlaps")
}


if (config[["processing"]][["censor_incomplete_observations"]][["perform"]]) {
  print("Censoring incomplete observations")
  temporally_linked_observations <- observation_data %>%
    dplyr::inner_join(observation_temporal_location_mapping, by = c(
      "location_period_id",
      "observation_id"
    ))


  observation_data_censored <- taxdat::do_censoring(temporally_linked_observations,
    unique_column_names = taxdat::get_unique_columns_by_group(temporally_linked_observations,
      grouping_columns = c("observation_id"), skip_columns = c(
        "observation_id",
        "shape", cases_column, paste0(cases_column, "_R"), paste0(
          cases_column,
          "_L"
        ), "tfrac"
      )
    ), colnames = cases_column, threshold = config[["processing"]][["censor_incomplete_observations"]][["threshold"]]
  )


  observation_data <- sf::st_as_sf(taxdat::project_to_groups(
    observation_data_censored,
    "observation_id", observation_data
  ))

  observation_temporal_location_mapping <- taxdat::project_to_groups(
    observation_data_censored,
    c("observation_id", "temporal_location_id"), observation_temporal_location_mapping
  )
  print("Finished censoring incomplete observations")
} else {
  observation_data <- dplyr::filter(observation_data, !is.na(!!rlang::sym(cases_column)))
  observation_data[[paste0(cases_column, "_L")]] <- as.numeric(NA)
  observation_data[[paste0(cases_column, "_R")]] <- as.numeric(NA)
}

if (!all(is.na(observation_data[[paste0(cases_column, "_R")]]) | is.na(observation_data[[cases_column]])) &&
  !all(is.na(observation_data[[cases_column]]) | is.na(observation_data[[paste0(
    cases_column,
    "_L"
  )]])) && !all(is.na(observation_data[[paste0(cases_column, "_R")]]) |
  is.na(observation_data[[paste0(cases_column, "_L")]]))) {
  stop(paste0(
    "All observations should have at most one observation from ", cases_column,
    "_R ", cases_column, " ", cases_column, "_L by this point in processing."
  ))
}

if (any(is.na(observation_data[[paste0(cases_column, "_R")]]) & is.na(observation_data[[cases_column]]) &
  is.na(observation_data[[paste0(cases_column, "_L")]]))) {
  warning(paste0(
    "All observations should have exactly one observation from ",
    cases_column, "_R ", cases_column, " ", cases_column, "_L by this point in processing."
  ))
}

## Replace me with a config call
potential_covariate_names <- colnames(as.data.frame(covar_cube)[, -c(1:6), drop = FALSE])
covariate_names <- as.character(sapply(config[["general"]][["covariates"]], function(x) {
  x[["name"]]
}))
if (!all(covariate_names %in% potential_covariate_names)) {
  stop(paste(
    "Could not find all covariates.", paste(covariate_names[!(covariate_names %in%
      potential_covariate_names)], collapse = ", "), "were not found amont options",
    paste(potential_covariate_names, collapse = ", ")
  ))
}


for (covariate in covariate_names) {
  covar_cube <- covar_cube[!is.na(covar_cube[[covariate]]), ]
}

# This is still in discussion
covariate_covered_grid_ids <- covar_cube %>%
  dplyr::filter(!is.na(id), !is.na(t), population >= 1) %>%
  dplyr::group_by(id) %>%
  dplyr::summarize(count = length(t), .groups = "drop") %>%
  dplyr::filter(count == max(count)) %>%
  .[["id"]]

covar_cube <- covar_cube %>%
  dplyr::filter(id %in% covariate_covered_grid_ids)


print("Reindexing")

fully_covered_indices <- as.data.frame(observation_data)[, c("observation_id"), drop = FALSE] %>%
  dplyr::full_join(observation_temporal_location_mapping[, c(
    "observation_id",
    "temporal_location_id"
  )], by = "observation_id") %>%
  dplyr::full_join(temporal_location_grid_mapping[, c(
    "temporal_location_id", "spatial_grid_id",
    "t"
  )], by = "temporal_location_id") %>%
  dplyr::full_join(as.data.frame(covar_cube)[, c("id", "t")], by = c(
    spatial_grid_id = "id",
    t = "t"
  )) %>%
  dplyr::group_by(spatial_grid_id) %>%
  dplyr::group_modify(function(.x, .y) {
    .x[["unique_time_slices"]] <- length(unique(dplyr::filter(
      .x, !is.na(observation_id),
      !is.na(temporal_location_id), !is.na(t)
    )[["t"]]))
    return(.x)
  }) %>%
  dplyr::filter(
    !is.na(observation_id), !is.na(temporal_location_id), !is.na(spatial_grid_id),
    !is.na(t), unique_time_slices == max(unique_time_slices)
  )
## TODO : We may want to do something different here when filtering.  Right
## now, we are filtering out places with 0 pop at any time for all time, even
## if all other covariates are present.  This seems like potentially a mistake.

fully_covered_grid_ids <- unique(fully_covered_indices[["spatial_grid_id"]])
fully_covered_temporal_location_ids <- unique(fully_covered_indices[["temporal_location_id"]])
fully_covered_observation_ids <- unique(fully_covered_indices[["observation_id"]])
fully_covered_ts <- unique(fully_covered_indices[["t"]])

## Actually doing filtering here

if (config[["processing"]][["remove_unobserved_time_slices"]]) {
  covar_cube <- covar_cube %>%
    dplyr::filter(id %in% fully_covered_grid_ids, t %in% fully_covered_ts)
}
if (config[["processing"]][["remove_unobserved_space_slices"]]) {
  covar_cube <- covar_cube %>%
    dplyr::filter(id %in% fully_covered_grid_ids)
}
covar_cube <- covar_cube %>%
  taxdat::reindex("id", "updated_id") %>%
  taxdat::reindex("t", "updated_t") %>%
  tibble::rownames_to_column() %>%
  dplyr::mutate(spacetime_grid_id = as.numeric(rowname)) %>%
  dplyr::select(-rowname)

spatial_grid_and_time_to_spacetime_grid_changer <- setNames(
  covar_cube[["spacetime_grid_id"]],
  paste(covar_cube[["updated_id"]], covar_cube[["updated_t"]], sep = "_")
)

grid_changer <- setNames(sort(unique(covar_cube[["updated_id"]])), sort(unique(covar_cube[["id"]])))
t_changer <- setNames(sort(unique(covar_cube[["updated_t"]])), sort(unique(covar_cube[["t"]])))
if (!all(grid_changer[as.character(covar_cube[["id"]])] == covar_cube[["updated_id"]])) {
  stop("There is a problem with computing the updated grid indices")
}

grid_adjacency <- grid_adjacency %>%
  dplyr::mutate(updated_id_1 = grid_changer[as.character(id_1)], updated_id_2 = grid_changer[as.character(id_2)]) %>%
  dplyr::filter(!is.na(updated_id_1), !is.na(updated_id_2))

temporal_location_changer <- setNames(
  seq_len(length(fully_covered_temporal_location_ids)),
  fully_covered_temporal_location_ids
)

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
  taxdat::reindex("observation_id", "updated_observation_id")

observation_changer <- setNames(
  sort(unique(observation_data[["updated_observation_id"]])),
  sort(unique(observation_data[["observation_id"]]))
)
if (!all(observation_changer[as.character(observation_data[["observation_id"]])] ==
  observation_data[["updated_observation_id"]])) {
  stop("There is a problem with computing the updated observation indices")
}

observation_temporal_location_mapping <- observation_temporal_location_mapping %>%
  dplyr::mutate(
    updated_observation_id = observation_changer[as.character(observation_id)],
    updated_temporal_location_id = temporal_location_changer[as.character(temporal_location_id)],
    updated_t = t_changer[as.character(t)]
  ) %>%
  dplyr::filter(!is.na(updated_observation_id), !is.na(updated_temporal_location_id), !is.na(updated_t))

temporal_location_grid_mapping <- temporal_location_grid_mapping %>%
  dplyr::mutate(
    updated_temporal_location_id = temporal_location_changer[as.character(temporal_location_id)],
    updated_spatial_grid_id = grid_changer[as.character(spatial_grid_id)], updated_t = t_changer[as.character(t)],
    spacetime_grid_id = spatial_grid_and_time_to_spacetime_grid_changer[paste(updated_spatial_grid_id,
      updated_t,
      sep = "_"
    )]
  ) %>%
  dplyr::filter(
    !is.na(updated_spatial_grid_id), !is.na(updated_temporal_location_id),
    !is.na(spacetime_grid_id)
  )

print("Finished reindexing")


covar_cube <- taxdat::transform_covariates(covar_cube, config[["general"]][["covariates"]])


if (config[["processing"]][["reorder_adjacency_matrix"]][["perform"]]) {
  print("Reordering adjacency matrix")
  bias <- covar_cube %>%
    dplyr::filter(t == min(t)) %>%
    dplyr::mutate(bias = x - y) %>%
    .[["bias"]]
  grid_adjacency <- taxdat::reorder_adjacency_matrix(grid_adjacency, bias, c(
    "updated_id_1",
    "updated_id_2"
  ))
  print("Finished reordering adjacency matrix")
}


## Make some things for stan data involving etas
## NB: this fails: sapply(sort(unique(covar_cube$t)), function(x){x == covar_cube$t})
if (config[["stan"]][["do_time_slice"]][["perform"]]) {
  tmp <- as.character(covar_cube$t)
  tmp2 <- as.character(sort(unique(covar_cube$t)))
  mat_grid_time <- sapply(tmp2, function(x) {
    x == tmp
  })
  has_data_year <- rep(1, times = nrow(covar_cube))
} else {
  mat_grid_time <- matrix(0, 2, 2)
  has_data_year <- integer(0)
}




### Construct some additional parameters based on the above Define relevent
### directories Name the output file

## Stan modeling section

print("*** STARTING STAN MODEL ***")

library(rstan)

# Initial Values ----------------------------------------------------------

## INITIAL VALUES


if (config[["initial_values"]][["warmup"]]) {
  print("Starting gam warmup")
  initial_values_df <- observation_data %>%
    dplyr::inner_join(observation_temporal_location_mapping, by = c(
      "observation_id",
      "location_period_id", "updated_observation_id"
    )) %>%
    dplyr::inner_join(temporal_location_grid_mapping, by = c(
      "temporal_location_id",
      "t"
    )) %>%
    dplyr::inner_join(as.data.frame(covar_cube), by = c("x", "y", "t", "spacetime_grid_id")) %>%
    dplyr::group_by(updated_observation_id) %>%
    dplyr::group_modify(function(.x, .y) {
      .x[[paste("raw", cases_column, sep = "_")]] <- .x[[cases_column]]
      .x[[cases_column]] <- .x[[cases_column]] * .x[["population"]] * .x[["sfrac"]] / sum(.x[["population"]] *
        .x[["sfrac"]])
      .x[[paste("sfrac_adjusted_", cases_column, sep = "_")]] <- .x[[cases_column]]
      .x[[paste0(cases_column, "_L")]] <- .x[[paste0(cases_column, "_L")]] * .x[["population"]] / .x[["tfrac"]] * .x[["sfrac"]] / sum(.x[["population"]] *
        .x[["sfrac"]])
      .x[[paste0(cases_column, "_R")]] <- .x[[paste0(cases_column, "_R")]] * .x[["population"]] / .x[["tfrac"]] * .x[["sfrac"]] / sum(.x[["population"]] *
        .x[["sfrac"]])
      .x[[cases_column]] <- mapply(
        r = .x[[paste0(cases_column, "_R")]],
        m = .x[[cases_column]],
        l = .x[[paste0(cases_column, "_L")]],
        function(r, m, l) {
          return(median(c(r, m, l), na.rm = TRUE))
        }
      )
      .x[[cases_column]] <- diff(c(0, round(cumsum(.x[[cases_column]]))))
      .x[[paste("log", cases_column, sep = "_")]] <- log(.x[[cases_column]])
      return(.x)
    }) %>%
    dplyr::mutate(log_y = log(y), gam_offset = log_y)

  number_of_gridcells <- covar_cube %>%
    dplyr::group_by(x, y) %>%
    dplyr::summarize(.groups = "drop") %>%
    nrow()

  gam_formula <- taxdat::get_gam_formula(
    cases_column_name = cases_column,
    include_spatial_smoothing = TRUE,
    include_covariates = length(covariate_names) > 0,
    covariate_names = covariate_names,
    max_knots = number_of_gridcells - 1,
    include_time_slice_effect = config[["stan"]][["do_time_slice"]][["perform"]] && (length(unique(covar_cube$updated_t)) > 1)
  )

  gam_fit <- mgcv::gam(
    gam_formula,
    family = "poisson",
    data = initial_values_df,
    drop.unused.levels = FALSE,
  )
  gam_predict <- mgcv::predict.gam(
    gam_fit,
    covar_cube
  )

  ## Extract parameters
  ## - beta0
  ## - betas
  ## - log_std_dev_w
  ## - eta_tilde
  ## - sigma_eta_tilde
  ## - rho
  ## - w
  ## Extract covariates
  initial_beta0 <- coef(gam_fit)["(Intercept)"]
  beta0_effect <- rep(initial_beta0, times = nrow(covar_cube))

  initial_betas <- coef(gam_fit)[covariate_names]
  covariate_effect <- as.vector(as.matrix(as.data.frame(covar_cube)[, covariate_names]) %*%
    initial_betas)

  initial_etas <- c(0, coef(gam_fit)[grepl("^as.factor.t.", names(coef(gam_fit)))])
  initial_sigma_eta_tilde <- 1
  initial_eta_tilde <- (initial_etas / config[["stan"]][["sigma_eta_scale"]])
  eta_effect <- initial_etas[as.factor(covar_cube[["t"]])]

  initial_rho <- 0.9999

  residuals <- gam_predict - beta0_effect - covariate_effect - eta_effect
  initial_ws <- dplyr::tibble(value = residuals, spatial_id = covar_cube[["updated_id"]]) %>%
    dplyr::group_by(spatial_id) %>%
    dplyr::summarize(value = mean(value)) %>%
    dplyr::arrange(spatial_id) %>%
    .[["value"]]

  ##

  initial_values_list <- lapply(seq_len(config[["stan"]][["nchain"]]), function(chain) {
    rc <- list(
      beta0 = as.array(rnorm(
        length(initial_beta0),
        initial_beta0,
        sqrt(sqrt(abs(initial_beta0)))
      )),
      rho = initial_rho,
      w = as.array(rnorm(length(initial_ws), initial_ws))
    )
    if (length(covariate_names) > 0) {
      rc$betas <- as.array(rnorm(
        length(coef(gam_fit)[covariate_names]),
        coef(gam_fit)[covariate_names],
      ))
    }
    if (config[["stan"]][["do_time_slice"]][["perform"]] && (length(unique(covar_cube$updated_t)) > 1)) {
      rc$eta_tilde <- as.array(rnorm(
        length(initial_eta_tilde),
        initial_eta_tilde,
        sqrt(var(initial_eta_tilde))
      ))
      rc$sigma_eta_tilde <- as.array(rnorm(
        length(initial_sigma_eta_tilde),
        initial_sigma_eta_tilde,
        .01
      ))

      if (config[["stan"]][["do_time_slice"]][["eta_simplex"]]) {
        rc$beta0 <- rc$beta0 - min(rc$eta_tilde) + 1e-13
        rc$eta_tilde <- rc$eta_tilde - min(rc$eta_tilde) + 1e-13

        rc$sigma_eta_tilde <- rc$sigma_eta_tilde * sum(rc$eta_tilde)
        rc$eta_tilde <- rc$eta_tilde / sum(rc$eta_tilde)
      }
    }
    return(rc)
  })


  covar_cube[["covariate_contribution"]] <- covariate_effect
  covar_cube[["spatial_smoothing_term"]] <- residuals
  covar_cube[["gam_output"]] <- gam_predict
  print("Finished gam warmup")
} else {
  initial_values_list <- "random"
  initial_values_df <- "no warmup performed"
  covar_cube[["covariate_contribution"]] <- NA
  covar_cube[["spatial_smoothing_term"]] <- NA
  covar_cube[["gam_output"]] <- NA
}

## END INITIAL VALUES

# Run model ---------------------------------------------------------------
stan_dir <- config[["stan"]][["directory"]]
stan_model_path <- taxdat::check_stan_model(stan_model_path = paste(stan_dir, config[["stan"]][["model"]],
  sep = "/"
), stan_dir = stan_dir)

options(mc.cores = config[["stan"]][["ncores"]])

standardize <- function(x) {
  if (length(unique(x)) == 1) {
    return(x * 0) + 1
  }
  return((x - mean(x)) / sd(x - mean(x)))
}
standardize_covar <- function(M) {
  return(apply(M, 2, standardize))
}

print("Creating stan data")
stan_data <- list(
  N = nrow(covar_cube), N_edges = nrow(grid_adjacency), smooth_grid_N = length(unique(covar_cube[["updated_id"]])),
  node1 = as.integer(grid_adjacency[["updated_id_1"]]), node2 = as.integer(grid_adjacency[["updated_id_2"]]),
  diag = nneighbors[["nneighbors"]], pop = covar_cube[["population"]], meanrate = 1,
  M = nrow(observation_data), M_right = sum(!is.na(observation_data[[paste0(
    cases_column,
    "_R"
  )]])), M_full = sum(!is.na(observation_data[[cases_column]])), M_left = sum(!is.na(observation_data[[paste0(
    cases_column,
    "_L"
  )]])), ind_right = as.array(which(!is.na(observation_data[[paste0(
    cases_column,
    "_R"
  )]]))), ind_full = as.array(which(!is.na(observation_data[[cases_column]]))),
  ind_left = as.array(which(!is.na(observation_data[[paste0(cases_column, "_L")]]))),
  T = taxdat::cast_to_int32(max(observation_temporal_location_mapping[["updated_t"]])), y = as.array(pmax(pmin(observation_data[[cases_column]],
    observation_data[[paste0(cases_column, "_R")]],
    na.rm = TRUE
  ), observation_data[[paste0(
    cases_column,
    "_L"
  )]], na.rm = TRUE)), L = length(unique(observation_temporal_location_mapping[["updated_temporal_location_id"]])),
  K1 = nrow(observation_temporal_location_mapping), K2 = nrow(temporal_location_grid_mapping),
  map_obs_loctime_obs = as.array(taxdat::cast_to_int32(observation_temporal_location_mapping[["updated_observation_id"]])),
  map_obs_loctime_loc = as.array(taxdat::cast_to_int32(observation_temporal_location_mapping[["updated_temporal_location_id"]])),
  tfrac = as.array(observation_temporal_location_mapping$tfrac),
  map_loc_grid_loc = as.array(taxdat::cast_to_int32(temporal_location_grid_mapping[["updated_temporal_location_id"]])),
  map_loc_grid_grid = as.array(taxdat::cast_to_int32(temporal_location_grid_mapping[["spacetime_grid_id"]])),
  map_loc_grid_sfrac = as.array(temporal_location_grid_mapping[["sfrac"]]), map_smooth_grid = as.array(taxdat::cast_to_int32(covar_cube[["updated_id"]])),
  rho = 0.999, covar = standardize_covar(as.matrix(as.data.frame(covar_cube)[
    ,
    covariate_names
  ])), ncovar = length(covariate_names), beta_sigma_scale = config[["stan"]][["beta_sigma_scale"]],
  sigma_eta_scale = config[["stan"]][["sigma_eta_scale"]], use_weights = FALSE,
  use_rho_prior = TRUE, do_censoring = (0 != (sum(!is.na(observation_data[[paste0(
    cases_column,
    "_L"
  )]])) + sum(!is.na(observation_data[[paste0(cases_column, "_R")]])))),
  do_time_slice_effect = config[["stan"]][["do_time_slice"]][["perform"]],
  do_time_slice_effect_autocor = config[["stan"]][["do_time_slice"]][["autocorrelated_prior"]],
  exp_prior = config[["stan"]][["exp_prior"]], # QZ: added exponential betas option in config
  narrower_prior = config[["stan"]][["narrower_prior"]], # QZ: added narrower prior option in config
  has_data_year = has_data_year,
  mat_grid_time = mat_grid_time,
  debug = FALSE
)

print("Finished creating stan data")


# Save input
print("Creating model input")
stan_input <- list(
  stan_data = stan_data, covar_cube = covar_cube, observation_data = observation_data, observation_data.bak = observation_data.bak,
  grid_adjacency = grid_adjacency, observation_temporal_location_mapping = observation_temporal_location_mapping,
  temporal_location_grid_mapping = temporal_location_grid_mapping, initial_values_list = initial_values_list,
  initial_values_df = initial_values_df, boundary_polygon = boundary_polygon
)
print("Saving model input")
save(stan_input, file = config[["file_names"]][["stan_input"]])
# raster::writeRaster(minimal_grid_population, file = config[["file_names"]][["minimal_grid_population_file"]])
raster_filenames <- character(nrow(minimal_grid_population))
for (row_idx in seq_len(nrow(minimal_grid_population))) {
  raster_filenames[row_idx] <- paste0(config[["file_names"]][["minimal_grid_population"]], "_", row_idx, "_raster.tif")
  print(paste("Saving raster", raster_filenames[row_idx]))
  stars::write_stars(minimal_grid_population$rast[[row_idx]], dsn = raster_filenames[row_idx])
  print(paste("Finished saving raster", raster_filenames[row_idx]))
}
minimal_grid_population$rast <- NULL
minimal_grid_population$raster_filename <- raster_filenames
minimal_grid_population$rid <- taxdat::cast_to_int32(minimal_grid_population$rid)
minimal_grid_population$temporal_grid_id <- taxdat::cast_to_int32(minimal_grid_population$temporal_grid_id)
readr::write_csv(minimal_grid_population, config[["file_names"]][["minimal_grid_population"]])
print("Finished saving model input")

print("Running STAN")
## Fix me : make sure this works
chol_model <- cmdstanr::cmdstan_model(stan_model_path, quiet = FALSE, force_recompile = config[["stan"]][["recompile"]])

## FIX ME : add seed to config FIX ME : Split warmup and sampling iterations
## into two arguments
start_time <- Sys.time()
cmdstan_fit <- chol_model$sample(
  seed = 1234, data = stan_data, chains = config[["stan"]][["nchain"]],
  parallel_chains = config[["stan"]][["ncores"]], iter_warmup = config[["stan"]][["niter"]] / 2,
  iter_sampling = config[["stan"]][["niter"]] / 2, max_treedepth = 15, init = initial_values_list,
  sig_figs = 14, save_warmup = F, refresh = max(1, floor(config[["stan"]][["niter"]] * 0.01))
)
end_time <- Sys.time()

## start_time <- Sys.time() model.rand <- rstan::stan(file = stan_model_path,
## data = stan_data, chains = config[['stan']][['nchain']], iter =
## config[['stan']][['niter']], pars = c('b', 't_rowsum', 'vec_var'), include =
## FALSE, control = list(max_treedepth = 15), refresh = 0) end_time <-
## Sys.time()

elapsed_time <- end_time - start_time

# FIX ME : don't use model.rand Save output Consider just using the cmdstanr
# output directly
cmdstan_fit$save_object(file = config[["file_names"]][["stan_output"]])

## Run generated quantities This is just an example: we would really do the
## non-CT_WORLD root locations here:
if (config[["generated"]][["perform"]]) {
  full_temporal_location_grid_mapping <- DBI::dbGetQuery(conn = conn_pg, statement = glue::glue_sql(
    .con = conn_pg,
    "SELECT * FROM pull_location_period_grid_map(
      {config[[\"generated\"]][[\"location_name\"]]},
       {config[[\"generated\"]][[\"start_date\"]]},
       {config[[\"generated\"]][[\"end_date\"]]},
       {config[[\"generated\"]][[\"width_in_km\"]]},
       {config[[\"generated\"]][[\"height_in_km\"]]},
       {config[[\"generated\"]][[\"time_scale\"]]}
    )"
  )) %>%
    dplyr::mutate(temporal_location_id = paste(location_period_id, t, sep = "_")) %>%
    dplyr::mutate(
      updated_temporal_location_id = temporal_location_changer[as.character(temporal_location_id)],
      updated_spatial_grid_id = grid_changer[as.character(spatial_grid_id)],
      updated_t = t_changer[as.character(t)], spacetime_grid_id = spatial_grid_and_time_to_spacetime_grid_changer[paste(updated_spatial_grid_id,
        updated_t,
        sep = "_"
      )]
    )

  updated_stan_data <- stan_data
  updated_stan_data$map_loc_grid_sfrac <- as.array(temporal_location_grid_mapping[["sfrac"]])
  updated_stan_data$map_loc_grid_grid <- as.array(taxdat::cast_to_int32(temporal_location_grid_mapping[["spacetime_grid_id"]]))
  updated_stan_data$map_loc_grid_loc <- as.array(taxdat::cast_to_int32(temporal_location_grid_mapping[["updated_temporal_location_id"]]))
  updated_stan_data$K2 <- nrow(temporal_location_grid_mapping)

  # cmdstan_draws <- posterior::as_draws(model.rand)
  chol_gen <- chol_model$generate_quantities(
    fitted_params = cmdstan_fit, data = updated_stan_data,
    parallel_chains = config[["stan"]][["nchain"]]
  )
  chol_gen$save_object(file = config[["file_names"]][["generated_quantities"]])
}

rmarkdown::render(
  rprojroot::find_root_file(
    criterion = ".choldir", "Analysis", "output",
    "country_data_report.Rmd"
  ),
  params = list(
    cholera_directory = rprojroot::find_root(criterion = ".choldir"),
    config = paste0(opt[["config"]], ".complete"),
    drop_nodata_years = TRUE
  ),
  output_file = config[["file_names"]][["report"]]
)
