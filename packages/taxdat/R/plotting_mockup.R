### options(error = recover) plot_sf_with_fill <- function(sf_object,
### color_scale, fill_column) { plot <- ggplot2::ggplot() +
### ggplot2::geom_sf(data = sf_object, ggplot2::aes_(fill =
### rlang::sym(fill_column))) + taxdat::color_scale(type = color_scale,
### use_case = 'ggplot map') + taxdat::map_theme() + ggplot2::facet_wrap(~set)
### return(plot) } plot_observed_cases_polygon_raw <- function(config, cache =
### new.env()) { get_observed_polygon_cases_disjoint_aggregated(config, cache)
### plot_sf_with_fill(sf_object =
### cache[['observed_polygon_cases_disjoint_aggregated']], color_scale =
### 'cases', fill_column = rlang::sym('suspected_cases')) }
### plot_modeled_cases_polygon_raw <- function(config, cache = new.env()) {
### get_observed_polygon_cases_disjoint(config, cache) plot <-
### ggplot2::ggplot() + ggplot2::geom_sf(data =
### cache[['observed_polygon_cases_disjoint']], ggplot2::aes(fill =
### suspected_cases)) + taxdat::color_scale(type = 'cases', use_case = 'ggplot
### map') + taxdat::map_theme() + ggplot2::facet_wrap(~set) return(plot) }
### function_cache <- new.env() cache_fun_results <- function(name, fun, ...,
### overwrite = FALSE) { if ((!overwrite) && (name %in% names(function_cache)))
### { stop('Please only cache a single function with the same name') }
### function_cache[[name]] <- TRUE return(function(config, cache = new.env()) {
### if (name %in% names(cache)) { return(invisible()) } cache[[name]] <-
### fun(config, cache) return(invisible()) }) } get_stan_input_no_cache <-
### function(config, cache) { load(config[['file_names']][['stan_input']])
### require(bit64) require(sf) return(stan_input) } get_stan_input <-
### cache_fun_results('stan_input', get_stan_input_no_cache)
### get_observation_data_no_cache <- function(config, cache) {
### get_stan_input(config, cache)
### return(cache[['stan_input']][['observation_data']]) } get_observation_data
### <- cache_fun_results('observation_data', get_observation_data_no_cache)
### separate_by_overlap <- function(sf_object, name_column =
### 'location_period_id') { unique_geometries <- sf_object %>%
### dplyr::group_by(!!!rlang::syms(name_column)) %>% dplyr::summarize(.groups =
### 'drop') %>% sf::st_as_sf() unique_geometries[['area']] <-
### sf::st_area(unique_geometries) unique_geometries <- unique_geometries %>%
### dplyr::arrange(-area) overlaps <- sf::st_relate(unique_geometries,
### unique_geometries, '2********') non_overlaps <- lapply(overlaps, setdiff, x
### = seq_len(nrow(unique_geometries))) unique_geometries[['set']] <- NA
### set_index <- 0 unassigned_elements <-
### which(is.na(unique_geometries[['set']])) while (length(unassigned_elements)
### > 0) { set_index <- set_index + 1
### unique_geometries[['set']][unassigned_elements[[1]]] <- set_index
### compatible_things <- non_overlaps[[unassigned_elements[[1]]]] while
### (length(compatible_things) > 0) {
### unique_geometries[['set']][compatible_things[[1]]] <- set_index
### compatible_things <- intersect(compatible_things,
### non_overlaps[[compatible_things[[1]]]]) } unassigned_elements <-
### which(is.na(unique_geometries[['set']])) } sf_object[['set']] <-
### unique_geometries[['set']][match(sf_object[[name_column]],
### unique_geometries[[name_column]])] return(sf_object) }
### normalize_cases_by_time <- function(cases, time_left, time_right) {
### return(cases/as.numeric(time_right - time_left + 1) * 365) }
### get_observed_polygon_cases_disjoint_no_cache <- function(config, cache) {
### get_stan_input(config, cache) get_observation_data(config, cache)
### separate_by_overlap(cache[['observation_data']], name_column =
### 'location_period_id') %>% return() } get_observed_polygon_cases_disjoint <-
### cache_fun_results('observed_polygon_cases_disjoint',
### get_observed_polygon_cases_disjoint_no_cache) aggregate_to_location_period
### <- function(sf_object, aggregation_function, grouping_columns =
### 'location_period_id', case_column = 'suspected_cases') { sf_object %>%
### dplyr::group_by(!!!rlang::syms(grouping_columns)) %>%
### dplyr::group_modify(function(.x, .y) { rc <- sf::st_sf(x =
### aggregation_function(cases = .x[[case_column]], time_left =
### .x[['time_left']], time_right = .x[['time_right']]), geom =
### sf::st_geometry(.x)[1]) names(rc)[[1]] <- case_column return(rc) }) %>%
### sf::st_as_sf() %>% return() }
### get_observed_polygon_cases_disjoint_aggregated_no_cache <- function(config,
### cache) { get_observed_polygon_cases_disjoint(config, cache)
### aggregate_to_location_period(cache[['observed_polygon_cases_disjoint']],
### aggregation_function = function(...) { mean(normalize_cases_by_time(...))
### }, grouping_columns = c('location_period_id', 'set'), case_column =
### 'suspected_cases') %>% return() }
### get_observed_polygon_cases_disjoint_aggregated <-
### cache_fun_results('observed_polygon_cases_disjoint_aggregated',
### get_observed_polygon_cases_disjoint_aggregated_no_cache) ## Unused but also
### examples get_model.rand_no_cache <- function(config, cache) {
### load(config[['file_names']][['stan_output']]) return(model.rand) }
### get_model.rand <- cache_fun_results('model.rand', get_model.rand_no_cache)
### get_grid_cases_no_cache <- function(config, cache) { get_model.rand(config,
### cache) return(MCMCvis::MCMCchains(cache[['model.rand']], params =
### 'grid_cases')) } get_grid_cases <- cache_fun_results('grid_cases',
### get_grid_cases_no_cache)
