## Basic test to make sure that testing setup works
context("Test 2 : Grid Adjacency")

dbuser <- Sys.getenv("USER", "app")
dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")

test_that("establishing postgres connection works", {
  expect_error(
    {
      conn_pg <- taxdat::connect_to_db(dbuser, dbname)
      DBI::dbClearResult(DBI::dbSendQuery(conn = conn_pg, "SET client_min_messages TO WARNING;"))
    },
    NA
  )
})


test_that("setup works", {
  expect_error(
    {
      dbuser <- Sys.getenv("USER", "app")
      dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")

      conn_pg <- taxdat::connect_to_db(dbuser, dbname)
      DBI::dbClearResult(DBI::dbSendQuery(conn = conn_pg, "SET client_min_messages TO WARNING;"))

      time_left <- lubridate::ymd("2000-01-01")
      time_right <- lubridate::ymd("2001-12-31")

      test_extent <- sf::st_bbox(c(xmin = 20, ymin = 0, xmax = 40, ymax = 20))
      test_raster <- taxdat::create_test_raster(nrows = 10, ncols = 10, nlayers = 2, test_extent = test_extent)
      test_polygons <- sf::st_make_valid(taxdat::create_test_layered_polygons(
        test_raster = test_raster,
        base_number = 1, n_layers = 2, factor = 10 * 10, snap = FALSE, randomize = FALSE
      ))

      all_dfs <- list()
      all_dfs$shapes_df <- test_polygons %>%
        dplyr::mutate(
          qualified_name = location, start_date = time_left,
          end_date = time_right
        )
      names(all_dfs$shapes_df)[names(all_dfs$shapes_df) == "geometry"] <- "geom"
      sf::st_geometry(all_dfs$shapes_df) <- "geom"
      all_dfs$location_period_df <- all_dfs$shapes_df %>%
        sf::st_drop_geometry()
      all_dfs$location_df <- all_dfs$shapes_df %>%
        sf::st_drop_geometry() %>%
        dplyr::group_by(qualified_name) %>%
        dplyr::summarize()
      all_dfs$observations_df <- data.frame(
        observation_collection_id = 1, time_left = time_left, time_right = time_right, qualified_name = "1", primary = TRUE, phantom = FALSE, suspected_cases = 0, deaths = NA, confirmed_cases = NA
      )

      test_covariates <- taxdat::create_multiple_test_covariates(test_raster = test_raster)
      covariate_raster_funs <- taxdat:::convert_simulated_covariates_to_test_covariate_funs(
        test_covariates,
        time_left, time_right
      )


      taxdat::setup_testing_database(conn_pg, drop = TRUE)
      taxdat::setup_testing_database_from_dataframes(conn_pg, all_dfs, covariate_raster_funs)
    },
    NA
  )
})

test_that("pull_grid_adjacency runs successfully", {
  conn_pg <- taxdat::connect_to_db(dbuser, dbname)
  expect_error(
    {
      taxdat::pull_grid_adjacency(conn_pg, "1", 1, 1)
    },
    NA
  )
})

test_that("pull_symmetric_grid_adjacency runs successfully", {
  conn_pg <- taxdat::connect_to_db(dbuser, dbname)
  expect_error(
    {
      taxdat::pull_symmetric_grid_adjacency(conn_pg, "1", 1, 1)
    },
    NA
  )
})

test_that("pull_symmetric_grid_adjacency has the right size", {
  conn_pg <- taxdat::connect_to_db(dbuser, dbname)
  symmetric_grid_adjacency <- taxdat::pull_symmetric_grid_adjacency(
    conn_pg, "1",
    1, 1
  )
  symmetric_grid_size <- c(10, 10)
  symmetric_grid_neighbors <- lapply(symmetric_grid_size, function(x) {
    data.frame(neighbors = c(1, 2), count = c(2, x - 2))
  })
  names(symmetric_grid_neighbors[[1]]) <- c("neighbors_1", "count_1")
  names(symmetric_grid_neighbors[[2]]) <- c("neighbors_2", "count_2")
  expected_counts <- dplyr::summarize(
    dplyr::group_by(dplyr::mutate(tidyr::crossing(
      symmetric_grid_neighbors[[1]],
      symmetric_grid_neighbors[[2]]
    ), neighbors = (neighbors_1 + 1) * (neighbors_2 +
      1) - 1, count = count_1 * count_2, contribution = count * neighbors), neighbors),
    count = sum(count), contribution = sum(contribution), .groups = "drop"
  )
  expect_equal(nrow(symmetric_grid_adjacency), sum(expected_counts$contribution))
  expect_equal(dplyr::summarize(
    dplyr::group_by(dplyr::summarize(dplyr::group_by(
      symmetric_grid_adjacency,
      id_1
    ), neighbors = length(id_2), .groups = "drop"), neighbors),
    count = length(neighbors),
    .groups = "drop"
  ), expected_counts[, c("neighbors", "count")])
})

test_that("pull_grid_adjacency has the right size", {
  conn_pg <- taxdat::connect_to_db(dbuser, dbname)
  grid_adjacency <- taxdat::pull_grid_adjacency(conn_pg, "1", 1, 1)
  grid_size <- c(10, 10)
  grid_neighbors <- lapply(grid_size, function(x) {
    data.frame(neighbors = c("l", "r", "lr"), count = c(1, 1, x - 2))
  })
  names(grid_neighbors[[1]]) <- c("neighbors_1", "count_1")
  names(grid_neighbors[[2]]) <- c("neighbors_2", "count_2")
  expected_counts <- dplyr::summarize(
    dplyr::group_by(dplyr::mutate(tidyr::crossing(
      grid_neighbors[[1]],
      grid_neighbors[[2]]
    ), neighbors = grepl("^l", neighbors_1) + grepl(
      "^l",
      neighbors_2
    ) + grepl("^l", neighbors_1) * nchar(neighbors_2), count = count_1 *
      count_2, contribution = count * neighbors), neighbors),
    count = sum(count),
    contribution = sum(contribution), .groups = "drop"
  )
  symmetric_grid_neighbors <- lapply(grid_size, function(x) {
    data.frame(neighbors = c(1, 2), count = c(2, x - 2))
  })
  names(symmetric_grid_neighbors[[1]]) <- c("neighbors_1", "count_1")
  names(symmetric_grid_neighbors[[2]]) <- c("neighbors_2", "count_2")
  symmetric_expected_counts <- dplyr::summarize(
    dplyr::group_by(dplyr::mutate(tidyr::crossing(
      symmetric_grid_neighbors[[1]],
      symmetric_grid_neighbors[[2]]
    ), neighbors = (neighbors_1 + 1) * (neighbors_2 +
      1) - 1, count = count_1 * count_2, contribution = count * neighbors), neighbors),
    count = sum(count), contribution = sum(contribution), .groups = "drop"
  )
  expect_equal(nrow(grid_adjacency), sum(symmetric_expected_counts$contribution) / 2)
  expect_equal(nrow(grid_adjacency), sum(expected_counts$contribution))
  expect_equal(dplyr::summarize(
    dplyr::group_by(dplyr::summarize(dplyr::group_by(
      grid_adjacency,
      id_1
    ), neighbors = length(id_2), .groups = "drop"), neighbors),
    count = length(neighbors),
    .groups = "drop"
  ), dplyr::filter(
    expected_counts[, c("neighbors", "count")],
    neighbors > 0
  ))
})
