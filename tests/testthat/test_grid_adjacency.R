## Basic test to make sure that testing setup works
context("Test 2 : Grid Adjacency")
test_that("testing works", {
    expect_error({
        1 + 1
    }, NA)
})

dbuser <- Sys.getenv("USER", "app")
dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")

test_that("establishing postgres connection works", {

    expect_error({
        conn_pg <- taxdat::connect_to_db(dbuser, dbname)
        DBI::dbClearResult(DBI::dbSendQuery(conn = conn_pg, "SET client_min_messages TO WARNING;"))
    }, NA)

})


test_that("setup works", {
    conn_pg <- taxdat::connect_to_db(dbuser, dbname)
    DBI::dbClearResult(DBI::dbSendQuery(conn = conn_pg, "SET client_min_messages TO WARNING;"))
    expect_error({
        taxdat::setup_testing_database(conn_pg, drop = TRUE)
    }, NA)
    expect_error({
        taxdat::insert_testing_locations(conn_pg, data.frame(qualified_name = "testlocation"))
    }, NA)
    expect_error({
        taxdat::insert_testing_location_periods(conn_pg, data.frame(qualified_name = "testlocation", 
            start_date = "2000-01-01", end_date = "2000-12-31"))
    }, NA)
    shapes_df <- data.frame(qualified_name = "testlocation", start_date = "2000-01-01", 
        end_date = "2000-12-31", geom = sf::st_sfc(sf::st_polygon(list(matrix(c(0, 
            0, 0, 1, 1, 1, 1, 0, 0, 0), ncol = 2, byrow = TRUE)))))
    names(shapes_df)[[4]] <- "geom"
    expect_error({
        taxdat::insert_testing_shapefiles(conn_pg, shapes_df)
    }, NA)
    expect_error({
        taxdat::refresh_materialized_views(conn_pg)
    }, NA)
    expect_error({
        taxdat::ingest_spatial_grid(conn_pg, width = 1, height = 1)
    }, NA)
    expect_error({
        taxdat::refresh_materialized_views(conn_pg)
    }, NA)
})

test_that("pull_grid_adjacency runs successfully", {
    conn_pg <- taxdat::connect_to_db(dbuser, dbname)
    expect_error({
        taxdat::pull_grid_adjacency(conn_pg, "testlocation", 1, 1)
    }, NA)
})

test_that("pull_symmetric_grid_adjacency runs successfully", {
    conn_pg <- taxdat::connect_to_db(dbuser, dbname)
    expect_error({
        taxdat::pull_symmetric_grid_adjacency(conn_pg, "testlocation", 1, 1)
    }, NA)
})

test_that("pull_symmetric_grid_adjacency has the right size", {
    conn_pg <- taxdat::connect_to_db(dbuser, dbname)
    symmetric_grid_adjacency <- taxdat::pull_symmetric_grid_adjacency(conn_pg, "testlocation", 
        1, 1)
    symmetric_grid_size <- c(10, 10)
    symmetric_grid_neighbors <- lapply(symmetric_grid_size, function(x) {
        data.frame(neighbors = c(1, 2), count = c(2, x - 2))
    })
    names(symmetric_grid_neighbors[[1]]) <- c("neighbors_1", "count_1")
    names(symmetric_grid_neighbors[[2]]) <- c("neighbors_2", "count_2")
    expected_counts <- dplyr::summarize(dplyr::group_by(dplyr::mutate(tidyr::crossing(symmetric_grid_neighbors[[1]], 
        symmetric_grid_neighbors[[2]]), neighbors = (neighbors_1 + 1) * (neighbors_2 + 
        1) - 1, count = count_1 * count_2, contribution = count * neighbors), neighbors), 
        count = sum(count), contribution = sum(contribution), .groups = "drop")
    expect_equal(nrow(symmetric_grid_adjacency), sum(expected_counts$contribution))
    expect_equal(dplyr::summarize(dplyr::group_by(dplyr::summarize(dplyr::group_by(symmetric_grid_adjacency, 
        id_1), neighbors = length(id_2), .groups = "drop"), neighbors), count = length(neighbors), 
        .groups = "drop"), expected_counts[, c("neighbors", "count")])
})

test_that("pull_grid_adjacency has the right size", {
    conn_pg <- taxdat::connect_to_db(dbuser, dbname)
    grid_adjacency <- taxdat::pull_grid_adjacency(conn_pg, "testlocation", 1, 1)
    grid_size <- c(10, 10)
    grid_neighbors <- lapply(grid_size, function(x) {
        data.frame(neighbors = c("l", "r", "lr"), count = c(1, 1, x - 2))
    })
    names(grid_neighbors[[1]]) <- c("neighbors_1", "count_1")
    names(grid_neighbors[[2]]) <- c("neighbors_2", "count_2")
    expected_counts <- dplyr::summarize(dplyr::group_by(dplyr::mutate(tidyr::crossing(grid_neighbors[[1]], 
        grid_neighbors[[2]]), neighbors = grepl("^l", neighbors_1) + grepl("^l", 
        neighbors_2) + grepl("^l", neighbors_1) * nchar(neighbors_2), count = count_1 * 
        count_2, contribution = count * neighbors), neighbors), count = sum(count), 
        contribution = sum(contribution), .groups = "drop")
    symmetric_grid_neighbors <- lapply(grid_size, function(x) {
        data.frame(neighbors = c(1, 2), count = c(2, x - 2))
    })
    names(symmetric_grid_neighbors[[1]]) <- c("neighbors_1", "count_1")
    names(symmetric_grid_neighbors[[2]]) <- c("neighbors_2", "count_2")
    symmetric_expected_counts <- dplyr::summarize(dplyr::group_by(dplyr::mutate(tidyr::crossing(symmetric_grid_neighbors[[1]], 
        symmetric_grid_neighbors[[2]]), neighbors = (neighbors_1 + 1) * (neighbors_2 + 
        1) - 1, count = count_1 * count_2, contribution = count * neighbors), neighbors), 
        count = sum(count), contribution = sum(contribution), .groups = "drop")
    expect_equal(nrow(grid_adjacency), sum(symmetric_expected_counts$contribution)/2)
    expect_equal(nrow(grid_adjacency), sum(expected_counts$contribution))
    expect_equal(dplyr::summarize(dplyr::group_by(dplyr::summarize(dplyr::group_by(grid_adjacency, 
        id_1), neighbors = length(id_2), .groups = "drop"), neighbors), count = length(neighbors), 
        .groups = "drop"), dplyr::filter(expected_counts[, c("neighbors", "count")], 
        neighbors > 0))
})
