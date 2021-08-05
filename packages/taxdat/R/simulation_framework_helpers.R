#' @include indexing_helpers.R

#' @name get_or_set_seed
#' @title get_or_set_seed
#' @description Set the seed to the given value (maybe) and return current seed
get_or_set_seed <- function(seed) {
    if (missing(seed)) {
        rnorm(1, 1)
        seed <- .Random.seed
    }
    .GlobalEnv$.Random.seed <- seed
    return(.GlobalEnv$.Random.seed)

}


#' @export
#' @name create_test_extent
#' @title create_test_extent
#' @description Create an extent to use with modeling
#' @param seed integer A seed to use for the randomly constructed portions of this object
create_test_extent <- function(seed) {
    seed <- get_or_set_seed(seed)
    rc <- sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 1, ymax = 1))
    attr(rc, "seed") <- seed
    return(rc)
}


#' @export
#' @name create_test_raster
#' @title create_test_raster
#' @description Create an raster to use with simulating test data
#' @param nrows integer The number of rows the raster should have.
#' @param ncols integer The number of columns the raster should have.
#' @param nlayers integer The number of layers the raster should have.  Each layer is intended to express time.
#' @param test_extent The extent the raster should cover.
#' @param seed integer A seed to use for the randomly constructed portions of this object
create_test_raster <- function(nrows = 8, ncols = 8, nlayers = 2, seed, test_extent = create_test_extent(seed)) {
    seed <- get_or_set_seed(seed)
    one_rc <- sf::st_sf(sf::st_make_grid(sf::st_as_sfc(test_extent), n = c(nrows, 
        ncols)))
    sf::st_crs(one_rc) <- 4326
    one_rc$id = seq_len(nrow(one_rc))
    one_rc$col <- ((one_rc$row = one_rc$id - 1)%%nrows) + 1
    one_rc$row <- ((one_rc$row = one_rc$id - 1)%/%nrows) + 1
    rc <- list()
    for (i in seq_len(nlayers)) {
        rc[[i]] <- one_rc
        rc[[i]]$t <- i
    }
    rc <- do.call(sf:::rbind.sf, rc)
    attr(rc, "seed") <- seed
    return(rc)
}

## @name create_test_2d_polygons See create_test_polygons for details
## (dimension=2)
create_test_2d_polygons <- function(test_raster = create_test_raster(), number = 10, 
    snap = FALSE, randomize = TRUE, seed) {
    seed <- get_or_set_seed(seed)

    dims <- c(max(test_raster$row), max(test_raster$col))
    test_points <- sf::st_centroid(test_raster)
    original_crs <- sf::st_crs(test_points)
    test_points <- sf::st_transform(test_points, crs = 32610)
    if (randomize) {
        sample_points <- sf::st_union(sample(sf::st_geometry(test_points[seq_len(prod(dims)), 
            ]), number))
    } else {
        grid <- split_into_factors(number, dims[1], dims[2])
        gridsize <- dims/grid
        grid_points <- tidyr::crossing(row = (seq_len(grid[1]) * gridsize[1]) - floor((gridsize[1]/2)), 
            col = (seq_len(grid[2]) * gridsize[2]) - floor((gridsize[2]/2)))
        grid_points <- sf::st_as_sf(dplyr::left_join(grid_points, test_points))
        sample_points <- sf::st_union(sf::st_geometry(grid_points))
    }
    test_voronoi <- sf::st_voronoi(sample_points)
    test_voronoi <- sf::st_transform(test_voronoi, crs = original_crs)
    test_polygons <- sf::st_cast(x = test_voronoi, do_split = T)
    test_boundary <- sf::st_as_sfc(sf::st_bbox(test_raster))
    sf::st_crs(test_boundary) <- sf::st_crs(test_polygons)
    if (!snap) {
        valid_idx <- sf::st_is_valid(test_polygons)
        test_polygons[!valid_idx] <- sf::st_make_valid(test_polygons[!valid_idx])
        rc <- sf::st_intersection(test_boundary, test_polygons)
        attr(rc, "seed") <- seed
        return(rc)
    } else {
        stop("Not yet implemented")
    }
}

## @name create_test_lines See create_test_polygons for details (dimension=1)
create_test_lines <- function(test_raster = create_test_raster(), number = 10, snap = FALSE, 
    randomize = TRUE, seed) {
    seed <- get_or_set_seed(seed)
    if (snap) {
        stop("This functionality is not yet written.")
    }
    dims <- c(max(test_raster$row), max(test_raster$col))
    test_points <- sf::st_centroid(test_raster)
    original_crs <- sf::st_crs(test_points)
    test_points <- sf::st_transform(test_points, crs = 32610)
    if (randomize) {
        sample_points <- sample(sf::st_geometry(test_points[seq_len(prod(dims)), 
            ]), number)
        rc <- sf::st_sf(sample_points)
        rc$idx <- seq_len(nrow(rc))
        sample_points <- sf::st_union(sample_points)
    } else {
        stop("This functionality is not yet written")
    }
    test_voronoi <- sf::st_sf(sf::st_collection_extract(sf::st_voronoi(sample_points)))
    test_voronoi$point_idx <- rc$idx[unlist(sf::st_intersects(test_voronoi, rc))]
    matches <- sf::st_sf(sf::st_intersection(test_voronoi))
    matches$dim <- sf::st_dimension(matches)
    matches <- matches[matches$dim == 1, ]
    matches$match_idx <- seq_len(nrow(matches))
    matches$lhs <- sapply(matches$origins, function(x) {
        x[[1]]
    })
    matches$lhs_point_idx <- test_voronoi$point_idx[matches$lhs]
    matches$rhs <- sapply(matches$origins, function(x) {
        x[[2]]
    })
    matches$rhs_point_idx <- test_voronoi$point_idx[matches$rhs]
    matches$geometry <- sf::st_geometry(matches)
    sf::st_geometry(matches) <- NULL
    rc2 <- sf::st_cast(dplyr::summarize(dplyr::group_by(sf::st_as_sf(dplyr::rename(dplyr::left_join(dplyr::select(tibble::as_tibble(tidyr::gather(matches, 
        side, idx, lhs_point_idx, rhs_point_idx)), -geometry), rc), geometry = sample_points)), 
        match_idx)), "LINESTRING")
    rc2 <- sf::st_transform(rc2, crs = original_crs)
    keep_idx <- sample(nrow(rc2), rpois(1, nrow(rc)))
    rc <- rc2[keep_idx, ]
    attr(rc, "seed") <- seed
    return(rc)
}

## @name create_test_points See create_test_polygons for details (dimension=0)
create_test_points <- function(test_raster = create_test_raster(), number = 10, snap = FALSE, 
    randomize = TRUE, seed) {
    seed <- get_or_set_seed(seed)
    test_points <- sf::st_centroid(test_raster)
    original_crs <- sf::st_crs(test_points)
    dims <- c(max(test_raster$row), max(test_raster$col))
    test_points <- test_points[sample(prod(dims), number), ]
    attr(test_points, "seed") <- seed
    return(test_points)
}


#' @export
#' @name create_test_polygons
#' @title create_test_polygons
#' @description Create a set of polygons to use for simulating covariates or observation regions
#' @param test_raster raster::raster A raster to cover with interior disjoint polygons
#' @param number integer The number of polygons to create
#' @param snap boolean Whether to snap the edges of the created polygons to the raster grid
#' @param randomize boolean Whether to randomly (as opposed to uniformly) generate the polygons
#' @param dimension integer The dimension of polygons to create (0,1, or 2)
#' @param seed integer A seed to use for the randomly constructed portions of this object
#' @export
create_test_polygons <- function(test_raster = create_test_raster(), number = 10, 
    snap = FALSE, randomize = TRUE, dimension = 2, seed) {
    seed <- get_or_set_seed(seed)
    if (dimension == 2) {
        return(create_test_2d_polygons(test_raster = test_raster, number = number, 
            snap = snap, randomize = randomize, seed = seed))
    }
    if (dimension == 1) {
        return(create_test_lines(test_raster = test_raster, number = number, snap = snap, 
            randomize = randomize, seed = seed))
    }
    if (dimension == 0) {
        return(create_test_points(test_raster = test_raster, number = number, snap = snap, 
            randomize = randomize, seed = seed))
    }
}


make_layered_polygons_explicit <- function(test_polygons) {
    n_polygon_layers <- length(names(test_polygons)[grepl("name", names(test_polygons))])
    explicit_layered_polygons <- NULL
    all_layer_names <- rlang::syms(paste("name", seq_len(n_polygon_layers), sep = "_"))
    if (n_polygon_layers > 1) {
        for (layer in seq_len(n_polygon_layers)) {
            layer_names <- rlang::syms(paste("name", seq_len(layer), sep = "_"))
            tmp <- dplyr::summarize(dplyr::group_by(test_polygons, !!!layer_names))
            for (layer2 in seq_len(n_polygon_layers)) {
                layer2_name <- rlang::sym(paste("name", layer2, sep = "_"))
                if (!any(grepl(layer2_name, names(tmp)))) {
                  tmp <- dplyr::mutate(tmp, `:=`(!!layer2_name, NA))
                }
            }
            if (!is.null(explicit_layered_polygons)) {
                explicit_layered_polygons <- rbind(explicit_layered_polygons, tmp[, 
                  c(paste("name", seq_len(n_polygon_layers), sep = "_"), "geometry")])
            } else {
                explicit_layered_polygons <- tmp[, c(paste("name", seq_len(n_polygon_layers), 
                  sep = "_"), "geometry")]
            }
        }
    } else {
        explicit_layered_polygons <- test_polygons
    }
    explicit_layered_polygons <- tidyr::unite(explicit_layered_polygons, "location", 
        !!!all_layer_names, sep = "::", na.rm = TRUE)
    return(explicit_layered_polygons)
}


#' @export
#' @name create_test_layered_polygons
#' @title create_test_layered_polygons
#' @description Create a layered set of polygons to use for simulating covariates
#' or observation regions.  Each layer completely covers the same underlying area
#' @param test_raster raster::raster The raster to cover with polygons
#' @param base_number integer The number of polygons in the coarsest layer
#' @param factor integer The factor by which to increase the number of polygons between layers
#' @param n_layers integer The number of layers of polygons to make.
#' @param snap boolean Whether to snap the edges of the polygon to the boundaries of raster cells
#' @param randomize boolean Whether to randomly generate the polygons (as opposed to uniformly)
#' @param seed integer A seed to use for the randomly constructed portions of this object
create_test_layered_polygons <- function(test_raster = create_test_raster(), base_number = 4, 
    n_layers = 3, factor = 3, snap = FALSE, randomize = TRUE, seed) {
    seed <- get_or_set_seed(seed)
    test_boundary <- sf::st_as_sfc(sf::st_bbox(create_test_extent()))
    sf::st_crs(test_boundary) <- sf::st_crs(test_raster)
    layers <- list(`0` = sf::st_sf(geometry = test_boundary, name_0 = "0"))
    rc <- sf::st_sf(geometry = layers[[1]])
    smallest_layer <- sf::st_sf(geometry = create_test_polygons(test_raster = test_raster, 
        number = base_number * factor^(n_layers - 1), randomize = randomize, snap = snap, 
        dimension = 2, seed = seed))
    smallest_layer[[paste("name", n_layers, sep = "_")]] <- seq_len(nrow(smallest_layer))
    for (layer in rev(seq_len(n_layers - 1))) {
        parent_name <- rlang::sym(paste("name", layer + 1, sep = "_"))
        layer_name <- rlang::sym(paste("name", layer, sep = "_"))
        previous_layer_names <- unique(smallest_layer[[parent_name]])
        start_names <- sample(previous_layer_names, length(previous_layer_names)/factor)
        smallest_layer[[layer_name]] <- 1 * NA
        counter <- 1
        for (name in start_names) {
            smallest_layer[smallest_layer[[parent_name]] == name, paste(layer_name)] <- counter
            counter <- counter + 1
        }
        ## While any are unclassified
        while (any(is.na(smallest_layer[[layer_name]]))) {
            tmp <- dplyr::summarize(dplyr::group_by(smallest_layer, !!layer_name), 
                size = length(!!layer_name))
            tmp <- tmp[!is.na(tmp[[layer_name]]), ]
            tmp_frame <- smallest_layer[is.na(smallest_layer[[layer_name]]), ]
            intersections <- sf::st_intersects(tmp, tmp_frame)
            idx <- sample(length(intersections), 1, prob = 1/tmp$size)
            if (length(intersections[[idx]]) > 0) {
                polygon_idx <- intersections[[idx]][sample(length(intersections[[idx]]), 
                  1)]
                polygon_name <- tmp_frame[[parent_name]][[polygon_idx]]
                if (!sf::st_intersects(tmp_frame[polygon_idx, ], tmp[idx, ], sparse = FALSE)) {
                  stop("This code is not yet written")
                }
                idx2 <- smallest_layer[[parent_name]] == polygon_name
                if (!sf::st_intersects(tmp_frame[polygon_idx, ], sf::st_union(smallest_layer[idx2, 
                  ]), sparse = FALSE)) {
                  stop("This code is not yet written")
                }
                smallest_layer[idx2, paste(layer_name)] <- tmp[[layer_name]][idx]
            }
        }

    }
    smallest_layer <- make_layered_polygons_explicit(smallest_layer)
    attr(smallest_layer, "seed") <- seed
    return(smallest_layer)
}


## @name independent_covariate See create_test_covariate for details
independent_covariate <- function(test_raster, spatially_variable, temporally_variable, 
    seed) {
    seed <- get_or_set_seed(seed)
    n_grid <- nrow(test_raster)
    n_spatial <- max(test_raster$id)
    n_temporal <- max(test_raster$t)
    rc <- matrix(0, nrow = n_spatial, ncol = n_temporal)
    if (spatially_variable & temporally_variable) {
        rc <- matrix(scale(rnorm(n_grid)), nrow = n_spatial, ncol = n_temporal)
    } else if (spatially_variable) {
        rc <- matrix(scale(rnorm(n_spatial)), nrow = n_spatial, ncol = n_temporal)
    } else if (temporally_variable) {
        rc <- matrix(scale(rnorm(n_temporal)), nrow = n_spatial, ncol = n_temporal, 
            byrow = TRUE)
    }
    attr(rc, "seed") <- seed
    return(rc)
}

## @name smoothed_covariate See create_test_covariate for details
smoothed_covariate <- function(test_raster, spatially_variable, temporally_variable, 
    smoothing_function, rho, seed) {
    seed <- get_or_set_seed(seed)
    n_grid <- nrow(test_raster)
    n_spatial <- max(test_raster$id)
    n_temporal <- max(test_raster$t)
    dims <- c(max(test_raster$row), max(test_raster$col), max(test_raster$t))
    rc <- matrix(0, nrow = n_spatial, ncol = n_temporal)
    ## Construct adjacency matrix
    if (spatially_variable & temporally_variable) {
        arr <- array(NA, dims)
    } else if (spatially_variable) {
        arr <- array(NA, dims[seq_len(2)])
    } else if (temporally_variable) {
        arr <- array(NA, dims[3])
    }
    if (spatially_variable || temporally_variable) {
        adjacency <- array_to_adjacency(arr)
        centers <- array_to_centers(arr)
        diagonal <- Matrix::Diagonal(nrow(adjacency), Matrix::rowSums(adjacency))

        covariance <- diagonal - rho * adjacency

        if (spatially_variable & temporally_variable) {
            rc <- (matrix(scale(smoothing_function(1, rep(0, n_grid), covariance, 
                centers)), nrow = n_spatial, ncol = n_temporal))
        } else if (spatially_variable) {
            rc <- (matrix(scale(smoothing_function(1, rep(0, n_spatial), covariance, 
                centers)), nrow = n_spatial, ncol = n_temporal))
        } else if (temporally_variable) {
            rc <- (matrix(scale(smoothing_function(1, rep(0, n_temporal), covariance, 
                centers)), nrow = n_spatial, ncol = n_temporal, byrow = TRUE))
        }
    }
    attr(rc, "seed") <- seed
    return(rc)
}

## @name polygonal_covariate See create_test_covariate for details
polygonal_covariate <- function(test_raster, polygonal, polygons, seed) {
    seed <- get_or_set_seed(seed)

    n_grid <- nrow(test_raster)
    n_spatial <- max(test_raster$id)
    n_temporal <- max(test_raster$t)
    rc <- NULL
    if (!polygonal) {
        rc <- matrix(0, nrow = n_spatial, ncol = n_temporal)
    } else {
        polygonal_contribution <- rep(0, times = c(nrow(test_raster)))
        tmp_names <- names(polygons)
        tmp_names <- tmp_names[tmp_names != "geometry"]
        for (name in tmp_names) {
            polygons[[paste(name, "value", sep = "_")]] <- rnorm(nrow(polygons))
            test_points <- sf::st_centroid(test_raster)
            tmp <- sf::st_intersects(polygons, test_points)
            for (p_idx in seq_len(nrow(polygons))) {
                polygonal_contribution[tmp[[p_idx]]] <- polygonal_contribution[tmp[[p_idx]]] + 
                  polygons[[paste(name, "value", sep = "_")]][p_idx]
            }
        }
        rc <- matrix(scale(polygonal_contribution), nrow = n_spatial, ncol = n_temporal)
    }

    attr(rc, "seed") <- seed
    return(rc)
}

## @name radiating_covariate See create_test_covariate for details
radiating_covariate <- function(test_raster, radiating, radiating_polygons, radiating_means, 
    radiation_function, seed) {
    seed <- get_or_set_seed(seed)
    n_grid <- nrow(test_raster)
    n_spatial <- max(test_raster$id)
    n_temporal <- max(test_raster$t)
    rc <- matrix(0, nrow = n_spatial, ncol = n_temporal)
    if (radiating) {
        raster_points <- sf::st_centroid(test_raster)

        radiation_contribution <- apply(sf::st_distance(raster_points, radiating_polygons), 
            1, function(distances) {
                sum(radiation_function(mu = radiating_means, x = distances))
            })
        rc <- matrix(scale(radiation_contribution), nrow = n_spatial, ncol = n_temporal)
    }

    attr(rc, "seed") <- seed
    return(rc)
}

## @name constant_covariate See create_test_covariate for details
constant_covariate <- function(test_raster, constant, seed) {
    seed <- get_or_set_seed(seed)
    n_spatial <- max(test_raster$id)
    n_temporal <- max(test_raster$t)
    value <- ifelse(constant, 1, 0)
    rc <- matrix(value, nrow = n_spatial, ncol = n_temporal)
    attr(rc, "seed") <- seed
    return(rc)
}


#' @export
#' @name create_test_covariate
#' @title create test covariate
#' @description Create a test covariate.  The covariate is the sum of various layers. Each layer is governed by various flags and parameters.  The components are:
#'   independent : each cell is drawn from rnorm(0,1);
#'   locally correlated : cells are drawn from mvrnorm(0,Sigma), where Sigma is based on the adjacency of gridcells;
#'   polygonal : Polygons are assigned random values, and each cell is the sum of the value of any polygon it is contained in;
#'   radiating : Polygons are assigned random values, and each cell is assigned a value based on the distance to each polygon;
#'   temporal trend : Work in progress;
#'   and temporal cyclic : Work in progress.
#' @param test_raster raster::raster The raster to make the covariate over.
#' It should have one layer per time step
#' @param nonspatial boolean Whether the independent layer varies in space
#' @param nontemporal boolean Whether the independent layer varies in time
#' @param spatially_smooth boolean Whether the locally correlated layer is spatially correlated
#' @param temporally_smooth boolean Whether the locally correlated layer is temporally correlated
#' @param polygonal boolean Whether to include the polygonal layer
#' @param radiating boolean Whether to include the radiating layer
#' @param constant boolean Whether to include a constant layer
#' @param rho numeric A parameter governing spatial correlation used to determine the covariance matrix
#' @param smoothing_function function(n ,mu, covariance, centers) The function to determine the covariate matrix for the multivariate normal to draw from
#' @param polygons sf::sf The polygons to use for the polygonal layer
#' @param radiation_function function(x, mu) The function that determines how quickly the radiation decays in the radiating layer
#' @param radiating_polygons sf::sf The polygons that the radiating layer radiates from
#' @param radiating_means numeric The values that the radiating layer radiates away from (one for each polygon)
#' @param weights numeric(5) How heavily each layer contributes to the final result
#' @param family character Currently unused
#' @param seed integer A seed to use for the randomly constructed portions of this object
create_test_covariate <- function(test_raster = create_test_raster(), nonspatial = TRUE, 
    nontemporal = TRUE, spatially_smooth = TRUE, temporally_smooth = TRUE, polygonal = TRUE, 
    radiating = TRUE, constant = FALSE, rho = 0.999999, smoothing_function = function(n, 
        mu, covariance, centers) {
        return(scale(MASS::mvrnorm(n = n, mu = mu, Matrix::solve(covariance))))
    }, polygons = create_test_layered_polygons(), radiation_function = function(x, 
        mu) {
        mu * exp(-(x/10000)^2)
    }, radiating_polygons = sf::st_sf(sf::st_union(create_test_polygons(dimension = 1))), 
    radiating_means = rnorm(nrow(radiating_polygons)), weights = c(1, 1, 1, 1, 1), 
    family = "Gaussian", seed) {
    seed <- get_or_set_seed(seed)
    if (family != "Gaussian") {
        stop("This is not yet implemented")
    }

    n_grid <- nrow(test_raster)
    n_spatial <- max(test_raster$id)
    n_temporal <- max(test_raster$t)

    ## Start at 0 and add covariates
    test_raster[["covariate"]] <- 0
    ## Add in covariates covariate functions return appropriately sized matrices which
    ## are 0 if the covariate is not used
    test_raster[["covariate"]] <- test_raster[["covariate"]] + as.numeric(independent_covariate(test_raster, 
        nonspatial, nontemporal, seed = seed) * weights[1])
    test_raster[["covariate"]] <- test_raster[["covariate"]] + as.numeric(smoothed_covariate(test_raster, 
        spatially_smooth, temporally_smooth, smoothing_function, rho, seed = seed) * 
        weights[2])
    test_raster[["covariate"]] <- test_raster[["covariate"]] + as.numeric(polygonal_covariate(test_raster, 
        polygonal, polygons, seed = seed) * weights[3])
    test_raster[["covariate"]] <- test_raster[["covariate"]] + as.numeric(radiating_covariate(test_raster, 
        radiating, radiating_polygons, radiating_means, radiation_function, seed = seed) * 
        weights[4])
    test_raster[["covariate"]] <- test_raster[["covariate"]] + as.numeric(constant_covariate(test_raster, 
        constant, seed = seed) * weights[5])

    attr(test_raster, "seed") <- seed
    return(test_raster)
}


#' @export
#' @name create_multiple_test_covariates
#' @title create multiple test covariates
#' @param test_raster raster::raster The raster to make the covariate over.
#' It should have one layer per time step
#' @param ncovariates integer the number of covariates to make
#' @param nonspatial boolean(ncovariates) Whether the independent layer varies in space for each covariate
#' @param nontemporal boolean(ncovariates) Whether the independent layer varies in time for each covariate
#' @param spatially_smooth boolean(ncovariates) Whether the locally correlated component is spatially correlated for each covariate
#' @param temporally_smooth boolean(ncovariates) Whether the locally correlated component is temporally correlated for each covariate
#' @param polygonal boolean(ncovariates) Whether to include the polygonal layer for each covariate
#' @param radiating boolean(ncovariates) Whether to include the radiating layer for each covariate
#' @param constant boolean(ncovariates) Whether to include a constant layer for each covariate
#' @param rho numeric(ncovariates) A parameter governing spatial correlation used to determine the covariance matrix for each covariate
#' @param smoothing_function list(function(n ,mu, covariance, centers)) The function to determine the covariate matrix for the multivariate normal to draw from for each covariate
#' @param polygons sf::sf The polygons to use for the polygonal layer
#' @param radiation_function function(x, mu) The function that determines how quickly the radiation decays in the radiating layer
#' @param radiating_polygons list(sf::sf) The polygons that the radiating layer radiates from for each covariate
#' @param radiating_means list(numeric()) A list with one element for each covariate.  The list elements should have a number for each radiating_polygon for that covariate which is the value that radiates away from that polygon
#' @param weights list(numeric(5)) How heavily each layer contributes to the final result for each covariate
#' @param magnitude numeric(ncovariates) The variance over gridcells for each covariate (i.e., sets the relative scale for that covariate)
#' @param family character Currently unused
#' @param seed integer A seed to use for the randomly constructed portions of this object
create_multiple_test_covariates <- function(test_raster = create_test_raster(), ncovariates = 5, 
    nonspatial = c(TRUE, FALSE, FALSE, FALSE, FALSE), nontemporal = c(FALSE, TRUE, 
        FALSE, FALSE, FALSE, TRUE), spatially_smooth = c(FALSE, FALSE, TRUE, TRUE, 
        FALSE), temporally_smooth = c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE), polygonal = c(TRUE, 
        FALSE, FALSE, FALSE, FALSE), radiating = c(FALSE, TRUE, FALSE, FALSE, TRUE), 
    constant = c(FALSE, FALSE, FALSE, FALSE, FALSE), rho = rep(0.999999, times = ncovariates), 
    polygons = create_test_layered_polygons(), radiating_polygons = list(NA, create_test_polygons(dimension = 0, 
        number = 2), NA, NA, sf::st_union(create_test_polygons(dimension = 1))), 
    smoothing_function = rep(list(function(n, mu, covariance, centers) {
        return(scale(MASS::mvrnorm(n = n, mu = mu, Matrix::solve(covariance))))
    }), ncovariates), radiation_function = rep(list(function(x, mu) {
        mu * exp(-(x/10000)^2)
    }), ncovariates), radiating_means = list(NA, rnorm(2), NA, NA, 1), weights = rep(list(c(0.3, 
        1, 1, 1, 1)), ncovariates), magnitude = rnorm(ncovariates), family = "Gaussian", 
    seed) {
    seed <- get_or_set_seed(seed)
    rc <- list()
    for (idx in seq_len(ncovariates)) {
        tmp <- create_test_covariate(test_raster = test_raster, nonspatial = nonspatial[idx], 
            nontemporal = nontemporal[idx], spatially_smooth = spatially_smooth[idx], 
            temporally_smooth = temporally_smooth[idx], polygonal = polygonal[idx], 
            radiating = radiating[[idx]], constant = constant[[idx]], rho = rho[idx], 
            polygons = polygons, smoothing_function = smoothing_function[[idx]], 
            radiation_function = radiation_function[[idx]], radiating_polygons = radiating_polygons[[idx]], 
            radiating_means = radiating_means[[idx]], weights = weights[[idx]], family = family, 
            seed = seed)
        if (length(unique(as.vector(tmp[["covariate"]]))) > 1) {
            tmp[["covariate"]] <- scale(tmp[["covariate"]]) * magnitude[idx]
        } else {
            tmp[["covariate"]] <- magnitude[idx]
        }

        tmp$geometry <- sf::st_geometry(tmp)
        tmp <- sf::st_as_sf(sf::st_drop_geometry(tmp))
        tmp$covariate <- as.numeric(tmp$covariate)
        rc[[idx]] <- tmp
    }
    attr(rc, "seed") <- seed
    return(rc)
}

#' @export
#' @name create_underlying_distribution
#' @title create_underlying_distribution
#' @description Create a distribution of ground truth cholera rates
#' @param covariates The covariates to use when creating the distribution.  Create using create_test_covariates
#' @param normalization The normalization function to use when computing the rate raster.  This function is used to scale the lambda 'covariate' so that it is within a sensible range.
#' @param family The family of distribution to use (only Poisson supported for now)
#' @param seed integer A seed to use for the randomly constructed portions of this object
create_underlying_distribution <- function(covariates = create_multiple_test_covariates(), 
    normalization = function(x) {
        if (length(unique(x[])) == 1) {
            return(x * 0 + 1)
        }
        x <- exp(x)
        x[] <- 0.1 * ((x[] - min(x[]))/(max(x[]) - min(x[])))
        return(x)
    }, family = "Poisson", seed) {
    seed <- get_or_set_seed(seed)
    offset <- 10^covariates[[1]][["covariate"]] + 1
    offset[offset >= 2^(32)] <- 2^32 - 1
    if (length(covariates) > 1) {
        covariates <- covariates[-1]
    } else {
        covariates[[1]] <- covariates[[1]] * 0 + 1
    }
    if (length(covariates) <= 0) {
        stop("Covariates should be a list of sf objects representing spacetime grids")
    }
    if (!all(sapply(covariates, function(x) {
        nrow(x) == nrow(covariates[[1]])
    }))) {
        stop("All covariates must have the number of cells")
    }
    lambdas <- Reduce(function(x, y) {
        return(x + y[["covariate"]])
    }, covariates, init = covariates[[1]][["covariate"]] * 0)
    lambdas <- normalization(lambdas)
    tmp <- lambdas
    lambdas <- covariates[[1]]
    lambdas[["covariate"]] <- NULL
    lambdas[["rate"]] <- tmp
    lambdas[["cases"]] <- tmp * offset
    cases <- offset * tmp
    fun <- function(n) {
        rc <- list()
        for (i in seq_len(n)) {
            rc[[i]] <- covariates[[1]]
            rc[[i]][["cases"]] <- rpois(length(cases), cases[])
            rc[[i]][["draw"]] <- i
        }
        return(do.call(sf:::rbind.sf, rc))
    }
    rc <- list(mean = lambdas, rcases = fun)
    attr(rc, "seed") <- seed
    return(rc)
}

#' @export
#' @name observe_gridcells
#' @title observe_gridcells
#' @description Simulate observation of the integral of the underlying distribution over the grid
#' @param underlying_distribution The distribution to observe.  Create with create_underlying_distribution
#' @param proportion_observed The percent of gridcells whose value is observed
#' @param number_draws The number of independent observations to simulate
#' @param spatial_observation_bias Whether to choose gridcells to observe in a spatially biased way
#' @param temporal_observation_bias Whether to choose gridcells to observe in a temporally biased way
#' @param value_observation_bias Whether to choose gridcells to observe biased by the number of cases
#' @param noise Whether to add noise to the observations
#' @param seed integer A seed to use for the randomly constructed portions of this object
observe_gridcells <- function(underlying_distribution = create_underlying_distribution(), 
    proportion_observed = 1, number_draws = 1, spatial_observation_bias = TRUE, temporal_observation_bias = TRUE, 
    value_observation_bias = TRUE, noise = FALSE, seed) {
    seed <- get_or_set_seed(seed)
    if (noise) {
        stop("This is not yet implemented")
    }
    results <- underlying_distribution$rcases(number_draws)
    spatial_weights <- 0
    value_weights <- 0
    temporal_weights <- 0

    if (value_observation_bias) {
        value_weights <- (results$cases + 1)/sum(results$cases + 1)
        if (length(unique(value_weights)) > 1) {
            value_weights <- as.vector(scale(value_weights))
        } else {
            value_weights <- seq_len(length(value_weights)) * 0 + 1
        }
    }
    if (temporal_observation_bias) {
        temporal_weights <- create_test_covariate(test_raster = results, nonspatial = FALSE, 
            nontemporal = FALSE, spatially_smooth = FALSE, temporally_smooth = TRUE, 
            polygonal = FALSE, radiating = FALSE, constant = FALSE)[["covariate"]]
        temporal_weights <- scale(temporal_weights)
    }
    if (spatial_observation_bias) {
        spatial_weights <- create_test_covariate(test_raster = results, nonspatial = FALSE, 
            nontemporal = FALSE, spatially_smooth = TRUE, temporally_smooth = FALSE, 
            polygonal = FALSE, radiating = FALSE, constant = FALSE)[["covariate"]]
        spatial_weights <- scale(spatial_weights)
    }
    n <- nrow(results)
    n_samp <- rbinom(1, n, proportion_observed)
    indices <- sample(seq_len(n), n - n_samp, prob = seq_len(n) * 0 + exp(spatial_weights + 
        value_weights + temporal_weights))
    for (i in seq_len(number_draws)) {
        results[indices, "cases"] <- NA
    }
    attr(results, "seed") <- seed
    return(results)
}

#' @export
#' @name observe_polygons
#' @title observe_polygons
#' @param test_polygons A set of polygons to (potentially) observe cases over
#' @param test_covariates A set of covariates to create a distribution out of (not needed if underlying distribution is specified)
#' @param underlying_distribution An underlying distribution of cases
#' @param grid_proportion_observed What percentage of gridcells get observed by this process
#' @param number_draws Number of independent draws from the underlying distribution present
#' @param grid_spatial_observation_bias Whether grid observation is spatially correlated (see observe_gridcells)
#' @param grid_spatial_observation_bias Whether grid observation is temporally correlated (see observe_gridcells)
#' @param grid_spatial_observation_bias Whether grid observation is correlated to number of cases (see observe_gridcells)
#' @param noise Whether or not to include observation noise at the gridcell level
#' @param polygon_proportion_observed Percent of polygons observed (for each draw)
#' @param polygon_observation_rates Not used
#' @param polygon_size_bias Whether or not to sample larger polygons more frequently
#' @param nonlinear_covariates Not used
#' @param min_time_left The first time associated with an observation
#' @param max_time_right The last time associated with an observation
#' @param seed A random seed
observe_polygons <- function(test_polygons = create_test_layered_polygons(), test_covariates = create_multiple_test_covariates(polygons = test_polygons), 
    underlying_distribution = create_underlying_distribution(covariates = test_covariates), 
    grid_proportion_observed = 1, number_draws = 1, grid_spatial_observation_bias = TRUE, 
    grid_temporal_observation_bias = TRUE, grid_value_observation_bias = TRUE, noise = FALSE, 
    polygon_proportion_observed = 1, polygon_observation_rates = exp(rnorm(nrow(test_polygons), 
        -1)), polygon_observation_idx = NA, polygon_size_bias = TRUE, nonlinear_covariates = FALSE, 
    min_time_left = lubridate::ymd("2000-01-01"), max_time_right = lubridate::ymd("2001-01-01"), 
    do_temporal_subset = FALSE, seed) {
    seed <- get_or_set_seed(seed)
    observed_grid <- observe_gridcells(underlying_distribution = underlying_distribution, 
        proportion_observed = grid_proportion_observed, number_draws = number_draws, 
        spatial_observation_bias = grid_spatial_observation_bias, temporal_observation_bias = grid_temporal_observation_bias, 
        value_observation_bias = grid_value_observation_bias, noise = noise, seed = seed)

    all_draws <- NULL
    nlayers <- max(observed_grid$t)
    minmax <- as.integer(c(NA, NA))
    intersections <- sf::st_drop_geometry(sf::st_intersection(test_polygons, sf::st_centroid(observed_grid)))

    time_censored_observations <- intersections %>%
        dplyr::group_by(location, draw) %>%
        dplyr::group_map(function(.x, .y) {
            minmax <- c(1, nlayers)
            if (do_temporal_subset) {
                minmax <- sort(sample(nlayers, 2, replace = TRUE))
            }
            time_bounds <- (max_time_right - min_time_left) * (minmax - c(1, 0))/nlayers + 
                lubridate::as_datetime(min_time_left)
            time_bounds <- as.Date(time_bounds)
            tfrac <- (minmax[2] - minmax[1] + 1)/(nlayers)
            .y$tmin <- minmax[1]
            .y$tmax <- minmax[2]
            .x <- cbind(.x, .y)
            .x <- .x[(.x$t >= .x$tmin) & (.x$t <= .x$tmax), ]
            .x <- .x %>%
                dplyr::group_by(location, draw) %>%
                dplyr::summarize(cases = sum(cases, na.rm = TRUE), tmin = unique(tmin), 
                  tmax = unique(tmax), )
            .x$time_left <- time_bounds[1]
            .x$time_right <- time_bounds[2]
            .x$tfrac <- tfrac
            return(.x)
        }) %>%
        do.call(what = rbind) %>%
        dplyr::left_join(test_polygons) %>%
        sf::st_as_sf()

    time_censored_observations$weight <- 1
    if (polygon_size_bias) {
        time_censored_observations$weight <- time_censored_observations$weight * 
            as.numeric(sf::st_area(time_censored_observations))
    }
    if (all(is.na(polygon_observation_idx))) {
        n_observed_observations <- rbinom(1, nrow(time_censored_observations), polygon_proportion_observed)
        polygon_observation_idx <- sample(seq_len(nrow(time_censored_observations)), 
            n_observed_observations, prob = time_censored_observations$weight)
    }
    observed_observations <- sf::st_as_sf(time_censored_observations[polygon_observation_idx, 
        ] %>%
        dplyr::select(location, draw, cases, tmin, tmax, time_left, time_right, tfrac, 
            geometry))
    attr(observed_observations, "seed") <- seed
    return(observed_observations)
}

#' @name create_standardized_test_data
#' @title create_standardized_test_data
#' @description create a testing data set for modeling
#' @export
# Extent parameters: NONE, Raster parameters:
#' @param nrows integer The number of rows the raster should have.
#' @param ncols integer The number of columns the raster should have.
#' @param nlayers integer The number of layers the raster should have.  Each layer is intended to express time.
# Polygon Parameters,
#' @param base_number integer The number of polygons in the coarsest layer
#' @param n_layers integer The number of layers of polygons to make.
#' @param factor integer The factor by which to increase the number of polygons between layers
#' @param snap boolean Whether to snap the edges of the polygon to the boundaries of raster cells
#' @param randomize boolean Whether to randomly generate the polygons (as opposed to uniformly)
# Covariate Parameters
#' @param ncovariates integer the number of covariates to make
#' @param nonspatial boolean(ncovariates) Whether the independent layer varies in space for each covariate
#' @param nontemporal boolean(ncovariates) Whether the independent layer varies in time for each covariate
#' @param spatially_smooth boolean(ncovariates) Whether the locally correlated layer is spatially correlated for each covariate
#' @param temporally_smooth boolean(ncovariates) Whether the locally correlated layer is temporally correlated for each covariate
#' @param polygonal boolean(ncovariates) Whether to include the polygonal layer for each covariate
#' @param radiating boolean(ncovariates) Whether to include the radiating layer for each covariate
#' @param constant boolean(ncovariates) Whether to include the constant layer for each covariate
#' @param rho numeric(ncovariates) A parameter governing spatial correlation used to determine the covariance matrix for each covariate
#' @param radiating_polygons list(sf::sf) The polygons that the radiating layer radiates from for each covariate
#' @param smoothing_function list(function(n ,mu, covariance, centers)) The function to determine the smooth part of each covariate in terms of a covariance matrix (or array centers).
#' @param radiation_function function(x, mu) The function that determines the decayed value x units away from a polygon of value mu
#' @param radiating_means list(numeric()) A list with one element for each covariate.  The list elements should have a number for each radiating_polygon for that covariate which is the value that radiates away from that polygon
#' @param weights list(numeric(5)) How heavily each layer contributes to the final result for each covariate
#' @param magnitude numeric(ncovariates) The variance over gridcells for each covariate (i.e., sets the relative scale for that covariate)
#' @param family character Currently unused
# Distribution Parameters
#' @param normalization The normalization function to use when computing the rate raster.  This function is used to scale the lambda 'covariate' so that it is within a sensible range.
# Grid Observation Parameters
#' @param grid_proportion_observed The percent of gridcells whose value is observed (0-1)
#' @param number_draws The number of independent realizations of the underlying distribution
#' @param grid_spatial_observation_bias Whether to choose gridcells to observe in a spatially biased way
#' @param grid_temporal_observation_bias Whether to choose gridcells to observe in a temporally biased way
#' @param grid_value_observation_bias Whether to choose gridcells to observe biased by the number of cases
#' @param noise Whether to add noise to the observations
# Polygon Observation Parameters
#' @param polygon_proportion_observed Percent of polygons observed (for each draw) (0-1)
#' @param polygon_observation_rates Not used
#' @param polygon_size_bias Whether or not to sample larger polygons more frequently
#' @param polygon_observation_idx integer or NA.  If not NA, then this is the indices of all polygons to observe.
#' @param nonlinear_covariates Not used
#' @param min_time_left The first time associated with an observation
#' @param max_time_right The last time associated with an observation
# This functions parameter
#' @param seed integer A seed to use for the randomly constructed portions of this object
#' @export
create_standardized_test_data <- function(nrows = 8, ncols = 8, nlayers = 2, base_number = 4, 
    n_layers = 3, factor = 4, snap = FALSE, randomize = FALSE, ncovariates = 5, nonspatial = c(TRUE, 
        FALSE, FALSE, FALSE, FALSE), nontemporal = c(FALSE, TRUE, FALSE, FALSE, FALSE), 
    spatially_smooth = c(FALSE, FALSE, TRUE, TRUE, FALSE), temporally_smooth = c(FALSE, 
        FALSE, FALSE, TRUE, FALSE), polygonal = c(TRUE, FALSE, FALSE, FALSE, FALSE), 
    radiating = c(FALSE, TRUE, FALSE, FALSE, TRUE), constant = c(FALSE, FALSE, FALSE, 
        FALSE, FALSE), rho = rep(0.999999, times = ncovariates), radiating_polygons = list(NA, 
        create_test_polygons(dimension = 0, number = 2), NA, NA, sf::st_union(create_test_polygons(dimension = 1))), 
    radiation_function = rep(list(function(x, mu) {
        mu * exp(-(x/10000)^2)
    }), ncovariates), radiating_means = list(NA, rnorm(2), NA, NA, 1), smoothing_function = rep(list(function(n, 
        mu, covariance, centers) {
        return(scale(MASS::mvrnorm(n = n, mu = mu, Matrix::solve(covariance))))
    }), ncovariates), weights = rep(list(c(0.3, 1, 1, 1, 1)), ncovariates), magnitude = c(6, 
        rnorm(ncovariates - 1)), family = "Gaussian", normalization = function(x) {
        if (length(unique(x[])) == 1) {
            return(x * 0 + 1)
        }
        x <- exp(x)
        x[] <- 0.1 * ((x[] - min(x[]))/(max(x[]) - min(x[])))
        return(x)
    }, grid_proportion_observed = 1, number_draws = 1, grid_spatial_observation_bias = TRUE, 
    grid_temporal_observation_bias = TRUE, grid_value_observation_bias = TRUE, noise = FALSE, 
    polygon_proportion_observed = 0.1, polygon_observation_rates = exp(rnorm(nrow(test_polygons), 
        -1)), polygon_observation_idx = NA, polygon_size_bias = TRUE, nonlinear_covariates = FALSE, 
    min_time_left = lubridate::ymd("2000-01-01"), max_time_right = lubridate::ymd("2001-01-01"), 
    seed) {
    seed <- get_or_set_seed(seed)

    test_extent <- create_test_extent(seed = seed)

    test_raster <- create_test_raster(nrows = nrows, ncols = ncols, nlayers = nlayers, 
        test_extent, seed = .GlobalEnv$.Random.seed)

    test_polygons <- create_test_layered_polygons(test_raster = test_raster, base_number = base_number, 
        n_layers = n_layers, factor = factor, snap = snap, randomize = randomize, 
        seed = .GlobalEnv$.Random.seed)

    test_covariates <- create_multiple_test_covariates(test_raster = test_raster, 
        ncovariates = ncovariates, nonspatial = nonspatial, nontemporal = nontemporal, 
        spatially_smooth = spatially_smooth, temporally_smooth = temporally_smooth, 
        polygonal = polygonal, radiating = radiating, constant = constant, rho = rho, 
        polygons = test_polygons, radiating_polygons = radiating_polygons, smoothing_function = smoothing_function, 
        radiation_function = radiation_function, radiating_means = radiating_means, 
        weights = weights, magnitude = magnitude, family = family, seed = .GlobalEnv$.Random.seed)

    test_underlying_distribution <- create_underlying_distribution(covariates = test_covariates, 
        normalization = normalization, family = family, seed = .GlobalEnv$.Random.seed)

    test_observed_polygons <- observe_polygons(test_polygons = test_polygons, test_covariates = test_covariates, 
        underlying_distribution = test_underlying_distribution, grid_proportion_observed = grid_proportion_observed, 
        number_draws = number_draws, grid_spatial_observation_bias = grid_spatial_observation_bias, 
        grid_temporal_observation_bias = grid_temporal_observation_bias, grid_value_observation_bias = grid_value_observation_bias, 
        noise = noise, polygon_proportion_observed = polygon_proportion_observed, 
        polygon_observation_rates = polygon_observation_rates, polygon_observation_idx = polygon_observation_idx, 
        polygon_size_bias = polygon_size_bias, nonlinear_covariates = nonlinear_covariates, 
        min_time_left = min_time_left, max_time_right = max_time_right, seed = .GlobalEnv$.Random.seed)
    rc <- list(observed_polygons = test_observed_polygons, covariates = test_covariates, 
        underlying_distribution_mean = test_underlying_distribution[["mean"]], underlying_distribution_rcases = test_underlying_distribution[["rcases"]], 
        raster = test_raster, seed = .GlobalEnv$.Random.seed)
    attr(rc, "seed") <- seed
    return(rc)
}
