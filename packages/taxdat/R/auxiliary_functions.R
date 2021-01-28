#' @include readData.R
#' @export classify_date
#' @name classify_date
#' @title classify_date
#' @description Take the length of a report and classify it as daily, weekly, biweekly, monthly, bimonthly, yearly, or biyearly.
#' @param date The length of time as a difftime.
classify_date = function(date){
  ifelse(date <= 2,
         "day",
         ifelse(
           (date > 5) & (date <= 8),
           'week',
           ifelse(
             (date > 12) & (date <= 16),
             'biweek',
             ifelse(
               (date > 26) & (date <= 32),
               'month',
               ifelse(
                 (date > 56) & (date <= 64),
                 'bimonth',
                 ifelse(
                   (date > 360) & (date <= 370),
                   'year',
                   ifelse(
                     (date > 720) & (date <= 740),
                     'biyear',
                     'NA'
                   )
                 )
               )
             )
           )
         )
  )
}

#' @export get_report_type
#' @name get_report_type
#' @title get_report_type
#' @description This function takes all reports in a taxonomy and classifies them by their duration
#' @param taxonomy A taxonomy read with read_taxonomy_data
get_report_type = function(taxonomy){
  return(
    #' @importFrom dplyr mutate
    mutate(
      taxonomy,
      report_length = TR-TL,
      report_length_class = classify_date(report_length)
    )
  )
}

### Double check what this does before adding documentation
get_report_classes <- function(taxonomy,ISO_L2_level=0){
  if(!('report_length_class' %in% names(taxonomy))){
    taxonomy = get_report_type(taxonomy)
  }
  ISO_cols = 'who_region'
  if(ISO_L2_level >= 0){
    ISO_cols = c(ISO_cols,'ISO_A1')
  } else {
    stop("Not written")
  }
  if(ISO_L2_level > 0){
    for(i in 1:ISO_L2_level){
      ISO_cols = c(ISO_cols,paste('ISO_A2_L',i,sep=''))
    }
  }
  #' @importFrom tidyr spread
  spread(
    #' @importFrom dplyr summarize
    summarize(
      #' @importFrom dplyr group_by_
      group_by_(
        #' @importFrom dplyr filter_
        mutate_(
          taxonomy,
          .dots = setNames(
            list(paste('is.na(ISO_A2_L',ISO_L2_level+ 1,')',sep='')),
            'sublevel'
          )
        ),
        .dots=c(
          ISO_cols,
          'sublevel',
          'report_length_class'
        )
      ),
      count = length(report_length_class)
    ),
    ## Would ideally like to do this and sublevel...
    key = report_length_class,
    value=count,
    fill=0
  )
}

#' @export
#' @name stan_result_apply
#' @title stan_result_apply
### Update the documentation here
#' @param result The result of a (particular kind of) stan model
#' @param fun The function to apply to summarize the results
stan_result_summarize <- function(result,fun){
  lapply(result,function(x){
    if(is.null(dim(x)) || (length(dim(x)) <= 1)){
      fun(x)
    } else {
      apply(x,2:length(dim(x)),fun)
    }
  })
}

#' @export
#' @name rdist_stan_model
#' @title rdist_stan_model
#' @param n The number of times to sample
#' @param result The results of a stan model to draw from
rdist_stan_model = function(n,result){
  rc <- matrix(NA,n,dim(result$log_lambda)[2])
  for(trial in 1:n){
    rc[trial,] <- stan_result_summarize(result['log_lambda'],function(x){quantile(x,runif(1,0,1))})[[1]]
  }
  return(rc)
}

#' @export
#' @name get_observed_cells
#' @title get_observed_cells
#' @description Get the indices of which cells contain meaningful data given a set of observed locations and a population raster of appropriate size
#' @param locs A character vector of location names.  Must be in the standardized form.
#' @param pop A raster of population.  Used both to determine where the grids are, and ignore observations with no population
#' @return An integer vector of indices of pop, where the data is included in at least one location and also has non NA population
get_observed_cells = function(locs,pop,taxonomy_dir='taxonomy_verified',verbose=FALSE){
  contain_observations = unique(unlist(
    sapply(
      locs,
      function(loc){
        #' @importFrom raster extract
        extract(pop,get_shape_file(loc,taxonomy_dir,verbose),cellnumbers=TRUE)[[1]][,'cell']
      }
    )
  ))
  contain_population = which(!is.na(pop.a[]))
  intersect(contain_observations,contain_population)
}

#' @export
#' @name sample_stan_model
#' @title sample_stan_model
#' @description get samples from the results of a stan model
#' @param n The number of times to sample
#' @param result The model results to sample from
#' @param locs A character vector of location names where there are observations
#' @param pop_raster The underlying population raster.  Used for determining the raster shape and finding places with no population
sample_stan_model = function(n,result,locs,pop_raster){
  rc <- raster(pop_raster)
  samples <- rdist_stan_model(n,result)
  non_na_cells <- get_observed_cells(locs,pop_raster)
  rc <- lapply(1:n,function(x){raster(pop_raster)})
  for(idx in 1:n){
    rc[[idx]][non_na_cells] <- samples[idx,]
  }
  return(rc)
}

#' @export
#' @name country_data_overview
#' @title country_data_overview
#' @description produces an overview of data for a country
#' @param country name of country
#' @param taxonomy.dir taxonomy directory
#' @param simplified if TRUE returns a simplified overview of DESC files, if FALSE a table with all details from DESC files
#' @return knitr table with country data overview
country_data_overview <- function(country,taxonomy.dir="taxonomy-verified",simplified=TRUE,pretty=TRUE){

    if(!dir.exists(taxonomy.dir)) stop("check taxonomy directory and your working directory")

    cntry_iso <- fix_country_name(country)
    cntry_dat <- read_description_taxonomy(taxonomy.dir, paste0("grepl('",cntry_iso,"',ISO_A1)"))

    full_epis <- read_epi_taxonomy(taxonomy.dir,uids=cntry_dat$uid)
    cntry_dat <- left_join(cntry_dat,
                           full_epis %>%
                           group_by(uid) %>%
                           summarize(num_obs=n(),
                                     median_obs_time_span=round(median(TR-TL,na.rm=T),0),
                                     min_obs_time_span=round(min(TR-TL,na.rm=T),0),
                                     max_obs_time_span=round(max(TR-TL,na.rm=T),0)
                                     )
                           )

  ## should probably convert to SE for all these functions...
  if(simplified){
    #' @importFrom dplyr select
        cntry_dat <- select(cntry_dat,
           source,day_start,day_end,ISO_A2_L1,ISO_A2_L2,deaths,cases,uid,num_obs,median_obs_time_span)
  }

  if(pretty){
  return(
  #' @importFrom knitr kable
  kable(
    #' @importFrom dplyr arrange
       arrange(cntry_dat,day_start)
  )
  )
  } else {
    return(
    #' @importFrom dplyr arrange
    arrange(cntry_dat,day_start)
    )
  }
}

#' @export
get_log_scores <- function(predicted,actual,lookup,probs){
  predicted = MCMCvis::MCMCchains(predicted)
  samples = abind::abind(predicted[,grep(lookup,colnames(predicted))],along=1)
  qsamp = apply(samples,2,quantile,probs=probs)
  logscores = log(dpois(actual,qsamp))
  logscores[is.infinite(logscores)] <- -10000
  errors <- abs(qsamp - actual)
  # plot(sort(log(errors)))
  return(mean(logscores))
}

#' @export
get_expectation <- function(predicted,lookup,probs){
  predicted = MCMCvis::MCMCchains(predicted)
  samples = abind::abind(predicted[,grep(lookup,colnames(predicted))],along=1)
  qsamp = apply(samples,2,mean)
  return(qsamp)
}

#' @export
get_quantile <- function(predicted,lookup,probs){
  predicted = MCMCvis::MCMCchains(predicted)
  samples = abind::abind(predicted[,grep(lookup,colnames(predicted))],along=1)
  qsamp = apply(samples,2,quantile,probs=probs)
  return(qsamp)
}

#' @export
get_standard_deviation <- function(predicted,lookup){
  predicted = MCMCvis::MCMCchains(predicted)
  samples = abind::abind(predicted[,grep(lookup,colnames(predicted))],along=1)
  qsamp = apply(samples,2,sd)
  return(qsamp)
}

#' @export
non_na_cells = function(raster_layer){
  if('population' %in% names(raster_layer)){
    non_na_gridcells = which(
      (taxdat::aggregate_raster_xlayers(raster_layer,function(x){!any(is.na(x))})[]) &
      ((raster_layer[['population']])[] >= 1)
    )
  } else {
    non_na_gridcells = which(taxdat::aggregate_raster_xlayers(raster_layer,function(x){!any(is.na(x))})[])
  }
  if(length(non_na_gridcells)==0){
    stop("All gridcells are NA")
  }
  gridcell_converter = setNames(1:length(non_na_gridcells),non_na_gridcells)
  names(non_na_gridcells) = gridcell_converter
  return(list(non_na_gridcells,gridcell_converter))
}



## find all primes less than a number
all_primes_less_than <- function(n) {
  n <- as.integer(n)
  if (n > 1e6) {
    stop("n too large")
  }
  primes <- rep(TRUE, n)
  primes[1] <- FALSE
  last_prime <- 2L
  for (i in last_prime:floor(sqrt(n))) {
    primes[seq.int(2L * last_prime, n, last_prime)] <- FALSE
    last_prime <- last_prime + min(which(primes[(last_prime + 1):n]))
  }
  which(primes)
}


## finds all prime factors of a number
all_prime_factors <- function(n) {
  all_primes <- all_primes_less_than(n)
  rc  <- c()
  for (prime in all_primes) {
    while ((n %/% prime) == (n / prime)) {
      n  <- n %/% prime
      rc  <- c(rc, prime)
    }
  }
  return(rc)
}

## split a number into two pieces which multiply to the original number and are factors of two other given numbers
## param n the number to factor
## param a the first factor of n divides a
## param b the second factor of n divides b
split_into_factors <- function(n, a, b) {
  if (n == 1) {
    return(c(1, 1))
  }
  n_factors  <- all_prime_factors(n)
  a_factors  <- all_prime_factors(a)
  b_factors  <- all_prime_factors(b)
  a_exclusive_factors  <- c()
  b_exclusive_factors <- c()

  tmp_a_factors  <- a_factors
  tmp_b_factors  <- b_factors

  shared_factors  <- n_factors
  skip_next  <- FALSE
  for (idx in rev(seq_len(length(n_factors)))) {
    if (skip_next) {
      skip_next  <- FALSE
    } else {
      a_ok  <- n_factors[idx] %in% tmp_a_factors
      b_ok  <- n_factors[idx] %in% tmp_b_factors
      a_exclusive_factors
      if (a_ok & b_ok) {
        tmp_a_factors  <- tmp_a_factors[
          -which(tmp_a_factors == n_factors[idx])[1]
        ]
        tmp_b_factors  <- tmp_b_factors[
          -which(tmp_b_factors == n_factors[idx])[1]
        ]
        if (idx != 1) {
          if (n_factors[idx] == n_factors[idx - 1]) {
            skip_next  <- TRUE
          }
        }
      } else if (a_ok) {
        tmp_a_factors  <- tmp_a_factors[
          -which(a_factors == n_factors[idx])[1]
        ]
        a_exclusive_factors  <- c(a_exclusive_factors, n_factors[idx])
        a_factors  <- a_factors[
          -which(a_factors == n_factors[idx])[1]
        ]
        shared_factors  <- shared_factors[-idx]
      } else if (b_ok) {
        tmp_b_factors  <- tmp_b_factors[
          -which(tmp_b_factors == n_factors[idx])[1]
        ]
        b_exclusive_factors  <- c(b_exclusive_factors, n_factors[idx])
        b_factors  <- b_factors[
          -which(b_factors == n_factors[idx])[1]
        ]
        shared_factors  <- shared_factors[-idx]
      } else {
        stop(paste(
          n,
          "cannot be split into a number that divides",
          a,
          "and a number that divides",
          b
        ))
      }
    }
  }

  for (factor in rev(sort(shared_factors))) {
    if (
      (factor %in% b_factors) &
      (
        (prod(a_exclusive_factors) / a > prod(b_exclusive_factors) / b) |
        (!(factor %in% a_factors))
      )
    ) {
      b_factors  <- b_factors[
        -which(b_factors == factor)[1]
      ]
      b_exclusive_factors  <- c(b_exclusive_factors, factor)
    } else if (factor %in% a_factors) {
      a_factors  <- a_factors[
        -which(a_factors == factor)[1]
      ]
      a_exclusive_factors  <- c(a_exclusive_factors, factor)
    } else {
    }
  }
  return(c(prod(a_exclusive_factors), prod(b_exclusive_factors)))
}

## turn a multidimensional index into a single dimensional index
## param dims The dimension of the array
## idx the multidimensional index
sub2ind <- function(dims, idx) {
  if (isTRUE(any(dims < idx))) {
    stop("Invalid index")
  }
  sum  <- 1
  for (i in seq_len(length(idx))) {
    sum  <- sum + prod(dims[seq_len(i)]) * (idx[i] - 1) / dims[i]
  }
  return(sum)
}

## turn a single dimensional index into a multidimensional index
## param dims The dimension of the array
## idx the single dimensional index
ind2sub <- function(dims, idx) {
  return(arrayInd(idx, dims))
}

## convert between single dimensional and multidimensional indices
convert_index <- function(dims, idx) {
  if (is.null(dim(idx))) {
    if (length(idx) == length(dims)) {
      return(sub2ind(dims, idx))
    }
    if (length(idx) == 1) {
      return(ind2sub(dims, idx))
    }
    return(sapply(idx, ind2sub, dims = dims))
  }
  return(apply(idx, 1, sub2ind, dims = dims))
}

#' @export
#' @name array_to_centers
#' @title array_to_centers
#' @description get the centroids of the gridcells of an array
#' @param arr array The array to get the centers of
array_to_centers <- function(arr) {
  dimensions  <- dim(arr)
  stepsize  <- 1 / (dimensions)
  idx  <- seq_len(prod(dimensions))
  rc <- as.data.frame(t(stepsize * convert_index(dimensions, idx) - stepsize / 2))
  return(rc)
}

#' @export
#' @name array_to_adjacency
#' @title array_to_adjacency
#' @description get the adjacency matrix of the gridcells of an array
#' @param arr array The array to get the centers of
#' @param sparse boolean whether to return a sparse or dense matrix
array_to_adjacency <- function(arr, sparse = TRUE) {
  rc  <- Matrix::sparseMatrix(
    i = 1,
    j = 1,
    x = 0,
    dims = c(prod(dim(arr)), prod(dim(arr)))
  )
  rc  <- Matrix::drop0(rc)
  all_elements  <- tibble::tibble()
  for (idx in seq_len(length(dim(arr)))) {
    tmp  <- tibble::tibble(index = seq_len(dim(arr)[idx]))
    names(tmp)[1] <- paste("dimension", idx, sep = "_")
    if (nrow(all_elements) == 0) {
      all_elements  <- tmp
    } else {
      all_elements  <- tidyr::crossing(all_elements, tmp)
    }
  }
  tmp_elements  <- all_elements[, seq_len(length(dim(arr)))]
  all_elements[["self"]] <- convert_index(dim(arr), tmp_elements)
  for (idx in seq_len(length(dim(arr)))) {
    tmp_elements  <- all_elements[, seq_len(length(dim(arr)))]
    tmp_elements[[paste("dimension", idx, sep = "_")]] <- ifelse(
      tmp_elements[[paste("dimension", idx, sep = "_")]] + 1 <= dim(arr)[[idx]],
      tmp_elements[[paste("dimension", idx, sep = "_")]] + 1,
      NA
    )
    all_elements[[paste("neighbor", idx, sep = "+")]] <-
      convert_index(dim(arr), tmp_elements)

    tmp_elements  <- all_elements[, seq_len(length(dim(arr)))]
    tmp_elements[[paste("dimension", idx, sep = "_")]] <- ifelse(
      tmp_elements[[paste("dimension", idx, sep = "_")]] - 1 > 0,
      tmp_elements[[paste("dimension", idx, sep = "_")]] - 1,
      NA
    )
    all_elements[[paste("neighbor", idx, sep = "-")]] <-
      convert_index(dim(arr), tmp_elements)
  }
  for (idx in seq_len(length(dim(arr)))) {
    self  <- all_elements[["self"]]
    neighbor1  <- all_elements[[paste("neighbor", idx, sep = "-")]]
    neighbor2  <- all_elements[[paste("neighbor", idx, sep = "+")]]
    for (row in seq_len(nrow(all_elements))) {
      if (!is.na(neighbor1[row])) {
        rc[self[row], neighbor1[row]] <- 1
      }
      if (!is.na(neighbor2[row])) {
        rc[self[row], neighbor2[row]] <- 1
      }
    }
  }
  return(rc)
}

#' @description Perform checks to see if a shapefile is valid for use in the pipeline. This function will look at each row of the provided object, run it through several checks, issue a single warning if at least one check fails, and return a report of all failed objects.
#' @param shapefile an sf or sfc object
#' @export
is_shapefile_valid <- function(shapefile, allowed_geometry_types = c("POLYGON","MULTIPOLYGON")){

  if("sfc" %in% class(shapefile)){
    shapefile <- sf::st_sf(shapefile)
  }
  if(!"sf" %in% class(shapefile)){
    stop("This function only supports sf and sfc objects")
  }

  rc <- list(valid = rep(TRUE,times = nrow(shapefile)), notes = lapply(seq_len(nrow(shapefile)),function(x){return(list())}))

  geometries <- sf::st_sfc(unique(sf::st_geometry(shapefile)))
  geometry_indices <- match(sf::st_geometry(shapefile),geometries)

  ## DIMENSION
  geometry_dimensions <- sf::st_dimension(geometries)
  valid_dimensions <- geometry_dimensions == 2
  valid_dimensions[is.na(valid_dimensions)] <- FALSE
  if(!all(valid_dimensions)){
    warning("At least one geometry had an invalid dimension.  See output for details")
    invalid_dimension <- which(geometry_indices %in% which(!valid_dimensions))
    for(idx in invalid_dimension){
      rc$notes[[idx]]$dimension <- paste("This location has dimension",geometry_dimensions[geometry_indices[idx] ],"but only dimension 2 are allowed.")
      if(isTRUE(geometry_dimensions[geometry_indices[idx] ] == 0)){
        rc$notes[[idx]]$dimension <- paste(rc$notes[[idx]]$dimension,"This location has dimension 0, which may be ok if it is a special location (e.g., hospital) best represented by a point")
      }
    }
  }

  ## TOPOLOGY
  valid_topology <- sf::st_is_valid(geometries)
  if(!all(valid_topology)){
    warning("At least one shapefile had invalid topology.  See output for details")
    invalid_topology <- which(geometry_indices %in% which(!valid_topology))
    rc$valid[invalid_topology] <- FALSE
    for(idx in invalid_topology){
      rc$notes[[idx]]$topology <- "This location had an invalid topology."
    }
    geometries[!valid_topology] <- sf::st_make_valid(geometries[!valid_topology])
    new_valid_topology <- sf::st_is_valid(geometries)
    fixed_topologies <- which((geometry_indices %in% which(!valid_topology & new_valid_topology)))
    for(idx in invalid_topology){
      rc$notes[[idx]]$topology <- paste(rc$notes[[idx]]$topology,"sf::st_make_valid seems to have fixed the topology.")
    }
  }

  ## GEOMETRY TYPE
  geometry_types <- sf::st_geometry_type(geometries)
  valid_geometry_types <- geometry_types %in% allowed_geometry_types
  if(!all(valid_geometry_types)){
    warning("At least one geometry was an inappropriate type.  See output for details")
    invalid_type <- which(!(geometry_indices %in% which(valid_geometry_types)))
    for(idx in invalid_type){
      rc$notes[[idx]]$type <- paste("This location has a geometry of type",geometry_types[geometry_indices[idx] ],"but only types: (",paste(allowed_geometry_types,collapse = ', '),") are allowed")
    }
  }

  rc$valid <- geometry_indices %in% (valid_dimensions & valid_geometry_types & valid_topology)
  return(rc)
}

check_missing_cases <- function(sf_cases,column_names = c("attributes.fields.suspected_cases","attributes.fields.deaths")){
  missing_shapefile <- is.na(sf_cases$attributes.location_period_id)
  missing_cases <- sf_cases[missing_shapefile,]
  present_cases <- sf_cases[!missing_shapefile,]
  report <- is_shapefile_valid(present_cases)
  invalid_cases <- present_cases[!report$valid,]
  sf_cases$missing <- missing_shapefile
  sf_cases$invalid <- !sf_cases$missing
  sf_cases$invalid[sf_cases$invalid] <- !report$valid
  return(dplyr::summarize(dplyr::group_by(sf_cases,missing,invalid),dplyr::across(.cols=dplyr::all_of(column_names),.fns=function(x){sum(x,na.rm=TRUE)})))
}
