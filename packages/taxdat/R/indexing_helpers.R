#' @name all_primes_less_than
#' @title all_primes_less_than
#' @description return a vector of primes less than n
#' @param n integer to bound the primes
all_primes_less_than <- function(n) {
  n <- as.integer(n)
  if (n > 1e+06) {
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
#' @name all_prime_factors
#' @title all_prime_factors
#' @description prime decomposition for n
#' @param n integer to decompose
all_prime_factors <- function(n) {
  all_primes <- all_primes_less_than(n)
  rc <- c()
  for (prime in all_primes) {
    while ((n %/% prime) == (n / prime)) {
      n <- n %/% prime
      rc <- c(rc, prime)
    }
  }
  return(rc)
}
#' @name split_into_factors
#' @title split_into_factors
#' @description factor n into two numbers such that the first divides a, and the second divides b
#' @param n The number to factor
#' @param a The number the first return should divide
#' @param b The number the second return should divide
split_into_factors <- function(n, a, b) {
  if (n == 1) {
    return(c(1, 1))
  }
  n_factors <- all_prime_factors(n)
  a_factors <- all_prime_factors(a)
  b_factors <- all_prime_factors(b)


  a_exclusive_factors <- c()
  b_exclusive_factors <- c()

  tmp_a_factors <- a_factors
  tmp_b_factors <- b_factors

  shared_factors <- n_factors
  skip_next <- FALSE
  for (idx in rev(seq_len(length(n_factors)))) {
    if (skip_next) {
      skip_next <- FALSE
    } else {
      a_ok <- n_factors[idx] %in% tmp_a_factors
      b_ok <- n_factors[idx] %in% tmp_b_factors
      a_exclusive_factors
      if (a_ok & b_ok) {
        tmp_a_factors <- tmp_a_factors[-which(tmp_a_factors == n_factors[idx])[1]]
        tmp_b_factors <- tmp_b_factors[-which(tmp_b_factors == n_factors[idx])[1]]
        if (idx != 1) {
          if (n_factors[idx] == n_factors[idx - 1]) {
            skip_next <- TRUE
          }
        }
      } else if (a_ok) {
        tmp_a_factors <- tmp_a_factors[-which(a_factors == n_factors[idx])[1]]
        a_exclusive_factors <- c(a_exclusive_factors, n_factors[idx])
        a_factors <- a_factors[-which(a_factors == n_factors[idx])[1]]
        shared_factors <- shared_factors[-idx]
      } else if (b_ok) {
        tmp_b_factors <- tmp_b_factors[-which(tmp_b_factors == n_factors[idx])[1]]
        b_exclusive_factors <- c(b_exclusive_factors, n_factors[idx])
        b_factors <- b_factors[-which(b_factors == n_factors[idx])[1]]
        shared_factors <- shared_factors[-idx]
      } else {
        stop(paste(
          n, "cannot be split into a number that divides", a, "and a number that divides",
          b
        ))
      }
    }
  }

  for (factor in rev(sort(shared_factors))) {
    if ((factor %in% b_factors) & ((prod(a_exclusive_factors) / a > prod(b_exclusive_factors) / b) |
      (!(factor %in% a_factors)))) {
      b_factors <- b_factors[-which(b_factors == factor)[1]]
      b_exclusive_factors <- c(b_exclusive_factors, factor)
    } else if (factor %in% a_factors) {
      a_factors <- a_factors[-which(a_factors == factor)[1]]
      a_exclusive_factors <- c(a_exclusive_factors, factor)
    } else {
    }
  }
  return(c(prod(a_exclusive_factors), prod(b_exclusive_factors)))
}

#' @export
#' @name array_to_centers
#' @title array_to_centers
#' @description get the centroids of the gridcells of an array
#' @param arr array The array to get the centers of
array_to_centers <- function(arr) {
  dimensions <- dim(arr)
  stepsize <- 1 / (dimensions)
  idx <- seq_len(prod(dimensions))
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
  rc <- Matrix::sparseMatrix(i = 1, j = 1, x = 0, dims = c(prod(dim(arr)), prod(dim(arr))))
  rc <- Matrix::drop0(rc)
  all_elements <- tibble::tibble()
  for (idx in seq_len(length(dim(arr)))) {
    tmp <- tibble::tibble(index = seq_len(dim(arr)[idx]))
    names(tmp)[1] <- paste("dimension", idx, sep = "_")
    if (nrow(all_elements) == 0) {
      all_elements <- tmp
    } else {
      all_elements <- tidyr::crossing(all_elements, tmp)
    }
  }
  tmp_elements <- all_elements[, seq_len(length(dim(arr)))]
  all_elements[["self"]] <- convert_index(dim(arr), tmp_elements)
  for (idx in seq_len(length(dim(arr)))) {
    tmp_elements <- all_elements[, seq_len(length(dim(arr)))]
    tmp_elements[[paste("dimension", idx, sep = "_")]] <- ifelse(tmp_elements[[paste("dimension",
      idx,
      sep = "_"
    )]] + 1 <= dim(arr)[[idx]], tmp_elements[[paste("dimension",
      idx,
      sep = "_"
    )]] + 1, NA)
    all_elements[[paste("neighbor", idx, sep = "+")]] <- convert_index(
      dim(arr),
      tmp_elements
    )

    tmp_elements <- all_elements[, seq_len(length(dim(arr)))]
    tmp_elements[[paste("dimension", idx, sep = "_")]] <- ifelse(tmp_elements[[paste("dimension",
      idx,
      sep = "_"
    )]] - 1 > 0, tmp_elements[[paste("dimension", idx, sep = "_")]] -
      1, NA)
    all_elements[[paste("neighbor", idx, sep = "-")]] <- convert_index(
      dim(arr),
      tmp_elements
    )
  }
  for (idx in seq_len(length(dim(arr)))) {
    self <- all_elements[["self"]]
    neighbor1 <- all_elements[[paste("neighbor", idx, sep = "-")]]
    neighbor2 <- all_elements[[paste("neighbor", idx, sep = "+")]]
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


## turn a multidimensional index into a single dimensional index param dims The
## dimension of the array idx the multidimensional index
sub2ind <- function(dims, idx) {
  if (isTRUE(any(dims < idx))) {
    stop("Invalid index")
  }
  sum <- 1
  for (i in seq_len(length(idx))) {
    sum <- sum + prod(dims[seq_len(i)]) * (idx[i] - 1) / dims[i]
  }
  return(sum)
}

## turn a single dimensional index into a multidimensional index param dims The
## dimension of the array idx the single dimensional index
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
