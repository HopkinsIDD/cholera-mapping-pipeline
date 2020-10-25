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



