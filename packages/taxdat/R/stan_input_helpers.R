#' @title Reorder for single source
#'
#' @description Reorder adjacency matrix to have single source
#'
#' @param A
#' @param coords
#'
#' @return the reodered adjacency matrix
#' @export
reorder_single_source <- function(A,
                                  coords) {

  # Check if the matrix is symetric
  if (!Matrix::isSymmetric(A)) {
    stop("Adjacency matrix not symetric fed to re-ordering")
  }

  print("Reordering adjacency to have single source")

  # Number of vertices
  N <- nrow(A)

  # Reoder with first top-left
  topleftorder <- order(coords[, 1] - coords[, 2])
  Atopleft <- A[topleftorder, topleftorder]

  # Undirected graph
  g_un <- igraph::graph_from_adjacency_matrix(Atopleft, mode = "undirected")

  # Determine vertex ordering using the breadth-first algorithm
  bfs_ordering <- igraph::bfs(g_un, root = 1)
  bfs_order <- as.vector(bfs_ordering$order)

  # Extract directed reodered matrix
  A_bfs <- A[bfs_order, bfs_order]
  A_bfs_directed <- Matrix::triu(A_bfs)

  return(list(
    A = A_bfs_directed,
    reordering = bfs_order
  ))
}
