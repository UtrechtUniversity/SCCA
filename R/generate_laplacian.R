#' Generate Laplacian matrix (L) from the Simmilariry matrix (S) of a bipartite adjacency matrix (M)
#'
#' @param matrix_a A numeric matrix ......
#' @param cluster_dim Character string with value row or column
#'
generate_laplacian <- function(matrix_a, cluster_dim) {

  # check input parameters
  if (!cluster_dim %in% c('cols', 'rows')) {
    stop('Unknown cluster dimension')
  }

  #
  Dr_inv <- solve(diag(Matrix::rowSums(A))) # n X n
  Dc_inv <- solve(diag(Matrix::colSums(A))) # m X m

  if (cluster_dim == 'cols') {
    S    <- matrix_a %*% Dc_inv %*% t(matrix_a)
    L    <- Dr_inv %*% S
    DCA  <- Dc_inv %*% t(matrix_a)
  }
  if (cluster_dim == 'rows') {
    S   <- t(matrix_a) %*% Dr_inv %*% matrix_a
    L   <- Dc_inv %*% S
    DCA <- Dr_inv %*% matrix_a
  }
  return(list(L = L, DCA = DCA, S = S))
}





