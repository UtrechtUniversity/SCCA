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
  matrix_a <- Matrix::Matrix(matrix_a, sparse = TRUE)
  #
  Dr_inv <- Matrix::solve(Matrix::Diagonal(x = Matrix::rowSums(matrix_a)), sparse = TRUE) # n X n
  Dc_inv <- Matrix::solve(Matrix::Diagonal(x = Matrix::colSums(matrix_a)), sparse = TRUE) # m X m

  if (cluster_dim == 'cols') {
    S    <- matrix_a %*% Dc_inv %*% Matrix::t(matrix_a)
    L    <- Dr_inv %*% S
    DCA  <- Dc_inv %*% Matrix::t(matrix_a)
  }
  if (cluster_dim == 'rows') {
    S   <- Matrix::t(matrix_a) %*% Dr_inv %*% matrix_a
    L   <- Dc_inv %*% S
    DCA <- Dr_inv %*% matrix_a
  }
  return(list(L = L, DCA = DCA, S = S))
}





