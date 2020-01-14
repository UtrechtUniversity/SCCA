#' Generate Laplacian matrix (L) from the Simmilariry matrix (S) of a bipartite adjacency matrix (M)
#'
#' @param A A numeric matrix ......
#' @param cluster_dim Character string with value row or column
#'
generate_laplacian <- function(A, cluster_dim) {

  # check input parameters
  if (!cluster_dim %in% c('cols', 'rows')) {
    stop('Unknown cluster dimension')
  }
  #

  # formula?
  Dr_inv <- matlib::inv(diag(Matrix::rowSums(A)))
  Dc_inv <- matlib::inv(diag(Matrix::colSums(A)))

  if (cluster_dim == 'cols') {
    S    <- A %*% Dc_inv %*% t(A)
    L    <- Dr_inv %*% S
    DCA  <- Dc_inv %*% A
  }
  if (cluster_dim == 'rows') {
    S   <- t(A) %*% Dr_inv %*% A
    L   <- Dc_inv %*% S
    DCA <- Dr_inv %*% A
  }
  return(list(L = L, DCA = DCA, S = S))
}





