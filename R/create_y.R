#' Mysterious function
#'
#' @param sub_matrix Numeric matrix of n * n
#' @param eigenvectors Numeric matrix of n * m. Columns are eigenvectors of sub_matrix. Only the first k columns
#'    are used
#' @param eigenvalues Numeric vector of at least k largest eigenvalues of sub_matrix
#' @param k Numeric; the number of eigenvalues to be considered
#'
create_y <- function(sub_matrix, eigenvectors, eigenvalues, k) {

  N <-  sum(sub_matrix)
  Dv <- Matrix::rowSums(sub_matrix)

  stds <- vector(mode = 'numeric', length = k)
  X    <- eigenvectors[ , 1:k]

  for (u in 1:k) {
    ux      <- eigenvectors[ , u]
    stds[u] <- sqrt(((ux * Dv) %*% ux) / N)
  }

  X <- Matrix::t(Matrix::t(X) / stds)
  Y <- Matrix::t(Matrix::t(X) * sqrt(eigenvalues[1:k]))
  return(Y)
}

