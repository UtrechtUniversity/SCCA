#' Mysterious function
#'
#' @param sub_matrix Numeric matrix; Laplacian
#' @param eigenvectors Matrix; columns are eigenvectors of sub_matrix
#' @param eigenvalues Numeric vector
#' @param k Numeric; the number of eigenvalues to be considered
#'
create_y <- function(sub_matrix, eigenvectors, eigenvalues, k) {

  N <-  sum(sub_matrix)
  Dv <- rowSums(sub_matrix)

  stds <- vector(mode = 'numeric', length = k)
  X    <- eigenvectors[ , 1:k]
  for (u in 1:k) {
    ux      <- eigenvectors[ , u]
    stds[u] <- sqrt(((ux * Dv) %*% ux) / N)
  }
  print(stds)
  X <- t(t(X) / stds)
  Y <- t(t(X) * sqrt(eigenvalues[1:k]))
  return(Y)
}

