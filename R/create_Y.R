#' Mysterious function
#'
#' @param subM Numeric matrix; Laplacian
#' @param Eigenvecs Matrix; columns are eigenvectors of subM
#' @param Eigenvals Numeric vector
#' @param k Numeric; the number of eigenvalues to be considered
#'
create_Y <- function(subM, Eigenvecs, Eigenvals, k) {

  N <-  sum(subM)
  Dv <- rowSums(subM)

  stds <- vector(mode = 'numeric', length = k)
  X    <- Eigenvecs[ , 1:k]
  for (u in 1:k) {
    ux      <- Eigenvecs[ , u]
    stds[u] <- sqrt(((ux * Dv) %*% ux) / N)
  }
  print(stds)
  X <- t(t(X) / stds)
  Y <- t(t(X) * sqrt(Eigenvals[1:k]))
  return(Y)
}

