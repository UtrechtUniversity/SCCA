#
# Eigenvalue decomposition of a Laplacian , and sort the eigenvectors by the eigenvalues
# -> Calculate right eigenvectors of transpose of P, to get left eigenvectors of C
# -> Transform to C eigenvectors and sort
#

eigen_decomp <- function(laplacian, dca, axis) {

  # Can Laplacian assumed to be symmetric ?
  # Is this algorithm Laplacian  specific ?
  eigen <- rARPACK::eigs(A = laplacian, k = min(nrow(laplacian), 25))

  s <- sort(eigen$values, index.return=TRUE, decreasing = TRUE)
  eigenvalues <- s$x

  eigenvectors <- eigen$vectors
  if (axis == 'rows') {
    eigenvectors <- dca %*% eigenvectors
  }
  eigenvectors[ , s$ix]
  return(list(values = eigenvalues, vectors = eigenvectors))
}
