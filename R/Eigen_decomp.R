#
# Eigendecomposition of a Laplacian , and sort the eigenvectors by the eigenvalues
# -> Calculate right eigenvectors of transpose of P, to get left eigenvectors of C
# -> Transform to C eigenvectors and sort
#

Eigen_decomp <- function(Laplacian, DCA, axis) {

  # Can Laplacian assumed to be symmetric ?
  # Is this algorithm Laplacian  specific ?
  Eigen <- rARPACK::eigs(A = Laplacian, k = min(nrow(Laplacian), 25))

  s <- sort(Eigen$values, index.return=TRUE, decreasing = TRUE)
  Eigenvalues <- s$x

  Eigenvectors <- Eigen$vectors
  if (axis == 'rows') {
    Eigenvectors <- DCA %*% Eigenvectors
  }
  Eigenvectors[ , s$ix]
  return(list(values = Eigenvalues, vectors = Eigenvectors))
}
