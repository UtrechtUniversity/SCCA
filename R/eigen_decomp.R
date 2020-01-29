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
  eigenvalues <- Re(s$x)

  eigenvectors <- as.matrix(Re(eigen$vectors))
  if (axis == 'rows') {
    eigenvectors <- dca %*% eigenvectors
  }
  eigenvectors <- eigenvectors[ , s$ix]

  return(list(values = eigenvalues, vectors = eigenvectors))
}

# eigen_decomp_simm <- function(simm, dca, axis, d_inv) {
#
#   # Can Laplacian assumed to be symmetric ?
#   # Is this algorithm Laplacian  specific ?
#
#   S <- sqrt(d_inv) %*% simm %*% sqrt(d_inv)
#
#   eigen <- rARPACK::eigs_sym(A = S, k = min(nrow(laplacian), 25))
#
#   s <- sort(eigen$values, index.return=TRUE, decreasing = TRUE)
#   eigenvalues <- Re(s$x)
#
#   eigenvectors <- sqrt(d_inv) %*% as.matrix(Re(eigen$vectors))
#
#   if (axis == 'rows') {
#     eigenvectors <- dca %*% eigenvectors
#   }
#   eigenvectors <- eigenvectors[ , s$ix]
#
#   return(list(values = eigenvalues, vectors = eigenvectors))
# }


