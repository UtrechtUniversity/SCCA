#' Decompose Large and Sparse Matrix
#'
#' Computes Eigenvalues of the similarity matrix.
#' As input, the contingency matrix M should be given.
#' The similarity matrix and associated eigenvectors and eigenvalues are determined internally.
#' See for a elaborate description \strong{van Dam, et al, 2021}
#'
#' @references
#' van Dam, et al. (2021), \strong{Correspondence analysis, spectral clustering and graph embedding: applications to ecology and economic complexity}; *name of journal*; DOI: <doi>.
#'
#' @param matrix Incidence matrix (e.g. species - location), which can be interpreted as the  bi-adjacency matrix of a bipartite network.
#' @param max_eigenvalues Max. number of eigenvalues to compute. Default is 25.
#' @param decomp The decomposition to use: \strong{svd} (default) or \strong{svds}.
#' The later only computes the k leading singular values and vectors of a rectangular matrix
#'
#' @details If both dimensions of data matrix are greater than max_eigenvalues, then the number of computed eigenvalues is restricted
#' to max_eigenvalues; otherwise, the shortest dimension is chosen.
#'
#' \strong{svds} uses the rARPACK implementation of the singular value decomposition for efficient approximation of singular decomposition for large sparse matrices.
#'
#'

decomp_symmetric <- function(matrix, max_eigenvalues, decomp = 'svd') {

  if (!decomp %in% c('svds', 'svd')) {
    stop('Unknown decomposition function!')
  }



  matrix_a <- Matrix::Matrix(matrix, sparse = TRUE)     # Use sparse matrix to speed up computations

  # remove disconnected columns
  #
  matrix_a <- matrix_a[ , Matrix::colSums(matrix_a) != 0, drop = FALSE]

  r_sums   <- Matrix::rowSums(matrix_a)
  c_sums   <- Matrix::colSums(matrix_a)



  # Compute inverted diagonal matrices.
  # Will be used for scaling and axis transformations
  #
  d_r_inv <- Matrix::Diagonal(x = 1/r_sums) # n X n
  d_c_inv <- Matrix::Diagonal(x = 1/c_sums) # m X m

  # For explanation
  #
  a_hat <- sqrt(d_r_inv) %*% matrix_a %*% sqrt(d_c_inv)

  #  number of eigenvalues may not be larger than smallest dimension of the matrix
  #
  min_dim       <- min(dim(a_hat)[1], dim(a_hat)[2])
  n_eigenvalues <- ifelse(max_eigenvalues > min_dim, min_dim, max_eigenvalues)

  if (decomp == 'svds') {
    singular_decomp <- rARPACK::svds(
      A    = a_hat,
      k    = n_eigenvalues,
      opts = list(maxitr = 1000))
  } else {
    singular_decomp <- base::svd(
      x  = a_hat,
      nu = n_eigenvalues,
      nv = 0)
  }

  row_eigen_vectors <- sqrt(d_r_inv) %*% singular_decomp$u
  eigen_values      <- singular_decomp$d[1:n_eigenvalues]^2


  # due to rounding errors values don't have to be exactly zero which they theoretically should be. They even can be negative and that
  # will give errors when taking square roots. Such rounding errors within a small tolerance will be set to 0,
  # otherwise negative eigenvalues will raise errors.
  #
  tolerance = 1e-7
  if (any(eigen_values < -tolerance)) {
    stop('Negative Eigenvalues! Something has definitely gone wrong!')
  }
  eigen_values[abs(eigen_values) < tolerance] <- 0

  # scaling
  #
  row_eigen_vectors <- row_eigen_vectors %*% sqrt(Matrix::Diagonal(x=eigen_values))

  return(list(r_vectors = row_eigen_vectors,
              values    = eigen_values))
}



