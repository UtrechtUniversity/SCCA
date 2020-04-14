#' Decompose Large and Sparse Matrix
#'
#' See <<link>>
#'
#' @param matrix An incidence matrix. The rows are the observations and
#'   the columns are the variables
#' @param n_eigenvalues Number of most prominent Eigenvalues to return. Default is 25.
#' @param decomp The decomposition to use: 'svd' (default) or 'svds'.
#'

decomp_symmetric <- function(matrix, n_eigenvalues = 25, decomp = 'svd') {

  if (!decomp %in% c('svds', 'svd')) {
    stop('Unknown decomposition function!')
  }
  # Compute symmetric matrix S
  # Steps:
  #   A  matrix with observations (n rows) and variables (m columns)
  #   D_c = colsum A    (m*m)
  #   D_r = rowsum A    (n*n)
  #   S_r = A * D_c^{-1} * A^T (n*m X m*m X m*n --> n*n)
  #   S_c = A^T * D_r^{-1} * A (m*n X n*n X n*m --> m*m)


  matrix_a <- Matrix::Matrix(matrix, sparse = TRUE)     # Use sparse matrix to speed up computations

  # remove disconnected columns
  #
  matrix_a <- matrix_a[ , Matrix::colSums(matrix_a) != 0, drop = FALSE]

  r_sums   <- Matrix::rowSums(matrix_a)
  c_sums   <- Matrix::colSums(matrix_a)

  #matrix_a <- matrix_a[ , c_sums != 0]

  # columns/rows can become disconnected (sum == 0). To prevent division by 0
  # set the sum to 1
  #
  r_sums[r_sums == 0] <- 1
  c_sums[c_sums == 0] <- 1

  # Compute inverted diagonal matrices.
  # Will be used for scaling and axis transformations
  #
  d_r_inv <- Matrix::Diagonal(x = 1/r_sums) # n X n
  d_c_inv <- Matrix::Diagonal(x = 1/c_sums) # m X m

  # For explanation of following decomposition see:
  #
  a_hat <- sqrt(d_r_inv) %*% matrix_a %*% sqrt(d_c_inv)

  #  number of eigenvalues may not be larger than smallest dimension of the matrix
  #
  min_dim       <- min(dim(a_hat)[1], dim(a_hat)[2])
  n_eigenvalues <- ifelse(n_eigenvalues > min_dim, min_dim, n_eigenvalues)

  if (decomp == 'svds') {
    singular_decomp <- rARPACK::svds(
      A    = a_hat,
      k    = n_eigenvalues,
      opts = list(maxitr = 1000))
  } else {
    singular_decomp <- base::svd(x = a_hat, nu = n_eigenvalues, nv = 0)
  }

  row_eigen_vectors <- sqrt(d_r_inv) %*% singular_decomp$u
  #col_eigen_vectors <- sqrt(d_c_inv) %*% singular_decomp$v
  eigen_values      <- singular_decomp$d[1:n_eigenvalues]^2


  # due to rounding errors zero's, values don't have to be exactly zero. They even can be negative and that
  # will give errors when taking square roots. Such values within a small tolerance will be reset to 0,
  # otherwise will raise errors
  #
  tolerance = 1e-7
  if (any(eigen_values < -tolerance)) {
    stop('Negative Eigenvalues! Something is definitely going wrong!')
  }
  eigen_values[abs(eigen_values) < tolerance] <- 0

  # some scaling but why?
  #
  row_eigen_vectors <- row_eigen_vectors %*% sqrt(Matrix::Diagonal(x=eigen_values))
  #col_eigen_vectors <- row_eigen_vectors %*% sqrt(Matrix::Diagonal(x=eigen_values))

  return(list(r_vectors = row_eigen_vectors,
              #c_vectors = col_eigen_vectors,
              values = eigen_values))
}



