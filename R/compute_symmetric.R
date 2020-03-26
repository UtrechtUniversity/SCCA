#' Decompose Large and Sparse Matrix
#'
#' See <<link>>
#'
#' @param matrix An incidence matrix. The rows are the observations and
#'   the columns are the
#' @param decomp_axis Character string with value 'row' or 'column' indicating the axis along
#'   which the decomposition will initially take place. At the end the decomposition will be transformed
#'   to the row axis (= observations)
#' @param n_eigenvalues Number of most prominent Eigenvalues to return. Default is 25.
#'

decomp_symmetric <- function(matrix, decomp_axis, n_eigenvalues = 25) {

  # check input parameters
  if (!decomp_axis %in% c('cols', 'rows')) {
    stop('Unknown decomposition axis')
  }

  # Compute symmetric matrix S
  # Steps:
  #   A  matrix with observations (n rows) and variables (m columns)
  #   D_c = colsum A    (m*m)
  #   D_r = rowsum A    (n*n)
  #   S_r = A * D_c^{-1} * A^T (n*m X m*m X m*n --> n*n)
  #   S_c = A^T * D_r^{-1} * A (m*n X n*n X n*m --> m*m)


  matrix_a <- Matrix::Matrix(matrix, sparse = TRUE)     # Use sparse matrix to speed up computations
  r_sums   <- Matrix::rowSums(matrix_a)
  c_sums   <- Matrix::colSums(matrix_a)

  # In the proces columns/rows can become disconnected (sum == 0). To prevent division by 0
   # set the sum to 1
   #
  r_sums[r_sums == 0] <- 1
  c_sums[c_sums == 0] <- 1

   # Compute inverted diagonal matrices
   #
  d_r_inv <- Matrix::Diagonal(x = 1/r_sums) # n X n
  d_c_inv <- Matrix::Diagonal(x = 1/c_sums) # m X m

    # compute symmetric matrix S
    #
  if (decomp_axis == 'rows') {
    s     <- matrix_a %*% d_c_inv %*% Matrix::t(matrix_a)  # Sr
    d_inv <- d_r_inv
  } else {                             # decomp_axis == 'cols'
    s     <- Matrix::t(matrix_a) %*% d_r_inv %*% matrix_a # Sc
    d_inv <- d_c_inv
  }

  # Decompose symmetric matrix S
  #   Shat = D_r^{-1/2}  S D_r^{-1/2}
  #   U, lambdas = eig(Shat)
  #   V = D_r^{-1/2} U          #
  #   return V,lambdas

  s_hat <- sqrt(d_inv) %*% s %*% sqrt(d_inv)

   # Eigenvalues and Eigenvectors of symmetric S
   #
  if (nrow(s_hat) > n_eigenvalues ) {                        # Don't compute more than 25 Eigenvalues for performance reasons
    #oldw <- getOption("warn")
    #options(warn = -1)
    eigen_decomp <- rARPACK::eigs_sym(A = s_hat, k = 25)
    #options(warn = oldw)
  } else {
    eigen_decomp <- eigen(x = s_hat, symmetric = TRUE)
  }

  # Eigenvalues and Eigenvectors of the original matrix. Along the decomp_axis
  #
  eigen_vectors <- sqrt(d_inv) %*% eigen_decomp$vectors
  eigen_values  <- eigen_decomp$values

    # Get 'row' Eigenvectors
    #
  if (decomp_axis == 'cols') {
    eigen_vectors <- d_r_inv %*% matrix_a %*% eigen_vectors
  }

  eigen_sorted  <- sort(eigen_decomp$values, index.return=TRUE, decreasing = TRUE)
  eigen_values  <- eigen_sorted$x
  eigen_vectors <- eigen_vectors[ , eigen_sorted$ix]

  # due to rounding errors zero's, don't have to be exactly zero. They even can be negative and that
  # can cause errors in some comming computations. Within a small tolerance values will be reset to 0
  #
  tolerance = 1e-7
  if (any(eigen_values < -tolerance)) {
    stop('Negative Eigenvalues! Something is definitely going wrong!')
  }
  eigen_values[abs(eigen_values) < tolerance] <- 0

  # some scaling but why?
  #
  eigen_vectors <- eigen_vectors %*% sqrt(Matrix::Diagonal(x=eigen_values))

  return(list(vectors = eigen_vectors, values = eigen_values))
}



