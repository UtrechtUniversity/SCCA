#' Compute Symmetric Matrix of a bipartite adjacency matrix
#'
#' @param matrix A numeric matrix
#' @param decomp_axis Character string with value 'row' or 'column'.
#'
compute_symmetric <- function(matrix, decomp_axis) {

  # check input parameters
  if (!decomp_axis %in% c('cols', 'rows')) {
    stop('Unknown decomposition axis')
  }

  # A n*m
  # D_c = colsum A    m*m
  # D_r = rowsum A    n*n
  # S_r = A D_c^{-1} A^T n*m X m*m X m*n --> n*n
  # return S

  matrix_a <- Matrix::Matrix(matrix, sparse = TRUE)
  #
  r_sums <- Matrix::rowSums(matrix_a)
  c_sums <- Matrix::colSums(matrix_a)
  if (any(r_sums == 0) || any(c_sums == 0)) {
    warning('any row/col sum equals 0')
  }


  d_r_inv <- Matrix::solve(Matrix::Diagonal(x = r_sums), sparse = TRUE) # n X n
  d_c_inv <- Matrix::solve(Matrix::Diagonal(x = c_sums), sparse = TRUE) # m X m

  cluster_axis = 'rows'
  decomp_axis = 'cols'                # the columns will be decomposed in Eigenvectors
                                        # should be able to switch between rows/cols depending on which dimension
                                        # is smaller

    # calculate symmetric matrix S with dimensions n*n (n is number of rows)
  if (decomp_axis == 'rows') {
    s <- matrix_a %*% d_c_inv %*% Matrix::t(matrix_a)  # Sr
    d_inv <- d_r_inv
  }

    # calculate symmetric matrix S with dimensions m*m (m is number of columns)
  if (decomp_axis == 'cols') {
    s <- Matrix::t(matrix_a) %*% d_r_inv %*% matrix_a # Sc
    d_inv <- d_c_inv
  }

  # Decompose symmetric matrix S
#   Shat = D_r^{-1/2}  S D_r^{-1/2}
# U, lambdas = eig(Shat)
# V = D_r^{-1/2} U
# return V,lambdas

  s_hat         <- sqrt(d_inv) %*% s %*% sqrt(d_inv)
  eigen_decomp  <- rARPACK::eigs(A = s_hat, k = min(nrow(s_hat), 25))
  eigen_vectors <- sqrt(d_inv) %*% eigen_decomp$vectors
  eigen_values  <- eigen_decomp$values

  if (cluster_axis == 'rows' && decomp_axis == 'cols') {
    eigen_vectors <- d_r_inv %*% matrix_a %*% eigen_vectors
  } else if (cluster_axis == 'cols' && decomp_axis== 'rows') {
    eigen_vectors <- d_c_inv %*% t(matrix_a) %*% eigen_vectors
  }

  eigen_sorted  <- sort(eigen_decomp$values, index.return=TRUE, decreasing = TRUE)
  eigen_values  <- eigen_sorted$x
  eigen_vectors <- eigen_vectors[ , eigen_sorted$ix]

  eigen_vectors <- eigen_vectors %*% sqrt(Matrix::Diagonal(x=abs(eigen_values)))


  return(list(vectors = eigen_vectors, values = eigen_values))
}



