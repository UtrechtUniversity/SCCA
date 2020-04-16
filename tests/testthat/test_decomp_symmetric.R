context('decomp_symmetric')

library(readr)


#' Benchmark SCCA Decomposition Against a Known Decomposition
#'
#' @return TRUE if decompositions are equal
#

bm_decomp_symmetric <- function() {

  # read benchmark data
  #

  bm_vectors  <- read_csv("test_vectors.csv", col_names = FALSE)
  bm_vectors  <- abs(as.matrix(bm_vectors)) # the sign of eigenvector is unpredictable

  bm_spectrum <- read_csv("test_spectrum.csv", col_names = FALSE)
  bm_spectrum <- as.numeric(pull(bm_spectrum, X1))

  bm_matrix   <- read_csv("test_matrix.csv", col_names = FALSE)
  bm_matrix   <- as.matrix(bm_matrix)

  # decompose matrix with decomp_symmetric
  #
  scca        <- sccar:::decomp_symmetric(
                   matrix            = bm_matrix,
                   max_eigenvalues     = 180)

  sc_vectors  <- as.matrix(scca$r_vectors)
  sc_vectors  <- abs(round(sc_vectors, digits = 4))  # benchmark data has only 4 digits
                                                     # and get rid of sign issues
  sc_spectrum <- scca$values
  sc_spectrum <- round(sc_spectrum, digits = 4)

  n_bm  <- length(bm_spectrum)
  n_sc  <- length(sc_spectrum)

  if (n_sc > n_bm) {
    sc_spectrum <- sc_spectrum[1:n_bm]
    sc_vectors  <- sc_vectors[ ,1:n_bm]
  } else {
    bm_spectrum <- bm_spectrum[1:n_sc]
    bm_vectors  <- bm_vectors[ ,1:n_sc]
  }


  if (any(sc_spectrum != bm_spectrum)) {
    return(FALSE)
  }

  if (any(sc_vectors != bm_vectors)) {
    return(FALSE)
  }
  return(TRUE)
}


test_that("decomp_symmetric produces correct decomposition", {
  expect_true(object = bm_decomp_symmetric(), 'Eigenvalues/vectors not correct')
})








