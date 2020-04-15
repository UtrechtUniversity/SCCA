#' Transform Bi-partite Matrix to RCA Matrix
#'
#'
#' @param m A data bi-partite data matrix
#' @param binary If TRUE binary value (0/1) of RCA is calculated
#'
#' @export

compute_rca <-function(m, binary = TRUE) {

  # computes RCA matrix from raw data matrix, either binary or not

  rca_m <- sum(m)* m / outer(rowSums(m),colSums(m))

  if (isTRUE(binary)) {
    rca_m[rca_m >= 1] <- 1
    rca_m[rca_m < 1]  <- 0
  }

  return(rca_m)
}
