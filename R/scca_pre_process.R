#' Compute RCA Matrix
#'
#' Computes Revealed Comparative Advantage (a.k.a. Location Quotient) of a an incidence matrix (e.g. country <-> exported products)
#'
#' See: \href{https://en.wikipedia.org/wiki/Revealed_comparative_advantage}{Wikipedia}.
#'
#' @param m A data matrix 
#' @param binary If TRUE (default) the function returns a binary matrix with entry 1 if RCA>1 and 0 otherwise
#'
#' @export

compute_rca <-function(m, binary = TRUE) {

  rca_m <- sum(m)* m / outer(rowSums(m),colSums(m))

  if (isTRUE(binary)) {
    rca_m <- ifelse(rca_m > 1, 1, 0)
  }
  return(rca_m)
}
