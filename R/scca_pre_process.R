#' Compute RCA Matrix
#'
#' Computes Revealed Comparative Advantage (a.k.a. Location Quotient) bi-partite graph (e.g. country <-> exported products)
#' matrix.
#' See: \href{https://en.wikipedia.org/wiki/Revealed_comparative_advantage}{Wikipedia}.
#'
#' @param m A data matrix representing a bi-partite graph
#' @param binary If TRUE (default) binary values are calculated
#'
#' @export

compute_rca <-function(m, binary = TRUE) {

  rca_m <- sum(m)* m / outer(rowSums(m),colSums(m))

  if (isTRUE(binary)) {
    rca_m <- ifelse(rca_m > 1, 1, 0)
  }
  return(rca_m)
}
