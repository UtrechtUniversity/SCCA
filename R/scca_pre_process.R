#' Compute RCA Matrix
#'
#' Computes Revealed Comparative Advantage (a.k.a. Location Quotient) of a bi-partite network (e.g. country <-> exported products)
#'
#' See: \href{https://en.wikipedia.org/wiki/Revealed_comparative_advantage}{Wikipedia}.
#'
#' @param m A data matrix representing a bi-partite network
#' @param binary If TRUE (default) binary RCA values are calculated
#'
#' @export

compute_rca <-function(m, binary = TRUE) {

  rca_m <- sum(m)* m / outer(rowSums(m),colSums(m))

  if (isTRUE(binary)) {
    rca_m <- ifelse(rca_m > 1, 1, 0)
  }
  return(rca_m)
}
