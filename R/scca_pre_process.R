#' Compute RCA Matrix
#'
#' Computes Revealed Comparative Advantage (a.k.a. Location Quotient) from a country/products matrix
#' (like the exports dataset)
#' If RCA > 1 the product is 'over-represented' in a country.
#' A binary version has values 1 if RCA > 1 and 0 otherwise.
#' See: \href{https://en.wikipedia.org/wiki/Revealed_comparative_advantage}{Wikipedia}.
#'
#' @param m A data matrix representing a bi-partite graph
#' @param binary If TRUE (default) binary version is calculated
#'
#' @export

compute_rca <-function(m, binary = TRUE) {

  rca_m <- sum(m)* m / outer(rowSums(m),colSums(m))

  if (isTRUE(binary)) {
    rca_m <- ifelse(rca_m > 1, 1, 0)
  }
  return(rca_m)
}
