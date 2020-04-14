#' Heuristics to Calculate the Expected Number of Clusters and Embedding Matrix
#'
#' Given a spectrum (decreasingly sorted Eigenvalues) and the corresponding Eigenvalues the heuristics compute the number of centers and
#' input matrix for kmeans.
#'
#'
#' @param eigenvalues Numeric vector of eigenvalues
#' @param eigenvectors Numeric matrix containing eigenvectors (columns)
#' @return A list with 3 elements
#' \describe{
#'   \item{Y}{The embedding matrix}
#'   \item{min.nc}{The minimum number of cluster centers}
#'   \item{max.nc}{The maximum number of cluster centers}
#' }
#'
#' @export

eigengap_heuristic <- function(eigenvalues, eigenvectors) {

  # checking parameters
  #
  if (is.unsorted(rev(eigenvalues))) {
    stop('Eigenvalues are not sorted decreasingly')
  }
  error <- 1e-7      # ?

  # if a number ( > 1) of Eigenvalues equals 1 (within error) then k is equal to that number else
  # k is the number of the first Eigenvalue of the two consecutive Eigenvalues with the greatest gap
  # between their values
  #
  value_is_one <- abs(eigenvalues - 1) < error
  n_ones       <- length(which(value_is_one))
  if (n_ones > 1) {
    threshold <- n_ones
  } else {
    # look for successive eigenvalues with greatest difference in value (gradient)
    # number of Eigenvalues before this gap is the required value for kmeans
    #
    gaps      <- eigenvalues[2:length(eigenvalues)] - eigenvalues[1:(length(eigenvalues)-1)] # gaps
    threshold <- match(min(gaps), gaps)   # returns first match
  }

  Y <- NULL
  if (as.integer(threshold) > 1) {
    Y <- eigenvectors[ , 2:threshold, drop = FALSE]
  }
  return(list(Y = Y, min.nc = as.integer(threshold), max.nc = as.integer(threshold)))
}

#' trace_heuristic: An Heuristic to Calculate the Expected Number of Clusters and Embedding Matrix
#'
#' @param eigenvalues Numeric vector of eigenvalues
#' @param eigenvectors Numeric matrix containing eigenvectors (columns)
#' @return
#' @export

#' @rdname eigengap_heuristic
trace_heuristic <- function(eigenvalues, eigenvectors) {

  #compute embedding dimension using the trace (which is sum(eigen_values)-1 )
  #
  eigenvalue_thresh  <- (sum(eigenvalues) - 1) / length(eigenvalues)
  nr_axes            <- sum(eigenvalues>eigenvalue_thresh)  # (-1) number of eigenvectors in embedding

  #define embedding with nr_axes columns (excluding first trivial eigenvec)
  Y = eigenvectors[, 2:(nr_axes+1)]  #dit is de 'embedding' - ik verwijder de eerste (constante) eigenvector en behoud nr_axes kolommen. Hierop ga ik Kmeans doen.

  max.nc = 20
  min.nc = 1
  return(list(Y = Y, min.nc = min.nc, max.nc = max.nc))
}
