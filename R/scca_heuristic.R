#' Heuristic to Calculate the Expected Number of Clusters based on the spectrum of of eigenvalues of the normalized Laplacian.
#'
#' Given the spectrum (a set of decreasingly sorted eigenvalues),
#' \emph{eigengap_heuristic} computes the position of the largest decrease in the spectrum,
#' indicating the expected number of clusters to be found in the data.
#' The matrix of corresponding eigenvectors is also returned.
#' The number of clusters and the matrix of eigenvectors can serve as an input for a clustering algorithm like kmeans.
#'
#'
#' @param eigenvalues Numeric vector of eigenvalues
#' @param eigenvectors Numeric matrix containing eigenvectors (columns)
#' @return A list with 3 elements
#' \describe{
#'   \item{Y}{Matrix with observations as input for \emph{kmeans}}
#'   \item{k}{The position of the eigengap (the expected number of clusters in the data}
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
  return(list(Y = Y, k = as.integer(threshold)))
}

