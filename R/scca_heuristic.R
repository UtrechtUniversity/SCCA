#' Heuristic to Calculate the Expected Number of Clusters based on the spectrum of of eigenvalues.
#'
#' Given the spectrum (a set of sorted eigenvalues in descending order),
#' \emph{eigengap_heuristic} looks for the position of the largest gap (difference in value of 2 consecutive eigenvalues) in the spectrum,
#' indicating the expected number of clusters to be found in the data. If N (N>= 2) eigenvalues are equal to 1,
#' then N N is the expected number of clusters is equal
#' The matrix of corresponding eigenvectors is also returned.
#' The number of clusters and the matrix of eigenvectors can serve as an input for a clustering algorithm like kmeans.
#'
#'
#' @param eigenvalues Numeric vector of sorted eigenvalues
#' @param eigenvectors Numeric matrix containing eigenvectors (columns)
#' @return A list with 3 elements
#' \describe{
#'   \item{Y}{Matrix with observations as input for \emph{kmeans}}
#'   \item{k}{The position of largest gap or number the expected number of clusters in the data}
#' }
#'
#' @export

eigengap_heuristic <- function(eigenvalues, eigenvectors) {

  # checking parameters
  #
  if (is.unsorted(rev(eigenvalues))) {
    stop('Eigenvalues are not sorted in descending order')
  }
  error <- 1e-7      #

  value_is_one <- abs(eigenvalues - 1) < error # are there eigenvalues == 1?
  n_ones       <- length(which(value_is_one))
  if (n_ones > 1) {
    threshold <- n_ones
  } else {
    # look for successive eigenvalues with greatest difference in value (gradient)
    # number of the eigenvalue before the gap will be returned
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

