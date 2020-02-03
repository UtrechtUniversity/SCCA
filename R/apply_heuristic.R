#' Apply scca heuristic
#'
#'
#' The function \code{apply_heuristic} implements heuristic for SCCA
#' -> Returns index and vectors of # weakly connected components
#' -> Calculate gaps, check for disconnect components, and otherwise for the largest gap
#'
#' @param eigenvalues Numeric vector of eigenvalues
#'
#' @return The number of weakly connected components (possibly clusters)

apply_heuristic <- function(eigenvalues) {

  # checking parameters
  #
  if (is.unsorted(rev(eigenvalues))) {
    stop('Eigenvalues are not decreasingly sorted')
  }

  error <- 1e-7      # ?

  # if more than one Eigenvalue equals 1 (within error) then .....
  #
  value_is_one <- abs(eigenvalues - 1) < error
  n_ones       <- length(which(value_is_one))
  if (n_ones > 1) {
    return(n_ones)
  } else {
    # look for successive eigenvalues with greatest difference in value (gradient)
    # number of Eigenvalues before this gap is the required value for kmeans
    #
    gaps <- eigenvalues[2:length(eigenvalues)] - eigenvalues[1:(length(eigenvalues)-1)] # gaps
    threshold <- match(min(gaps), gaps)    # returns first match
  }
  return(threshold)
}


