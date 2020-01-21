#' Apply SCCA Heuristic
#'
#'
#' The function \code{apply_heuristic} implements heuristic
#' -> Returns index and vectors of # weakly connected components
#' -> Calculate gaps, check for disconnect components, and otherwise for the largest gap
#'
#' @param Eigenvalues Numeric vector of eigenvalues
#'
#' @return The number of weakly connected components (possibly clusters)

apply_heuristic <- function(Eigenvalues) {

  # checking parameters
  #
  if (is.unsorted(rev(Eigenvalues))) {
    stop('Eigenvalues are not decreasingly sorted')
  }

  error <- 1e-7      # ?

  # if more than one Eigenvalue equals 1 (within error) then .....
  #
  value_is_one <- abs(Eigenvalues - 1) < error
  n_ones       <- length(which(value_is_one))
  if (n_ones > 1) {
    return(n_ones)
  } else {
    # look for successive eigenvalues with greatest difference in value (gradient)
    # number of Eigenvalues before this gap is the required value for kmeans
    #
    gradients <- Eigenvalues[2:length(Eigenvalues)] - Eigenvalues[1:(length(Eigenvalues)-1)]
    threshold <- match(min(gradients), gradients)    # returns first match
  }
  return(threshold)
}


