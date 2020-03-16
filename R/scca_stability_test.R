#' SCCA Stability Test
#'
#' Tests the stability of an SCCA clustering.
#'
#' @param m Matrix representing an incidence (or bipartite) network
#' @param drop_vars Integer vector, the index of the variables (columns) to be dropped (one by one) when testing
#' the stability. When omitted, all the columns are succesively dropped.
#'
#' @details
#' Performs an SCCCA stability test on a clustering of a dataset.
#' First, a clustering on the complete dataset is done.
#' This is the base clustering.
#' Then, one by one, a variable of the dataset is dropped and a new
#' clustering is calculated.
#' Each new clustering is compared with the base clustering.
#' The stability measure is the average proportion of overlap (APO: the reverse of APN).
#' APO can be understood as the chance that if 2 observations are in the same cluster in the base clustering they are also
#' in the same cluster in the clustering with 1 column dropped.
#' If the user provides a list of column indices (integers), only those columns will be taken into account.
#' If no columns are provided all variables are successively dropped.
#'
#' @return A tibble with two columns:
#' \describe{
#'   \item{var_id}{Index of the dropped variable}
#'   \item{var_apo}{The average proportion of overlap between base clustering and clustering after dropping variable}
#' }
#'
#' @export
#'
scca_stability_test <- function(m, drop_vars = NULL) {

  # checking function arguments
  #
  if (is.null(drop_vars)) {          # do all columns
    drop_vars <- 1:dim(m)[2]
  }

  if(any(!is.integer(drop_vars))) {
    warning('illegal column indices!')
    return(NULL)
  }

  if (max(drop_vars > dim(m)[2]) || min(drop_vars < 1)) {
    warning('illegal column indices!')
    return(NULL)
  }

  # run an SCCA on all variables (columns) of the dataset
  #
  base      <- scca_compute(m = m)

  # data frame to hold the results
  #
  stability <- tibble::tibble(
    var_id   = integer(),
    var_APO  = double(),
  )

  # one by one drop a variable from the list and compute the APO
  #
  for (i in 1:length(drop_vars)) {
    drop_var                <- scca_compute(m = m[,-drop_vars[i]])
    stability[i, 'var_id']  <- drop_vars[i]
    stability[i, 'var_APO'] <- scca_overlap_test(base, drop_var, plot = FALSE)$avg_overlap.x
  }
  return(stability)
}

