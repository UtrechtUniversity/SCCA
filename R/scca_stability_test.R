#' SCCA Stability Test
#'
#' Tests the stability of the clustering method.
#'
#' @param m Matrix representing an incidence (or bipartite) network
#' @param del_columns Integer vector, the index of the columns to be deleted (one for one) when testing
#' the stability. When omitted, all the columns are deleted (one for one)
#' @param dist_m Object of class dist; the distance matrix that corresponds with the incidence matrix m. If NULL (default),
#' scca_stability_test calculates the distance matrix
#'
#'
#' @details
#' Performs an SCCCA stability test on a clustering of a dataset.
#' First, a clustering on the complete dataset is done.
#' This is the base clustering.
#' Then, one for one, each variable in the dataset is removed and a new
#' clustering is calculated.
#' Each new clustering is compared with the base clustering.
#' The measures are APN, AD, ADM and FOM.
#' The returned values are averages of the outcomes of the measure per removed variable.
#' If the user provides a list of columns, only those columns will be taken into account.
#'
#' @importFrom stats dist
#'
#' @importFrom dplyr summarise
#'
#' @export
#'
scca_stability_test <- function(m, del_columns = NULL, dist_m = NULL) {

  # checking function arguments
  #
  if (is.null(del_columns)) {          # do all columns
    del_columns <- 1:dim(m)[2]
  }

  if(any(!is.integer(del_columns))) {
    warning('illegal column indices!')
    return(NULL)
  }

  if (max(del_columns > dim(m)[2]) || min(del_columns < 1)) {
    warning('illegal column indices!')
    return(NULL)
  }

  if (is.null(dist_m)) {
    dist_m <- stats::dist(x = m, method = 'euclidian')
  }

  # scca run on all variabels of the dataset
  #
  base    <- scca_compute(m = m)
  base_cl <- scca_get_clustering(scca = base)


  stability <- tibble::tibble(
    APN = double(),
    AD  = double(),
    ADM = double(),
    FOM = double()
  )

  for (i in 1:length(del_columns)) {
    del_tree       <- scca_compute(m = m[,-del_columns[i]])
    del_cl         <- scca_get_clustering(scca = del_tree)
    stability[i, ] <- clValid::stability(
      mat        = m,
      Dist       = dist_m,
      del        = del_columns[i],
      cluster    = base_cl$cluster,
      clusterDel = del_cl$cluster)
  }
  stability <- cbind(stability, del = del_columns)
  stability <- dplyr::summarise(.data = stability,
                 APN = mean(APN),
                 AD  = mean(AD),
                 ADM = mean(ADM),
                 FOM = mean(FOM))

  return(stability)
}

#' SCCA compute and store distance matrix
#'
#' @param m Incidence or adjacency matrix
#' @param filename Name of the file in which to store the distance matrix. If NULL (default) the
#' distance will not be stored, but only returned
#' @param method See ?stats::dist
#'
#' @return The distance matrix as an dist object
#'

#'
#' @export
#'
scca_compute_dist <- function(m, filename = NULL, method = 'euclidian') {
  if (is.null(filename)) {
    filepath = NULL
  } else {
    filepath <- file.path(getwd(), sprintf('%s.Rds', filename))
    print(filepath)
    if (file.exists(filepath)) {
      warning(sprintf('file %s allready exists!', filepath))
      return(NULL)
    }
  }
  dist_m <- dist(x = m, method = method )
  if (!is.null(filepath)) {
    readr::write_rds(x = dist_m, path = filepath)
  }
  return(dist_m)
}

# @importFrom readr write_rds
# @importFrom stats dist
# @importFrom clValid stability

