#' SCCA Validity Test
#'
#' Tests the validity of a clustering produced by \emph{scca_compute}.
#'
#' @details
#' Computes the three internal validity measures: \strong{Connectivity}, \strong{Silhouette Width} and \strong{Dunn Index}. See
#' \href{https://cran.r-project.org/web/packages/clValid/vignettes/clValid.pdf}{clValid, an R package for cluster validation} for more
#' information
#'
#' @param scca List; an SCCA clustering tree
#' @param dist A \emph{dist} object; the distance matrix of the data used for producing \emph{scca}
#'
#' @return
#' A list with the three measures
#'
#' @export

scca_validity_test <- function(scca, dist) {

  cl <- scca_get_clusters(scca)

  # silhouette
  #
  sil_obj <- cluster::silhouette(cl$cluster, dist = dist)
  sil     <- summary(sil_obj)$avg.width

  # Dunn
  #
  dunn    <- clValid::dunn(dist = dist, cl$cluster)

  # connectivity
  #
  conn    <- clValid::connectivity(dist = dist, clusters = cl$cluster)

  return(list(sil = sil, dunn = dunn, conn = conn))
}

#' Silhouette Width of each Cluster in an SCCA Clustering
#'
#' @param scca An SCCA clustering tree
#' @param dist A \emph{dist} object; a distance matrix of the data used for producing \emph{scca}
#' @param plot Boolean; if TRUE (default) plots the silhouette width of every obeservation
#'
#' @export
scca_silhouette_test <- function(scca, dist, plot = TRUE) {
  cl         <- scca_get_clusters(scca)
  sil_obj    <- cluster::silhouette(cl$cluster, dist = dist)
  if (plot) {
    sil_plot    <- factoextra::fviz_silhouette(sil_obj, label = FALSE, print.summary = FALSE)
    plot(sil_plot)
  }
  return(sil_obj)
}

#' Compute and Store Distance Matrix
#'
#' @details This function is a wrapper around \link[stats]{dist}.
#' Some validity and stability tests use a distance (or dissimilarity) matrixto calculate their measures (e.g. Dunn Index).
#' Calculating a distance matrix can take a long time (up to hours) depending on the number of observations (rows).
#' This function has the option to store the distance matrix in a .rds file.
#'
#' @param m Incidence or adjacency matrix
#' @param filename String, name of the file in which to store the distance matrix. If NULL (default) the
#' distance will not be stored, but only returned
#' @param method See: \code{\link[stats]{dist}}
#'
#' @return
#' The distance matrix as an \emph{dist} object
#'
#' @export

scca_compute_dist <- function(m, filename = NULL, method = 'euclidian') {
  if (is.null(filename)) {
    filepath = NULL
  } else {
    filepath <- file.path(getwd(), sprintf('%s.Rds', filename))
    if (file.exists(filepath)) {
      warning(sprintf('file %s allready exists!', filepath))
      return(NULL)
    }
  }
  dist_m <- stats::dist(x = m, method = method )
  if (!is.null(filepath)) {
    readr::write_rds(x = dist_m, path = filepath)
  }
  return(dist_m)
}




