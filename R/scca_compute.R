#' Spectral Clustering Correpondence Analysis.
#'
#' The function \code{scca} performs a spectral clustering correspondence analysis on a given
#' bi-partite graph. It is a form a hierarchical clustering. On every level the sub-clustering is guided by
#' a spectral analysis to estimate the number of sub-clusters (the k of kmaens). See .....
#'
#'
#' @param m A matrix representing a bi-partite graph. The rows and columns must be labeled
#'
#' @return \code{scca} returns a cluster tree as a recursive list.
#'
#' @examples
#' \dontrun{
#' data('carnivora', package = 'sccar')
#' scca_compute(M = carnivora$M)
#' }
#' @export
scca_compute <- function(m) {
  if (!is.matrix(m)) { stop('input not a matrix')}

  if (is.null(rownames(m)) || is.null(colnames(m))) {
    stop('matrix M must have row and column labels')
  }

  # clustering takes place on the labels of the axis with the longest dimension (?)
  #
  dim_axis <- ifelse(dim(m)[1] >= dim(m)[2], 'rows', 'cols')     # >= ?

  # scca_node function recursively constructs the the cluster tree node for node
  # Of course, it all starts with the top node and all row/column labels as initial cluster
  #
  if (dim_axis == 'rows') {
    labels <- rownames(m)
  } else {
    labels <- colnames(m)
  }
  scca_top_node   <- scca_compute_tree(m = m, labels = labels, level = 1, axis = dim_axis)
  return(scca_top_node)
}
