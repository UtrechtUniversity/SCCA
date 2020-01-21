#' Spectral Clustering Correpondence Analysis.
#'
#' The function \code{scca} performs a spectral clustering correspondence analysis on a given
#' bi-partite graph. It is a form a hierarchical clustering. On every level the sub-clustering is guided by
#' a spectral analysis to estimate the number of sub-clusters (the k of kmaens). See .....
#'
#'
#' @param M A matrix representing a bi-partite graph. The rows and columns must be labeled
#'
#' @return \code{scca} returns a cluster tree as a recursive list.
#'
#' @examples
#' \dontrun{
#' data('carnivora', package = 'SCCAR')
#' scca(M = carnivora$M)
#' }
#' @export
scca <- function(M) {
  if (!is.matrix(M)) { stop('input not a matrix')}

  if (is.null(rownames(M)) || is.null(colnames(M))) {
    stop('matrix M must have row and column labels')
  }

  # clustering takes place on the labels of the axis with the longest dimension (?)
  #
  dim_axis <- ifelse(dim(M)[1] >= dim(M)[2], 'rows', 'cols')     # >= ?

  # scca_node function recursively constructs the the cluster tree node for node
  # Of course, it all starts with the top node and all row/column labels as initial cluster
  #
  if (dim_axis == 'rows') {
    labels <- rownames(M)
  } else {
    labels <- colnames(M)
  }
  scca_top_node   <- scca_node(M = M, labels = labels, level = 1, axis = dim_axis)
  return(scca_top_node)
}
