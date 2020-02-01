#' Spectral Clustering Correpondence Analysis.
#'
#' The function \code{scca_compute} performs a spectral clustering correspondence analysis on a given
#' bi-partite graph. It is a form a hierarchical clustering. On every level the sub-clustering is guided by
#' a spectral analysis to estimate the number of sub-clusters (the k of kmeans). See .....
#'
#'
#' @param m A matrix representing a bi-partite graph. The matrix must have row names and column names
#'
#'
#' @return A recursive which describes the hierarchical clustering process.
#' Each node in the tree represents a stage in the analysis. The subnodes  :
#' \describe{
#'   \item{level}{The level in the tree.}
#'   \item{child}{Numbering of the siblings. No order intended}
#'   \item{spectrum}{Vector of the Eigen values found at this node. The Eigenvalues are
#'   sorted on explained variance in descending order.}
#'   \item{labels}{The labels of the cluster in this node}
#'   \item{eigen_vec_1}{The first Eigenvector found in the cluster of this node}
#'   \item{eigen_vec_2}{The second}
#' }
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

  # decomposition takes place on the labels of the axis with the longest dimension (?)
  #
  decomp_axis <- ifelse(dim(m)[1] >= dim(m)[2], 'cols', 'rows')     # >= ?

  # clustering takes place along the rows of the matrix
  #
  labels <- rownames(m)


  # scca_node function recursively constructs the the cluster tree node for node
  # Of course, it all starts with the top node and all row/column labels as initial cluster
  #

  scca_top_node   <- scca_compute_tree1(m = m, child = 1, labels = labels, level = 1, decomp_axis = decomp_axis)
  return(scca_top_node)
}
