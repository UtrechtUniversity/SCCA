#' Spectral Clustering Correpondence Analysis.
#'
#' The function \code{scca_compute} performs a spectral clustering correspondence analysis on a given
#' bi-partite or incidence graph (matrix m).
#' Starting with the the input matrix the spectrum (sorted Eigenvalues) of the matrix is computed. The spectrum is used by a heuristic to determine
#' the input parameter k of kmeans. If k > 1 then the current clusters is split by kemaens into k subclusters and the proces is
#' repeated for the k subclusters. Else, the decomposition of this branch ends with current cluster (leaf).
#'
#' @param m A matrix representing a bi-partite or incidence graph. The matrix must have row names and column names
#'
#'
#' @return A tree which describes the hierarchical clustering process.
#' Each node in the tree represents a stage in the analysis. The subnodes  :
#' \describe{
#'   \item{level}{The level in the tree.}
#'   \item{labels}{The labels (rownames) of the cluster in this node}
#'   \item{child}{Number of this cluster among its siblings. No order intended}
#'   \item{spectrum}{Vector of the Eigen values found at this node. The Eigenvalues are
#'   sorted on explained variance in descending order.}
#'   \item{eigen_vec_1}{The first Eigenvector found in the cluster of this node}
#'   \item{eigen_vec_2}{The second ...}
#'   \item{eigen_vec_3}{The third ...}
#'   \item{k}{The number of relevant Eigenvalues. This is the value for parameter k of 'kmeans' }
#'   \item{node_type}{The value is 'leaf' if k equals 1, else 'branch'}
#'   \item{node}{A list of k child branches if node_type == 'branch'}
#' }
#'
#' @examples
#' \dontrun{
#' data('carnivora', package = 'sccar')
#' scca_compute(carnivora)
#' }
#' @export
scca_compute <- function(m) {
  if (!is.matrix(m)) { stop('input not a matrix')}

  if (is.null(rownames(m)) || is.null(colnames(m))) {
    stop('matrix m must have row and column labels')
  }

  # The actual Eigen decomposition takes place on the axis (rows/cols) with the shortest dimension. This for
  # performance reasons only.
  # The clustering always takes place along the rows. If the decomposition axis is 'columns' then the Eigenvectors will be
  # translated to the Eigenvectors of rows. See function 'compute_symmetric'.
  #
  decomp_axis <- ifelse(dim(m)[1] >= dim(m)[2], 'cols', 'rows')     # >= ?

  # rows (columns) must be labeled. The labels indentify the cases in the proces of clustering
  #
  if (is.null(rownames(m))) {
    rownames(m) <- sprintf("%d", 1:nrow(m))
  }
  if (is.null(colnames(m))) {
    colnames(m) <- sprintf("%d", 1:ncol(m))
  }

  # clustering takes place along the rows of the matrix
  #
  labels <- rownames(m)


  # scca_compute_tree recursively constructs the cluster tree
  # Of course, it all starts with the top node
  #

  scca_top_node   <- scca_compute_tree(m = m, child = 1, labels = labels, level = 1, decomp_axis = decomp_axis)
  return(scca_top_node)
}
