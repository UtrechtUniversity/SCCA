#' Spectral Clustering Correpondence Analysis.
#'
#' The function \code{scca_compute} performs a hierarchical, Spectral Clustering Correspondence Analysis on a
#' bi-partite or incidence matrix. The proces consists of a decompostion of the matrix (svd), a (user-provided) heuristic which transforms
#' transforms the decompostion to input for kmeans clustering and the kmeans clustering itself. Each of the resulting clusters (sub-matrices) can be analyzed again
#' And so on till some stopping condition is met.
#' The output of \code{sccs_compute} is a tree (list of lists) in which every node is a proces step.
#' The top node represents the first step on the entire matrix
#'
#' @param m A matrix representing a bi-partite or incidence graph. The matrix must have row names and column names.
#' @param iter.max Integer, the maximum number of iterations \code{kmeans} is allowed. Default is 10.
#' @param nstart Integer, number of random cluster centers kmeans may choose to start with. Default is 25.
#' @param disconnect.rm If TRUE (default) disconnected rows and columns in the input data will be removed.
#' @param max_eigenvalues At each stage restrict the number of computed eigenvalues to max_eigenvalues. The default is 25.
#' @param decomp The decomposition function to use. Choices are 'svd' (default) and 'svds'.
#' @param heuristic The function to use for calculating the number of expected clusters.
#'
#'
#' @return A tree which describes the hierarchical clustering process.
#' Each node in the tree represents a stage in the analysis. The subnodes  :
#' \describe{
#'   \item{depth}{The depth in the tree.}
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
scca_compute <- function(
  m,
  iter.max        = 10,
  nstart          = 25,
  disconnect.rm   = TRUE,
  max_eigenvalues = 25,
  decomp          = 'svd',
  heuristic       = eigengap_heuristic) {
  if (!is.matrix(m)) { stop('input not a matrix')}

  if (is.null(rownames(m)) || is.null(colnames(m))) {
    stop('matrix m must have row and column labels')
  }

  if (!decomp %in% c('svd', 'svds')) {
    stop('unknown decomposition algoritm')
  }

  if (!rlang::is_function(heuristic)) {
    stop('heuristic is not a function')
  }

  # rows (columns) must be labeled. The labels indentify the cases in the proces of clustering
  #
  if (is.null(rownames(m))) {
    rownames(m) <- sprintf("%d", 1:nrow(m))
  }
  if (is.null(colnames(m))) {
    colnames(m) <- sprintf("%d", 1:ncol(m))
  }

  if (disconnect.rm) {
    m <- m[ , colSums(m) != 0]
    m <- m[rowSums(m) != 0,  ]
  }
  # clustering takes place along the row axis
  #

  labels <- rownames(m)


  # scca_compute_tree recursively constructs teh analysis tree
  # Of course, it all starts with the top node
  #

  scca_top_node   <- scca_compute_tree(
    m         = m,
    child     = 1,
    labels    = labels,
    depth     = 1,
    max_depth = Inf,
    n_node    = 1,
    iter.max  = iter.max,
    nstart    = nstart,
    max_eigenvalues = max_eigenvalues,
    decomp          = decomp,
    heuristic       = heuristic)
  return(scca_top_node$cluster_node)
}
