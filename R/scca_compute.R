#' Spectral Clustering Correpondence Analysis
#'
#' Please refer to \href{http://www.uu.nl}{Dam, Alje van, e.a. 2021} for a
#' detailed description of the theory and mathematical foundation of Spectral Clustering Correspondence Analysis.
#'
#' The function \emph{scca_compute} performs a hierarchical, Spectral Clustering Correspondence Analysis on a matrix M representing a
#' bi-partite network. The process consists of the following steps:
#' \enumerate{
#'     \item Computation of the eigenvalues and eigenvectors of MM* (M* transpose of M) by means of a singular value decomposition of matrix M.
#'     \item Application of an heuristic on the resulting spectrum (ordered eigenvalues on descending values) to establish the set most relevant k eigenvectors.
#'     \item If k is more then 1, apply kmeans on the matrix of corresponding eigenvectors to find the k clusters.
#' }
#'
#' The process can be (hierarchical) repeated on the resulting clusters till no cluster can't be split anymore.
#' The output of \emph{sccs_compute} is a tree (list of lists) in which every node represents one step in the process.
#'
#'
#' @param m A matrix representing a bi-partite network. The matrix must have row names and column names.
#' @param iter.max The maximum number of iterations \emph{kmeans} is allowed to make. Default is 10.
#' @param nstart Number of random cluster sets kmeans may choose to start with. Default is 25.
#' @param disconnect.rm If TRUE (default) disconnected rows and columns in the input data will be removed.
#' @param max_eigenvalues Restrict the number of computed eigenvalues to max_eigenvalues. The default is 25.
#' @param max_depth The maximum allowed depth of the analysis proces. If Inf (default) the analysis goes on until a stop condition has been met.
#' @param decomp The decomposition function to use. Choices are \emph{svd} (default) and \emph{svd}
#' @param heuristic The function to use for calculating the number of clusters. The default is \emph{eigengap_heuristic}
#' @param disconnect.rm If TRUE (default) disconnected rows and columns in the input data will be removed.
#'
#' @return A tree which describes the hierarchical SCCA process. Every node contains the following information:
#' \describe{
#'   \item{depth}{The depth of the node in the tree.}
#'   \item{labels}{The labels (rownames) of the subset in this node}
#'   \item{n_labs}{The number of labels (observations) in the subset}
#'   \item{n_node}{Depth-first, pre-order numbering of nodes in the scca tree}
#'   \item{child}{Number of this node among its siblings. No order intended}
#'   \item{spectrum}{Vector of the Eigen values found at this node. The eigenvalues are
#'   sorted on explained variance in descending order.}
#'   \item{eigen_vec_1}{The first Eigenvector of the subset of this node}
#'   \item{eigen_vec_2}{The second ...}
#'   \item{eigen_vec_3}{The third ...}
#'   \item{k}{The number of relevant eigenvalues. This is the value for parameter k of 'kmeans'.}
#'   \item{node_type}{The value is 'leaf' if k equals -1, 0, or 1, else 'branch'}
#'   \item{node}{A list of k child nodes, if node_type == 'branch'}
#' }
#'
#' @details The hierarchical decomposition on a branch stops, when the number of relevant eigenvalues equals 1 or,
#' the maximum depth has been reached. This is signaled by \emph{k = 0}.
#' When the subset is too small to decompose any further, processing on the branch also stops and a warning is raised.
#' Also the value of \emph{k} is set to -1.
#'
#' The function \emph{scca_compute} is a wrapper function around the workhorse \code{\link{scca_compute_tree}}
#'
#' @examples
#' \dontrun{
#' scca_compute(carnivora)
#' }
#' @export
#'
scca_compute <- function(
  m,
  iter.max        = 10,
  nstart          = 25,
  disconnect.rm   = TRUE,
  max_eigenvalues = 25,
  decomp          = 'svd',
  max_depth       = Inf,
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

  # scca_compute_tree recursively constructs the analysis tree
  # Of course, it all starts with the top node
  #

  scca_top_node   <- scca_compute_tree(
    m               = m,
    child           = 1,
    labels          = labels,
    depth           = 1,
    max_depth       = max_depth,
    n_node          = 1,
    iter.max        = iter.max,
    nstart          = nstart,
    max_eigenvalues = max_eigenvalues,
    decomp          = decomp,
    heuristic       = heuristic)
  return(scca_top_node$cluster_node)
}
