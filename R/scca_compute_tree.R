#' Build recursively a scca tree
#'
#' See \code{\link{scca_compute}} for description
#'
#' @param m A matrix representing a bi-partite network. The matrix must have row names and column names.
#' @param iter.max The maximum number of iterations \emph{kmeans} is allowed to make. Default is 10.
#' @param nstart Number of random cluster sets kmeans may choose to start with. Default is 25.
#' @param max_eigenvalues Restrict the number of computed eigenvalues to max_eigenvalues. The default is 25.
#' @param max_depth The maximum allowed depth of the analysis proces. If Inf (default) the analysis goes on untill a stop condition has been met.
#' @param decomp The decomposition function to use. Choices are \emph{svd} (default) and \emph{svd}
#' @param heuristic The function to use for calculating the number of clusters. The default is \emph{eigengap_heuristic}
#'
#' @param child The child number of this node within its siblings (= nodes with same parent)
#' @param labels The labels (character vector) defining the (sub-)set of data set m to be analyzed
#' @param n_node Number of the node in the tree. This is a depth-first, pre-order numbering, starting with 1 at the top node (a.k.a. root)
#' @param depth The depth (integer) of this node in the tree.

#' @details Function \emph{scca_compute_tree} calls itself recursively for every sub cluster found at a node until one of the  stop conditions
#' is met:
#' \itemize{
#'   \item{The heuristic finds only one prominent eigenvalue (k = 1)}
#'   \item{The maximum depth in a branch has been reached. \emph{k} will be set to 0}
#'   \item{The number of observations in the subset is too small to perform \emph{svd(s)}. This will raise a warning and \emph{k} will be set to -1}
#' }
#'
#'
#'
scca_compute_tree <- function(labels, m, child, depth, max_depth, n_node,
                              iter.max         = 10,
                              nstart           = 50,
                              max_eigenvalues  = max_eigenvalues,
                              decomp           = 'svd',
                              heuristic        = eigengap_heuristic) {

  if (!is.matrix(m)) {stop("Argument m is not a matrix")}

  # Subset the data matrix for the rows to be analyzed at this node
  #
  subM <- m[labels, , drop = FALSE]

  # create a list of attributes to collect analysis data
  #
  zero_vector  <- rep(0, length(labels))            # Used as a place holder for Eigen vectors
  cluster_node <- list(depth       =  depth,        # Steps taken form top to this node
                       child       =  child,        # Number for distinguishing this node from its siblings
                       labels      =  labels,       #
                       n_labs      =  length(labels), # Number of labels (observations) in the subset
                       k           =  0,            # The optimal number of clusters found in this subset
                       n_node      =  n_node,       # Depth-first, pre-order numbering of nodes in the scca tree
                       node_type   = 'branch',      # Values are 'branch' or 'leaf' If 'leaf' the subset can't be analyzed any further
                       eigen_vec_1 =  zero_vector,  # Eigenvectors after decomposition of this subset
                       eigen_vec_2 =  zero_vector,
                       eigen_vec_3 =  zero_vector,
                       spectrum    =  vector(mode = 'integer'),  # Eigenvalues of this subset sorted on explained variance
                       node        =  NULL)         # Contains list of children if node-type is 'branch

  # Sets of fewer than 3 observations are not decomposed any further.
  #
  if (length(labels) < 3 ) {
    warning_message <- sprintf('Submatrix at node %d is too small ( < 3) to calculate Eigenvectors', n_node)
    warning(warning_message)
    cluster_node[['k']]         <- -1
    cluster_node[['node_type']] <- 'leaf'
    return(list(cluster_node = cluster_node, n_node = n_node))
  }

  # Decompose in eigenvalues and eigenvectors
  #
  decomposition <- decomp_symmetric(matrix = subM, max_eigenvalues = max_eigenvalues, decomp = decomp)
  eigen_vectors <- decomposition$r_vectors
  eigen_values  <- decomposition$values

  # Store the spectrum and up to 3 eigenvectors
  #
  n_eigen <- ifelse (dim(eigen_vectors)[2] < 3, dim(eigen_vectors)[2], 3)
  for (i in 1:n_eigen) {
    eigen_vec_name                 <- sprintf('eigen_vec_%d', i)
    cluster_node[[eigen_vec_name]] <- eigen_vectors[ , i]
  }
  cluster_node[['spectrum']]       <- eigen_values

  if (depth == max_depth) {
    cluster_node[['node_type']] <- 'leaf'
    cluster_node[['k']]         <-  0
    return(list(cluster_node = cluster_node, n_node = n_node))
  }
  # Apply a user-provided heuristic on spectrum (eigenvalues) to calculate the optimal number (k) of clusters
  # and/or the embedding matrix. See @details for conditions an heuristic must meet
  #
  h_out <- heuristic(eigen_values, eigen_vectors)

  cluster_node[['k']] <- h_out$k

  if (h_out$k == 1) {
    cluster_node[['node_type']] <- 'leaf'
    return(list(cluster_node = cluster_node, n_node = n_node))
  } else {
    cl <- stats::kmeans(x = h_out$Y, centers = h_out$k, iter.max = iter.max, nstart = nstart)  # returns vector cl$cluster which gives the cluster id for each label
  }

  # repeat process for each cluster (C_i with i in 1:k) and combine results in a list which will be
  # stored in the 'nodes' attribute of this node
  #
  for (child in 1:h_out$k) {
    child_labels <- rownames(subM)[cl$cluster == child]
    child_tree   <- scca_compute_tree(
      labels          = child_labels,
      m               = m,
      child           = child,
      depth           = depth + 1,
      max_depth       = max_depth,
      n_node          = n_node + 1,
      iter.max        = iter.max,
      nstart          = nstart,
      max_eigenvalues = max_eigenvalues,
      decomp          = decomp,
      heuristic       = heuristic)
    cluster_node$node[[child]] <- child_tree$cluster_node
    n_node                     <- child_tree$n_node
  }
  return(list(cluster_node = cluster_node, n_node = n_node))
}




