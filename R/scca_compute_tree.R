#' Build recursively a scca tree
#'
#' @param m Numeric matrix; Laplacian
#' @param child Numeric. Node number within its siblings (= same parent)
#' @param labels Character vector; The labels defining the (sub-)cluster to be analyzed
#' @param depth Integer the depth of this node in the tree
#' @param n_node Number of this node in the tree. This is a depth-first, pre-order numbering
#' @param iter.max Integer; the maximum number of iterations kmeans may take to compute the clusters
#' @param nstart Integer; the number of clusterings from which kmeans chooses the best
#' @param decomp The decomposition method to use: 'svd' or 'svds'.
#'
#'
#'
#'
scca_compute_tree <- function(
  labels, m, child, depth, n_node,
  iter.max     =  10,
  nstart       =  50,
  decomp) {

  if (!is.matrix(m)) {stop("Argument m is not a matrix")}

  # Select the rows of the sub matrix that has to be analyzed at this node
  #
  subM <- m[labels, , drop = FALSE]


  #
  zero_vector  <- rep(0, length(labels))            # used as a place holder for eigen_vectors of this sub-matrix
  cluster_node <- list(depth       =  depth,        # the steps taken form top depth to this node
                       child       =  child,        # number for distinguishing this node from its siblings
                       labels      =  labels,       # observations in this cluster
                       n_labs      =  length(labels), # Number of labels in this cluster (node)
                       k           =  0,            # number of most contributing eigenvalues
                       n_node      =  n_node,       # depth-first, pre-order numbering of nodes
                       node_type   = 'branch',      # branch or leaf
                       eigen_vec_1 =  zero_vector,  # eigen vectors after decomposition of this cluster
                       eigen_vec_2 =  zero_vector,
                       eigen_vec_3 =  zero_vector,
                       spectrum    =  vector(mode = 'integer'),  # eigenvalues sorted on explained variance
                       node        =  NULL)   # will contain list of children (if any)

  #
  #warning('depth: ', depth, ' / child: ', child, ' / labels: ', length(labels))

  if (length(labels) > 2) {
    decomposition <- decomp_symmetric(matrix = subM, n_eigenvalues = 25, decomp = decomp)   #s and d_inv
    eigen_vectors <- decomposition$r_vectors
    eigen_values  <- decomposition$values

    n_eigen <- ifelse (dim(eigen_vectors)[2] < 3, dim(eigen_vectors)[2], 3)
    for (i in 1:n_eigen) {
      eigen_vec_name                 <- sprintf('eigen_vec_%d', i)
      cluster_node[[eigen_vec_name]] <- eigen_vectors[ , i]
    }
    cluster_node[['spectrum']]       <- eigen_values

    # apply heuristic on spectrum (eigenvalues) to calculate the number (k) of expected clusters in matrix
    #
    k <- apply_heuristic(eigen_values)
    cluster_node[['k']] <- k
  } else {
    k <- 1  # to stop decomposition
  }

  # If k == 1 then this cluster can't be split any further in meaningful sub-clusters. So recursion on
  # this path stops here
  #
  if (k == 1) {
    cluster_node[['node_type']] = 'leaf'
    return(list(cluster_node = cluster_node, n_node = n_node))
  }

  # Compute input matrix Y for the kmeans.
  # Y is a matrix with the eigenvectors as columns minus the first trivial one
  #
  Y <- eigen_vectors[ , 2:k]

  centers <- k

  cl <- stats::kmeans(
    x        = Y,
    centers  = centers,
    iter.max = iter.max,
    nstart   = nstart)  # returns vector cl$cluster which gives the cluster id for each label

  # repeat proces for each cluster (C_i with i in 1:k) and combine results in a list which will be
  # stored in the 'nodes' attribute of this node as a list of lists
  #
  for (child in 1:k) {
    child_labels <- rownames(subM)[cl$cluster == child]
    child_tree   <- scca_compute_tree(
      labels   = child_labels,
      m        = m,
      child    = child,
      depth    = depth + 1,
      n_node   = n_node + 1,
      iter.max = iter.max,
      nstart   = nstart,
      decomp   = decomp)
    cluster_node$node[[child]] <- child_tree$cluster_node
    n_node                     <- child_tree$n_node
  }
  return(list(cluster_node = cluster_node, n_node = n_node))
}




