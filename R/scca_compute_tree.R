#' Build recursively a scca tree
#'
#'
#' @param m Dataset as an bipartite or incidence matrix
#' @param child The child number of this node within its siblings (= same parent)
#' @param labels The labels (character vector) defining the (sub-)set of dataset m to be analyzed
#' @param depth The depth (integer) of this node in the tree.
#' @param max_depth The maximum depth the hierarchical process may go down to.
#' @param n_node Number of this node in the tree. This is a depth-first, pre-order numbering
#' @param iter.max The maximum number (integer) of iterations kmeans may take to compute the clusters
#' @param nstart The number of clusterings from which kmeans chooses the best
#' @param max_eigenvalues Restrict number of computed eigenvalues to max_eigenvalues.
#' @param decomp The decomposition method to use: 'svd' (default) or 'svds'.
#' @param heuristic The function to use for calculating the number of expected clusters (k) and the embedding
#'
#'
#'
#'
scca_compute_tree <- function(labels, m, child, depth, max_depth, n_node,
                              iter.max        = 10,
                              nstart          = 50,
                              max_eigenvalues = max_eigenvalues,
                              decomp           = 'svd',
                              heuristic        = eigengap_heuristic) {

  if (!is.matrix(m)) {stop("Argument m is not a matrix")}

  # Subset the data matrix for the rows to be analyzed at this node
  #
  subM <- m[labels, , drop = FALSE]

  # create a list of attributes to collect analysis data
  #
  zero_vector  <- rep(0, length(labels))            # Used as a place holder for eigenvectors
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

  # Sets of fewer than 3 observations are not decomposed and clusterd any further.
  #
  if (length(labels) < 3 || depth == max_depth) {
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

  # Apply a user-provided heuristic on spectrum (eigenvalues) to calculate the optimal number (k) of clusters
  # and/or the embedding matrix. See @details for conditions an heuristic must meet
  #
  h_out <- heuristic(eigen_values, eigen_vectors)

  # An heuristic always has an embedding matrix as outcome and
  # 1. A range for the optimal number of clusters (min.nc and max.rc), or
  # 2. the exact number of clusters (min.rc = max.rc)
  #
  if (h_out$min.nc == h_out$max.nc) {
    k        <- h_out$max.nc   # the heuristic has determined the optimal number of clusters --> kmeans will compute the clusters
    clusters <- NULL
    if (k > 1) {
      cl       <- stats::kmeans(x = h_out$Y, centers = k, iter.max = iter.max, nstart = nstart)  # returns vector cl$cluster which gives the cluster id for each label
      clusters <- cl$cluster
    }
  } else {
    if (h_out$min.nc < h_out$max.nc) { # the optimal number of clusters lies between min.nc and max.nc --> NbClust will find the best k.
      cl       <- NbClust::NbClust(data = h_out$Y, max.nc = h_out$max.nc, min.nc = h_out$min.nc, method = 'kmeans', index = 'dunn')
      k        <- cl$Best.nc[1]         # this is the best value for k
      clusters <- cl$Best.partition     # and these are the clusters

      if (k == 1) {                     # no clustering at all
        clusters <- NULL
      }
      max_depth <- depth + 1            # hierarchical process must stop at next level
    } else {
      stop('illegal values for min.nc or max.nc')
    }
  }

  cluster_node[['k']] <- k

  # If k == 1 then this subset can't be split any further in meaningful sub-clusters. So recursion on this path stops here
  #
  if (k == 1) {
    cluster_node[['node_type']] <- 'leaf'
    return(list(cluster_node = cluster_node, n_node = n_node))
  }

  # repeat proces for each cluster (C_i with i in 1:k) and combine results in a list which will be
  # stored in the 'nodes' attribute of this node
  #
  for (child in 1:k) {
    child_labels <- rownames(subM)[clusters == child]
    child_tree   <- scca_compute_tree(
      labels   = child_labels,
      m        = m,
      child    = child,
      depth    = depth + 1,
      max_depth = max_depth,
      n_node    = n_node + 1,
      iter.max  = iter.max,
      nstart    = nstart,
      max_eigenvalues = max_eigenvalues,
      decomp   = decomp,
      heuristic = heuristic)
    cluster_node$node[[child]] <- child_tree$cluster_node
    n_node                     <- child_tree$n_node
  }
  return(list(cluster_node = cluster_node, n_node = n_node))
}




