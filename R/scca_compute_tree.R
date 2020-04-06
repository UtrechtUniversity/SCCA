#' Build recursively a scca tree
#'
#' @param m Numeric matrix; Laplacian
#' @param child Numeric. Node number within its siblings (= same parent)
#' @param labels Character vector; The labels defining the (sub-)cluster to be analyzed
#' @param depth Integer the depth of this node in the tree
#' @param iter.max Integer; the maximum number of iterations kmeans may take to compute the clusters
#' @param nstart Integer; the number of clusterings from which kmeans chooses the best
#' @param decomp The decomposition method to use: 'svd' or 'svds'.
#'
#'
#'
#'
scca_compute_tree <- function(
  labels, m, child, depth,
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
                       nlabs       =  length(labels),
                       k           =  0,            # number of most contributing eigenvalues
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
    return(cluster_node)
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
  # stored in the 'nodes' attribute of this node
  #
  cluster_labels <- list()
  cluster_labels <- lapply(X = 1:k, FUN = function(i) rownames(subM)[cl$cluster == i])

  # for each set of labels in list repeat scca proces and combine results in a list
  # lapply means 'list apply'
    #
  cluster_node[['node']] <- mapply(FUN = scca_compute_tree,
                                   cluster_labels,               # will be mapped to argument 'labels'
                                   as.list(1:k),                 # will be mapped to argument 'child'
                                   MoreArgs = list(m = m,
                                                   depth    = depth + 1,
                                                   iter.max = iter.max,
                                                   nstart   = nstart,
                                                   decomp   = decomp),
                                   SIMPLIFY = FALSE)
  return(cluster_node)
}




