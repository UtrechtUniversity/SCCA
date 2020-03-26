#' Build recursively a scca tree
#'
#' @param m Numeric matrix; Laplacian
#' @param child Numeric. Node number within its siblings (= same parent)
#' @param labels Character vector; The labels defining the (sub-)cluster to be analyzed
#' @param level Integer; the depth of this node in the tree
#' @param decomp_axis Character string; the choices are: 'row' or 'col'
#' @param pre_centers If TRUE, a list of k cluster centers will be computed
#' If FALSE (default) kmeans must decides with which k cluster it must start
#' @param iter.max Integer; the maximum number of iterations kmeans may take to compute the clusters
#' @param nstart Integer; the number of clusterings from which kmeans chooses the best
#'
#'

scca_compute_tree <- function(labels, child, m, level, decomp_axis,
                              pre_centers  =  FALSE,
                              iter.max     =  10,
                              nstart       =  50) {

  if (!is.matrix(m)) {stop("Argument m is not a matrix")}

  # Select the rows of the sub matrix that has to be analyzed at this node
  #
  subM <- m[labels, , drop = FALSE]

  #
  zero_vector  <- rep(0, length(labels))            # used as a place holder for eigen_vectors of this sub-matrix
  cluster_node <- list(level       =  level,        # the steps taken form top level to this node
                       child       =  child,        # number to distinguish this node from its siblings
                       labels      =  labels,       # current cluster
                       k           =  NULL,         # number of non-trivial, contributing eigenvalues
                       node_type   = 'branch',      # branch or leaf
                       eigen_vec_1 =  zero_vector,  # eigen vectors after decomposition of this cluster
                       eigen_vec_2 =  zero_vector,
                       eigen_vec_3 =  zero_vector,
                       spectrum    =  vector(mode = 'integer'),  # eigenvalues sorted on explained variance
                       node        =  list(NULL))   # will contain list of children (if any)

  #
  decomposition <- decomp_symmetric(matrix = subM, decomp_axis = decomp_axis)   #s and d_inv

  n_eigen <- ifelse (dim(decomposition$vectors)[2] < 3, dim(decomposition$vectors)[2], 3)
  for (i in 1:n_eigen) {
    eigen_vec_name                 <- sprintf('eigen_vec_%d', i)
    cluster_node[[eigen_vec_name]] <- decomposition$vectors[ , i]
  }
  cluster_node[['spectrum']]       <- decomposition$values

  # apply heuristic on spectrum (eigenvalues) to calculate the number (k) of expected clusters in matrix
  #
  #
  k <- apply_heuristic(decomposition$values)

  cluster_node[['k']] <- k



  # If k == 1 then this cluster can't be split any further in meaningful sub-clusters. So recursion on
  # this path stops here
  #
  if (k == 1) {
    cluster_node[['node_type']] = 'leaf'
    return(cluster_node)
  }

  #
  if (isTRUE(pre_centers)){
    centers <- compute_centers(k)
  } else {
    centers <- k
  }

  # Compute input matrix Y for the kmeans.
  # Y is a matrix with the eigenvectors as columns minus the first trivial one
  #
  Y <- decomposition$vectors[ , 2:k]

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
                                   MoreArgs = list(m = m, level = level + 1, decomp_axis = decomp_axis,
                                                   pre_centers = pre_centers,
                                                   iter.max    = iter.max,
                                                   nstart      = nstart),
                                   SIMPLIFY = FALSE)
  return(cluster_node)
}




