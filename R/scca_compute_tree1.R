#' Build recursively a scca tree
#'
#' @param m Numeric matrix; Laplacian
#' @param child Numeric. Child number among its siblings (= same parent) in the tree
#' @param labels Character vector; The labels defining the rows/columns of the submatrix
#' @param level Integer; the depth of the node in the tree
#' @param decomp_axis Vector; should the rows or columns be subsetted
#'
#' @note Expects an matrix M accessible from within its environment
#'

scca_compute_tree1 <- function(labels, child, m, level, decomp_axis) {

  if (!is.matrix(m)) {stop("Argument m is not a matrix")}

  # Select the rows of the sub matrix that has to be analyzed at this node
  #
  subM <- m[labels, , drop = FALSE]

  #
  zero_vector  <- rep(0, length(labels))            # used as a place holder for eigen_vectors of this sub-matrix
  cluster_node <- list(level       =  level,        # the steps taken form top level to this node
                       child       =  child,        # number to distinguish from its siblings
                       labels      =  labels,       # current cluster
                       k           =  NULL,         # number of non-trivial, contributing eigenvalues
                       node_type   = 'branch',      # branch or leaf
                       eigen_vec_1 =  zero_vector,  # eigen vectors after decomposition of this cluster
                       eigen_vec_2 =  zero_vector,
                       eigen_vec_3 =  zero_vector,
                       spectrum    =  vector(mode = 'integer'),  # eigenvalues sorted on explained variance
                       node        =  list(NULL))   # will contain list of children (if any)

  # C
  decomposition <- compute_symmetric(matrix = subM, decomp_axis = decomp_axis)   #s and d_inv


  cluster_node[['eigen_vec_1']] <- decomposition$vectors[ , 1]
  cluster_node[['spectrum']]    <- decomposition$values

  # apply heuristic on spectrum (eigenvalues) to calculate the number (k) of expected clusters in matrix
  # k <- heuristic(spectrum = spectrum)
  #
  k <- apply_heuristic(decomposition$values)
  cluster_node[['k']] <- k


  # If k == 1 then this cluster can't be split further in meaningful sub-clusters. So recursion on
  # this path stops here
  #
  if (k == 1) {
    cluster_node[['node_type']] = 'leaf'
    return(cluster_node)
  }

  # Compute input matrix Y for the kmeans.
  # Y is a matrix with the eigenvectors as columns minus the first trivial one
  #
  Y <- decomposition$vectors[ , 2:k]

  cl <- stats::kmeans(x = Y, centers = k)  # returns vector cl$cluster which gives the cluster id for each label

  # repeat proces for each cluster (C_i with i in 1:k) and combine results in a list which will be
  # stored in the 'nodes' attribute of this node
  #
  cluster_labels <- list()
  cluster_labels <- lapply(X = 1:k, FUN = function(i) rownames(subM)[cl$cluster == i])

  # for each set of labels in list repeat scca proces and combine results in a list
  # lapply means 'list apply'
    #
  cluster_node[['node']] <- mapply(FUN = scca_compute_tree1,
                                   cluster_labels,               # will be mapped to argument 'labels'
                                   as.list(1:k),                 # will be mapped to argument 'child'
                                   MoreArgs = list(m = m, level = level + 1, decomp_axis = decomp_axis),
                                   SIMPLIFY = FALSE)
  return(cluster_node)
}




