#' Build recursively a scca tree
#'
#' @param m Numeric matrix; Laplacian
#' @param child Numeric. Child number among its siblings (= same parent) in the tree
#' @param labels Character vector; The labels defining the rows/columns of the submatrix
#' @param level Integer; the depth of the node in the tree
#' @param axis Vector; should the rows or columns be subsetted
#'
#' @note Expects an matrix M accessible from within its environment
#'

scca_compute_tree <- function(labels, child, m, level, axis) {

  if (!is.matrix(m)) {stop("Argument m is not a matrix")}

  # Select the part of the matrix that has to be analyzed
  #
  if (axis == 'rows') {
    subM <- m[labels, ]
  } else {
    subM <- m[ ,labels]
  }

  # we expect the current matrix can be split in k clusters
  # we build a tree (recursive list) with a node per cluster
  #
  cluster_node <- list(level       =  level,
                       child       =  child,
                       labels      =  labels,
                       k           =  NULL,
                       node_type   = 'branch',
                       eigen_vec_1 =  NULL,
                       spectrum    =  NULL,
                       node        =  NULL)

  # Calculate Laplacian, Simmilarity matrix and DCA
  gl <- generate_laplacian(matrix_a = subM, cluster_dim = axis)

  # calculate Eigenvectors and Eigenvalues of Laplacian

  if (nrow(gl$L) < 3) {                       # 'eigs' needs a matrix with more than 2 rows
    #warning('stop 3: ', length(labels))
    cluster_node[['node_type']] = 'leaf'
    cluster_node[['node']]      =  labels
    return(cluster_node)
  }
  eigen_dc <- eigen_decomp(laplacian = gl$L, dca = gl$DCA, axis = axis)
  cluster_node[['eigen_vec_1']] <- eigen_dc$vectors[ , 1]
  cluster_node[['spectrum']]    <- eigen_dc$values

  # apply heuristic on spectrum (eigenvalues) to calculate the number (k) of expected clusters in matrix
  # k <- heuristic(spectrum = spectrum)
  #
  k <- apply_heuristic(eigen_dc$values)
  cluster_node[['k']] <- k


  # stop recursion when clustering doesn't make sense anymore
  # return the set of labels as a leaf node
  #
  if (k <= 1 || k >= length(labels)) {
    #warning('stop k')
    cluster_node[['node_type']] = 'leaf'
    cluster_node[['node']]      =  labels
    return(cluster_node)
  }

  # Create matrix Y
  Y <- create_y(
    eigenvectors = eigen_dc$vectors,
    eigenvalues =  eigen_dc$values,
    sub_matrix =   subM,
    k =            k
  )



  # clustering with kmeans and n_clusters = k -->  list of k subclusters
  #
  if (nrow(unique.matrix(Y)) <= k) {
    #warning('stop Y')
    cluster_node[['node_type']] = 'leaf'
    cluster_node[['node']]      =  labels
    return(cluster_node)
  }
  cl <- stats::kmeans(x = Y, centers = k)  # cl$cluster is a num vector of length rows/cols
                                           # the value (=1:k) of element i gives the cluster id the row/col i is in

  # repeat proces for each cluster and combine results in a list which will be stored as element 'node'
  #

    # create list of label set; for each subcluster one
    #
  cluster_labels <- list()

  if (axis == 'rows') {
    cluster_labels <- lapply(X = 1:k, FUN = function(i) rownames(subM)[cl$cluster == i])
  } else {
    cluster_labels <- lapply(X = 1:k, FUN = function(i) colnames(subM)[cl$cluster == i])
  }
    # for each set of labels in list repeat scca proces and combine results in a list
    # lapply means 'list apply'
    #
  cluster_node[['node']] <- mapply(FUN = scca_compute_tree,
                                   cluster_labels,
                                   as.list(1:k),
                                   MoreArgs = list(m = m, level = level + 1, axis =axis),
                                   SIMPLIFY = FALSE)
  return(cluster_node)
}




