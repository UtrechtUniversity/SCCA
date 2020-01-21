#' Build recursively a SCCA tree
#'
#' @param M Numeric matrix; Laplacian
#' @param labels Character vector; The labels defining the rows/columns of the submatrix
#' @param level Integer; the depth of the node in the tree
#' @param axis Vector; should the rows or columns be subsetted
#'
#' @note Expects an matrix M accessible from within its environment
#'

scca_node <- function(M, labels, level, axis) {

  if (!is.matrix(M)) {stop("Argument M is not a matrix")}


  # Select the part of the matrix that has to be analyzed
  #
  if (axis == 'rows') {
    subM <- M[labels, ]
  } else {
    subM <- M[ ,labels]
  }

  # Calculate Laplacian, Simmilarity matrix and DCA
  gl <- generate_laplacian(A = subM, cluster_dim = axis)

  # calculate Eigenvectors and Eigenvalues of Laplacian
  Eigen_dc <- Eigen_decomp(Laplacian = gl$L, DCA = gl$DCA, axis = axis)

  # apply heuristic on spectrum (eigenvalues) to calculate the number (k) of expected clusters in matrix
  # k <- heuristic(spectrum = spectrum)
  #
  k <- apply_heuristic(Re(Eigen_dc$values))

  # we expect the current matrix can be split in k clusters and so on
  # we build a tree (recursive list) with a node per cluster
  #
  cluster_node <- list(level = level, k = k, node_type = 'branch', node = NULL)

  # stop recursion when clustering doesn't make sense anymore
  # return the set of labels as a leaf node
  #
  if (k <= 1 || k >= length(labels)) {
    cluster_node[['node_type']] = 'leaf'
    cluster_node[['node']]      =  labels
    return(cluster_node)
  }

  # Create matrix Y
  Y <- create_Y(Eigenvecs = Eigen_dc$vectors, Eigenvals = Eigen_dc$values, subM = subM, k = k)

  # clustering with kmeans and n_clusters = k -->  list of k subclusters
  #
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
  cluster_node[['node']] <- lapply(X = cluster_labels, FUN = scca_node, M = M, level = level + 1, axis =axis)
  return(cluster_node)
}




