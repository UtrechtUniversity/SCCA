#' Spectral Clustering Correpondence Analysis.
#'
#' \code{scca} returns the ... of its argument M.
#'
#' This function ....
#'
#' @param M A numeric matrix ......
#'
#' @return The function returns a list with .....
#'
#' @examples
#' \dontrun{
#' data('carnivora', package = 'SCCA')
#' scca(M = carnivora$M)
#' }
#' @export
scca <- function(M) {
  if (!is.matrix(M)) {
    stop('input not a matrix')
  }
  return(M)
}

scca_new <- function(M) {

  # The clustering takes place on the labels of the longest dimension of matrix
  #
  cluster_dim <- ifelse(dim(M)[1] >= dim(M)[2], 'rows', 'cols')     # >= ?

  # Generate Laplacian matrix (L) from the Simmilariry matrix (S) of
  # the bipartite adjacency matrix (M). But why? They aren't used in the program anywhere
  #

  lds <- generate_laplacian(M, cluster_dim) # returns lds$L(aplacian), lds$DCA and lds$S(im)

  # Iteratively find the clustering using the heuristic
  #
  work_to_do <- TRUE
  while (work_to_do) {

    # hier wordt het voor mij onbegrijpelijk
    #
    for (group in unique(labels)) {

      # calculate sub adj. matrix

      subM <- M[labels == group]
      if (len(subM) > 1) {            # number of rows
        lds <- generate_laplacian(subM, cluster_dim)      # L, DCA, S
        kvv <- heuristic(lds$L, lds$DCA)     # KVV$k, KVV$vecs, KVV$vals # k is number of clusters for kmeans
      }
      else {
        k <- 1   #
      }

      if (k > 1) {
        Y <- create_Y()                            # complex numbers ?
        kmeans = kmeans(x = Re(Y), centers = k)    # random state?   is KMeans in Python the same as kmeans in R?

        # do some accounting
        #
      } else if (k == 1) {
        # do some accounting
        #
      } else {
        stop("Something is wrong")
      }
    }

    # check clustering condition
    #
  }
  return(NULL)
}
