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

  # Generate Laplacian matrix (L) from the Simmilariry matrix (S) of the bipartite adjacency matrix (M)
  #

  L <- generate_laplacian(M)

  # Iteratively find the clustering using the heuristic
  #
  work_to_do <- TRUE
  while (work_to_do) {

    # hier wordt het voor mij onbegrijpelijk
    #

    # check clustering condition
    #
  }
  return(NULL)
}
