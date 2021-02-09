#' Print SCCA tree
#'
#' Prints the output of \emph{compute_scca} to screen in a human-readable format.
#'
#' @param scca An output of \code{\link{scca_compute}} to screen
#' @param ... The attributes to be printed: 'depth', 'k', 'node_type' and/or 'n_labs'
#'
#' @details
#'   The nodes in the analysis tree (clusters) are numbered depth-first and pre-order. \code{depth} is the level in the tree. \code{n_labs} is the number of
#'   of labels in the (sub-)cluster. \code{child} is the numbering within its siblings. And \code{k} is the number of relevant Eigenvalues.
#'   Every attribute can be omitted. If no attribute is given, only the tree hierarchy will be printed.
#'
#' @examples
#'   \dontrun{
#'      s <- scca_compute(t(carnivora))
#'      scca_print(scca = s, 'k', 'n_labs')
#' }
#' @export

scca_print <- function(scca, ... ) {
  scca_dt <- data.tree::FromListExplicit(scca, nameName = 'n_node', childrenName = 'node')
  print(scca_dt, ...)
}

#' Plot Spectrum of a Node
#'
#' Output and plot the spectrum (Eigenvalues in descending order) found at a particular node in a SCCA tree.
#'
#' @param scca The output of \code{scca_compute}
#' @param node_id The node number (numeric) in the output tree
#' @param plot If TRUE (default) the spectrum will be plotted. If FALSE only the spectrum data is returned.
#'
#' @details
#'   The node number can be found by printing the tree with \code{scca_print(scca)}.
#'
#' @return
#'   The function returns a tibble containing the spectrum data.
#'
#'
#' @importFrom rlang .data
#'
#' @examples
#'   \dontrun{
#'      s <- scca_compute(t(carnivora))
#'      scca_print(scca = s, 'k', 'n_labs')
#'      scca_plot_spectrum(scca = s, node_id = '3')
#' }
#' @export

scca_plot_spectrum <- function(scca, node_id, plot = TRUE) {

  # helper for Get to retrieve a spectrum of a specific node
  #
  get_spectrum <- function(node, n_node) {
    if (node$name != n_node) {
      return(NA)
    }
    return (node$spectrum)
  }

  # check input
  #
  node_id <- as.character(node_id)

  # Convert scca tree to a data.tree (dt) object
  #
  scca_dt <- data.tree::FromListExplicit(scca, nameName = 'n_node', childrenName = 'node')


  spectrum <- scca_dt$Get(get_spectrum, n_node = node_id, simplify = 'array')[[node_id]]

  spectrum_tbl <- tibble::tibble(order = 1:length(spectrum), value = spectrum)

  if (plot) {
    g <- ggplot2::ggplot(data = spectrum_tbl, mapping = ggplot2::aes(x = order, y = .data$value)) +
      ggplot2::geom_point(color = 'steelblue') +
      ggplot2::labs(title = sprintf("Spectrum of node %s", node_id))
    print(g)
  }

  return(spectrum_tbl)
}



#' Retrieve Attributes of a Node in an SCCA tree
#'
#' @param scca An SCCA tree
#' @param node The number of the node from which to retrieve the attributes.
#'
#' @details
#' The node number can be found by printing the tree with scca_print(scca).
#'
#' Returns NULL if \emph{node} doesn't exists.
#'
#' @return List with 5 elements:
#' \describe{
#'   \item{labels}{The labels (rownames) defining the subset (cluster) of observations at this node}
#'   \item{spectrum}{Sorted eigenvalues}
#'   \item{eigen_vec_1}{First eigenvector}
#'   \item{eigen_vec_2}{Second eigenvector}
#'   \item{eigen_vec_3}{Third eigenvector}
#' }
#'
#' @export

scca_get_node <- function(scca, node) {
  if (scca$n_node == node) {
    return(list(spectrum    = scca$spectrum,
                eigen_vec_1 = scca$eigen_vec_1,
                eigen_vec_2 = scca$eigen_vec_2,
                eigen_vec_3 = scca$eigen_vec_3,
                labels      = scca$labels))
  } else {
    if (scca$node_type == 'leaf') {
      return(NULL)
    } else {
      for( child in 1:scca$k) {
        result <- NULL
        result <- scca_get_node(scca = scca$node[[child]], node = node)
        if (!is.null(result)) {
          return(result)
        }
      }
    }
  }
}





