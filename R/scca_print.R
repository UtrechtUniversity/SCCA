#' Print SCCA Analysis
#'
#' Prints the hierarchical analysis to screen in character-based, human-readable fashion
#'
#' @param scca An output of scca_compute
#' @param ... The attributes to be printed: 'depth', 'k', 'node_type' and 'n_labs'
#'
#' @details
#'   The nodes (subclusters) are numbered in depth-first pre-order. \code{depth} is the level in the tree. \code{n_labs} is the number of
#'   of labels in the (sub-)cluster. \code{child} is the numbering within its siblings. And \code{k} is the number of relevant Eigenvalues.
#'   Each attribute can be ommited. If no attribute is given, then only the hierarchy is printed.
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

#' Plot Spectrum of a Cluster
#'
#' Plots the sorted spectrum of a (sub-)cluster at a specific node in the output tree of
#' \code{scca_compute}.
#'
#' @param scca The output of \code{scca_compute}
#' @param node_id The node number (integer or coercible to) of the cluster in the output tree
#' @param plot If TRUE (default) the spectrum will be plotted. If FALSE only the spectrum data is returned.
#'
#' @details
#'   The node numbers can be displayed by \code{scca_print(scca)}. The function returns a tibble containing
#'   the spectrum data which can (e.g.) be used for your own plotting endeavours.
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
    g <- ggplot2::ggplot(data = spectrum_tbl, mapping = ggplot2::aes(x = order, y = value)) +
      ggplot2::geom_point(color = 'steelblue') +
      ggplot2::labs(title = sprintf("Spectrum of node %s", node_id))
    print(g)
  }

  return(spectrum_tbl)
}

scca_get_node <- function(scca, node_id) {

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
    g <- ggplot2::ggplot(data = spectrum_tbl, mapping = ggplot2::aes(x = order, y = value)) +
      ggplot2::geom_point(color = 'steelblue') +
      ggplot2::labs(title = sprintf("Spectrum of node %s", node_id))
    print(g)
  }

  return(spectrum_tbl)
}

#' Retrieve Attributes of a Node in an SCCA tree
#'
#'
#'
#' @param scca An SCCA tree
#' @param node The number of the node to retrieve the attributes from. The supported attributes are
#' currently \emph{spectrum} and \emph{labels}
#'
#' @details Returns NULL if \emph{node} doesn't exists.
#'
#' @return List with two elements:
#' \describe{
#'   \item{labels}{The labels (rownames) of the (sub-)cluster in this node}
#'   \item{spectrum}{The Eigenvalues of the data matrix}
#' }
#'
#' @export

scca_get_node <- function(scca, node) {
  if (scca$n_node == node) {
    return(list(spectrum = scca$spectrum, labels = scca$labels))
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





