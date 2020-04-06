#' Print SCCA Analysis
#'
#' Prints the hierarchical analysis to screen in character-based, human-readable fashion
#'
#' @param scca An output of scca_compute
#' @param ... The attributes to be printed: 'depth', 'k', 'node_type' and 'nlabs'
#'
#' @details
#'   The numbers in the tree are the child numbers. \code{depth} is the level in the tree and \code{nlabs} the number of
#'   of labels in the (sub-)cluster. Each attribute can be ommited. If no attribute is given, then only the hierarchy is printed.
#' @examples
#'   \dontrun{
#'      s <- scca_compute(t(carnivora))
#'      scca_print(scca = s, 'k', 'nlabs')
#' }
#' @export

scca_print <- function(scca, ...) {

  scca_dt <- data.tree::FromListExplicit(scca, nameName = 'child', childrenName = 'node')
  print(scca_dt, ...)
}


