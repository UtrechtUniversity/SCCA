#' Write a SCCA outcomne to file
#'
#' @details
#' Writes the results of an SCCA analysis to .csv files. At every tree node the subcluster is is printed along
#' with its 3 most prominent Eigenvectors. The filename contains the path from the top to the tree node.
#' Another file contains the spectra found at this stage.
#'
#' @param scca_tree Output tree of an call to scca_compute
#' @param vec_name Prefix of the files with the Eigenvectors
#' @param spec_name Prefix of files with the spectra
#'
#' @return
#' Function returns TRUE
#'
#' @export
scca_write_analysis <- function(scca_tree, vec_name = 'V_', spec_name = 'S_') {
  if (!is.list(scca_tree) || is.null(scca_tree)) {
    stop("argument 'analysis_tree' does not have a valid value.")
  }

  if (!is.character(vec_name) || !is.character(spec_name)) {
    stop("argument vec_name and/or spec_name not a character string.")
  }
  scca_print_tree(tree_node = scca_tree, path = list(), v_name = vec_name, s_name = spec_name)
}

scca_print_tree <- function(tree_node, path, v_name, s_name) {

  # update path with number of this node
  #
  path      <- append(path, tree_node$child)

  # write cluster labels and Eigen vectors of this node
  #
  file_name <- file.path(getwd(), sprintf('%s_%s.csv', v_name, paste(unlist(path), collapse = '_')))
  vecs_tbl  <- tibble::tibble(labels = tree_node$labels, eig_1 = tree_node$eigen_vec_1)
  readr::write_csv(x = vecs_tbl, path = file_name, col_names = TRUE)

  # write spectrum of this node
  #
  file_name <- file.path(getwd(), sprintf('%s_%s.csv', s_name, paste(unlist(path), collapse = '_')))
  spec_tbl  <- tibble::tibble(Spectrum = tree_node$spectrum)
  readr::write_csv(x = spec_tbl, path = file_name, col_names = TRUE)

  # if this node is a 'branch' then recursively call the childs

  if (tree_node$node_type == 'branch') {
    lapply(X = tree_node$node, FUN = scca_print_tree, path = path, v_name = v_name, s_name = s_name)
  }
  return(TRUE)
}
