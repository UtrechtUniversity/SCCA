#' Write an Output of SCCA to Files
#'
#' Writes the results of an SCCA to .csv files. For every tree node two files are written:
#'
#' \describe{
#'   \item{Eigenvectors}{The first column contains labels of the subcluster which is analyzed at the node,
#' and columns 2 to 4 are the 3 most prominent Eigenvectors}
#'   \item{Spectrum}{The first column contains the spectrum of the subcluster and the second column the explained variance}
#' }
#'
#' The filename contains the path from the top to the tree node.
#'
#' @param scca_tree list; the tree resulting from a call to 'scca_compute'
#' @param vec_name Prefix  (string) of the names of files with the labels and the Eigenvectors
#' @param spec_name Prefix of the names of he files with the spectra
#' @param leaves_only If TRUE (default) only the final clusters (leaf nodes) are written
#'
#' @return
#' TRUE
#'
scca_write_clustering <- function(scca_tree, vec_name = 'V_', spec_name = 'S_', leaves_only = TRUE) {
  if (!is.list(scca_tree) || is.null(scca_tree)) {
    stop("argument 'analysis_tree' does not have a valid value.")
  }

  if (!is.character(vec_name) || !is.character(spec_name)) {
    stop("argument vec_name and/or spec_name not a character string.")
  }
  scca_print_tree(
    tree_node = scca_tree,
    path = list(),
    v_name = vec_name,
    s_name = spec_name,
    leaves_only = leaves_only)
}

scca_print_tree <- function(tree_node, path, v_name, s_name, leaves_only) {

  # update path with number of this node
  #
  path      <- append(path, tree_node$child)

  write_node <- TRUE
  if (isTRUE(leaves_only)) {
    write_node <- ifelse(tree_node$node_type == 'leaf', TRUE, FALSE)
  }


  if (isTRUE(write_node)) {
    # write cluster labels and Eigen vectors of this node
    #
    file_name <- file.path(getwd(), sprintf('%s_%s.csv', v_name, paste(unlist(path), collapse = '_')))
    vecs_tbl  <- tibble::tibble(labels = tree_node$labels,
                                eig_1 = tree_node$eigen_vec_1,
                                eig_2 = tree_node$eigen_vec_2,
                                eig_3 = tree_node$eigen_vec_3)
    readr::write_csv(x = vecs_tbl, path = file_name, col_names = TRUE)

    # write spectrum of this node
    #
    file_name <- file.path(getwd(), sprintf('%s_%s.csv', s_name, paste(unlist(path), collapse = '_')))
    spec_tbl  <- tibble::tibble(Spectrum = tree_node$spectrum)
    readr::write_csv(x = spec_tbl, path = file_name, col_names = TRUE)
  }

  # if this node is a 'branch' then call the childs (recursively)

  if (tree_node$node_type == 'branch') {
    lapply(X = tree_node$node, FUN = scca_print_tree, path = path, v_name = v_name, s_name = s_name,
           leaves_only = leaves_only)
  }
  return(TRUE)
}

