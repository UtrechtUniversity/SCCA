#' Write  a SCCA to File
#'
#' Writes the results of an analysis to .csv files
#'
#'
#' @param analysis_tree Output tree of an SCCA
#' @param vec_name Prefix of the names for files with the Eigenvectors
#' @param spec_name Prefix of the filenames with the spectra
#'
#' @return TRUE
#'
#' @export
scca_write_analysis <- function(analysis_tree, vec_name, spec_name) {
  if (!is.list(analysis_tree) || is.null(analysis_tree)) {
    stop("argument 'analysis_tree' does not have a valid value.")
  }
  if (is.null(vec_name))  { vec_name = 'Vecs'}
  if (is.null(spec_name)) { spec_name = 'Spec'}
  if (!is.character(vec_name) || !is.character(spec_name)) {
    stop("argument vec_name and/or spec_name not a character string.")
  }
  scca_print_tree(node = analysis_tree, path = list(), v_name = vec_name, s_name = spec_name)
}

scca_print_tree <- function(node, path, v_name, s_name) {

  # update path with number of this node
  #
  path      <- append(path, node$child)

  # write cluster labels and Eigen vectors of this node
  #
  file_name <- file.path(getwd(), sprintf('%s_%s.csv', v_name, paste(unlist(path), collapse = '_')))
  vecs_tbl  <- tibble::tibble(labels = node$labels, eig_1 = node$eigen_vec_1)
  readr::write_csv(x = vecs_tbl, path = file_name, col_names = TRUE)

  # write spectrum of this node
  #
  file_name <- file.path(getwd(), sprintf('%s_%s.csv', s_name, paste(unlist(path), collapse = '_')))
  spec_tbl  <- tibble::tibble(Spectrum = node$spectrum)
  readr::write_csv(x = spec_tbl, path = file_name, col_names = TRUE)

  # if this node is a 'branch' then recursively call the childs

  if (node$node_type == 'branch') {
    lapply(X = node$node, FUN = scca_print_tree, path = path, v_name = v_name, s_name = s_name)
  }
  return(TRUE)
}
