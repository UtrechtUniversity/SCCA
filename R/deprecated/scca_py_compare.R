#' Get the Clustering Produced by Python Script
#'
#' Output files of the Python script are read a to construct
#' a tibble with clusters which can be compared with a clustering from SCCA provided both clusterings are
#' based on the same dataset and axis.
#'
#' @param path The path from the working directory to the directory containing the Python output
#'
#' @return A tibble with 3 variables:
#' \describe{
#'   \item{label}{The label of the case/observation.}
#'   \item{cluster}{The id of the cluster to which the observation is assigned.}
#'   \item{path}{The sequence of child numbers (seperated by '.') at each level from top to the cluster.}
#' }
#'
#' @details
#' This function is a helper for \code{\link{scca_py_overlap_test}}

get_py_clustering <- function(path = NULL) {

  if (is.null(path)) {
    path <- getwd()
  } else {
    path <- file.path(getwd(), path)
  }
  if (!dir.exists(path)) {
    stop('directory does not exists')
  }

  py_files        <- list.files(path = path, pattern = '^Vecs_0\\..*\\.csv$')
  py_cluster_path <- stringr::str_match(py_files, "(?:\\.)(.*)(?:\\.csv)")[,2]

  python_clusters <- tibble::tibble(
    label   = character(),
    cluster = integer(),
    path    = character()
  )

  cluster_id <- 0L
  for (cluster_path in py_cluster_path) {
    cluster_id     <- cluster_id + 1
    cluster_data   <- readr::read_csv(file      = file.path(path, sprintf('/Vecs_0.%s.csv', cluster_path)),
                                      skip      = 1,
                                      col_types = readr::cols(),
                                      col_names = FALSE)
    cluster_data %<>% dplyr::mutate(label = as.character(.data$X2))
    cluster_data %<>% dplyr::mutate(cluster = cluster_id,
                                    path    = cluster_path)
    cluster_data %<>% dplyr::select(.data$label, .data$cluster, .data$path)

    python_clusters %<>% rbind(cluster_data)
  }
  return(python_clusters)
}

#' Overlap Test between an SSCA clustering and a 'Python clustering'
#'
#' The \strong{SCCA package} replaces an implementation of SCCA in Python.
#' \emph{scca_py_overlap_test} compares clusterings from both implementations by calculating
#' the average proportion of overlap.
#' For more info see \link{scca_overlap_test}.
#' In due time this function will be phased out.
#'
#' @param scca An SSCA tree.
#' @param py_output
#' The path from the working directory to the directory with the Python output files.
#' If NULL (default), then the working directory is taken as path.
#' @param plot If TRUE an overlap graph is plotted; default is FALSE.
#'
#' @details
#'   For more info see \code{\link{scca_overlap_test}}.
#'
#'   In due time this function will be phased out.
#'
scca_py_overlap_test <- function(scca, py_output = NULL, plot = FALSE) {

  cl.scca <- scca_get_clusters(scca = scca)
  cl.py   <- get_py_clustering(path = py_output)

  if (is.null(cl.scca) || is.null(cl.py)) {
    warning("Something wrong in scca and/or Py clustering")
    return(NULL)
  }
  return(clustering_overlap(cl.x = cl.scca, cl.y = cl.py, plot = plot))
}
