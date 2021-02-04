#' Get Final Clusters from an SCCA Analysis
#'
#' Produces for every observation a record with the label of the observation
#' and the final (or leaf) cluster to which the observation has been assigned by scca_compute.
#'
#'
#' @param scca An SCCA analysis tree as output from a run of \code{\link{scca_compute}} on a dataset
#'
#'
#' @return A tibble with 3 variables (columns)
#' \describe{
#'   \item{label}{The label of the case or observation}
#'   \item{cluster}{The id of the cluster to which the observation has been assigned. }
#'   \item{path}{The sequence of child numbers (seperated by '.') on the path from the top node to the leaf node}
#'
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#'
#' @export
#'
scca_get_clusters <- function(scca) {
  cl <- get_clusters_recursive(scca = scca, cluster_path = NULL)
  if(is.null(cl$clustering)) {
    return(NULL)
  }
  return(cl$clustering %>% dplyr::arrange(factor(labels, levels = scca$labels)) %>%
                           dplyr::rename(label = labels, cluster = .data$id, path = .data$cluster_path))
}

#' Get Clusters
#'
#' Descend recursively the analysis tree and collect the final or leaf clusters at the leaves.
#'
#' @param scca An SCCA tree
#' @param cluster_path Sequence of child numbers (seperated by '.') visited from top node to this node.
#' @noRd
#'
get_clusters_recursive <- function(scca, cluster_path = NULL) {

  if (is.null(cluster_path)) {
    cluster_path <- as.character(scca$child)
  } else {
    cluster_path <- paste(cluster_path, as.character(scca$child), sep = '.')
  }

  if (scca$node_type == 'leaf') {
    clustering  <- tibble::tibble(labels = scca$labels, id = scca$n_node, cluster_path = cluster_path) # these cases belong to the same cluster
  }

  # if this node is a 'branch' then recursively call the children and bind the results of those children
  #
  if (scca$node_type == 'branch') {
    clustering <- tibble::tibble(labels = character(), id = integer(), cluster_name = character())
    for (child in 1:length(scca$node)) {
      cl            <- get_clusters_recursive(scca = scca$node[[child]], cluster_path = cluster_path)
      clustering    <- rbind(clustering, cl$clustering)
    }
  }
  #return(list(clustering=clustering, id = id, cluster_path = cluster_path))
  return(list(clustering = clustering))
}

