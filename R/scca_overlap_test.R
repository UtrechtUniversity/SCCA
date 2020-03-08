#' Get clustering from an output of an SCCA run
#'
#'
#' @param scca An SCCA analysis tree; output from a run of scca_compute on a dataset
#'
#' @return A tibble with two columns: label and cluster. Each row represents a case identified by
#'  a label and it's cluster.
#'
#' @details get_clustering is a wrapper around get_clustering_recursive
#'
#' @return Returns a tibble. Each row contains a label of a case and the id of the cluster it's assigned to.
#'
#' @import dplyr
#' @import magrittr
#'
#' @export
#'
scca_get_clustering <- function(scca) {
  cl <- get_clustering_recursive(scca = scca, id = 0, leaves_only = TRUE)
  if(is.null(cl$clustering)) {
    return(NULL)
  }
  return(cl$clustering %>% arrange(factor(labels, levels = scca$labels)) %>% rename(label = labels, cluster = id))
}

#' Get Cluster Recursively
#'
#' Descends recursively the analysis trees and collects the final clusters (leaves) in one dataframe (tibble)
#'
#' @param scca An SCCA tree or sub-tree
#' @param id Integer; the number of the last found node. Equals 0 when no cluster has been found yet.
#' @param leaves_only Boolean, default is TRUE. Not implemented. Do not use!
#'
get_clustering_recursive <- function(scca, id = 0, leaves_only = TRUE) {

  if (scca$node_type == 'leaf') {
    id          <- id + 1
    clustering  <- tibble::tibble(labels = scca$labels, id = id) # these cases belong to the same cluster
  }

  # if this node is a 'branch' then recursively call the childs and bind the results of those childs
  #
  if (scca$node_type == 'branch') {
    clustering <- tibble::tibble(labels = character(), id = integer())
    for (child in 1:length(scca$node)) {
      cl            <- get_clustering_recursive(scca = scca$node[[child]], id = id, leaves_only = leaves_only)
      clustering    <- rbind(clustering, cl$clustering)
      id            <- cl$id
    }
  }
  return(list(clustering=clustering, id = id))
}

#' SCCA Overlap Test
#'
#' The function \code{scca_overlap_test} compares two SCCA clusterings of the same dataset and category to
#' establish their ovelap. It calculates an average proportion of overlap between the two clusterings.
#'
#'
#' @param x An output tree of an SCCA run
#' @param y An output of another SCCA run on the same dataset/category as in x
#' @param plot Boolean; plot an bi-partite, overlap graph between clusters of x and y. Default is FALSE
#'
#'
#' @return A list with 3 elements:
#' \describe{
#'   \item{avg_overlap.x}{The chance that a pair of cases which are in the same cluster of x
#'   are also in the same cluster of y. The weighted average of overlaps of all clusters in x with clusters in y }
#'   \item{avg_overlap.y}{The weighted average of overlaps of all clusters in y }
#'   \item{overlap.xy}{Tibble. Overlap between each (x,y) pair of clusters}
#' }
#'
#' @examples
#' \dontrun{
#' data('carnivora', package = 'sccar')
#' sc1 <- scca_compute(carnivora)
#' sc2 <- scca_compute(carnivora)
#' scca_overlap_test(x = sc1, y = sc2)
#' }
#' @import magrittr
#' @import dplyr
#'
#' @export

scca_overlap_test <- function(x, y, plot = FALSE) {
  if (!all(x$labels == y$labels)) {
    warning("Not from the same dataset and or category!")
    return(NULL)
  }
  cl.x <- scca_get_clustering(scca = x)
  cl.y <- scca_get_clustering(scca = y)

  if (is.null(cl.x) || is.null(cl.y)) {
    warning("Something wrong in x or y")
    return(NULL)
  }

  cl.x <- group_by(.data = cl.x, .data$cluster) %>% mutate(card = n())      # number of labels in this cluster (a.k.a cardinal)
  cl.y <- group_by(.data = cl.y, .data$cluster) %>% mutate(card = n())

  # for each cluster pair (one from x and one from y) calculate the intersection (overlap)
  #
  cl.xy <- inner_join(x = cl.x, y = cl.y, by = c('label' = 'label'))

  cl.xy <- group_by(.data = cl.xy, .data$cluster.x, .data$cluster.y, .data$card.x, .data$card.y)
  cl.xy <- summarise(.data = cl.xy, inter =n())
  cl.xy <- ungroup(x = cl.xy)

  # for both clusters in a pair, calculate their average proportions of overlap
  #
  cl.xy <- mutate(
    .data = cl.xy,
    overlap.x = .data$inter/.data$card.x,
    overlap.y = .data$inter/.data$card.y)

  # For all clusters (in x or y) compute the weighted average of overlap
  #
  avg_overlap.x   <- sum(cl.xy$inter * cl.xy$overlap.x) / sum(cl.xy$inter)
  avg_overlap.y   <- sum(cl.xy$inter * cl.xy$overlap.y) / sum(cl.xy$inter)

  if(plot) {
    cl.xy %<>% ungroup()
    plot_overlap(cl.xy)
  }

  return(list(avg_overlap.x = avg_overlap.x,
              avg_overlap.y = avg_overlap.y,
              overlap.xy    = cl.xy))
}

#' Plot the overlap between two clusterings of the same dataset and category
#'
#' @param overlap_xy A tibble with overlaps between pairs of clusters
#'
#' @import magrittr
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom graphics plot
#' @importFrom rlang .data
#'
#' @note Do not use; contains bugs
#'
plot_overlap <- function(overlap_xy) {

  overlap_xy <- overlap_xy %>%
    dplyr::mutate(edge = sprintf('%2.1f/%2.1f', overlap_xy$overlap.x, overlap_xy$overlap.y))

  overlap_xy <- dplyr::select(.data = overlap_xy, .data$cluster.x, .data$cluster.y, .data$edge)

  # wider format for conversion to matrix
  overlap_xy <- tidyr::pivot_wider(
    data         = overlap_xy,
    names_from   = cluster.y,
    names_prefix = 'y_',
    values_from  = edge,
    values_fill  = list(edge = 0))

  row_names                <- sprintf('x_%d', pull(overlap_xy, cluster.x))    # for conversion to matrix
  overlap_xy               <- dplyr::select(.data = overlap_xy, -.data$cluster.x)
  overlap_matrix           <- as.matrix(overlap_xy)
  rownames(overlap_matrix) <- row_names

  # create weighted bipartite network
  #
  bip = network::network(overlap_matrix,
                   matrix.type = "bipartite",
                   ignore.eval = FALSE,
                   names.eval  = "weights")

  col = c("actor" = "grey", "event" = "gold")
  p <- GGally::ggnet2(bip,
         color           = 'mode',
         palette         = col,
         label           = TRUE,
         edge.label      = 'weights',
         edge.label.size = 3,
         label.size      = 3,
         alpha           = 0.3,
         size            = 9,
         edge.label.color = 'red')
  plot(p)
}


