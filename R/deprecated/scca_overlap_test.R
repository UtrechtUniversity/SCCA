#' SCCA Overlap Test
#'
#' The function \emph{scca_overlap_test} compares two SCCA clusterings of the same dataset and category to
#' establish their ovelap. It calculates the average proportion of overlap between the two clusterings.
#'
#'
#' @param x An output tree of an SCCA run
#' @param y An output of another SCCA run on the same dataset/category as in x
#' @param plot Boolean; plot an bi-partite, overlap graph between clustering x and clustering y. Default is FALSE
#'
#'
#' @return A list with 3 elements:
#' \describe{
#'   \item{\strong{avg_overlap.x}}{The chance that a pair of cases which are in the same cluster of x
#'   are also in the same cluster of y}
#'   \item{avg_overlap.y}{The chance that a pair of cases which are in the same cluster of y
#'   are also in the same cluster of x}
#'   \item{overlap.xy}{A tibble with X variables:
#'     \itemize{
#'       \item \emph{cluster.x} Id of cluster in clustering x
#'       \item \emph{cluster.y} Id of cluster in clustering y
#'       \item \emph{card.x} Cardinal of x cluster
#'       \item \emph{card.y} Cardinal of y cluster
#'       \item \emph{inter} Intersection of x and y; the labels they have in common
#'       \item \emph{overlap.x} Chance two cases in the same x cluster are also in the same y cluster
#'       \item \emph{overlap.y} Chance two cases in the same y cluster are also in the same x cluster
#'       \item \emph{path.x} The path from the top of the analysis tree to this x cluster
#'       \item \emph{path.y} The path from the top of the analysis tree to this y cluster
#'    }
#'  }
#' }
#'
#' @examples
#' \dontrun{
#' data('carnivora', package = 'SCCA')
#' sc1 <- scca_compute(carnivora)
#' sc2 <- scca_compute(carnivora)
#' scca_overlap_test(x = sc1, y = sc2, plot = TRUE)
#' }
#' @import magrittr
#'
#' @noRd

scca_overlap_test <- function(x, y, plot = FALSE) {

  cl.x <- scca_get_clusters(scca = x)
  cl.y <- scca_get_clusters(scca = y)

  if (is.null(cl.x) || is.null(cl.y)) {
    warning("Something wrong in x or y")
    return(NULL)
  }
  return(clustering_overlap(cl.x, cl.y, plot = plot))
}

clustering_overlap <- function(cl.x, cl.y, plot = FALSE) {

  if(!dplyr::setequal(pull(cl.x, .data$label), pull(cl.y, .data$label))) {
    stop('Clusterings x and y not from the same dataset and category.')
  }

  path.x <- cl.x %>% dplyr::select(-.data$label) %>% dplyr::distinct()
  path.y <- cl.y %>% dplyr::select(-.data$label) %>% dplyr::distinct()


  cl.x <- dplyr::select(.data = cl.x, -.data$path)
  cl.y <- dplyr::select(.data = cl.y, -.data$path)

  cl.x <- cl.x %>% dplyr::group_by(.data$cluster) %>% dplyr::mutate(card = n())      # number of labels in this cluster (a.k.a cardinal)
  cl.y %<>% dplyr::group_by(.data$cluster) %>% dplyr::mutate(card = n())

  # for each cluster pair (one from x and one from y) calculate the intersection (overlap)
  #
  cl.xy <- dplyr::inner_join(x = cl.x, y = cl.y, by = c('label' = 'label'))

  cl.xy <- dplyr::group_by(.data = cl.xy, .data$cluster.x, .data$cluster.y, .data$card.x, .data$card.y)
  cl.xy <- dplyr::summarise(.data = cl.xy, inter =n())
  cl.xy <- dplyr::ungroup(x = cl.xy)

  # for both clusters in a pair, calculate their average proportions of overlap
  #
  cl.xy <- dplyr::mutate(
    .data = cl.xy,
    overlap.x = .data$inter/.data$card.x,
    overlap.y = .data$inter/.data$card.y)

  # For all clusters (in x or y) compute the weighted average of overlap
  #
  avg_overlap.x   <- sum(cl.xy$inter * cl.xy$overlap.x) / sum(cl.xy$inter)
  avg_overlap.y   <- sum(cl.xy$inter * cl.xy$overlap.y) / sum(cl.xy$inter)

  if(plot) {
    cl.xy %<>% dplyr::ungroup()
    plot_overlap(cl.xy)
  }

  cl.xy <- dplyr::inner_join(x = cl.xy, y = path.x, by = c('cluster.x' = 'cluster'))
  cl.xy <- dplyr::inner_join(x = cl.xy, y = path.y, by = c('cluster.y' = 'cluster'))

  return(list(avg_overlap.x = avg_overlap.x,
              avg_overlap.y = avg_overlap.y,
              overlap.xy    = cl.xy))
}

#' Plot the overlap between two final clusterings (X and Y) based on the same dataset and category
#'
#' @param overlap_xy A tibble with overlaps between pairs of clusters. See details.
#'
#' @import dplyr
#' @importFrom rlang .data
#'
#' @note If the number of clusters in X is equal to number of Y, there is a disconnected node
#' \strong{ignore_me} added to clustering X. This is node is added as a fix of a bug in ploting package (GGally)
#'
#' @noRd
plot_overlap <- function(overlap_xy) {

  overlap_xy <- overlap_xy %>%
    dplyr::mutate(edge = sprintf('%2.1f/%2.1f', overlap_xy$overlap.x, overlap_xy$overlap.y))

  overlap_xy <- dplyr::select(.data = overlap_xy, .data$cluster.x, .data$cluster.y, .data$edge)

  # wider format before conversion to matrix
  #
  overlap_xy <- tidyr::pivot_wider(
     data         =  overlap_xy,
     names_from   = .data$cluster.y,
     names_prefix = 'y_',
     values_from  = .data$edge,
     values_fill  =  list(edge = 0))  # interpreted by network as 'no edge'

  row_names                <- sprintf('x_%d', pull(overlap_xy, .data$cluster.x))    # for conversion to matrix
  overlap_xy               <- dplyr::select(.data = overlap_xy, -.data$cluster.x)
  overlap_matrix           <- as.matrix(overlap_xy)

  # ggnet does not plot a bipartite graph correctly if the
  # input matrix is square (the number of clusters in X is equal to
  # the number of clusters in Y). We fix this by adding a not connected node (cluster) in X
  # The node is appropriately called 'ignore_me'
  #

  if (dim(overlap_matrix)[1] == dim(overlap_matrix)[2]) {
    extra_row      <- rep(0, dim(overlap_matrix)[1])
    overlap_matrix <- rbind(overlap_matrix, extra_row)
    row_names      <- append(row_names, "ignore_me")
  }
  rownames(overlap_matrix) <- row_names

  # create weighted bipartite network
  #
  bip = network::network(overlap_matrix,
                   matrix.type = "bipartite",
                   ignore.eval = FALSE,
                   names.eval  = "weights")

  # bip %v% 'clustering' <- append(rep('X', dim(overlap_matrix)[1]),
  #                                rep('Y', dim(overlap_matrix)[2]))
  network::set.vertex.attribute(bip,
             attrname = "clustering",
             value    =  append(rep('cl.x', dim(overlap_matrix)[1]),
                                rep('cl.y', dim(overlap_matrix)[2])))


  col = c("cl.x" = "grey", "cl.y" = "gold")
  p <- GGally::ggnet2(bip,
         color           = 'clustering',
         palette         = col,
         label           = TRUE,
         edge.label      = 'weights',
         edge.label.size = 3,
         label.size      = 3,
         alpha           = 0.3,
         size            = 10,
         edge.label.color = 'red')
  graphics::plot(p)
}


