# scca_validity_test <- function(scca, m = NULL, dist = NULL, do_conn = FALSE, do_silh = FALSE, do_dunn = FALSE) {
#
#   if (( is.null(m) &&  is.null(dist)) ||
#       (!is.null(m) && !is.null(dist))){
#     warning("Incidence matrix 'm' XOR dissimilarity object 'dist' must be specified")
#     return(NULL)
#   }
#
#   if (!is.null(m)) {
#     if (!all(scca$labels == rownames(m))) {
#       warning("Tree 'scca' not outcome of an SCCA run with dataset 'm'")
#     }
#     dist <- stats::dist(x = m, method = 'euclidian')
#   }
#
#   cl <- get_clustering(scca)
#
#   silhouette <- NULL
#   if (isTRUE(do_silh)) {
#     silhouette <- cluster::silhouette(
#       x    = cl$cluster,
#       dist = dist)
#   }
#
#   dunn <- NULL
#   if (isTRUE(do_dunn)) {
#     dunn <- clValid::dunn()
#   }
#
#   connectivity <- NULL
#   if (isTRUE(do_conn)) {
#     connectivity <- clValid::connectivity()
#   }
# }
#
#

