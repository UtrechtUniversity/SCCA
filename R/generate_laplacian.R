#' Generate Laplacian matrix (L) from the Simmilariry matrix (S) of a bipartite adjacency matrix (M)
#'

generate_laplacian <- function(M) {
  L <- M
  S <- M
  return(list(Lap = L, Sim = S))
}
