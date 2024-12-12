#' Computes the spectral radius (the largest absolute eigenvalue) of a matrix
#'
#' @param W A matrix
#'
#' @return The spectral radius of the matrix
#' @export
#'
#' @examples
#' B <- matrix(c(0, 0.6, -0.5, 0), nrow = 2, byrow = TRUE)
#' spectral_radius(B)
spectral_radius <- function(W) {
  eigen_values <- eigen(W, only.values = TRUE)$values
  max(abs(eigen_values))
}

#' Checks if matrix is a directed acyclic graph
#'
#' @param W A matrix that represents a directed graph
#' @param threshold A threshold to determine if a value is zero
#'
#' @return A boolean indicating if the matrix is a directed acyclic graph
#' @export
#'
#' @examples
#' d <- 100
#' B <- matrix(0, d, d)
#' idx <- sample(1:d)
#' B[lower.tri(B)] <- runif(d*(d - 1) / 2, min = 1, max = 100)
#' is_dag(B[idx, idx]) # DAG
#'
#' B[1,10] <- 1
#' is_dag(B[idx, idx]) # Not a DAG
is_dag <- function(W, threshold = 1e-5) {
  # TODO: Need better condition here...
  all(eigen(abs(W) > threshold, only.values = TRUE)$values == 0)
}

#' Converts an adjacency matrix to a long format dataframe
#'
#' @param W An adjacency matrix
#'
#' @return A dataframe with columns `from`, `to`, and `value`
#' @export
adj_mat_to_long <- function(W, include_zeros = FALSE) {
  d <- ncol(W)
  W_long <- data.frame(
    from = rep(1:d, each = d),
    to = rep(1:d, times = d),
    value = as.vector(W)
  )
  if (!include_zeros) {
    W_long[W_long$value != 0, ]
  } else {
    W_long
  }
}
