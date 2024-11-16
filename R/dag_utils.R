off_diagonal <- function(W) {
  # Lower or upper triangular but not both
  xor(lower.tri(W), upper.tri(W))
}

spectral_radius <- function(W) {
  eigen_values <- eigen(W)$values
  max(abs(eigen_values))
}

is_dag <- function(W, threshold = 1e-5) {
  h_logdet(abs(W) > threshold) == 0
}

total_to_direct <- function(
  W,
  restrict_dag = TRUE) {
  d <- ncol(W)
  stopifnot(nrow(W) == d)
  stopifnot(`W must be a DAG unless restrict_dag = FALSE` = !restrict_dag || is_dag(W))

  result <- diag(d) - solve(diag(d) + W)
  if (spectral_radius(result) >= 1) {
    stop("Spectral radius of direct effect matrix >= 1. Not valid.")
  }
  return(result)
}

direct_to_total <- function(
    W,
    restrict_dag = TRUE) {
  d <- ncol(W)
  stopifnot(nrow(W) == d)
  stopifnot(`W must be a DAG unless restrict_dag = FALSE` = !restrict_dag || is_dag(W))

  if (!restrict_dag){
    sr <- spectral_radius(W)
    if (sr >= 1) stop("Cannot convert direct to total since spectral radius of W = ", sr, " >= 1")
  }
  solve(diag(d) - W) - diag(d)
}
