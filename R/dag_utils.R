off_diagonal <- function(W) {
  # Lower or upper triangular but not both
  xor(lower.tri(W), upper.tri(W))
}

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
  eigen_values <- eigen(W)$values
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

#' Converts total effects to direct effects
#'
#' Assuming a matrix of total linear effects W we can convert this to a matrix of direct effects W_{dir}
#' This assume that W_{tot} = W_{dir} + W_{dir}^2 + W_{dir}^3 + ...
#' We can solve for W_{dir} = I - (I + W_{tot})^{-1} assume that the spectral radius (largest absolute eigenvalue) of W_{dir} < 1
#' This condition is the same that abs(eigenvalue(W_{tot}) / (1 + eigenvalue(W_{tot}))) < 1
#'
#' @param W A matrix of total linear effects
#' @param restrict_dag A boolean indicating if the matrix must be a directed acyclic graph
#' @param enforce_spectral_radius A boolean indicating if the spectral radius of W_{dir} must be less than 1. Generally not recommended to disable
#'
#' @return A matrix of direct effects
#' @export
#'
#' @examples
#' B <- matrix(c(0, 0.6, -0.5, 0), nrow = 2, byrow = TRUE)
#' total_to_direct(B,  restrict_dag = F)
#' all(zapsmall(direct_to_total(total_to_direct(B, restrict_dag = F), restrict_dag = F)) == B)
#'
#' B2 <- matrix(c(0,0,0,
#'              1,0,0,
#'              1,1,0), nrow = 3, byrow = TRUE)
#' total_to_direct(B2)
total_to_direct <- function(
  W,
  restrict_dag = TRUE,
  enforce_spectral_radius = TRUE) {
  d <- ncol(W)
  stopifnot(nrow(W) == d)
  stopifnot(`W must be a DAG unless restrict_dag = FALSE` = !restrict_dag || is_dag(W))

  eigen_W <- eigen(W, only.values = TRUE)$values
  if (enforce_spectral_radius && min(Re(eigen_W)) < -0.5) {
    stop("Smallest eigenvalue of W < -0.5.
         This will make rho(W_{dir}) > 1.
         If you really want to do this then re-run with enforce_spectral_radius = FALSE")
  }

  # Eigenvalues of direct effect matrix W_{dir} = 1 - 1/(1 + eigen(W))
  spec_radius <- max(abs(eigen_W))
  spec_radius_dir <- spec_radius / (1 + spec_radius)
  if (spec_radius_dir >= 1) {
    stop("Spectral radius of direct effect matrix >= 1. Not valid.")
  }
  result <- diag(d) - solve(diag(d) + W)
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
