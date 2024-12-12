#' Converts total effects to direct effects and visa verse
#'
#' Assuming a matrix of total linear effects W we can convert this to a matrix of direct effects W_{dir}
#' This assume that W_{tot} = W_{dir} + W_{dir}^2 + W_{dir}^3 + ...
#' We can solve for W_{dir} = I - (I + W_{tot})^{-1} assume that the spectral radius (largest absolute eigenvalue) of W_{dir} < 1
#' This condition is the same that abs(eigenvalue(W_{tot}) / (1 + eigenvalue(W_{tot}))) < 1
#'
#' @param W A matrix of total/direct linear effects
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
#' @rdname total_direct_conversion
total_to_direct <- function(
    W,
    restrict_dag = TRUE,
    enforce_spectral_radius = TRUE) {
  d <- ncol(W)
  stopifnot(nrow(W) == d)
  stopifnot(`W must be a DAG unless restrict_dag = FALSE` = !restrict_dag || is_dag(W))

  eigen_W <- eigen(W, only.values = TRUE)$values
  eigen_dir <- eigen_W / (1 + eigen_W)

  # Eigenvalues of direct effect matrix W_{dir} = 1 - 1/(1 + eigen(W))
  spec_radius <- max(abs(eigen_dir))
  if (spec_radius >= 1) {
    stop("Spectral radius of direct effect matrix >= 1. Not valid.")
  }
  result <- diag(d) - solve(diag(d) + W)
  return(result)
}

#' @rdname total_direct_conversion
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
