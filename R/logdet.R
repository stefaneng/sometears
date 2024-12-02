#' Computes the log-determinant acyclicity characterization
#'
#' This function is exactly zero if and only if the matrix is a DAG.
#' The gradient is the gradient of the log-determinant acyclicity characterization is also zero exactly when the matrix is a DAG.
#'
#' From Bello, K., Aragam, B., & Ravikumar, P. (2023).
# DAGMA: Learning DAGs via M-matrices and a Log-Determinant Acyclicity Characterization (arXiv:2209.08037).
# arXiv. https://doi.org/10.48550/arXiv.2209.08037
#'
#' @param X a matrix
#' @param s a regularization parameter. Should be larger than spectral radius (largest absolute eigenvalue) of X.
#'
#' @return numeric
#' @export
#'
#' @examples
#' B <- matrix(c(0, 0.6, -0.5, 0), nrow = 2, byrow = TRUE)
#' h_logdet(B) # Not a DAG means > 0
#' B_dag <- matrix(c(0,0,1,0), nrow = 2, byrow = TRUE)
#' h_logdet(B_dag) # DAG means 0
#' @rdname h_logdet
h_logdet <- function(X, s = 1, transform = c("square", "abs")) {
  d <- ncol(X)
  if (transform == "abs") {
    X <- abs(X)
  } else if (transform == "square") {
    X <- X * X
  }
  result <- -log(det(s * diag(d) - X)) + d * log(s)
  if (is.nan(result)) {
    sr <- spectral_radius(X)
    warning("Determinant is negative. Consider increasing `s` above: ", sr)
  }
  result
}

#' @rdname h_logdet
h_logdet_grad <- function(X, s = 1) {
  # TODO! Need to fix for abs and square option
  d <- ncol(X)
  2 * solve(t(s * diag(d) - X * X)) * X
}
