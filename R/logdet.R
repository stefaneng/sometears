# From:
# Bello, K., Aragam, B., & Ravikumar, P. (2023).
# DAGMA: Learning DAGs via M-matrices and a Log-Determinant Acyclicity Characterization (arXiv:2209.08037).
# arXiv. https://doi.org/10.48550/arXiv.2209.08037
#' @export
log_det <- function(X, s = 1) {
  d <- ncol(X)
  -log(det(s * diag(d) - X * X)) + d * log(s)
}

#' @export
h_det_grad <- function(X, s = 1) {
  d <- ncol(X)
  2 * solve(t(s * diag(d) - X * X)) * X
}
