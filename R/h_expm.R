#' Implements the matrix exponential acyclicity regularizer from DAGs with no TEARS (Zheng et al 2018)
#'
#' @param W weighted adjacency matrix
#' @param include_grad whether to include the gradient of the regularizer
#'
#' @return either a list if gradient is included or a scalar
#' @export
#' @rdname h_expm
#' @examples
#'  X <- matrix(0, nrow = 3, ncol = 3)
#' X_lower <- lower.tri(X)
#'
#' # DAG
#' h_expm(lower.tri(X))
#'
#' # Not a DAG
#' X2 <- X_lower
#' X2[1, 2] <- 1
#' h_expm(X2)
h_expm <- function(W, include_grad = FALSE) {
  d <- ncol(W)
  stopifnot(nrow(W) == d)
  m_exp <- expm::expm(W * W)
  .h_exp <- sum(diag(m_exp)) - d
  if (!include_grad) {
    return(.h_exp)
  } else {
    return(
      list(
        h_expm = .h_exp,
        h_expm_grad = .grad_helper(m_exp, W)
      ))
  }
}

#' @rdname h_expm
h_expm_grad <- function(W) {
  d <- ncol(W)
  stopifnot(nrow(W) == d)
  m_exp <- expm::expm(W * W)
  .grad_helper(m_exp, W)
}

.grad_helper <- function(m_exp, W) {
  t(m_exp) * 2 * W
}
