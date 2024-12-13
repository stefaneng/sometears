
# Then X ~ MVN(0, (I - W)^-T %*% Sigma %*% (I - W)^-1)

#' Simulate from linear SEM
#'
#' Simulate from the model X = X %*% W + eps
#' If we pass in upper triangular then we take transpose
#' X ~ MVN(0, (I - W)^-T %*% Sigma %*% (I - W)^-1)
#' Where eps ~ MVN(0, Sigma)
#'
#' @param W Adjacency matrix representing SEM (d x d)
#' @param n Number of samples
#' @param Sigma Covariance matrix of noise
#'
#' @return Matrix of samples (n x d)
#' @export
#'
#' @examples
#' B <- matrix(
#' c(0, 3, 0, 3,
#'   0, 0, 0, 5,
#'   0, 0, 0, 2,
#'   0, 0, 0, 0),
#' nrow = 4, ncol = 4, byrow = TRUE)
#' d <- ncol(B)
#' X <- sim_linear_sem(B, n = 500, Sigma = 1 * diag(ncol(B)))
sim_linear_sem <- function(W, n = 1, Sigma = diag(ncol(W)), check_lower = TRUE) {
  d <- ncol(W)
  if (check_lower && ! is_upper_tri(W) && ! is_lower_tri(W)) {
    # TODO: Could attempt to topologically sort here before failing
    stop("W must be upper or lower triangular")
  }
  if (all(diag(W) == 1)) {
    warning("Removing ones from diagonal of W")
    diag(W) <- 0
  }

  inv_W <- solve(diag(d) - W)
  if (is_upper_tri(W)) {
    inv_W <- t(inv_W)
  }
  new_Sigma <-  inv_W %*% tcrossprod(Sigma, inv_W)
  MASS::mvrnorm(n = n, mu = rep(0, d), Sigma = new_Sigma)
}
