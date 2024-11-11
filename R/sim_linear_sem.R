# If we pass in upper triangular then we take transpose
# Simulate from the model X = X %*% W + eps
# Where eps ~ MVN(0, Sigma)
# Then X ~ MVN(0, (I - W)^-T %*% Sigma %*% (I - W)^-1)
sim_linear_sem <- function(W, n = 1, Sigma = diag(ncol(W))) {
  d <- ncol(W)
  if (! is_upper_tri(W) && ! is_lower_tri(W)) {
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
