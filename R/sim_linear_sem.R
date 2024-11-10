# If we pass in upper triangular then we take transpose
# Simulate from the model X = t(W) %*% X + eps
# Where eps ~ MVN(0, Sigma)
# Then X ~ MVN(0, (I - W)^-1 %*% Sigma %*% (I - W)^-T)
sim_linear_sem <- function(W, n = 1, Sigma = diag(ncol(W))) {
  d <- ncol(W)
  # THis is not the right expression
  if (! is_upper_tri(W) && ! is_lower_tri(W)) {
    stop("W must be upper or lower triangular")
  } else if (is_upper_tri(W)) {
    W <- t(W)
  }
  if (all(diag(W) == 1)) {
    warning("Removing ones from diagonal of W")
    diag(W) <- 0
  }

  inv_W <- solve(diag(d) - W)
  new_Sigma <-  inv_W %*% tcrossprod(Sigma, inv_W)
  MASS::mvrnorm(n = n, mu = rep(0, d), Sigma = new_Sigma)
}B

