l2_cov <- function(W, X_cov) {
  I_W <- diag(1, nrow = nrow(W)) - W
  rhs <- X_cov %*% I_W
  0.5 * sum(diag(t(I_W) %*% rhs))
}

l2_cov_grad <- function(W, X_cov) {
  I_W <- diag(1, nrow = nrow(W)) - W
  return(-X_cov %*% I_W)
}

## These parts copy from the notears function in R
loss_func_linear <- function(X, W, loss_type='l2') { # ?
  M <- X %*% W
  if (loss_type=='l2') {
    R <- X - M
    loss <- 0.5 / dim(X)[1] * sum(R ** 2)
  } else if (loss_type=='logistic') {
    loss <- 1.0 / dim(X)[1] * sum(log(sum(exp(M)+1)) - X * M)
  } else if (loss_type=='poisson') {
    S <- exp(M)
    loss <- 1.0 / dim(X)[1] * sum(S - X * M)
  }
  return(loss)
}

grad_loss_func_linear <- function(X, W, loss_type='l2') {
  M <- X %*% W
  loss_type <- tolower(loss_type)
  if (loss_type=='l2') {
    R <- X - M
    loss <- -1.0 / dim(X)[1] * t(X) %*% R
  } else if (loss_type=='logistic') {
    loss <- 1.0 / dim(X)[1] * t(X) %*% (1.0 / (1 + exp(-1 * M)) - X)
  } else if (loss_type=='poisson') {
    S <- exp(M)
    loss <- 1.0 / dim(X)[1] * t(X) %*% (S - X)
  }
  return(loss)
}
