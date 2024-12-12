# Torch version of logdet
torch_h_logdet <- function(theta, s = 1) {
  d <- ncol(theta)
  M <- torch::torch_multiply(theta, theta)
  logdet_value <- -torch::torch_logdet(s * torch::torch_eye(d) - M) + d * torch::torch_log(s)
  return(logdet_value)
}

torch_l2 <- function(X, W) {
  0.5 / nrow(X) * torch::torch_sum(torch::torch_square(X - torch::torch_matmul(X, W)))
}

torch_l2_cov <- function(cov, W) {
  I_W <- torch::torch_eye(nrow(W)) - W
  rhs <- torch::torch_matmul(cov, I_W)
  0.5 * torch::torch_trace(torch::torch_matmul(torch::torch_t(I_W), rhs))
}
