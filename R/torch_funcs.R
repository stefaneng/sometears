# Torch version of logdet
torch_h_logdet <- function(theta, s = 1) {
  d <- ncol(theta)
  M <- torch::torch_square(theta)
  logdet_value <- -torch::torch_logdet(s * torch::torch_eye(d) - M) + d * torch::torch_log(s)
  return(logdet_value)
}

torch_l2 <- function(X, W) {
  0.5 / nrow(X) * torch::torch_sum(torch::torch_square(X - torch::torch_matmul(X, W)))
}
