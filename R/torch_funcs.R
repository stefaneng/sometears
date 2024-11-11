# Torch version of logdet
torch_h_logdet <- function(theta, s = 1) {
  d <- ncol(theta)
  M <- torch_square(theta)
  logdet_value <- -torch_logdet(s * torch_eye(d) - M) + d * torch_log(s)
  return(logdet_value)
}

torch_l2 <- function(X, W) {
  0.5 / nrow(X) * torch_sum(torch_square(X - torch_matmul(X, W)))
}
