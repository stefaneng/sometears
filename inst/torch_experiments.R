library(torch)
set.seed(13)
# Define parameters
B <- matrix(
  c(0, 0, 0, 0,
  3, 0, 0, 0,
  3, 0, 0, 0,
  0,5, 2, 0),
  ncol = 4, byrow = T)
d <- ncol(B)
n <- 1000
tol <- 1e-6
X <- sim_linear_sem(B, n = n)

#lm(X2[, 4] ~ X2[, -4] - 1)
#lm(X2[, 3] ~ X2[, -3] - 1)
#lm(X2[, 2] ~ X2[, -2] - 1)
#lm(X2[, 1] ~ X2[, -1] - 1)

W_fit <- dagma_fit_adam(
  torch_tensor(X), trace = T,
  lr = 3e-4,
  epoch = c(rep(2e4, 4), 7e4),
  l1_beta = 0.01,
  mu = c(1, 0.1, 0.01, 0))
print(round(as.matrix(W_fit), 2))
