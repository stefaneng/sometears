library(torch)
set.seed(13)
# Define parameters
B <- matrix(
  c(0, 0, 0, 0,
  1, 0, 0, 0,
  1, 0, 0, 0,
  0,3, 0, 0),
  ncol = 4, byrow = T)
d <- ncol(B)
n <- 10000
tol <- 1e-6
X <- torch_tensor(sim_linear_sem(B, n = n))

W_fit <- dagma_fit_adam(X, trace = T, lr = 0.1, epoch = 10000)
print(round(as.matrix(W_fit), 2))
