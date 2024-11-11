set.seed(1234)

B <- matrix(
  c(0, 3, 3, 0,
    0, 0, 0, 5,
    0, 0, 0, 0,
    0, 0, 0, 0),
  nrow = 4, ncol = 4, byrow = TRUE)

d <- ncol(B)
X <- sim_linear_sem(B, n = 1000)
(est_B <- fit(X, 1:d)$W)
