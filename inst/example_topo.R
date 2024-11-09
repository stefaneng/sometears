set.seed(1234)

#d <- 10
#X <- matrix(runif(d * d, -2, 2), nrow = d)
B <- matrix(
  c(0, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    0,0.5,0,0),
  ncol = 4, byrow = T)

d <- ncol(B)
X <- sim_linear_sem(B, n = 1000)
est_B <- t(fit(X, 1:d)$W)
