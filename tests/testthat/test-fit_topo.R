test_that("fit_topo works on linear SEM", {
  B <- matrix(
    c(0, 3, 3, 1,
      0, 0, 1, 5,
      0, 0, 0, 1,
      0, 0, 0, 0),
    nrow = 4, ncol = 4, byrow = TRUE)

  for (i in 1:50) {
    d <- ncol(B)
    X <- sim_linear_sem(B, n = 1000, Sigma = diag(0.01, nrow = d))
    est_B <- fit_topo(X, d:1)
    expect_equal(est_B$topo, 1:d)
    expect_lt(est_B$loss, 0.04)
  }
})
