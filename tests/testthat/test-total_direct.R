test_that("direct_to_total and total_to_direct are inverses", {
  for (i in 1:50) {
    d <- 10
    # Non-DAG test
    W <- matrix(0, nrow = d, ncol = d)
    # Randomly unif [0.05, 0.2] on off-diagonal with sparsity
    off_diag_d <- d * (d - 1)
    sparsity <- 0.5

    # Sample direct effect matrix that have spectral radius < 1
    W <- matrix(runif(d^2, min = 0.05, max = 0.2), ncol = d, nrow = d)
    U <- qr.Q(qr(W))
    eigenvals <- diag(runif(d, min = -1, max = 1))
    W <- U %*% eigenvals

    expect_equal(total_to_direct(direct_to_total(W, restrict_dag = F), restrict_dag = F), W)
  }
})
