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
    W_eigen <- eigen(W)
    # Eigendecomposition but replace eigenvalues in correct range (-1, 1)
    eigenvals <- diag(runif(d, min = -1, max = 1))

    W_new <- W_eigen$vectors %*% eigenvals %*% solve(W_eigen$vectors)

    expect_equal(total_to_direct(direct_to_total(W_new, restrict_dag = F), restrict_dag = F), W_new)
  }
})

test_that("direct_to_total and total_to_direct are inverses", {
  for (i in 1:50) {
    d <- 10
    # Non-DAG test
    W <- matrix(0, nrow = d, ncol = d)
    # Randomly unif [0.05, 0.2] on off-diagonal with sparsity
    off_diag_d <- d * (d - 1)
    sparsity <- 0.5

    # Sample direct effect matrix that have direct spectral radius < 1
    W <- matrix(runif(d^2, min = 0.05, max = 0.2), ncol = d, nrow = d)
    W_eigen <- eigen(W)
    eigenvals <- diag(runif(d, min = -0.5, max = 1.5))
    # Eigendecomposition but replace eigenvalues in correct range (-0.5, infty)
    W_new <- W_eigen$vectors %*% eigenvals %*% solve(W_eigen$vectors)
    expect_equal(direct_to_total(total_to_direct(W_new, restrict_dag = F), restrict_dag = F), W_new)
  }
})
