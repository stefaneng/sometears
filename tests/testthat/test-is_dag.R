test_that("is_dag works", {
  for (i in 1:100) {
    d <- 100
    B <- matrix(0, d, d)
    idx <- sample(1:d)
    B[lower.tri(B)] <- runif(d*(d - 1) / 2, min = 1, max = 100)
    expect_true(is_dag(B[idx, idx]))

    B[upper.tri(B)] <- runif(d*(d - 1) / 2, min = 1, max = 100) *
      runif(sample(c(1,0), size = d * (d - 1) / 2, replace = T))
    expect_false(is_dag(B[idx, idx]))
  }
})
