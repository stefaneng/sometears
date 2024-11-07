test_that("logdet works", {
  X <- matrix(0, nrow = 3, ncol = 3)
  X_lower <- lower.tri(X)

  expect_equal(h_expm(lower.tri(X)), 0)
  expect_true(all(h_expm_grad(lower.tri(X)) == 0))

  expect_equal(h_expm(X_lower, include_grad = TRUE)$h_expm, 0)
  expect_true(all(h_expm(X_lower, include_grad = TRUE)$h_expm_grad == 0))
})
