test_that("logdet works", {
  X <- matrix(0, nrow = 3, ncol = 3)

  expect_equal(log_det(lower.tri(X)), 0)
  expect_true(all(log_det_grad(lower.tri(X)) == 0))
})
