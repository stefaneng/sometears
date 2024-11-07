test_that("logdet works", {
  X <- matrix(0, nrow = 3, ncol = 3)

  expect_equal(h_logdet(lower.tri(X)), 0)
  expect_true(all(h_logdet_grad(lower.tri(X)) == 0))
})
