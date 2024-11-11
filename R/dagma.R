# ADAM version of DAGMA
dagma_fit_adam <- function(
  X,
  loss = torch_l2,
  h_func = torch_h_logdet,
  s = 1,
  lambda = c(10^-seq(0,4), 0),
  epoch = 100,
  tol = 1e-6,
  l1_beta = 0.05,
  lr = 2e-4, betas = c(0.9, 0.999), eps = 1e-08,
  trace = FALSE
  ) {
  W <- torch::torch_tensor(matrix(0, nrow = d, ncol = d), requires_grad = TRUE)

  # Adam optimizer
  optimizer <- optim_adam(W, lr = lr, betas = betas, eps = eps)
  objective <- NULL
  obj_prev <- -1
  for (mu_t in lambda) {
    for (epoch in 1:epoch) {
      optimizer$zero_grad()  # Reset gradients
      if (!is.null(objective)) {
        obj_prev <- objective$item()
      }

      linear_loss <- loss(X, W)
      l1_penalty <- l1_beta * torch_sum(torch_abs(W))
      h_ldet_value <- torch_h_logdet(W)

      objective <- mu_t * (linear_loss + l1_penalty) + h_ldet_value

      # Note: These are automatically computed gradients but we could provide them
      objective$backward()

      optimizer$step()

      obj_new <- objective$item()

      if (trace) {
        cat("Epoch:", epoch, "Objective:", as.numeric(objective$item()), "\n")
      }

      if (obj_prev > 0 && abs((obj_prev - obj_new) / obj_prev) <= tol) {
        break
      }
    }
  }
  return(W)
}
