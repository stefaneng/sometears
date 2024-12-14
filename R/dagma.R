#' DAGMA algorithm to learn DAGs
#'
#' Implements the DAGMA algorithm to learn DAGs from Bello et al. (2023).
#'
#' @param X A matrix of data in which each of the columns is a variable of interest in a directed acyclic graph
#' @param loss A loss function to use. Default is L2 loss.
#' @param h_func The continuous acyclic function to use. Default is h_logdet.
#' @param s A regularization parameter. Should be larger than the spectral radius of X.
#' @param mu A vector of weights to put on the loss function.
#' @param epoch ADAM optimizer epochs.
#' @param tol Tolerance for convergence.
#' @param l1_beta L1 regularization parameter.
#' @param lr Learning rate for ADAM optimizer.
#' @param betas Beta parameters for ADAM optimizer.
#' @param eps Epsilon parameter for ADAM optimizer.
#' @param trace Boolean to enable progress printing
#'
#' @return A matrix of the estimated W matrix
#' @export
#'
#' @examples
#' \dontrun{
#' B <- matrix(
#'   c(0, 3, 0, 3,
#'    0, 0, 3, 0,
#'    0, 0, 0, 3,
#'    0, 0, 0, 0),
#'   nrow = 4, ncol = 4, byrow = TRUE)
#'   # Simulate from the DAG
#'   d <- ncol(B)
#'   X <- sim_linear_sem(B, n = 500, Sigma = 1 * diag(ncol(B)))
#'
#'   print(threshold_W(dagma_fit_linear(X, mu = c(10, 1, 0.1, 0.01, 0.001), l1_beta = 0.05), 0.1))
#'   print(threshold_W(dagma_fit_linear_optim(X, mu = c(10, 1, 0.1, 0.01, 0.001), l1_beta = 0.05), 0.1))
#' }
dagma_fit_linear <- function(
  X,
  loss = torch_l2_cov,
  h_func = torch_h_logdet,
  s = 1.1,
  mu = c(10, 1, 0.1, 0.01, 0),
  epoch = 5,
  l1_beta = 0.05,
  lr = 1,
  trace = FALSE
  ) {
  d <- ncol(X)
  X_cov <-  torch::torch_matmul(torch::torch_t(X), X) / nrow(X)
  W <- torch::torch_tensor(matrix(0, nrow = d, ncol = d), requires_grad = TRUE)

  # Adam optimizer
  optimizer <- torch::optim_lbfgs(W, lr = lr)

  params <- data.frame(
    mu,
    s,
    epoch,
    l1_beta
  )

  for (i in 1:nrow(params)) {
    # Note: this is not particularly efficient but really only using
    # dataframe capability to pass parameters of varying lengths
    params_i <- as.list(params[i,])
    mu_i <- params_i$mu
    epoch_i <- params_i$epoch
    s_i <- params_i$s
    l1_beta_i <- params_i$l1_beta

    .calc_loss <- function() {
      optimizer$zero_grad()

      linear_loss <- loss(X_cov, W)
      l1_penalty <- l1_beta_i * torch::torch_norm(W, p = 1)
      h_ldet_value <- h_func(W, s = s_i)

      objective <- mu_i * (linear_loss + l1_penalty) + h_ldet_value

      # Note: These are automatically computed gradients but we could provide them
      objective$backward()
      objective
    }

    for (j in 1:epoch_i) {
      has_error <- FALSE
      tryCatch({
        optimizer$step(.calc_loss)
      }, error = function(e) {
        warnings("s is probably too small")
        has_error <<- TRUE
      })
      if (has_error) break
    }

    if (trace) {
      cat("Params: ", paste0(names(params_i), ": ", params_i, collapse = ", "), "\n")
      print(W)
    }
  }

  return(as.matrix(W))
}
