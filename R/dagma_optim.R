#' @export
#' @rdname dagma_fit_linear
dagma_fit_linear_optim <- function(
    X,
    loss = l2_cov,
    h_func = h_logdet,
    s = 1.1,
    mu = c(1, 0.1, 0.01),
    l1_beta = 1e-4,
    trace = FALSE
    ) {
  if (! requireNamespace("lbfgs", quietly = TRUE)) {
    stop("Please install the 'lbfgs' package to use this function.")
  }
  n <- nrow(X)
  d <- ncol(X)

  X_cov <- cov(X)
  W_init <- as.vector(matrix(0, nrow = d, ncol = d))

  params <- data.frame(
    mu,
    s,
    l1_beta
  )

  prev_val <- 0
  # Convert obj function into a vector
  obj_func <- function(W_vec, mu, s, l1_beta) {
    W <- matrix(W_vec, nrow = d, ncol = d)

    linear_loss <- loss(W, X_cov)
    h_ldet_value <- h_func(W, s)
    if (is.na(h_ldet_value)) {
      warning("S is probably too small")
      # This should short-circuit the optimization
      return(prev_val)
    }

    # Pull L1 loss out of objective function here
    # Reason being that the `lbfgs` optimizer does this internally
    prev_val <<- mu * (linear_loss) + h_ldet_value
    prev_val
  }

  # Convert grad function into a vector
  obj_grad <- function(W_vec, mu, s, l1_beta) {
    # Reshape W_vec into a matrix
    W <- matrix(W_vec, nrow = d, ncol = d)

    # Compute the gradients of each term
    grad_linear <- l2_cov_grad(W, X_cov)
    grad_h_ldet <- h_logdet_grad(W, s)

    as.vector(mu * grad_linear + grad_h_ldet)
  }

  # Iterate over parameter set
  for (i in seq_len(nrow(params))) {
    # Extract current parameters
    mu <- params$mu[i]
    s <- params$s[i]
    l1_beta <- params$l1_beta[i]

    # Optimization using L-BFGS-B
    optim_result <- lbfgs::lbfgs(
      call_eval = obj_func,
      call_grad = obj_grad,
      mu = mu,
      s = s,
      l1_beta = l1_beta,
      vars = W_init,
      linesearch_algorithm = "LBFGS_LINESEARCH_BACKTRACKING",
      invisible = 1,
      orthantwise_c = l1_beta
    )

    # Update W_init for the next parameter set
    W_init <- optim_result$par

    if (trace) {
      cat("Params: mu =", mu, ", s =", s, ", l1_beta =", l1_beta, "\n")
      cat("Current W:\n")
      print(matrix(W_init, nrow = d, ncol = d))
    }
  }

  return(matrix(W_init, nrow = d, ncol = d))
}
