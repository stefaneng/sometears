#' Fit TOPO from Deng et al 2023
#'
#' @param X A matrix of data in which each of the columns is a variable of interest in a directed acyclic graph
#' @param topo A vector of integers indicating the initial topological order of the variables
#' @param size_small The minimum size of the search space.
#' @param size_large The maximum size of the search space.
#' @param use_large_space A boolean indicating if the search space should be expanded if the minimum size is not sufficient.
#' @param model_type The type of model to fit. Currently only linear models are supported.
#' @param h_func The continuous acyclic function to use. See h_logdet.
#' @param h_func_grad The gradient of the acyclic funtion to use. See h_logdet_grad.
#' @param loss_type
#' @param s
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
#'
#' B <- matrix(
#' c(0, 3, 3, 1,
#'   0, 0, 1, 5,
#'   0, 0, 0, 1,
#'   0, 0, 0, 0),
#' nrow = 4, ncol = 4, byrow = TRUE)
#'
#' d <- ncol(B)
#' X <- sim_linear_sem(B, n = 1000, Sigma = diag(0.01, nrow = d))
#' (est_B <- fit_topo(X, d:1))
fit_topo <- function(
    X,
    topo = 1:ncol(X),
    size_small = NULL, size_large = NULL,
    use_large_space = d <= 10,
    model_type = c('linear', 'nonlinear'),
    loss_type= c('l2'), # TODO: Different losses
    h_func = h_logdet,
    h_func_grad = h_logdet_grad,
    s = 1.1,
    verbose = F) {
  d <- ncol(X)
  n <- nrow(X)
  model_type <- match.arg(model_type)
  if (model_type != "linear") stop("Not implemented: only linear models are supported.")
  if (is.null(size_small)) size_small <- topo_init_min_size(d, model_type = model_type)
  if (is.null(size_large)) size_large <- topo_init_max_size(d, model_type = model_type)

  Z <- create_Z(topo)
  W <- init_W(X, Z)

  loss <- loss_func_linear(X, W)
  G_loss <- grad_loss_func_linear(X, W)

  h <- h_func(W, s = s)
  G_h <- h_func_grad(W, s = s)

  idx_set_both <- find_idx_set_updated(G_h, G_loss, Z, size_small, size_large = size_large)
  idx_set_small <- idx_set_both$idx_small
  idx_set_large <- idx_set_both$idx_large

  idx_set <- idx_set_small

  while (nrow(idx_set) > 0) {
    idx_len <- nrow(idx_set)
    loss_collections <- apply(idx_set, 1, function(idx){
      W_c <- update_topo_linear(W = W, topo = topo, idx = idx)$W_0
      loss_c <- loss_func_linear(X, W_c)
      return(loss_c)
    })

    if (any(loss > min(loss_collections))) {
      if (verbose) cat("Find better loss in small space\n")
      topo <- create_new_topo_greedy(topo, loss_collections, idx_set, loss)
    } else {
      if (use_large_space) {
        # Get the idx that are in idx_set_large but not in idx_set small
        idx_set <- dplyr::anti_join(idx_set_large, idx_set_small, by = c("row", "col"))
        idx_len <- nrow(idx_set)
        # loss_collections <- numeric(idx_len)

        loss_collections <- apply(idx_set, 1, function(idx){
          W_c <- update_topo_linear(W = W, topo = topo, idx = idx)[1]$W_0
          loss_c <- loss_func_linear(X, W_c)
          return(loss_c)
        })

        if (any(loss > loss_collections)) {
          if (verbose) cat("current loss :", loss, "and find better loss in large space\n")
          large_space_used <- large_space_used + 1
          topo <- create_new_topo_greedy(topo, loss_collections, idx_set, loss)
        } else {
          if (verbose) cat("Using larger search space, but we cannot find better loss\n")
          break
        }
      } else {
        break
      }
    }
    Z <- create_Z(topo)
    W <- init_W(X, Z)
    loss <- loss_func_linear(X, W)
    G_loss <- grad_loss_func_linear(X, W)

    h <- h_func(W, s = s)
    G_h <- h_func_grad(W, s = s)
    idx_set_both <- find_idx_set_updated(G_h, G_loss, Z, size_small, size_large = size_large)
    idx_set_small <- idx_set_both$idx_small
    idx_set_large <- idx_set_both$idx_large
    idx_set <- idx_set_small
  }

  return(list(W = W, topo = topo, Z = Z, loss = loss))
}
