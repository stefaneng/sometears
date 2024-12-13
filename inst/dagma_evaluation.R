library(sometears)
library(caret)

eval_results_10 <- readRDS("evaluation_results_10.rds")
eval_results_15 <- readRDS("evaluation_results_15.rds")

confusion_to_df <- function(confusion) {
  tibble::rownames_to_column(
    data.frame(
      value = c(confusion$overall, confusion$byClass)
    ), "metric")
}

results_to_df <- function(results) {
  bind_rows(lapply(results, function(x) {
    bind_rows(
      tibble::add_column(confusion_to_df(x$confusion_torch), method = "DAGMA-torch"),
      tibble::add_column(confusion_to_df(x$confusion_lbfgs), method = "DAGMA-lbfgs"),
      tibble::add_column(confusion_to_df(x$confusion_topo), method = "TOPO")
    )
  }), .id = "id")
}

all_results <- lapply(c(10, 15), function(d) {
  results <- replicate(100, {
    n <- 500
    lower_tri_size <- d * (d - 1) / 2
    sparsity <- 0.8

    B <- matrix(0, nrow = d, ncol = d)

    B[lower.tri(B)] <- runif(lower_tri_size, min = 0.5, max = 2) *
      sample(c(-1, 0, 1), size = lower_tri_size, replace = TRUE, prob = c((1 - sparsity)/2, sparsity, (1 - sparsity) / 2))
    # Suffle indices  of B
    idx_new <- sample(1:d)
    B <- B[idx_new, idx_new]

    X <- sim_linear_sem(B, n = n, Sigma = 0.5 * diag(ncol(B)), check_lower = FALSE)

    est_torch <- dagma_fit_linear(
      X,
      trace = T,
      s = 1.1,
      mu = c(10, 5, 1, 0.1),
      l1_beta = .1)

    est_lbfgs <- dagma_fit_linear_optim(
      X,
      trace = T,
      s = 1.1,
      mu = c(10, 5, 1, 0.1),
      l1_beta = .1
    )

    est_topo <- fit_topo(X, 1:d)

    true_edge <- t(B) != 0
    # Time each of the functions

    found_edge_torch <- threshold_W(est_torch, threshold = 0.1) != 0
    found_edge_lbfgs <- threshold_W(est_lbfgs, threshold = 0.1) != 0
    found_edge_topo <- threshold_W(est_topo$W, threshold = 0.1) != 0

    # Confusion matrix
    list(
      est_torch = est_torch,
      est_lbfgs = est_lbfgs,
      est_topo = est_topo,
      confusion_torch = caret::confusionMatrix(factor(found_edge_torch, levels = c(F,T)), factor(true_edge, levels = c(F,T))),
      confusion_lbfgs = caret::confusionMatrix(factor(found_edge_lbfgs, levels = c(F,T)), factor(true_edge, levels = c(F,T))),
      confusion_topo = caret::confusionMatrix(factor(found_edge_topo, levels = c(F,T)), factor(true_edge, levels = c(F,T)))
    )
  }, simplify = FALSE)


  saveRDS(results, sprintf("eval_results/evaluation_results_%s.rds", d))
  results_df <- results_to_df(results)
  results_df$d <- d
  saveRDS(results_df, sprintf("inst/report/evaluation_results_df_%s.rds", d))

  results_df
})

confusion_results <- bind_rows(
  results_df
)

saveRDS(confusion_results, "confusion_results_plot.rds")
