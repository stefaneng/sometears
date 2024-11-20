topo_init_min_size <- function(d, model_type = c('linear', 'nonlinear')) {
  model_type <- match.arg(model_type)
  if (model_type == "linear") {
    round(min(
      1.5 * d,
      sqrt(d) * (d - 1) * log10(d),
      d * (d - 1) / 2))
  }
}

topo_init_max_size <- function(d, model_type = c('linear', 'nonlinear')) {
  model_type <- match.arg(model_type)
  if (model_type == "linear") {
    round(max(
      1.5 * d,
      min(
        sqrt(d) * (d - 1) * log10(d),
        d * (d - 1) / 2)))
  }
}
