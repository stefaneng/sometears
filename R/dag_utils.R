total_to_direct <- function(W) {
  d <- ncol(W)
  stopifnot(nrow(W) == d)
  if (all(diag(W) == 0)) {
    diag(W) <- 1
  }
  diag(d) - solve(W)
}

direct_to_total <- function(W) {
  d <- ncol(W)
  stopifnot(nrow(W) == d)
  if (all(diag(W) == 0)) {
    diag(W) <- 1
  }
  solve(-W) - diag(d)
}
