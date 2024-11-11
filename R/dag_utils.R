total_to_direct <- function(W) {
  d <- ncol(W)
  stopifnot(nrow(W) == d)
  if (!all(diag(W) == 0)) {
    stop("W must have zeros on the diagonal")
  }
  diag(d) - solve(W + diag(d))
}

direct_to_total <- function(W) {
  d <- ncol(W)
  stopifnot(nrow(W) == d)
  if (! all(diag(W) == 0)) {
    stop("W must have zeros on the diagonal")
  }
  solve(diag(d) - W) - diag(d)
}
