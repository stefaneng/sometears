is_lower_tri <- function(X, allow_diag = TRUE) {
  all(X[upper.tri(X, diag = !allow_diag)] == 0)
}

is_upper_tri <- function(X, allow_diag = TRUE) {
  all(X[lower.tri(X, diag = !allow_diag)] == 0)
}
