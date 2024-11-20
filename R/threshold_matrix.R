#' Threshold a matrix by setting the variables less than threshold to 0
#'
#' @param W A matrix
#' @param threshold A threshold value
#'
#' @return
#' @export
#'
#' @examples
#' W <- matrix(c(0, 0.6, -0.5, 1e-3), nrow = 2, byrow = TRUE)
#' threshold_W(W, threshold = 0.1)
threshold_W <- function(W, threshold=0.3){
  return(W * (abs(W) > threshold))
}
