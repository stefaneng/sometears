% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logdet.R
\name{h_logdet}
\alias{h_logdet}
\alias{h_logdet_grad}
\title{Computes the log-determinant acyclicity characterization}
\usage{
h_logdet(X, s = 1, transform = c("square", "abs"))

h_logdet_grad(X, s = 1)
}
\arguments{
\item{X}{a matrix}

\item{s}{a regularization parameter. Should be larger than spectral radius (largest absolute eigenvalue) of X.}
}
\value{
numeric
}
\description{
This function is exactly zero if and only if the matrix is a DAG.
The gradient is the gradient of the log-determinant acyclicity characterization is also zero exactly when the matrix is a DAG.
}
\details{
From Bello, K., Aragam, B., & Ravikumar, P. (2023).
}
\examples{
B <- matrix(c(0, 0.6, -0.5, 0), nrow = 2, byrow = TRUE)
h_logdet(B) # Not a DAG means > 0
B_dag <- matrix(c(0,0,1,0), nrow = 2, byrow = TRUE)
h_logdet(B_dag) # DAG means 0
}
