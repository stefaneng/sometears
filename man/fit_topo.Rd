% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_topo.R
\name{fit_topo}
\alias{fit_topo}
\title{Fit TOPO from Deng et al 2023}
\usage{
fit_topo(
  X,
  topo = 1:ncol(X),
  size_small = NULL,
  size_large = NULL,
  use_large_space = d <= 10,
  model_type = c("linear", "nonlinear"),
  loss_type = c("l2"),
  h_func = h_logdet,
  h_func_grad = h_logdet_grad,
  s = 1.1,
  verbose = F
)
}
\arguments{
\item{X}{A matrix of data in which each of the columns is a variable of interest in a directed acyclic graph}

\item{topo}{A vector of integers indicating the initial topological order of the variables}

\item{size_small}{The minimum size of the search space.}

\item{size_large}{The maximum size of the search space.}

\item{use_large_space}{A boolean indicating if the search space should be expanded if the minimum size is not sufficient.}

\item{model_type}{The type of model to fit. Currently only linear models are supported.}

\item{loss_type}{Currently only supports l2 loss}

\item{h_func}{The continuous acyclic function to use. See h_logdet.}

\item{h_func_grad}{The gradient of the acyclic funtion to use. See h_logdet_grad.}

\item{s}{penalty parameter for the logdet function. Should be larger than the spectral radius of X.}

\item{verbose}{A boolean indicating if the function should print out progress}
}
\value{
A list containing the estimated W matrix, the topological order, the Z matrix, and the loss
}
\description{
Fit TOPO from Deng et al 2023
}
\examples{

B <- matrix(
c(0, 3, 3, 1,
  0, 0, 1, 5,
  0, 0, 0, 1,
  0, 0, 0, 0),
nrow = 4, ncol = 4, byrow = TRUE)

d <- ncol(B)
X <- sim_linear_sem(B, n = 1000, Sigma = diag(0.01, nrow = d))
(est_B <- fit_topo(X, d:1))
}
