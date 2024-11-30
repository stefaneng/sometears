set.seed(1234)

B <- matrix(
  c(0, 3, 3, 0,
    0, 0, 0, 5,
    0, 0, 0, 0,
    0, 0, 0, 0),
  nrow = 4, ncol = 4, byrow = TRUE)

d <- ncol(B)
X <- sim_linear_sem(B, n = 1000)
(est_B <- fit_topo(X, 1:d)$W)
(est_B <- fit_topo(X, c(1,4,2,3))$W)


load('C:/Users/13259/Desktop/course doc/BIOSTAT615/assignment/final_project/sometears/data/sachs.rda') # load the sachs dataset
d_sachs <- ncol(sachs)
# sachs <- scale(sachs) # if we do scale the output will be different(tested with simulated data)
(est_sachs <- fit_topo(as.matrix(sachs), 1:d_sachs, s=1.1)$W)


library(bnlearn)
col_names <- colnames(sachs)
result_df <- data.frame(from = character(), to = character(), strength = numeric(), direction = character(), stringsAsFactors = FALSE)
threshold_graph <- 0.5
for (i in 1:nrow(est_sachs)) {
  for (j in 1:ncol(est_sachs)) {
    if ((est_sachs[i, j] > threshold_graph) || (est_sachs[i, j] < -threshold_graph)) {
      new_row <- data.frame(from = col_names[i], to = col_names[j])
      result_df <- rbind(result_df, new_row)
    }
  }
}
dag <- empty.graph(nodes = colnames(sachs))
arcs(dag) <- result_df
graphviz.plot(dag)
