# Continuous Structure Learning in R

## Introduction

Network structure learning involves finding optimal solutions with the constraints that traits of interest have a directed acyclic graph (DAG) structure. Optimizing an objective function with the combinatorial constraint of a DAG is a difficult problem due to the number of DAGs increasing super-exponentially (Rodionov, 1992) so it is difficult to explore the entire graph space. The issue of finding a directed acyclic graph is related to finding a topological sort of a graph, i.e., a temporal ordering of the nodes in the graph such that for any i < j, there is no edge from node j to node i. This ordering is not generally unique and only exists if the graph is acyclic. Recent work has involved transforming the combinatorial acyclicity constraint to a continuous optimization problem (Bello et al., 2023; Wei et al., 2020; Zheng et al., 2018). The objective function with a DAG constraint is non-convex and has issues with local optimum. The latest work in this sequence of papers aims to escape local optima by swapping nodes in valid topological orderings to explore other regions of the DAG space with topological swaps (Deng et al., 2023).

## References

1. Bello, K., Aragam, B., & Ravikumar, P. (2023). DAGMA: Learning DAGs via M-matrices and a Log-Determinant Acyclicity Characterization (arXiv:2209.08037). arXiv. https://doi.org/10.48550/arXiv.2209.08037
2. Deng, C., Bello, K., Aragam, B., & Ravikumar, P. (2023). Optimizing NOTEARS Objectives via Topological Swaps (arXiv:2305.17277). arXiv. https://doi.org/10.48550/arXiv.2305.17277
3. Rodionov, V. I. (1992). On the number of labeled acyclic digraphs. Discrete Mathematics, 105(1), 319–321. https://doi.org/10.1016/0012-365X(92)90155-9
4. Sachs, K., Perez, O., Pe’er, D., Lauffenburger, D. A., & Nolan, G. P. (2005). Causal Protein-Signaling Networks Derived from Multiparameter Single-Cell Data. Science, 308(5721), 523–529. https://doi.org/10.1126/science.1105809
5. Wei, D., Gao, T., & Yu, Y. (2020). DAGs with No Fears: A Closer Look at Continuous Optimization for Learning Bayesian Networks. Advances in Neural Information Processing Systems, 33, 3895–3906. https://papers.nips.cc/paper/2020/hash/28a7602724ba16600d5ccc644c19bf18-Abstract.html
6. Zheng, X., Aragam, B., Ravikumar, P., & Xing, E. P. (2018). DAGs with NO TEARS: Continuous Optimization for Structure Learning (arXiv:1803.01422). arXiv. https://doi.org/10.48550/arXiv.1803.01422

