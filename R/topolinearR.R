# TOPO_linear = function(score, regress, X, topo) {
#   obj = list(
#     score = score,
#     regress = regress,
#     X = X,
#     topo = topo,
#     n = nrow(X),
#     d = ncol(X),
#     W = NULL,
#     Z = NULL
#   )
#   class(obj) = "TOPO_linear"
#   return(obj)
# }
#
# init_W_slice = function(self, idx_y, idx_x) {
#   y = self$X[, idx_y]
#   x = self$X[, idx_x]
#   w = self$regress(X = x, y = y)
#   return(w)
# }
#
# init_W = function(self, Z) {
#   W = matrix(0, nrow = self$d, ncol = self$d)
#   for (j in 1:self$d) {
#     if (any(!Z[, j])) {
#       W[!Z[, j], j] = self$regress(X = self$X[, !Z[, j]], y = self$X[, j])
#     } else {
#       W[, j] = 0
#     }
#   }
#   return(W)
# }
#
# h = function(self, W) {
#   I = diag(self$d)
#   s = 1
#   M = s * I - abs(W)
#   h_val = -det(M)[[1]] + self$d * log(s)
#   G_h = solve(M)
#   return(list(h_val, G_h))
# }
#
# update_topo_linear = function(self, W, topo, idx, opt = 1) {
#   topo0 = topo
#   W0 = matrix(0, nrow = self$d, ncol = self$d)
#   i = idx[1]
#   j = idx[2]
#   i_pos = match(i, topo)
#   j_pos = match(j, topo)
#
#   W0[, topo[1:j_pos]] = W[, topo[1:j_pos]]
#   W0[, topo[(i_pos + 1):length(topo)]] = W[, topo[(i_pos + 1):length(topo)]]
#   topo0 = create_new_topo(topo0, idx, opt)
#
#   for (k in j_pos:i_pos) {
#     if (length(topo0[1:k]) != 0) {
#       W0[topo0[1:k], topo0[k]] = self$init_W_slice(topo0[k], topo0[1:k])
#     } else {
#       W0[, topo0[k]] = 0
#     }
#   }
#   return(list(W0, topo0))
# }
#
# fit = function(self, X, topo, no_large_search = -1, size_small = -1, size_large = -1, verbose = FALSE) {
#   vprint = if (verbose) print else function(...) NULL
#   size_info = set_sizes_linear(self$d, size_small, size_large, no_large_search)
#   size_small = size_info[[1]]
#   size_large = size_info[[2]]
#   no_large_search = size_info[[3]]
#
#   self$n = nrow(X)
#   self$d = ncol(X)
#   self$X = X
#   iter_count = 0
#   large_space_used = 0
#
#   if (!is.list(topo)) stop("Topology must be a list")
#
#   self$topo = topo
#   self$Z = create_Z(self$topo)
#   self$W = self$init_W(self$Z)
#
#   loss_info = self$score(self$X, self$W)
#   loss = loss_info[[1]]
#   G_loss = loss_info[[2]]
#   vprint(paste("Initial loss:", loss))
#
#   h_info = self$h(self$W)
#   h_val = h_info[[1]]
#   G_h = h_info[[2]]
#
#   idx_sets = find_idx_set_updated(G_h, G_loss, self$Z, size_small, size_large)
#   idx_set_small = idx_sets[[1]]
#   idx_set_large = idx_sets[[2]]
#
#   idx_set = idx_set_small
#   while (length(idx_set) > 0) {
#     loss_collections = numeric(length(idx_set))
#
#     for (i in 1:length(idx_set)) {
#       W_c = self$update_topo_linear(self$W, self$topo, idx_set[i])[[1]]
#       loss_c = self$score(self$X, W_c)[[1]]
#       loss_collections[i] = loss_c
#     }
#
#     if (any(loss > min(loss_collections))) {
#       vprint(paste("Current loss:", loss, "and found better loss in small space"))
#       self$topo = create_new_topo_greedy(self$topo, loss_collections, idx_set, loss)
#     } else {
#       if (large_space_used < no_large_search) {
#         vprint(paste("Current loss:", loss, "and cannot find better loss in small space"))
#         vprint(paste("Using larger search space for", large_space_used + 1, "times"))
#         idx_set = setdiff(idx_set_large, idx_set_small)
#
#         loss_collections = numeric(length(idx_set))
#         for (i in 1:length(idx_set)) {
#           W_c = self$update_topo_linear(self$W, self$topo, idx_set[i])[[1]]
#           loss_c = self$score(self$X, W_c)[[1]]
#           loss_collections[i] = loss_c
#         }
#
#         if (any(loss > loss_collections)) {
#           large_space_used = large_space_used + 1
#           self$topo = create_new_topo_greedy(self$topo, loss_collections, idx_set, loss)
#           vprint(paste("Current loss:", loss, "and found better loss in large space"))
#         } else {
#           vprint("Using larger search space, but no better loss found")
#           break
#         }
#       } else {
#         vprint(paste("Reached the number of chances to search large space:", no_large_search))
#         break
#       }
#     }
#
#     self$Z = create_Z(self$topo)
#     self$W = self$init_W(self$Z)
#     loss_info = self$score(self$X, self$W)
#     loss = loss_info[[1]]
#     G_loss = loss_info[[2]]
#     h_info = self$h(self$W)
#     h_val = h_info[[1]]
#     G_h = h_info[[2]]
#
#     idx_sets = find_idx_set_updated(G_h, G_loss, self$Z, size_small, size_large)
#     idx_set_small = idx_sets[[1]]
#     idx_set_large = idx_sets[[2]]
#
#     idx_set = idx_set_small
#     iter_count = iter_count + 1
#   }
#
#   return(list(self$W, self$topo, iter_count, loss, h_val, G_h))
# }
