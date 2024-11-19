# Set the size of large search and small search
set_sizes_linear <- function(d, size_small = -1, size_large = -1, no_large_search = -1) {
  if (size_small == -1) {
    size_small <- round(min(1.5 * d, min(sqrt(d) * (d - 1) * log10(d), d * (d - 1) / 2)))
  }
  if (size_large == -1) {
    size_large <- round(max(1.5 * d, min(sqrt(d) * (d - 1) * log10(d), d * (d - 1) / 2)))
  }
  if (no_large_search == -1) {
    no_large_search <- round(min(d / 10, 1))
  }
  return(c(size_small, size_large, no_large_search))
}


# Set the variables less than threshold to 0 in adj matrix
threshold_W <- function(W, threshold=0.3){
  n <- nrow(W)
  m <- ncol(W)

  W_new <- matrix(0, nrow=n, ncol=m)
  W_new <- W
  W_new[abs(W_new) < threshold] <- 0
  return(W_new)
}


# change i and j in topo
# only tested the opt 1
create_new_topo <- function(topo, idx, opt = 1) {
  topo_0 <- topo

  i_pos <- which(topo_0 == idx[1])
  j_pos <- which(topo_0 == idx[2])


  if (opt == 1) {
    topo_0[c(i_pos, j_pos)] <- topo_0[c(j_pos, i_pos)]

  } else if (opt == 2) {
    topo_0 <- c(topo_0[1:(i_pos - 1)], idx[2], topo_0[(i_pos):(j_pos - 1)], topo_0[(j_pos + 1):length(topo_0)])

  } else {
    topo_0 <- c(topo_0[1:(j_pos - 1)], idx[1], topo_0[(j_pos):(i_pos - 1)], topo_0[(i_pos + 1):length(topo_0)])
  }

  return(topo_0)
}

# set pos_i to pos_j to -1
assign_negative <- function(i, j, topo) {
  succ <- FALSE
  if (length(which(topo == i)) > 0 && length(which(topo == j)) > 0) {
    pos_i <- which(topo == i)
    pos_j <- which(topo == j)

    if (pos_j[1] <= pos_i[1] && !any(topo[pos_j[1]:(pos_i[1] + 1)] == -1, na.rm = TRUE)) {
      topo[pos_j[1]:(pos_i[1])] <- -1
      succ <- TRUE
    }
  }
  return(list(topo = topo, succ = succ))
}


# update the topo
create_new_topo_greedy <- function(topo, loss_collections, idx_set, loss, opt = 1) {
  min_loss <- idx_set[which.min(loss_collections),,drop = TRUE]
  #browser()
  # Swap row and col in topo
  create_new_topo(topo, min_loss, opt)

  # loss_table <- cbind(
  #   idx_set,
  #   loss = loss_collections
  # )
  # browser()
  # loss_table_good <- loss_table[loss_table$loss < loss, ]
  # sorted_loss_table_good <- loss_table_good[order(loss_table_good$loss), ]
  # len_loss_table_good <- nrow(sorted_loss_table_good)
  #
  # topo_0 <- topo
  # for (k in 1:len_loss_table_good) {
  #   i <- as.integer(sorted_loss_table_good[k, 1])
  #   j <- as.integer(sorted_loss_table_good[k, 2])
  #   result <- assign_negative(i, j, topo_0)
  #   topo_0 <- result$topo
  #   if (result$succ) {
  #     topo_0 <- create_new_topo(topo_0, c(i, j), opt)
  #   }
  # }
  # return(topo_0)
}

# bool matrix Z Z[i, j] = True if i is before j in topo seq
create_Z <- function(ordering) {
  d <- length(ordering)
  Z <- matrix(TRUE, nrow = d, ncol = d)

  for (i in 1:(d - 1)) {
    next_indices <- ordering[(i + 1):d]
    next_indices <- ifelse(next_indices < 0, next_indices + d + 1, next_indices)
    Z[ordering[i], next_indices] <- FALSE
  }

  return(Z)
}

find_hgrad_index <- function(G_h, Z, thres = 1e-2) {
  index <- which(G_h <= thres & Z, arr.ind = TRUE)
  index0 <- index[index[, 2] != index[, 1], ]
  return(index0)
}


# find possible pairs to change according to the gradient
find_idx_set_updated <- function(G_h, G_loss, Z, size_small, size_large) {
  d <- nrow(Z)
  Zc <- Z
  diag(Zc) <- FALSE
  if (size_large > d * (d - 1) / 2) {
    stop("large search space should be less than d(d-1)/2")
  }

  if (size_small < 1) {
    stop("small search space should be more than 0")
  }
  values <- G_h[Zc]
  values <- sort(values)
  if (size_large == -1){
    size_large <- d
  }
  g_h_thre_small <- values[size_small]
  g_h_thre_large <- values[size_large]

  index_set_small <- find_hgrad_index(G_h, Zc, thres = g_h_thre_small)
  index_set_large <- find_hgrad_index(G_h, Zc, thres = g_h_thre_large)

  return(list(idx_small = data.frame(index_set_small), idx_large = data.frame(index_set_large)))
}

update_topo_linear <- function(W, topo, idx, opt = 1) {
  # filter -1 in topo
  valid_topo <- topo[topo != -1]

  topo_0 <- valid_topo
  W_0 <- matrix(0, nrow = nrow(W), ncol = ncol(W))
  i <- idx[1]
  j <- idx[2]

  i_pos <- which(valid_topo == i)
  j_pos <- which(valid_topo == j)

  if (length(j_pos) > 0 && j_pos > 0) {
    W_0[, valid_topo[1:j_pos]] <- W[, valid_topo[1:j_pos]]
  }

  if (length(i_pos) > 0 && i_pos < length(valid_topo)) {
    W_0[, valid_topo[(i_pos + 1):length(valid_topo)]] <- W[, valid_topo[(i_pos + 1):length(valid_topo)]]
  }

  topo_0 <- create_new_topo(topo = topo_0, idx = idx, opt = opt)


  if (length(j_pos) > 0 && length(i_pos) > 0 && j_pos <= i_pos) {
    for (k in seq(j_pos, i_pos)) {
      if (k > 1) {
        W_0[topo_0[1:(k - 1)], topo_0[k]] <- init_W_slice(X, idx_y = topo_0[k], idx_x = topo_0[1:(k - 1)])
      } else if (k == 1) {
        W_0[, topo_0[k]] <- 0
      }
    }
  }

  return(list(W_0 = W_0, topo_0 = topo_0))
}

init_W_slice <- function(X, idx_y, idx_x) {
  y <- X[, idx_y]
  x <- X[, idx_x]
  w <- lm(y ~ x - 1)
  return(coef(w))
}

init_W <- function(X, Z) {
  d <- ncol(X)
  W <- matrix(0, d, d)
  for (j in 1:d) {
    non_Z_j <- which(!Z[, j])

    if (length(non_Z_j) > 0) {
      X_subset <- X[, non_Z_j, drop = FALSE]
      y <- X[, j]
      W[non_Z_j, j] <- coef(lm(y ~ X_subset - 1))
    } else {
      W[, j] <- 0
    }
  }
  return(W)
}

# fit the model
fit <- function(
    X, topo, no_large_search = -1, size_small = -1, size_large = -1, loss_type='l2',
    s = 1.1) {
  d <- ncol(X)
  size_info <- set_sizes_linear(d, size_small, size_large, no_large_search)
  size_small <- size_info[1]
  size_large <- size_info[2]
  no_large_search <- size_info[3]
  large_space_used <- 0
browser()
  n <- nrow(X)
  d <- ncol(X)

  Z <- create_Z(topo)
  W <- init_W(X, Z)

  loss <- loss_func_linear(X, W)
  G_loss <- grad_loss_func_linear(X, W)

  sr <- spectral_radius(W^2)

  h <- h_logdet(W, s = sr + 0.05)
  G_h <- h_logdet_grad(W, s = sr + 0.05)

  idx_set_both <- find_idx_set_updated(G_h, G_loss, Z, size_small, size_large = size_large)
  idx_set_small <- idx_set_both$idx_small
  idx_set_large <- idx_set_both$idx_large

  idx_set <- idx_set_small

  while (nrow(idx_set) > 0) {
    idx_len <- nrow(idx_set)
    browser()
    loss_collections <- apply(idx_set, 1, function(idx){
      W_c <- update_topo_linear(W = W, topo = topo, idx = idx)[1]$W_0
      loss_c <- loss_func_linear(X, W_c)
      return(loss_c)
    })

    if (any(loss > min(loss_collections))) {
      cat("Find better loss in small space\n")
      topo <- create_new_topo_greedy(topo, loss_collections, idx_set, loss)
    } else {
      if (large_space_used < no_large_search){
        # Get the idx that are in idx_set_large but not in idx_set small
        browser()
        idx_set <- dplyr::anti_join(idx_set_large, idx_set_small, by = c("row", "col"))
        idx_len <- nrow(idx_set)
        # loss_collections <- numeric(idx_len)

        loss_collections <- apply(idx_set, 1, function(idx){
          W_c <- update_topo_linear(W = W, topo = topo, idx = idx)[1]$W_0
          loss_c <- loss_func_linear(X, W_c)
          return(loss_c)
        })

        if (any(loss > loss_collections)) {
          cat("current loss :", loss, "and find better loss in large space\n")
          large_space_used <- large_space_used + 1
          topo <- create_new_topo_greedy(topo, loss_collections, idx_set, loss)
        } else {
          cat("Using larger search space, but we cannot find better loss\n")
          break
        }
      } else {
        break
      }
    }
    Z <- create_Z(topo)
    W <- init_W(X, Z)
    loss <- loss_func_linear(X, W)
    G_loss <- grad_loss_func_linear(X, W)

    cat("Spectral Radius of W: ", spectral_radius(W^2), "\n")
    h <- h_logdet(W, s = s)
    G_h <- h_logdet_grad(W, s = s)
    idx_set_both <- find_idx_set_updated(G_h, G_loss, Z, size_small, size_large = size_large)
    idx_set_small <- idx_set_both$idx_small
    idx_set_large <- idx_set_both$idx_large
    idx_set <- idx_set_small
  }

  return(list(W = W, topo = topo, Z = Z, loss = loss))
}

