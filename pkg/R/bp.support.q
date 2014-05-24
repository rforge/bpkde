bp.support <- function(grid, bandwidths, level = 3, ...)
{
  M <- grid$M
  d <- grid$d
  deltas <- grid$deltas

  alphas <- bandwidths$alphas
  lambdas <- bandwidths$lambdas

  seq.fun <- function(d, k) seq(-k/2 * d, (k/2-1)*d, by = d)
  axes <- apply(matrix(deltas, nrow = 1), 2, seq.fun, k = M)
  X <- as.matrix(expand.grid(as.data.frame(axes)))
  dimnames(X) <- list(1:dim(X)[1], 1:d)

  for(i in 1:(dim(alphas)[2])) {
    dist <- abs(X %*% alphas[, i, drop = FALSE])
    index <- dist < level*lambdas[i]
    X <- X[index, ]
  }

  list(X = X, index = as.integer(dimnames(X)[[1]]),
       axes = axes)
}


