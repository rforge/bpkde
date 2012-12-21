bp.support <- function(grid, bandwidths, level = 3, ...)
{
  M <- grid$M
  d <- grid$d
  delta <- grid$delta

  alphas <- bandwidths$alphas
  lambdas <- bandwidths$lambdas

  x <- seq(from = -M*delta/2, by = delta, length = M)
  X <- list()
  X[1:d] <- list(x)
  X <- as.matrix(expand.grid(X))
  dimnames(X) <- list(1:dim(X)[1], 1:d)

  for(i in 1:(dim(alphas)[2])) {
    dist <- abs(X %*% alphas[, i, drop = FALSE])
    index <- dist < level*lambdas[i]
    X <- X[index, ]
  }

  list(X = X, index = as.integer(dimnames(X)[[1]]),
       axes = matrix(rep(x, d), ncol = d))
}


