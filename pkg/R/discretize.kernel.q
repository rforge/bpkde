discretize.kernel <- function(grid, kern.fun, ..., grid.fun = NULL,
                              scale = TRUE)
{
  M <- grid$M
  d <- grid$d
  deltas <- grid$deltas
  var.names <- colnames(grid$X)

  if(is.null(grid.fun)) {
    seq.fun <- function(d, k) seq(-k/2 * d, (k/2-1)*d, by = d)
    axes <- apply(matrix(deltas, nrow = 1), 2, seq.fun, k = M)
    X <- as.matrix(expand.grid(as.data.frame(axes)))
    index <- 1:(M^d)
  }

  else {
    grid <- grid.fun(grid, ...)
    X <- grid$X
    index <- grid$index
    axes <- grid$axes
  }

  colnames(axes) <- var.names

  z <- array(0.0, dim = rep(M, d))
  z[index] <- kern.fun(X, ...)

  if(scale) {
    scale <- sum(z[index]) * prod(deltas)
    z[index] <- z[index] / scale
  }

  idx <- c((M/2+1):M, 1:(M/2))
  args <- list()
  args[1:d] <- list(idx)
  z <- do.call("[", c(list(z), args))
  axes <- axes[idx, ]

  ans <- list(axes = axes, z = z, kern.fun = deparse(substitute(kern.fun)),
              params = list(...), scale = scale)

  oldClass(ans) <- "discretize.kernel"
  ans
}


