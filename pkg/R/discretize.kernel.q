discretize.kernel <- function(grid, kern.fun, ..., grid.fun = NULL,
                              scale = TRUE)
{
  M <- grid$M
  d <- grid$d
  delta <- grid$delta

  if(is.null(grid.fun)) {
	  x <- seq(-M*delta/2, (M/2-1)*delta, by = delta)
	  axes <- matrix(rep(x, d), ncol = d)
    X <- list()
    X[1:d] <- list(x)
    X <- as.matrix(expand.grid(X))
    index <- 1:(M^d)
  }

  else {
    grid <- grid.fun(grid, ...)
    X <- grid$X
    index <- grid$index
    axes <- grid$axes
  }

  z <- array(0.0, dim = rep(M, d))
  z[index] <- kern.fun(X, ...)

  if(scale) {
    scale <- sum(z[index]) * delta^d
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


