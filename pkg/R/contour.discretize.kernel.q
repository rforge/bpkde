contour.discretize.kernel <- function(x, center = c(0, 0), ...)
{
  if(length(dim(x$z)) != 2)
    stop("plot can only be used with 2 dimensional data")

  M <- dim(x$axes)[1]
  idx <- c((M/2+1):M, 1:(M/2))
  z <- x$z[idx, idx]
  y <- x$axes[idx, 2]
  x <- x$axes[idx, 1]

  zx <- rowSums(z)
  zy <- colSums(z)

  x.idx <- abs(x) < 1.5*max(x[zx > 0])
  y.idx <- abs(y) < 1.5*max(y[zy > 0])

  x <- x[x.idx] + center[1]
  y <- y[y.idx] + center[2]
  z <- z[x.idx, y.idx]

  contour.default(x, y, z, ...)
}


