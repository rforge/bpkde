contour.mvkde <- function(x, ...)
{
  if(x$linbin$d != 2)
    stop("contour.mvkde can only be used with 2 dimensional data")

  x <- list(x = x$linbin$axes[, 1], y = x$linbin$axes[, 2], z = x$z)
  contour.default(x, ...)
}


