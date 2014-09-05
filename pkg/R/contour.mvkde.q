contour.mvkde <- function(x, ...)
{
  if(x$d != 2)
    stop("contour.mvkde can only be used with 2 dimensional data")

  x <- list(x = x$axes[, 1], y = x$axes[, 2], z = x$z)
  contour.default(x, ...)
}


