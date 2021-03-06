image.mvkde <- function(x, ...)
{
  if(x$d != 2)
    stop("image.mvkde can only be used with 2 dimensional data")

  z <- list(x = x$axes[, 1], y = x$axes[, 2], z = x$z)

  if("col" %in% names(list(...)))
    image.default(z, ...)
  else
    image.default(z, col = gray((8:0)/8), ...)

  invisible()
}


