image.mvlinbin <- function(x, ...)
{
  if(x$d != 2)
    stop("image.mvlinbin can only be used with 2 dimensional data")

  z <- list(x = x$axes[, 1], y = x$axes[, 2], z = x$xi)

  if("col" %in% names(list(...)))
    image(z, ...)
  else
    image(z, col = gray((8:0)/8), ...)

  invisible()
}


