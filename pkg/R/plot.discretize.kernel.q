plot.discretize.kernel <- function(x, y, plot.fun = c("contour", "image", "persp"),
                                   trim = 0.75, ...)
{
  if(length(dim(x$z)) != 2)
    stop("plot can only be used with 2 dimensional data")

  plot.fun <- match.arg(plot.fun)
  dots <- list(...)

  M <- dim(x$axes)[1]
  l <- floor(trim/2 * M)
  r <- ceiling((1 - trim/2) * M)
  idx <- c((M/2+1):M, 1:(M/2))[l:r]

  dots$x <- x$axes[idx, 1]
  dots$y <- x$axes[idx, 2]
  dots$z <- x$z[idx, idx]
  if(is.null(dots$xlab)) dots$xlab <- ""
  if(is.null(dots$ylab)) dots$ylab <- ""
  if(plot.fun == "persp" && is.null(dots$zlab)) dots$zlab <- ""
  if(plot.fun == "image" && is.null(dots$col)) dots$col <- gray((0:32)/32)

  op <- par(pty = "s")
  on.exit(par(op))
  do.call(plot.fun, dots)

  invisible(x)
}


