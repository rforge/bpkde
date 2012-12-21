plot.mvkde <- function(x, y, what = c("kde", "kernel", "data"),
                       plot.fun = c("image", "contour", "persp"), ...)
{
  if(length(dim(x$z)) != 2)
    stop("plot can only be used with 2 dimensional data")

  what <- match.arg(what)
  plot.fun <- match.arg(plot.fun)

  if(what == "kernel")
    return(plot(x$kernel, plot.fun = plot.fun, ...))

  if(what == "data")
    return(plot(x$linbin, ...))

  dots <- list(...)
  dots$x <- x$linbin$axes[, 1]
  dots$y <- x$linbin$axes[, 2]
  dots$z <- x$z
  if(is.null(dots$col)) dots$col <- gray((0:32)/32)
  if(is.null(dots$xlab)) dots$xlab <- ""
  if(is.null(dots$ylab)) dots$ylab <- ""
  if(plot.fun == "persp" && is.null(dots$zlab)) dots$zlab <- ""
  if(plot.fun == "image" && is.null(dots$col)) dots$col <- gray((0:32)/32)

  op <- par(pty = "s")
  on.exit(par(op))
  do.call(plot.fun, dots)

  invisible(x)
}


