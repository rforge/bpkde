plot.mvlinbin <- function(x, y, type = c("binned", "scatter"), ...)
{
  if(x$d != 2)
    stop("plot can only be used with 2 dimensional data")

  type <- match.arg(type)
  dots <- list(...)

  if(type == "scatter") {
    dots$x <- x$X
    dots$xlim <- x$limits[[1]]
    dots$ylim <- x$limits[[2]]
    if(is.null(dots$pch)) dots$pch <- 16
    if(is.null(dots$xlab)) dots$xlab <- ""
    if(is.null(dots$ylab)) dots$ylab <- ""
    if(is.null(dots$main)) dots$main <- paste("Scatter Plot of", x$name)

    op <- par(pty = "s")
    on.exit(par(op))
    do.call(plot, dots)
  }

  if(type == "binned") {
    dots$x <- x$axes[,1]
    dots$y <- x$axes[,2]
    dots$z <- x$xi
    if(is.null(dots$col)) dots$col <- gray((32:0)/32)
    if(is.null(dots$xlab)) dots$xlab <- ""
    if(is.null(dots$ylab)) dots$ylab <- ""
    if(is.null(dots$main)) dots$main <- paste("Binned Estimate of", x$name)

    op <- par(pty = "s")
    on.exit(par(op))
    do.call(image, dots)
  }

  invisible(x)
}


