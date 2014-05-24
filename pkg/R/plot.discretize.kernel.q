plot.discretize.kernel <- function(x, y, ...)
{
  contour.discretize.kernel(x, ...)

  if(!is.null(var.names <- colnames(x$axes))) {
    dot.names <- names(list(...))
    if(!("xlab" %in% dot.names))
      mtext(var.names[1], side = 1, line = par("mgp")[1])
    if(!("ylab" %in% dot.names))
      mtext(var.names[2], side = 2, line = par("mgp")[1])
  }

  invisible()
}


