plot.mvkde <- function(x, y, ...)
{
  if(length(dim(x$z)) != 2)
    stop("plot.mvkde can only be used with 2 dimensional data")

  image.mvlinbin(x$linbin, ...)
  contour.mvkde(x, add = TRUE, ...)

  if(!is.null(var.names <- colnames(x$linbin$axes))) {
    dot.names <- names(list(...))
    if(!("xlab" %in% dot.names))
      mtext(var.names[1], side = 1, line = par("mgp")[1])
    if(!("ylab" %in% dot.names))
      mtext(var.names[2], side = 2, line = par("mgp")[1])
  }

  invisible()
}


