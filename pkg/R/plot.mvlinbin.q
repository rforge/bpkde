plot.mvlinbin <- function(x, y, type = c("binned", "scatter"), ...)
{
  if(x$d != 2)
    stop("plot.mvlinbin can only be used with 2 dimensional data")

  type <- match.arg(type)

  if(type == "scatter")
    plot(x$X, ...)

  if(type == "binned") {
    image.mvlinbin(x, ...)

    if(!is.null(var.names <- colnames(x$axes))) {
      dot.names <- names(list(...))
      if(!("xlab" %in% dot.names))
        mtext(var.names[1], side = 1, line = par("mgp")[1])
      if(!("ylab" %in% dot.names))
        mtext(var.names[2], side = 2, line = par("mgp")[1])
    }
  }

  invisible()
}


