print.mvkde <- function(x, ...)
{
  cat("  A kernel desnity esimtate of", x$name, "\n")
  cat("  Least-squares cross-validation score:", x$lscv.score, "\n")
  invisible(x)
}


