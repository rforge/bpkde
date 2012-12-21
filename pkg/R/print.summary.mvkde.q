print.summary.mvkde <- function(x, ...)
{
  cat("  A kernel desnity esimtate of", x$name, "\n")
  cat("  Least-squares cross-validation socre:", x$lscv.score, "\n")
  invisible(x)
}


