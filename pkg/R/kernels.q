biweight <- function(x)
{
  ans <- numeric(length(x))
  index <- abs(x) < 1
  ans[index] <- 15 * (1 - x[index]^2)^2 / 16
  ans
}


epanechnikov <- function(x)
{
  ans <- numeric(length(x))
  index <- abs(x) < sqrt(5)
  ans[index] <- 3 * (1 - 0.2*x[index]^2) / (4 * sqrt(5))
  ans
}


rectangular <- function(x)
{
  ans <- numeric(length(x))
  index <- abs(x) < 1
  ans[index] <- 0.5
  ans
}


triangular <- function(x)
{
  ans <- numeric(length(x))
  index <- abs(x) < 1
  ans[index] <- 1 - abs(x[index])
  ans
}


