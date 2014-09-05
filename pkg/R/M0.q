M0 <- function(grid, kern.fun, ...)
{
  X <- grid$X
  M <- grid$M
  n <- grid$n
  d <- grid$d

  Y <- matrix(0.0, n*(n-1), d)
  k <- 1

  score <- matrix(0, n, n)

  for(i in 1:n) {
    for(j in (1:n)[-i]) {
      Y[k, ] <- X[i,] - X[j,]
      k <- k + 1
    }
  }

  score <- kern.fun(Y, ...)

  k <- discretize.kernel(grid, kern.fun, ...)
  k.dft <- prod(grid$deltas) * fft(k$z)
  grid.dft <- fft(grid$xi) / M^d
  z <- Re(fft(grid.dft * k.dft, inverse = TRUE))

  if(is.numeric(k$scale))
    score <- score / k$scale

  sum(z^2)*prod(grid$deltas) - 2/(n*(n-1)) * sum(score)
}


