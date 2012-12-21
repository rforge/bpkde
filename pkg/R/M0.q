M0 <- function(grid, kern.fun, ...)
{
  X <- grid$X
  M <- grid$M
  N <- grid$N
  d <- grid$d

  Y <- matrix(0.0, N*(N-1), d)
  k <- 1

  score <- matrix(0, N, N)

  for(i in 1:N) {
    for(j in (1:N)[-i]) {
      Y[k, ] <- X[i,] - X[j,]
      k <- k + 1
    }
  }

  score <- kern.fun(Y, ...)

  k <- discretize.kernel(grid, kern.fun, ...)
  k.dft <- prod(sapply(grid$limits, diff)) * fft(k$z) / M^d
  grid.dft <- fft(grid$xi) / M^d
  z <- Re(fft(grid.dft * k.dft, inverse = TRUE))

  if(is.numeric(k$scale))
    score <- score / k$scale

  sum(z^2)*grid$delta^d - 2/(N*(N-1)) * sum(score)
}


