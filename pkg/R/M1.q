M1 <- function(grid, kern.fun, ...)
{
  M <- grid$M
  N <- grid$N
  d <- grid$d
  k <- discretize.kernel(grid, kern.fun, ...)
  k.dft <- prod(sapply(grid$limits, diff)) * fft(k$z) / M^d
  kstarft <- k.dft^2 - 2*k.dft
  grid.dft <- fft(grid$xi) / M^d
  k0 <- do.call("[", c(list(k$z), as.list(rep(1, d))))
  prod(sapply(grid$limits, diff)) * sum(Re(kstarft)*abs(grid.dft)^2) + 2/N*k0
}


