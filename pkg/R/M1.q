M1 <- function(grid, kern.fun, ...)
{
  M <- grid$M
  n <- grid$n
  d <- grid$d
  k <- discretize.kernel(grid, kern.fun, ...)
  k.dft <- prod(grid$deltas) * fft(k$z)
  kstarft <- k.dft^2 - 2*k.dft
  grid.dft <- fft(grid$xi) / M^d
  k0 <- do.call("[", c(list(k$z), as.list(rep(1, d))))
  M^d * prod(grid$deltas) * sum(Re(kstarft)*abs(grid.dft)^2) + 2/n*k0
}


