bpkde <- function(X, alphas, kernel = dnorm, bw = bw.SJ, score.fun = M1,
                  r = 7, padding = 3)
{
  name <- deparse(substitute(X))

  if(dim(X)[2] == 2 && missing(alphas)) {
    thetas <- seq(from = -pi/2, to = pi/2, length = 91)[-91]
    alphas <- rbind(cos(thetas), sin(thetas))
  }

  if(dim(X)[2] == 3 && missing(alphas)) {
    alphas <- matrix(rnorm(450), 3, 150)
    alphas <- apply(alphas, 2, function(u) u / sqrt(sum(u^2)))
  }

  bws <- mvbw(X, alphas, bw)
  xbin <- mvlinbin(X, r, padding = padding * max(bws$lambdas))
  M <- xbin$M
  d <- xbin$d
  xbin.dft <- fft(xbin$xi) / M^d

  lscv <- function(x, g, b, h, u)
    h(grid = g, kern.fun = bpk, a = x, bandwidths = b,
      kernel = u, grid.fun = bp.support)

  interval <- c(0.1, 2.0)

  opt <- optimize(lscv, interval, g = xbin, b = bws, h = score.fun, u = kernel)
  a.hat <- opt$minimum
  lscv.score <- opt$objective

  K <- discretize.kernel(xbin, bpk, a = a.hat, bandwidths = bws,
                         kernel = kernel, grid.fun = bp.support)

  K.dft <- prod(sapply(xbin$limits, diff)) * fft(K$z) / M^d

  z <- Re(fft(xbin.dft * K.dft, inverse = TRUE))
  z[z < 0.0] <- 0.0

  f.hat <- list(name = name,
                linbin = xbin,
                z = z,
                params = list(a.hat = a.hat, bandwidths = bws),
                kernel = K,
                lscv.score = lscv.score)

  oldClass(f.hat) <- c("bpkde", "mvkde")

  f.hat
}


