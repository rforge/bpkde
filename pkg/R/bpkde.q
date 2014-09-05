bpkde <- function(data, alphas, kernel = dnorm, bw = bw.SJ, score.fun = M1,
                  r = 7, padding = 4)
{
  name <- deparse(substitute(data))
  X <- data.matrix(data)

  if(dim(X)[2] == 2 && missing(alphas)) {
    thetas <- seq(from = -pi/2, to = pi/2, length = 91)[-91]
    alphas <- rbind(cos(thetas), sin(thetas))
  }

  if(dim(X)[2] == 3 && missing(alphas)) {
    alphas <- matrix(rnorm(450), 3, 150)
    alphas <- apply(alphas, 2, function(u) u / sqrt(sum(u^2)))
  }

  b <- apply(alphas, 2, function(u, Y, bw) bw(drop(Y %*% u)), Y = X, bw = bw)
  bws <- list(alphas = alphas, lambdas = b)

  if(padding > 0)
    xbin <- mvlinbin(X, r, padding = padding * apply(X, 2, bw))
  else
    xbin <- mvlinbin(X, r)

  xi.dft <- fft(xbin$xi) / xbin$M^xbin$d

  lscv <- function(x, g, b, h, u)
    h(grid = g, kern.fun = bpk, a = x, bandwidths = b,
      kernel = u, grid.fun = bp.support)

  opt <- optimize(lscv, c(0.1, 2.0), g = xbin, b = bws,
                  h = score.fun, u = kernel)

  a.hat <- opt$minimum
  lscv.score <- opt$objective

  K <- discretize.kernel(xbin, bpk, a = a.hat, bandwidths = bws,
                         kernel = kernel, grid.fun = bp.support)

  K.dft <- prod(xbin$deltas) * fft(K$z)

  z <- Re(fft(xi.dft * K.dft, inverse = TRUE))
  z[z < 0.0] <- 0.0

  f.hat <- c(xbin,
             list(name = name, z = z, kernel = K,
             params = list(a.hat = a.hat, bandwidths = bws),
             lscv.score = lscv.score))

  oldClass(f.hat) <- c("bpkde", "mvkde")

  f.hat
}


