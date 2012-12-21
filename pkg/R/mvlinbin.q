mvlinbin <- function(X, r = 7, padding = 0)
{
  name <- deparse(substitute(X))

  if(!is.array(X))
    X <- matrix(X, ncol = 1)

  N <- dim(X)[1]
  d <- dim(X)[2]
  M <- 2^r

  ranges <- apply(X, 2, range)
  spreads <- apply(ranges, 2, diff)
  spread <- max(spreads) + 2 * padding
  delta <- spread / M
  spread <- spread / 2

  middles <- apply(ranges, 2, mean)
  limits <- lapply(middles, function(u, s) u + c(-1, 1)*s, s = spread)

  axes <- matrix(0.0, M+1, d)
  for(j in 1:d)
    axes[,j] <- seq(limits[[j]][1], limits[[j]][2], length = M+1)

  if(d == 1)
    xi <- matrix(KernSmooth:::linbin(X, axes[,1]), ncol = 1)

  else if(d == 2)
    xi <- KernSmooth:::linbin2D(X, axes[,1], axes[,2])

  else {
    int <- matrix(0, N, d)
    for(j in 1:d)
      int[,j] <- findInterval(X[, j], axes[,j], all.inside = TRUE)

    adj <- list()
    adj[1:d] <- list(0:1)
    adj <- as.matrix(expand.grid(adj))
    dimnames(adj) <- NULL

    pow2d <- 2^d
    J <- matrix((M+1)*(0:(d-1)), nrow = pow2d, ncol = d, byrow = TRUE)

    xi <- array(0.0, dim = rep(M+1, d))

    for(i in 1:N) {
      K <- matrix(int[i, ], nrow = pow2d, ncol = d, byrow = TRUE) + adj
      vertices <- apply(K+J, 1, function(u, Y) Y[u], Y = axes)
      B <- delta - abs(vertices - X[i,])
      xi[K] <- xi[K] + apply(B, 2, prod)
    }
  }

  xi <- do.call("[", c(list(xi), as.list(rep(-(M+1), d))))
  xi <- xi / (sum(xi) * delta^d)

  ans <- list(xi = xi, axes = axes[-(M+1),], X = X, name = name,
              delta = delta, M = M, N = N, d = d, limits = limits)

  oldClass(ans) <- "mvlinbin"
  ans
}


