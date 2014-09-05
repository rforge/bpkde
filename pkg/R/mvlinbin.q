mvlinbin <- function(X, r = 7, padding)
{
  X <- data.matrix(X)

  n <- dim(X)[1]
  d <- dim(X)[2]
  M <- 2^r

  ranges <- apply(X, 2, range)

  if(!missing(padding)) {
    if(length(padding) == d && !any(padding < 0))
      ranges <- ranges + (c(-1, 1) %o% padding)
    else
      warning("invalid padding argument; zero padding used")
  }

  deltas <- apply(ranges, 2, diff) / (M - 1)
  axes <- matrix(0.0, M + 1, d)
  for(j in 1:d)
    axes[, j] <- seq(from = ranges[1, j], by = deltas[j], length = M + 1)
  colnames(axes) <- colnames(X)

  if(d == 1)
    xi <- matrix(linbin(X, axes[,1]), ncol = 1)

  else if(d == 2)
    xi <- linbin2D(X, axes[,1], axes[,2])

  else {
    int <- matrix(0, n, d)
    for(j in 1:d)
      int[, j] <- findInterval(X[, j], axes[,j], all.inside = TRUE)

    adj <- list()
    adj[1:d] <- list(0:1)
    adj <- as.matrix(expand.grid(adj))
    dimnames(adj) <- NULL

    pow2d <- 2^d
    J <- matrix((M+1)*(0:(d-1)), nrow = pow2d, ncol = d, byrow = TRUE)

    xi <- array(0.0, dim = rep(M+1, d))

    for(i in 1:n) {
      K <- matrix(int[i, ], nrow = pow2d, ncol = d, byrow = TRUE) + adj
      vertices <- apply(K+J, 1, function(u, Y) Y[u], Y = axes)
      B <- deltas - abs(vertices - X[i,])
      xi[K] <- xi[K] + apply(B, 2, prod)
    }
  }

  xi <- do.call("[", c(list(xi), as.list(rep(-(M+1), d))))
  xi <- xi / (sum(xi) * prod(deltas))

  ans <- list(axes = axes[-(M+1),],
              xi = xi,
              X = X,
              deltas = deltas,
              M = M,
              n = n,
              d = d)

  oldClass(ans) <- "mvlinbin"
  ans
}


