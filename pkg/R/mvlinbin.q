mvlinbin <- function(X, r = 7, padding = 0, bw = bw.SJ)
{
  if(!is.array(X))
    X <- matrix(X, ncol = 1)

  N <- dim(X)[1]
  d <- dim(X)[2]
  M <- 2^r

  ranges <- apply(X, 2, range) + padding * (c(-1, 1) %o% apply(X, 2, bw))
  deltas <- apply(ranges, 2, diff) / M
  seq.fun <- function(u, len) seq(u[1], u[2], length = len)
  axes <- apply(ranges, 2, seq.fun, len = M + 1)
  colnames(axes) <- colnames(X)

  if(d == 1) {
    linbin <- eval(parse(text = paste("KernSmooth", "linbin", sep = ":::")))
    xi <- matrix(linbin(X, axes[,1]), ncol = 1)
  }

  else if(d == 2) {
    linbin2D <- eval(parse(text = paste("KernSmooth", "linbin2D", sep = ":::")))
    xi <- linbin2D(X, axes[,1], axes[,2])
  }

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
      B <- deltas - abs(vertices - X[i,])
      xi[K] <- xi[K] + apply(B, 2, prod)
    }
  }

  xi <- do.call("[", c(list(xi), as.list(rep(-(M+1), d))))
  xi <- xi / (sum(xi) * prod(deltas))

  ans <- list(xi = xi, axes = axes[-(M+1),], X = X, deltas = deltas,
              M = M, N = N, d = d)

  oldClass(ans) <- "mvlinbin"
  ans
}


