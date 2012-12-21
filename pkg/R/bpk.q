bpk <- function(X, bandwidths, a = 1.0, kernel = dnorm, ...)
  apply(X, 1, function(u, v, b) mean(kernel((u%*%v)/b) / b),
        v = bandwidths$alphas, b = a*bandwidths$lambdas)


