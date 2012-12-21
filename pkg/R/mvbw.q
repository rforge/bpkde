mvbw <- function(X, alphas, bw = bw.SJ)
{
  lambdas <- apply(alphas, 2, function(u, Y, bw) bw(drop(Y %*% u)),
                   Y = X, bw = bw)
  list(alphas = alphas, lambdas = lambdas)
}


