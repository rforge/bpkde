\name{bpk}
\alias{bpk}

\title{
Back Projected Kernel
}

\description{
Evaluate a \code{d} dimensional back projected kernel.
}

\usage{
bpk(X, bandwidths, a = 1.0, kernel = dnorm, ...)
}

\arguments{
  \item{X}{a numeric matrix of dimension \code{N} by \code{d} who's rows contain the points where the kernel will be evaluated.}
  \item{bandwidths}{a list with elements \code{alphas}, the set of projection directions, and \code{lambdas}, their associated bandwidth estimates.}
  \item{a}{a single numeric value containing a common scaling parameter.}
  \item{kernel}{a function for evaluating the univariate kernel.}
  \item{\dots}{additional arguments are ignored.}
}

\value{
a numeric vector where element i contains the value of the kernel evaluated at row i of \code{X}.
}

\author{
Kjell Konis \email{kjell.konis@me.com}
}

\keyword{multivariate}

