\name{mvbw}
\alias{mvbw}

\title{
Multivariate Bandwidth
}

\description{
Estimate univariate bandwidths for a \code{d} dimensional sample over a given set of directions.
}

\usage{
mvbw(X, alphas, bw = bw.SJ)
}

\arguments{
  \item{X}{a numeric matrix of dimension \code{N} by \code{d} containing the sample points.}
  \item{alphas}{a numeric matrix of dimension \code{d} by \code{K} whose columns contain the directions (as unit vectors) in which the univariate bandwidths will be computed.}
  \item{bw}{the function used to compute the univariate bandwidths.}
}

\value{
a numeric vector of length \code{K} containing a bandwidth for each direction supplied in \code{alphas}.
}

\author{
Kjell Konis \email{kjell.konis@me.com}
}

\keyword{multivariate}

