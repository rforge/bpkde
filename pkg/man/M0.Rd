\name{M0}
\alias{M0}

\title{
M0 Least-Squares Cross-Validation
}

\description{
Compute the M0 least-squares cross-validation score.
}

\usage{
M0(grid, kern.fun, ...)
}

\arguments{
  \item{grid}{an object of class \code{linbin}.}
  \item{kern.fun}{the density function of the kernel. The first argument must be a matrix whose rows contain the points where the density function will be evaluated.  See, for example, the \code{\link[mvtnorm]{dmvnorm}} function in the \code{mvtnorm} package.}
  \item{\dots}{additional arguments are passed to \code{kern.fun}.}
}

\value{
a single numeric value: the M0 least-squares cross-validation score.
}

\references{
Silverman, B. W. (1986) \emph{Density Estimation for Statistics and Data Analysis}. London: Chapman and Hall.
}

\author{
Kjell Konis \email{kjell.konis@me.com}
}

\keyword{multivariate}

