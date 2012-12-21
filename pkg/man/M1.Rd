\name{M1}
\alias{M1}

\title{
M1 Least-Squares Cross-Validation
}

\description{
Compute the M1 least-squares cross-validation score.
}

\usage{
M1(grid, kern.fun, ...)
}

\arguments{
  \item{grid}{an object of class \code{linbin}.}
  \item{kern.fun}{the density function of the kernel. The first argument must be a matrix whose rows contain the points where the density function will be evaluated.  See, for example, the \code{\link[mvtnorm]{dmvnorm}} function in the \code{mvtnorm} package.}
  \item{\dots}{additional arguments are passed to \code{kern.fun}.}
}

\details{
The computation is done using the Fourier transform of the kernel and the data as described in Silverman (1986).
}

\value{
a single numeric value: the M1 least-squares cross-validation score.
}

\references{
Silverman, B. W. (1986) \emph{Density Estimation for Statistics and Data Analysis}. London: Chapman and Hall.
}

\author{
Kjell Konis \email{kjell.konis@me.com}
}

\keyword{multivariate}

