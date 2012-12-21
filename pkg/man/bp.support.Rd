\name{bp.support}
\alias{bp.support}

\title{Back-Projected Kernel Support}

\description{
This function is intended to be used for the \code{\link{discretize.kernel}} function's \code{grid.fun} argument. It finds the grid points where the back-projected kernel has nonzero density.
}

\usage{
bp.support(grid, bandwidths, level = 3, ...)
}

\arguments{
  \item{grid}{a list created by the \code{mvlinbin} function.}
  \item{bandwidths}{a list with elements \code{alphas}, the set of projection directions, and \code{lambdas}, their associated bandwidth estimates.}
  \item{level}{a positive numeric value. Only grid points that are less than \code{level} bandwidths from the origin in all directions are assigned nonzero density.}
  \item{\dots}{additional arguments are ignored.}
}

\value{
a list with elements:
\item{X}{a matrix whose rows contain the grid points where the back projected kernel has nonzero density.}
\item{index}{a vector of indices into an \eqn{M^d} array corresponding to the points in \code{X}.}
\item{axes}{a matrix whose columns contains the axes of the grid for the kernel.}
}

\author{
Kjell Konis \email{kjell.konis@me.com}
}


\keyword{multivariate}


