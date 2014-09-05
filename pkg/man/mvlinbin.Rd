\name{mvlinbin}
\alias{mvlinbin}

\title{
Multivariate Linear Binning
}

\description{
Compute a binned approximation of the data on a regularly spaced grid using the multivariate linear binning rule described in Wand (1994).
}

\usage{
mvlinbin(X, r = 7, padding)
}

\arguments{
  \item{X}{a numeric matrix.}
  \item{r}{a positive integer value. The number of grid points \eqn{M} in each dimension is given by \eqn{M = 2^r}.}
  \item{padding}{a numeric vector of positive values with length equal to the number of columns of \code{X} specifying the amount of zero-padding added to each coordinate direction.  No padding is added when this argument is missing.}
}

\value{
a list with class \code{mvlinbin} containing the following elements.
\item{axes}{a numeric matrix whose columns contain the grid points used along each axis to bin the data.}
\item{xi}{a numeric array containing the binned approximation of the data.}
\item{X}{a numeric matrix containing the input data.}
\item{deltas}{a numeric vector containing the grid spacing.}
\item{M}{an integer value giving the number of grid points used in each coordinate direction.}
\item{n}{an integer value containing the number of data points binned.}
\item{d}{an integer value giving the dimensionality of the data.}
}

\references{
Wand, M. P. (1994). Fast Computation of Multivariate Kernel Estimators. \emph{Journal of Computational and Graphical Statistics}, 3, 433-445.
}

\author{
Kjell Konis \email{kjell.konis@me.com}
}

\keyword{multivariate}


