\name{mvlinbin}
\alias{mvlinbin}

\title{
Multivariate Linear Binning
}

\description{
Compute a binned approximation of the data on a regularly spaced grid using the multivariate linear binning rule described in Wand (1994).
}

\usage{
mvlinbin(X, r = 7, padding = 0, bw = bw.SJ)
}

\arguments{
  \item{X}{a numeric matrix containing the data.}
  \item{r}{a positive integer value. The number of grid points \eqn{M} in each dimension is given by \eqn{M = 2^r}.}
  \item{padding}{a postive numeric value specifying the amount of zero-padding in units of bandwidth.}
  \item{bw}{the function used to compute the univariate bandwidth estimates.}
}

\value{
a list with class \code{linbin} containing the following elements.
\item{xi}{a numeric array containing the binned approximation of the data.}
\item{dft}{the discrete Fourier tranform of \code{X}.}
\item{axes}{a numeric matrix whose columns contain the grid points used along each axis to bin the data.}
\item{X}{a numeric matrix containing the input data.}
\item{name}{a character string containing the name of \code{X}.}
\item{delta}{a single numeric value specifying the gird spacing.}
\item{M}{an integer value giving the number of grid points used in each coordinate direction.}
\item{N}{an integer value containing the number of data points binned.}
\item{d}{an integer value giving the dimensionality of the data.}
\item{limits}{a list containing the lower and upper bounds for the grid in each corrdinate direction.}
}

\references{
Wand, M. P. (1994). Fast Computation of Multivariate Kernel Estimators. \emph{Journal of Computational and Graphical Statistics}, 3, 433-445.
}

\author{
Kjell Konis \email{kjell.konis@me.com}
}

\keyword{multivariate}


