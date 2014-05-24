\name{bpkde}
\alias{bpkde}

\title{
Back-Projected Kernel Density Estimation
}

\description{
Compute a \code{d}-dimensional kernel density estimate using a back-projected kernel.
}

\usage{
bpkde(X, alphas, kernel = dnorm, bw = bw.SJ, score.fun = M1, r = 7, padding = 4)
}

\arguments{
  \item{X}{a numeric matrix of dimension \code{N} by \code{d} containing the sample points.}
  \item{alphas}{a numeric matrix of dimension \code{d} by \code{K} whose columns contain the directions (as unit vectors) used for the back projection. If missing, 90 equally space directions spanning the interval \eqn{[-\pi/2, \pi/2)} are used when \code{d == 2} and 450 randomly chosen directions are used when \code{d == 3}.  This argument must be provided for \code{d >= 4}.}
  \item{kernel}{a function for evaluating the univariate kernel.}
  \item{bw}{the function used to compute the univariate bandwidth estimates.}
  \item{score.fun}{the function used to compute the least squares cross-validation score for the kernel; see \code{\link{M0}} and \code{\link{M1}}.}
  \item{r}{the computations are performed using linear binning and the discrete Fourier transform. The number of the grid points used is \eqn{2^r}.}
  \item{padding}{a postive numeric value specifying the amount of zero-padding in units of bandwidth.}
}

\value{
a list with class \code{c("bpkde", "mvkde")} containing the following elements.
\item{axes}{a numeric matrix whose columns contain the grid points used along each axis to bin the data.}
\item{z}{a numeric array containing the discrete kernel density estimate.}
\item{params}{a list containing the optimal common scaling parameter \code{omega.hat}, the input set of directions \code{alphas}, and the computed univariate bandwidths \code{lambdas}.}
}

\section{References}{
Panaretos, Victor M. and Konis, Kjell (2012). Nonparametric Construction of Multivariate Kernels. \emph{Journal of the American Statistical Association} 107(499):1085-1095.
}

\author{
Kjell Konis \email{kjell.konis@me.com}
}

\examples{
data(Trimodal2)
f.hat <- bpkde(Trimodal2)
}

\keyword{multivariate}

