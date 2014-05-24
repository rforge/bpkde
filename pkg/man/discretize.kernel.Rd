\name{discretize.kernel}
\alias{discretize.kernel}

\title{
Discretize Kernel
}

\description{
Computes a discrete approximation of the kernel.
}

\usage{
discretize.kernel(grid, kern.fun, ..., grid.fun = NULL, scale = TRUE)
}

\arguments{
  \item{grid}{a list created by the \code{mvlinbin} function.}
  \item{kern.fun}{a function for evaluating the kernel. The first argument must be a matrix whose rows contain the points where the density is to be evaluated.  See, for example, the \code{\link[mvtnorm]{dmvnorm}} function in the \code{mvtnorm} package.}
  \item{\dots}{additional arguments are passed to \code{kern.fun}.}
  \item{grid.fun}{a function that returns the grid points where the kernel will be evaulated.}
  \item{scale}{a logical value. If \code{TRUE} then the kernel is scaled so that it integrates to 1 on the provided grid.}
}

\value{
a list with class \code{kernel} contining the following elements.
\item{axes}{a numeric matrix whose columns contain the grid points along each axis where the kernel was evaluated.}
\item{z}{a numeric array containing the discrete representation of the kernel.}
\item{dft}{a numeric array containing the discrete Fourier transform of the kernel \code{z}.}
\item{kern.fun}{the name of \code{kern.fun}.}
\item{params}{a list containing the pearameters passed to \code{kern.fun} through \code{\dots}.}
}

\author{
Kjell Konis \email{kjell.konis@me.com}
}

\keyword{multivariate}


