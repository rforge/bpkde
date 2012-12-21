\name{plot.discretize.kernel}
\alias{plot.discretize.kernel}

\title{
Plot Kernel
}

\description{
Plot a discretized kernel.
}

\usage{
\method{plot}{discretize.kernel}(x, y, plot.fun = c("contour", "image", "persp"), trim = 0.75, ...)
}

\arguments{
  \item{x}{an object of class \code{kernel}.}
  \item{y}{this argument is not used.}
  \item{plot.fun}{a character string giving the name of the function used to plot the kernel.}
  \item{trim}{a positive numeric value between 0 and 1 specifying the amount of border that gets trimmed before ploting.}
  \item{\dots}{additional arguments are passed to the lower level plotting functions.}
}

\value{
\code{x} is invisibly returned.
}

\author{
Kjell Konis \email{kjell.konis@me.com}
}

\keyword{hplot}

