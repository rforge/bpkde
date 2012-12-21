\name{plot.mvlinbin}
\alias{plot.mvlinbin}

\title{
Plot a Linearly Binning
}

\description{
Plot a linearly binned approximation of the data.
}

\usage{
\method{plot}{mvlinbin}(x, y, type = c("binned", "scatter"), ...)
}

\arguments{
  \item{x}{an object of class \code{linbin}.}
  \item{y}{this argument is not used.}
  \item{type}{either \code{binned} or \code{scatter}. Alternatively a scatter plot using the same poltting region.}
  \item{\dots}{additional arguments are passed to the lower level plotting functions.}
}

\value{
\code{x} is invisibly returned.
}

\author{
Kjell Konis \email{kjell.konis@me.com}
}

\keyword{hplot}

