\name{plot.mvkde}
\alias{plot.mvkde}

\title{
Plot a Kernel Density Esimate
}

\description{
Plot a 2 dimensional kernel density esimate.
}

\usage{
\method{plot}{mvkde}(x, y, what = c("kde", "kernel", "data"), plot.fun = c("image", "contour", "persp"), ...)
}

\arguments{
  \item{x}{an object of class \code{convWlinbin}.}
  \item{y}{this argument is not used.}
  \item{what}{either \code{"kde"} for a plot of the kernel density estimate or \code{"kernel"} for a plot of the estimated kernel. }
  \item{plot.fun}{a character string giving the name of the function used to produce the plot.}
  \item{\dots}{additional arguments are passed to lower level plotting functions.}
}

\value{
\code{x} is invisibly returned.
}

\author{
Kjell Konis \email{kjell.konis@me.com}
}

\keyword{hplot}

