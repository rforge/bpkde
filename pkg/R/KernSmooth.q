## file KernSmooth/R/all.R
## original file Copyright (C) M. P. Wand
## modifications for use with R copyright (C) B. D. Ripley
## Unlimited use and distribution (see LICENCE).


## For application of linear binning to a univariate data set.
linbin <- function(X, gpoints, truncate = TRUE)
{
    n <- length(X)
    M <- length(gpoints)
    trun <- if (truncate) 1L else 0L
    a <- gpoints[1L]
    b <- gpoints[M]
    .Fortran(f_linbin, as.double(X), as.integer(n),
             as.double(a), as.double(b), as.integer(M),
             as.integer(trun), double(M))[[7]]
}


## Creates the grid counts from a bivariate data set X
## over an equally-spaced set of grid points
## contained in "gpoints" using the linear
## binning strategy. Note that the FORTRAN subroutine
## "lbtwod" is called.
linbin2D <- function(X, gpoints1, gpoints2)
{
    n <- nrow(X)
    X <- c(X[, 1L], X[, 2L])
    M1 <- length(gpoints1)
    M2 <- length(gpoints2)
    a1 <- gpoints1[1L]
    a2 <- gpoints2[1L]
    b1 <- gpoints1[M1]
    b2 <- gpoints2[M2]
    out <- .Fortran(f_lbtwod, as.double(X), as.integer(n),
                    as.double(a1), as.double(a2), as.double(b1), as.double(b2),
                    as.integer(M1), as.integer(M2), double(M1*M2))
    matrix(out[[9L]], M1, M2)
}


