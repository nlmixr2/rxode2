% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/llik.R
\name{llikUnif}
\alias{llikUnif}
\title{log likelihood and derivatives for Unif distribution}
\usage{
llikUnif(x, alpha, beta, full = FALSE)
}
\arguments{
\item{x}{variable distributed by a uniform distribution}

\item{alpha}{is the lower limit of the uniform distribution}

\item{beta}{is the upper limit of the distribution}

\item{full}{Add the data frame showing x, mean, sd as well as the
fx and derivatives}
}
\value{
data frame with \code{fx} for the log pdf value of with \code{dProb}
that has the derivatives with respect to the \code{prob} parameters at
the observation time-point
}
\description{
log likelihood and derivatives for Unif distribution
}
\details{
In an \code{rxode2()} model, you can use \code{llikUnif()} but you have to
use the x and rate arguments.  You can also get the derivative of \code{alpha} or \code{beta} with
\code{llikUnifDalpha()} and \code{llikUnifDbeta()}.
}
\examples{

\donttest{

llikUnif(1, -2, 2)

et  <- et(seq(1,1, length.out=4))
et$alpha <- -2
et$beta <- 2
 
model <- function() {
  model({
    fx <- llikUnif(time, alpha, beta)
    dAlpha<- llikUnifDalpha(time, alpha, beta)
    dBeta <- llikUnifDbeta(time, alpha, beta)
  })
}

rxSolve(model, et)
}
}
\author{
Matthew L. Fidler
}
