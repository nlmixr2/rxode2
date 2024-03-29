% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot.rxSolve}
\alias{plot.rxSolve}
\alias{plot.rxSolveConfint1}
\alias{plot.rxSolveConfint2}
\title{Plot rxode2 objects}
\usage{
\method{plot}{rxSolve}(x, y, ..., log = "", xlab = "Time", ylab = "")

\method{plot}{rxSolveConfint1}(x, y, ..., xlab = "Time", ylab = "", log = "")

\method{plot}{rxSolveConfint2}(x, y, ..., xlab = "Time", ylab = "", log = "")
}
\arguments{
\item{x}{rxode2 object to plot}

\item{y}{Compartments or left-hand-side values to plot either as a bare name
or as a character vector}

\item{...}{Ignored}

\item{log}{Should "" (neither x nor y), "x", "y", or "xy" (or "yx") be
log-scale?}

\item{xlab, ylab}{The x and y axis labels}
}
\value{
A ggplot2 object
}
\description{
Plot rxode2 objects
}
\seealso{
Other rxode2 plotting: 
\code{\link{rxTheme}()}
}
\concept{rxode2 plotting}
\keyword{Internal}
