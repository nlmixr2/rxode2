% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rudfui.R
\name{rxIntToLetter}
\alias{rxIntToLetter}
\title{Convert a positive integer to a letter series}
\usage{
rxIntToLetter(x, base = 26L)
}
\arguments{
\item{x}{integer to convert}

\item{base}{can be 2 to 26}
}
\value{
a sequence of letters representing the number(s) input
}
\description{
Convert a positive integer to a letter series
}
\examples{

rxIntToLetter(1:100)

}
\author{
Matthew L. Fidler
}
