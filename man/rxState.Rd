% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{rxState}
\alias{rxState}
\title{State variables}
\usage{
rxState(obj = NULL, state = NULL)
}
\arguments{
\item{obj}{rxode2 family of objects}

\item{state}{is a string indicating the state or compartment that
you would like to lookup.}
}
\value{
If state is missing, return a character vector of all the states.

If state is a string, return the compartment number of the named state.
}
\description{
This returns the model's compartments or states.
}
\seealso{
\code{\link[=rxode2]{rxode2()}}

Other Query model information: 
\code{\link{rxDfdy}()},
\code{\link{rxInits}()},
\code{\link{rxLhs}()},
\code{\link{rxModelVars}()},
\code{\link{rxParams}()}
}
\author{
Matthew L.Fidler
}
\concept{Query model information}
