% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/selu.R
\name{SELU}
\alias{SELU}
\title{Scaled Exponential Linear Unit (SELU) Activation Function}
\usage{
SELU(x)
}
\arguments{
\item{x}{A numeric vector. All elements must be finite and
non-missing.}
}
\value{
A numeric vector where the ReLU function has been applied
to each element of \code{x}.
}
\description{
Scaled Exponential Linear Unit (SELU) Activation Function
}
\examples{

SELU(c(-1, 0, 1, 2))

# Can also be used in rxode2:
x <- rxode2({
   r=SELU(time)
})

e <- et(c(-1, 0, 1, 2))

rxSolve(x, e)

}
\seealso{
Other Activation Functions: 
\code{\link{ELU}()},
\code{\link{GELU}()},
\code{\link{PReLU}()},
\code{\link{ReLU}()},
\code{\link{Swish}()},
\code{\link{dELU}()},
\code{\link{dGELU}()},
\code{\link{dPReLU}()},
\code{\link{dReLU}()},
\code{\link{dSELU}()},
\code{\link{dSwish}()},
\code{\link{dlReLU}()},
\code{\link{dsoftplus}()},
\code{\link{lReLU}()},
\code{\link{softplus}()}
}
\author{
Matthew Fidler
}
\concept{Activation Functions}
