#' Log likelihood for normal distribution
#'
#' @param x Observation
#' @param mean Mean for the likelihood
#' @param sd Standard devitation for the likelihood
#' @param full Add the data frame showing x, mean, sd as well as the
#'   fx and derivatives
#' @return data frame with `fx` for the pdf value of with `dMean` and
#'   `dSd` that has the derivatives with respect to the parameters at
#'   the observation time-point
#' @author Matthew L. Fidler
#' @export
#' @examples
#'
#' llikNorm(0)
#'
#' llikNorm(seq(-2,2,length.out=10), full=TRUE)
llikNorm <- function(x, mean = 0, sd = 1, full=FALSE) {
  checkmate::assertNumeric(x, any.missing=FALSE, finite=TRUE)
  checkmate::assertNumeric(mean, any.missing=FALSE, finite=TRUE)
  checkmate::assertNumeric(sd, lower=0, any.missing=FALSE, finite=TRUE)
  .df <- data.frame(x=x, mean=mean, sd=sd)
  .ret <- llikNormInternal(.df$x, .df$mean, .df$sd)
  if (full) .ret <- cbind(.df, .ret)
  .ret
}
