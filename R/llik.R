#' Log likelihood for normal distribution
#'
#' @param x Observation
#' @param mean Mean for the likelihood
#' @param sd Standard devitation for the likelihood
#' @param full Add the data frame showing x, mean, sd as well as the
#'   fx and derivatives
#'
#' @details
#'
#' In an `rxode2()` model, you can use `llikNorm()` but you have to
#' use all arguments.  You can also get the derivitaves with
#' `llikNormDmean()` and `llikNormDsd()`
#'
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
#'
#' et <- et(-3, 3, length.out=10)
#' et$mu <- 0
#' et$sigma <- 1
#'
#' model <- rxode2({
#'   fx <- llikNorm(time, mu, sigma)
#'   dMean <- llikNormDmean(time, mu, sigma)
#'   dSd <- llikNormDsd(time, mu, sigma)
#' })
#'
#' ret <- rxSolve(model, et)
#' ret
llikNorm <- function(x, mean = 0, sd = 1, full=FALSE) {
  checkmate::assertNumeric(x, min.len=1, any.missing=FALSE, finite=TRUE)
  checkmate::assertNumeric(mean, min.len=1, any.missing=FALSE, finite=TRUE)
  checkmate::assertNumeric(sd, min.len=1, lower=0, any.missing=FALSE, finite=TRUE)
  .df <- try(data.frame(x=x, mean=mean, sd=sd), silent=TRUE)
  if (inherits(.df, "try-error")) {
    stop("incompatible dimensions for x, mean and sd", call.=FALSE)
  }
  .ret <- llikNormInternal(.df$x, .df$mean, .df$sd)
  if (full) .ret <- cbind(.df, .ret)
  .ret
}
#' log-likelihood for the Poisson distribution
#'
#' @param x non negative integers
#' @param lambda non-negative means
#' @inheritParams llikNorm
#' @details
#'
#' In an `rxode2()` model, you can use `llikPois()` but you have to
#' use all arguments.  You can also get the derivitaves with
#' `llikPoisDlambda()`

#' @return data frame with `fx` for the pdf value of with
#'   `dLambda` that has the derivatives with respect to the parameters at
#'   the observation time-point
#' @author Matthew L. Fidler
#' @export
#' @examples
#'
#' llikPois(0:7, lambda = 1)
#'
#' llikPois(0:7, lambda = 4, full=TRUE)
#'
#' et <- et(0:10)
#' et$lambda <- 0.5
#'
#' model <- rxode2({
#'   fx <- llikPois(time, lambda)
#'   dLambda <- llikPoisDlambda(time, lambda)
#' })
#'
#' rxSolve(model, et)
llikPois <- function(x, lambda, full=FALSE) {
  checkmate::assertIntegerish(x, min.len=0, lower=0, any.missing=FALSE)
  checkmate::assertNumeric(lambda, min.len=0, lower=0, any.missing=FALSE, finite=TRUE)
  .df <- try(data.frame(x=x, lambda=lambda), silent=TRUE)
  if (inherits(.df, "try-error")) {
    stop("incompatible dimensions for x, lambda", call.=FALSE)
  }
  .ret <- llikPoisInternal(.df$x, .df$lambda)
  if (full) .ret <- cbind(.df, .ret)
  .ret
}

