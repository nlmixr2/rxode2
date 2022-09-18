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
#'
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

#' Calculate the log likelihood of the binomial function (and its derivatives)
#'
#' 
#' @param x  Number of successes
#' @param size Size of trial
#' @param prob probability of success
#' 
#' @inheritParams llikNorm
#'
#' @details
#' In an `rxode2()` model, you can use `llikBinom()` but you have to
#' use all arguments.  You can also get the derivative of `prob` with
#' `llikBinomDprob()`
#' @return data frame with `fx` for the pdf value of with
#'   `dProb` that has the derivatives with respect to the parameters at
#'   the observation time-point
#' @author Matthew L. Fidler
#' @export 
#' @examples
#' 
#' llikBinom(46:54, 100, 0.5)
#'
#' llikBinom(46:54, 100, 0.5, TRUE)
#'
#' et <- et(46:54)
#' et$size <- 100
#' et$prob <-0.5
#'
#' model <- rxode2({
#'   fx <- llikBinom(time, size, prob)
#'   dProb <- llikBinomDprob(time, size, prob)
#' })
#'
#' rxSolve(model, et)
llikBinom <- function(x, size, prob, full=FALSE) {
  checkmate::assertIntegerish(x, min.len=0, lower=0, any.missing=FALSE)
  checkmate::assertIntegerish(size, min.len=0, lower=0, any.missing=FALSE)
  checkmate::assertNumeric(prob, min.len=0, lower=0, upper=1, any.missing=FALSE, finite=TRUE)
  .df <- try(data.frame(x=x, size=size, prob=prob), silent=TRUE)
  if (inherits(.df, "try-error")) {
    stop("incompatible dimensions for x, size, prob", call.=FALSE)
  }
  .ret <- llikBinomInternal(.df$x, .df$size, .df$prob)
  if (full) .ret <- cbind(.df, .ret)
  .ret
}

#' Calculate the log likelihood of the negative binomial function (and its derivatives)
#' 
#' @param x  Number of successes
#' @param size Size of trial
#' @param prob probability of success
#' 
#' @inheritParams llikNorm
#'
#' @details
#' In an `rxode2()` model, you can use `llikNbinom()` but you have to
#' use all arguments.  You can also get the derivative of `prob` with
#' `llikNbinomDprob()`
#' @return data frame with `fx` for the pdf value of with
#'   `dProb` that has the derivatives with respect to the parameters at
#'   the observation time-point
#' @author Matthew L. Fidler
#' @export 
#' @examples
#' 
#' llikNbinom(46:54, 100, 0.5)
#'
#' llikNbinom(46:54, 100, 0.5, TRUE)
#'
#' et <- et(46:54)
#' et$size <- 100
#' et$prob <-0.5
#'
#' model <- rxode2({
#'   fx <- llikNbinom(time, size, prob)
#'   dProb <- llikNbinomDprob(time, size, prob)
#' })
#'
#' rxSolve(model, et)
llikNbinom <- function(x, size, prob, full=FALSE) {
  checkmate::assertIntegerish(x, min.len=0, lower=0, any.missing=FALSE)
  checkmate::assertIntegerish(size, min.len=0, lower=0, any.missing=FALSE)
  checkmate::assertNumeric(prob, min.len=0, lower=0, upper=1, any.missing=FALSE, finite=TRUE)
  .df <- try(data.frame(x=x, size=size, prob=prob), silent=TRUE)
  if (inherits(.df, "try-error")) {
    stop("incompatible dimensions for x, size, prob", call.=FALSE)
  }
  .ret <- llikNbinomInternal(.df$x, .df$size, .df$prob)
  if (full) .ret <- cbind(.df, .ret)
  .ret
}

#' Calculate the log likelihood of the negative binomial function (and its derivatives)
#' 
#' @param x  Number of successes
#' @param size Size of trial
#' @param mu mu parameter for negative binomial
#' 
#' @inheritParams llikNorm
#'
#' @details
#' In an `rxode2()` model, you can use `llikNbinomMu()` but you have to
#' use all arguments.  You can also get the derivative of `mu` with
#' `llikNbinomMuDmu()`
#' @return data frame with `fx` for the pdf value of with
#'   `dProb` that has the derivatives with respect to the parameters at
#'   the observation time-point
#' @author Matthew L. Fidler
#' @export 
#' @examples
#' 
#' llikNbinomMu(46:54, 100, 40)
#'
#' llikNbinomMu(46:54, 100, 40, TRUE)
#'
#' et <- et(46:54)
#' et$size <- 100
#' et$mu <- 40
#'
#' model <- rxode2({
#'   fx <- llikNbinomMu(time, size, mu)
#'   dProb <- llikNbinomMuDmu(time, size, mu)
#' })
#'
#' rxSolve(model, et)
llikNbinomMu <- function(x, size, mu, full=FALSE) {
  checkmate::assertIntegerish(x, min.len=0, lower=0, any.missing=FALSE)
  checkmate::assertIntegerish(size, min.len=0, lower=0, any.missing=FALSE)
  checkmate::assertNumeric(mu, min.len=0, lower=0, any.missing=FALSE, finite=TRUE)
  .df <- try(data.frame(x=x, size=size, mu=mu), silent=TRUE)
  if (inherits(.df, "try-error")) {
    stop("incompatible dimensions for x, size, mu", call.=FALSE)
  }
  .ret <- llikNbinomMuInternal(.df$x, .df$size, .df$mu)
  if (full) .ret <- cbind(.df, .ret)
  .ret
}

#' Calculate the log likelihood of the binomial function (and its derivatives)
#'
#' @inheritParams stats::dbeta
#' 
#' @inheritParams llikNorm
#'
#' @details
#' In an `rxode2()` model, you can use `llikBeta()` but you have to
#' use all arguments.  You can also get the derivative of `shape1` and `shape2` with
#' `llikBetaDshape1()` and `llikBetaDshape2()`.
#' @return data frame with `fx` for the log pdf value of with
#'   `dShape1` and `dShape2` that has the derivatives with respect to the parameters at
#'   the observation time-point
#' @author Matthew L. Fidler
#' @export 
#' @examples
#'
#' x <- seq(1e-4, 1 - 1e-4, length.out = 21)
#' 
#' llikBeta(x, 0.5, 0.5)
#'
#' llikBeta(x, 1, 3, TRUE)
#' 
#' et <- et(seq(1e-4, 1-1e-4, length.out=21))
#' et$shape1 <- 0.5
#' et$shape2 <- 1.5
#'
#' model <- rxode2({
#'   fx <- llikBeta(time, shape1, shape2)
#'   dShape1 <- llikBetaDshape1(time, shape1, shape2)
#'   dShape2 <- llikBetaDshape2(time, shape1, shape2)
#' })
#'
#' rxSolve(model, et)
#' 
llikBeta <- function(x, shape1, shape2, full=FALSE) {
  checkmate::assertNumeric(x, min.len=0, lower=0, upper=1, any.missing=FALSE, finite=TRUE)
  checkmate::assertNumeric(shape1, min.len=0, lower=0, any.missing=FALSE, finite=TRUE)
  checkmate::assertNumeric(shape2, min.len=0, lower=0, any.missing=FALSE, finite=TRUE)
  .df <- try(data.frame(x=x, shape1=shape1, shape2=shape2), silent=TRUE)
  if (inherits(.df, "try-error")) {
    stop("incompatible dimensions for x, shape1, shape2", call.=FALSE)
  }
  .ret <- llikBetaInternal(.df$x, .df$shape1, .df$shape2)
  if (full) .ret <- cbind(.df, .ret)
  .ret
}

#' log likelihood of T and it's derivatives (from stan) 
#'
#' @param x  Observation
#' @inheritParams llikNorm
#' @inheritParams stats::dnorm
#' @inheritParams stats::dt
#' @return data frame with `fx` for the log pdf value of with `dDf`
#'   `dMean` and `dSd` that has the derivatives with respect to the parameters at
#'   the observation time-point
#' @author Matthew L. Fidler
#' @details
#' In an `rxode2()` model, you can use `llikT()` but you have to
#' use all arguments.  You can also get the derivative of `df`, `mean` and `sd` with
#' `llikTDdf()`, `llikTDmean()` and `llikTDsd()`.
#' @export 
#' @examples
#'
#' x <- seq(-3, 3, length.out = 21)
#'
#' llikT(x, 7, 0, 1)
#'
#' llikT(x, 15, 0, 1, full=TRUE)
#'
#'  et <- et(-3, 3, length.out=10)
#'  et$nu <- 7
#'  et$mean <- 0
#'  et$sd <- 1
#'
#'  model <- rxode2({
#'    fx <- llikT(time, nu, mean, sd)
#'    dDf <- llikTDdf(time, nu, mean, sd)
#'    dMean <- llikTDmean(time, nu, mean, sd)
#'    dSd   <- llikTDsd(time, nu, mean, sd)
#'  })
#'
#'  rxSolve(model, et)
#'
llikT <-function(x, df, mean=0, sd=1, full=FALSE) {
  checkmate::assertNumeric(x, min.len=0, any.missing=FALSE, finite=TRUE)
  checkmate::assertNumeric(df, min.len=0, lower=0, any.missing=FALSE, finite=TRUE)
  checkmate::assertNumeric(mean, min.len=0, any.missing=FALSE, finite=TRUE)
  checkmate::assertNumeric(sd, min.len=0, lower=0, any.missing=FALSE, finite=TRUE)
  .df <- try(data.frame(x=x, df=df, mean=mean, sd=sd), silent=TRUE)
  if (inherits(.df, "try-error")) {
    stop("incompatible dimensions for x, df, mean, sd", call.=FALSE)
  }
  .ret <- llikTInternal(.df$x, .df$df, .df$mean, .df$sd)
  if (full) .ret <- cbind(.df, .ret)
  .ret
}

#' log likelihood and derivatives for chi-squared distribution
#'
#' @param x variable that is distributed by chi-squared distribution
#' @inheritParams llikNorm
#' @inheritParams stats::dchisq
#' @return data frame with `fx` for the log pdf value of with `dDf`
#'   that has the derivatives with respect to the `df` parameter
#'   the observation time-point
#' @author Matthew L. Fidler
#' @export 
#' @details
#' In an `rxode2()` model, you can use `llikChisq()` but you have to
#' use the x and df arguments.  You can also get the derivative of `df` with
#' `llikChisqDdf()`.
#' @examples
#' 
#' llikChisq(1, df = 1:3, full=TRUE)
#' 
#' llikChisq(1, df = 6:9)
#' 
#' et <- et(1:3)
#' et$x <- 1
#'
#' model <- rxode2({
#'    fx <- llikChisq(x, time)
#'    dDf <- llikChisqDdf(x, time)
#' })
#'
#' rxSolve(model, et)
llikChisq <-function(x, df, full=FALSE) {
  checkmate::assertNumeric(x, min.len=0, lower=0, any.missing=FALSE, finite=TRUE)
  checkmate::assertNumeric(df, min.len=0, lower=0, any.missing=FALSE, finite=TRUE)
  .df <- try(data.frame(x=x, df=df), silent=TRUE)
  if (inherits(.df, "try-error")) {
    stop("incompatible dimensions for x, df", call.=FALSE)
  }
  .ret <- llikChisqInternal(.df$x, .df$df)
  if (full) .ret <- cbind(.df, .ret)
  .ret
}

#' log likelihood and derivatives for exponential distribution
#' 
#' @param x variable that is distributed by exponential distribution
#' @inheritParams llikNorm
#' @inheritParams stats::dexp
#' @return data frame with `fx` for the log pdf value of with `dRate`
#'   that has the derivatives with respect to the `rate` parameter
#'   the observation time-point
#' 
#' @author Matthew L. Fidler
#' 
#' @export
#' 
#' @details
#' In an `rxode2()` model, you can use `llikExp()` but you have to
#' use the x and rate arguments.  You can also get the derivative of `rate` with
#' `llikExpDrate()`.
#' 
#' @examples
#'
#' llikExp(1, 1:3)
#'
#' llikExp(1, 1:3, full=TRUE)
#'
#' et <- et(1:3)
#' et$x <- 1
#'
#' model <- rxode2({
#'   fx <- llikExp(x, time)
#'   dRate <- llikExpDrate(x, time)
#' })
#'
#' rxSolve(model, et)
#' 
llikExp <-function(x, rate, full=FALSE) {
  checkmate::assertNumeric(x, min.len=0, lower=0, any.missing=FALSE, finite=TRUE)
  checkmate::assertNumeric(rate, min.len=0, lower=0, any.missing=FALSE, finite=TRUE)
  .rate <- try(data.frame(x=x, rate=rate), silent=TRUE)
  if (inherits(.rate, "try-error")) {
    stop("incompatible dimensions for x, rate", call.=FALSE)
  }
  .ret <- llikExpInternal(.rate$x, .rate$rate)
  if (full) .ret <- cbind(.rate, .ret)
  .ret
}

#' log likelihood and derivatives for F distribution
#'
#' @param x variable that is distributed by f distribution
#' @inheritParams llikNorm
#' @inheritParams stats::df
#' @return data frame with `fx` for the log pdf value of with `dDf1` and `dDf2`
#'   that has the derivatives with respect to the `df1`/`df2` parameters at 
#'   the observation time-point
#' 
#' @author Matthew L. Fidler
#' 
#' @export
#' 
#' @details
#' In an `rxode2()` model, you can use `llikF()` but you have to
#' use the x and rate arguments.  You can also get the derivative of `df1` and `df2` with
#' `llikFDdf1()` and `llikFDdf2()`.
#' 
#' @examples
#'
#' x <- seq(0.001, 5, length.out = 100)
#'
#' llikF(x^2, 1, 5)
#'
#' model <- rxode2({
#'  fx <- llikF(time, df1, df2)
#'  dMean <- llikFDdf1(time, df1, df2)
#'  dSd <- llikFDdf2(time, df1, df2)
#' })
#' 
#' et <- et(x)
#' et$df1 <- 1
#' et$df2 <- 5
#' 
#' rxSolve(model, et)
#' 
llikF <-function(x, df1, df2, full=FALSE) {
  checkmate::assertNumeric(x, min.len=0, lower=0, any.missing=FALSE, finite=TRUE)
  checkmate::assertNumeric(df1, min.len=0, lower=0, any.missing=FALSE, finite=TRUE)
  checkmate::assertNumeric(df2, min.len=0, lower=0, any.missing=FALSE, finite=TRUE)
  .rate <- try(data.frame(x=x, df1=df1, df2=df2), silent=TRUE)
  if (inherits(.rate, "try-error")) {
    stop("incompatible dimensions for x, df1, df2", call.=FALSE)
  }
  .ret <- llikFInternal(.rate$x, .rate$df1, .rate$df2)
  if (full) .ret <- cbind(.rate, .ret)
  .ret
}

#' log likelihood and derivatives for Geom distribution
#'
#' @param x variable distributed by a geom distribution
#' 
#' @inheritParams llikNorm
#' @inheritParams stats::dgeom
#' @return data frame with `fx` for the log pdf value of with `dProb`
#'   that has the derivatives with respect to the `prob` parameters at 
#'   the observation time-point
#' 
#' @author Matthew L. Fidler
#' 
#' @export
#' 
#' @details
#' In an `rxode2()` model, you can use `llikGeom()` but you have to
#' use the x and rate arguments.  You can also get the derivative of `prob` with
#' `llikGeomDprob()`.
#' 
#' @examples
#'
#' llikGeom(1:10, 0.2)
#'
#' et  <- et(1:10)
#' et$prob <- 0.2
#'  
#' model <- rxode2({
#'   fx <- llikGeom(time, prob)
#'   dProb <- llikGeomDprob(time, prob)
#' })
#'
#' rxSolve(model, et)
llikGeom <-function(x, prob, full=FALSE) {
  checkmate::assertIntegerish(x, lower=0, min.len=0, any.missing=FALSE)
  checkmate::assertNumeric(prob, min.len=0, lower=0, upper=1,any.missing=FALSE, finite=TRUE)
  .rate <- try(data.frame(x=x, prob=prob), silent=TRUE)
  if (inherits(.rate, "try-error")) {
    stop("incompatible dimensions for x, prob", call.=FALSE)
  }
  .ret <- llikGeomInternal(.rate$x, .rate$prob)
  if (full) .ret <- cbind(.rate, .ret)
  .ret
}

#' log likelihood and derivatives for Unif distribution
#'
#' @param x variable distributed by a uniform distribution
#' @param alpha is the lower limit of the uniform distribution
#' @param beta is the upper limit of the distribution
#' @inheritParams llikNorm
#' @inheritParams stats::dunif
#' @return data frame with `fx` for the log pdf value of with `dProb`
#'   that has the derivatives with respect to the `prob` parameters at 
#'   the observation time-point
#' 
#' @author Matthew L. Fidler
#' 
#' @export
#' 
#' @details
#' 
#' In an `rxode2()` model, you can use `llikUnif()` but you have to
#' use the x and rate arguments.  You can also get the derivative of `alpha` or `beta` with
#' `llikUnifDalpha()` and `llikUnifDbeta()`.
#' 
#' @examples
#'
#' llikUnif(1, -2, 2)
#'
#' et  <- et(seq(1,1, length.out=4))
#' et$alpha <- -2
#' et$beta <- 2
#'  
#' model <- rxode2({
#'   fx <- llikUnif(time, alpha, beta)
#'   dAlpha<- llikUnifDalpha(time, alpha, beta)
#'   dBeta <- llikUnifDbeta(time, alpha, beta)
#' })
#' 
#'
#' rxSolve(model, et)
#' 
llikUnif <-function(x, alpha, beta, full=FALSE) {
  checkmate::assertNumeric(x, min.len=0, any.missing=FALSE, finite=TRUE)
  checkmate::assertNumeric(alpha, min.len=0, any.missing=FALSE, finite=TRUE)
  checkmate::assertNumeric(beta, min.len=0, any.missing=FALSE, finite=TRUE)
  .rate <- try(data.frame(x=x, alpha=alpha, beta=beta), silent=TRUE)
  if (inherits(.rate, "try-error")) {
    stop("incompatible dimensions for x, alpha, beta", call.=FALSE)
  }
  .ret <- llikUnifInternal(.rate$x, .rate$alpha, .rate$beta)
  if (full) .ret <- cbind(.rate, .ret)
  .ret
}

#' log likelihood and derivatives for Weibull distribution
#' 
#' @param x variable distributed by a Weibull distribution
#' 
#' @inheritParams llikNorm
#' @inheritParams stats::dweibull
#' @return data frame with `fx` for the log pdf value of with `dProb`
#'   that has the derivatives with respect to the `prob` parameters at 
#'   the observation time-point
#' 
#' @author Matthew L. Fidler
#' 
#' @export
#' 
#' @details
#' 
#' In an `rxode2()` model, you can use `llikWeibull()` but you have to
#' use the x and rate arguments.  You can also get the derivative of `shape` or `scale` with
#' `llikWeibullDshape()` and `llikWeibullDscale()`.
#' 
#' @examples
#'
#' llikWeibull(1, 1, 10)
#'
#' et  <- et(seq(0.001, 1, length.out=10))
#' et$shape <- 1
#' et$scale <- 10
#'  
#' model <- rxode2({
#'   fx <- llikWeibull(time, shape, scale)
#'   dShape<- llikWeibullDshape(time, shape, scale)
#'   dScale <- llikWeibullDscale(time, shape, scale)
#' })
#' 
#'
#' rxSolve(model, et)
#' 
llikWeibull <-function(x, shape, scale, full=FALSE) {
  checkmate::assertNumeric(x, min.len=0, lower=0, any.missing=FALSE, finite=TRUE)
  checkmate::assertNumeric(shape, min.len=0, lower=0, any.missing=FALSE, finite=TRUE)
  checkmate::assertNumeric(scale, min.len=0, lower=0,any.missing=FALSE, finite=TRUE)
  .rate <- try(data.frame(x=x, shape=shape, scale=scale), silent=TRUE)
  if (inherits(.rate, "try-error")) {
    stop("incompatible dimensions for x, shape, scale", call.=FALSE)
  }
  .ret <- llikWeibullInternal(.rate$x, .rate$shape, .rate$scale)
  if (full) .ret <- cbind(.rate, .ret)
  .ret
}


#' log likelihood and derivatives for Gamma distribution
#'
#' @param x variable that is distributed by gamma distribution
#' @param shape this is the distribution's shape parameter. Must be positive.
#' @param rate this is the distribution's rate parameters.  Must be positive.
#' 
#' @inheritParams llikNorm
#' 
#' @inheritParams stats::dgamma
#' 
#' @return data frame with `fx` for the log pdf value of with `dProb`
#'   that has the derivatives with respect to the `prob` parameters at 
#'   the observation time-point
#' 
#' @author Matthew L. Fidler
#' 
#' @export
#' 
#' @details
#' 
#' In an `rxode2()` model, you can use `llikGamma()` but you have to
#' use the x and rate arguments.  You can also get the derivative of `shape` or `rate` with
#' `llikGammaDshape()` and `llikGammaDrate()`.
#' 
#' @examples
#'
#' llikGamma(1, 1, 10)
#'
#' et  <- et(seq(0.001, 1, length.out=10))
#' et$shape <- 1
#' et$rate <- 10
#'  
#' model <- rxode2({
#'   fx <- llikGamma(time, shape, rate)
#'   dShape<- llikGammaDshape(time, shape, rate)
#'   dRate <- llikGammaDrate(time, shape, rate)
#' })
#' 
#'
#' rxSolve(model, et)
#' 
llikGamma <-function(x, shape, rate, full=FALSE) {
  checkmate::assertNumeric(x, min.len=0, lower=0, any.missing=FALSE, finite=TRUE)
  checkmate::assertNumeric(shape, min.len=0, lower=0, any.missing=FALSE, finite=TRUE)
  checkmate::assertNumeric(rate, min.len=0, lower=0,any.missing=FALSE, finite=TRUE)
  .rate <- try(data.frame(x=x, shape=shape, rate=rate), silent=TRUE)
  if (inherits(.rate, "try-error")) {
    stop("incompatible dimensions for x, shape, rate", call.=FALSE)
  }
  .ret <- llikGammaInternal(.rate$x, .rate$shape, .rate$rate)
  if (full) .ret <- cbind(.rate, .ret)
  .ret
}


#' log likelihood of Cauchy distribution and it's derivatives (from stan) 
#'
#' @param x  Observation
#' @inheritParams llikNorm
#' @inheritParams stats::dnorm
#' @inheritParams stats::dcauchy
#' @return data frame with `fx` for the log pdf value of with 
#'   `dLocation` and `dScale` that has the derivatives with respect to the parameters at
#'   the observation time-point
#' @author Matthew L. Fidler
#' @details
#' In an `rxode2()` model, you can use `llikCauchy()` but you have to
#' use all arguments.  You can also get the derivative of `location` and `scale` with
#' `llikCauchyDlocation()` and `llikCauchyDscale()`.
#' @export 
#' @examples
#'
#' x <- seq(-3, 3, length.out = 21)
#'
#' llikCauchy(x, 0, 1)
#'
#' llikCauchy(x, 3, 1, full=TRUE)
#'
#'  et <- et(-3, 3, length.out=10)
#'  et$location <- 0
#'  et$scale <- 1
#'
#'  model <- rxode2({
#'    fx <- llikCauchy(time, location, scale)
#'    dLocation <- llikCauchyDlocation(time, location, scale)
#'    dScale <- llikCauchyDscale(time, location, scale)
#'  })
#'
#'  rxSolve(model, et)
#'
llikCauchy <-function(x, location=0, scale=1, full=FALSE) {
  checkmate::assertNumeric(x, min.len=0, any.missing=FALSE, finite=TRUE)
  checkmate::assertNumeric(location, min.len=0, any.missing=FALSE, finite=TRUE)
  checkmate::assertNumeric(scale, min.len=0, lower=0, any.missing=FALSE, finite=TRUE)
  .df <- try(data.frame(x=x, location=location, scale=scale), silent=TRUE)
  if (inherits(.df, "try-error")) {
    stop("incompatible dimensions for x, location, scale", call.=FALSE)
  }
  .ret <- llikCauchyInternal(.df$x, .df$location, .df$scale)
  if (full) .ret <- cbind(.df, .ret)
  .ret
}
