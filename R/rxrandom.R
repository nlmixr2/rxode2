#' Simulate random normal variable from threefry generator
#'
#' @inheritParams stats::rnorm
#'
#' @param n number of observations
#'
#' @param ncores Number of cores for the simulation
#'
#' `rxnorm` simulates using the threefry sitmo generator.
#'
#'
#' `rxnormV` used to simulate with the vandercorput simulator, but
#' since it didn't satisfy the normal properties it was changed to simple be
#' an alias of `rxnorm`. It is no longer supported in `rxode2({})` blocks
#'
#' @return normal random number deviates
#'
#' @examples
#' \donttest{
#' ## Use threefry engine
#'
#' rxnorm(n = 10) # with rxnorm you have to explicitly state n
#' rxnorm(n = 10, ncores = 2) # You can parallelize the simulation using openMP
#'
#' rxnorm(2, 3) ## The first 2 arguments are the mean and standard deviation
#'
#'
#' ## This example uses `rxnorm` directly in the model
#'
#' rx <- function() {
#'   model({
#'     a <- rxnorm()
#'   })
#' }
#'
#' et <- et(1, id = 1:2)
#'
#' s <- rxSolve(rx, et)
#'
#' }
#' @export
rxnormV  <- function(mean = 0, sd = 1, n = 1L, ncores = 1L) {
  checkmate::assertNumeric(mean, len = 1)
  checkmate::assertNumeric(sd, lower = 0, len = 1)
  checkmate::assertCount(n)
  checkmate::assertCount(ncores)
  rxSeedEng(ncores)
  .Call(`_rxode2_rxnorm_`, mean, sd, n, ncores)
}
#' @rdname rxnormV
#' @export
rxnorm <- rxnormV

#' Simulate random Poisson variable from threefry generator
#'
#' @inheritParams stats::rpois
#' @inheritParams rxnorm
#'
#' @template birthdayProblem
#' @return poission random number deviates
#' @examples
#' \donttest{
#' ## Use threefry engine
#'
#' rxpois(lambda = 3, n = 10) # with rxpois you have to explicitly state n
#' rxpois(lambda = 3, n = 10, ncores = 2) # You can parallelize the simulation using openMP
#'
#' rxpois(4) ## The first arguments are the lambda parameter
#'
#'
#' ## This example uses `rxpois` directly in the model
#'
#' rx <- function() {
#'   model({
#'     a <- rxpois(3)
#'   })
#' }
#'
#' et <- et(1, id = 1:2)
#'
#' s <- rxSolve(rx, et)
#' }
#' @export
rxpois <- function(lambda, n = 1L, ncores = 1L) {
  checkmate::assertNumeric(lambda, len = 1)
  checkmate::assertCount(n)
  checkmate::assertCount(ncores)
  rxSeedEng(ncores)
  .Call(`_rxode2_rxpois_`, lambda, n, ncores)
}


#' Simulate student t variable from threefry generator
#'
#' @inheritParams stats::rt
#' @inheritParams rxnorm
#'
#' @template birthdayProblem
#' @return t-distribution random numbers
#' @examples
#' \donttest{
#'
#' ## Use threefry engine
#' rxt(df = 3, n = 10) # with rxt you have to explicitly state n
#' rxt(df = 3, n = 10, ncores = 2) # You can parallelize the simulation using openMP
#'
#' rxt(4) ## The first argument is the df parameter
#'
#'
#' ## This example uses `rxt` directly in the model
#'
#' rx <- function() {
#'    model({
#'     a <- rxt(3)
#'    })
#' }
#'
#' et <- et(1, id = 1:2)
#'
#' s <- rxSolve(rx, et)
#'
#' }
#' @export
rxt <- function(df, n = 1L, ncores = 1L) {
  checkmate::assertNumeric(df, len = 1, lower = 0)
  if (df == 0) stop("'df' must be greater than 0", call. = FALSE)
  checkmate::assertCount(n)
  checkmate::assertCount(ncores)
  rxSeedEng(ncores)
  .Call(`_rxode2_rxt__`, df, n, ncores)
}

#' Simulate uniform variable from threefry generator
#'
#' @inheritParams stats::runif
#' @inheritParams rxnorm
#'
#' @template birthdayProblem
#' @return uniform random numbers
#' @examples
#' \donttest{
#'
#' ## Use threefry engine
#'
#' rxunif(min = 0, max = 4, n = 10) # with rxunif you have to explicitly state n
#' rxunif(min = 0, max = 4, n = 10, ncores = 2) # You can parallelize the simulation using openMP
#'
#' rxunif()
#'
#'
#' ## This example uses `rxunif` directly in the model
#'
#' rx <- function() {
#'   model({
#'     a <- rxunif(0, 3)
#'   })
#' }
#'
#' et <- et(1, id = 1:2)
#'
#' s <- rxSolve(rx, et)
#' }
#'
#' @export
rxunif <- function(min = 0, max = 1, n = 1L, ncores = 1L) {
  checkmate::assertNumeric(min, len = 1)
  checkmate::assertNumeric(max, len = 1)
  checkmate::assertCount(n)
  checkmate::assertCount(ncores)
  rxSeedEng(ncores)
  .Call(`_rxode2_rxunif_`, min, max, n, ncores)
}


#' Simulate Weibull variable from threefry generator
#'
#' @inheritParams stats::rweibull
#' @inheritParams rxnorm
#'
#' @template birthdayProblem
#' @return Weibull random deviates
#' @examples
#' \donttest{
#'
#' ## Use threefry engine
#'
#' # with rxweibull you have to explicitly state n
#' rxweibull(shape = 1, scale = 4, n = 10)
#'
#' # You can parallelize the simulation using openMP
#' rxweibull(shape = 1, scale = 4, n = 10, ncores = 2)
#'
#' rxweibull(3)
#'
#'
#' ## This example uses `rxweibull` directly in the model
#'
#' rx <- function() {
#'   model({
#'     a <- rxweibull(1, 3)
#'   })
#' }
#'
#' et <- et(1, id = 1:2)
#'
#' s <- rxSolve(rx, et)
#' }
#' @export
rxweibull <- function(shape, scale = 1, n = 1L, ncores = 1L) {
  checkmate::assertNumeric(shape, len = 1)
  checkmate::assertNumeric(scale, len = 1)
  checkmate::assertCount(n)
  checkmate::assertCount(ncores)
  rxSeedEng(ncores)
  .Call(`_rxode2_rxweibull_`, shape, scale, n, ncores)
}


#' Simulate geometric variable from threefry generator
#'
#' @inheritParams stats::rgeom
#' @inheritParams rxnorm
#'
#' @template birthdayProblem
#' @return geometric random deviates
#' @examples
#' \donttest{
#'
#' ## Use threefry engine
#'
#' rxgeom(0.5, n = 10) # with rxgeom you have to explicitly state n
#' rxgeom(0.25, n = 10, ncores = 2) # You can parallelize the simulation using openMP
#'
#' rxgeom(0.75)
#'
#'
#' ## This example uses `rxgeom` directly in the model
#'
#' rx <- function() {
#'   model({
#'     a <- rxgeom(0.24)
#'   })
#' }
#'
#' et <- et(1, id = 1:2)
#'
#' s <- rxSolve(rx, et)
#' }
#'
#' @export
rxgeom <- function(prob, n = 1L, ncores = 1L) {
  checkmate::assertNumeric(prob, len = 1, lower = 0, upper = 1)
  checkmate::assertCount(n)
  checkmate::assertCount(ncores)
  rxSeedEng(ncores)
  .Call(`_rxode2_rxgeom_`, prob, n, ncores)
}


#' Simulate beta variable from threefry generator
#'
#' @inheritParams stats::rbeta
#' @inheritParams rxnorm
#'
#' @template birthdayProblem
#'
#' @return beta random deviates
#'
#' @examples
#' \donttest{
#'
#' ## Use threefry engine
#'
#' rxbeta(0.5, 0.5, n = 10) # with rxbeta you have to explicitly state n
#' rxbeta(5, 1, n = 10, ncores = 2) # You can parallelize the simulation using openMP
#'
#' rxbeta(1, 3)
#'
#'
#' ## This example uses `rxbeta` directly in the model
#'
#' rx <- function() {
#'   model({
#'     a <- rxbeta(2, 2)
#'   })
#' }
#'
#' et <- et(1, id = 1:2)
#'
#' s <- rxSolve(rx, et)
#' }
#' @export
rxbeta <- function(shape1, shape2, n = 1L, ncores = 1L) {
  checkmate::assertNumeric(shape1, len = 1, lower = 0)
  if (shape1 == 0) stop("'shape1' cannot be 0", call. = FALSE)
  checkmate::assertNumeric(shape2, len = 1, lower = 0)
  if (shape2 == 0) stop("'shape2' cannot be 0", call. = FALSE)
  checkmate::assertCount(n)
  checkmate::assertCount(ncores)
  rxSeedEng(ncores)
  .Call(`_rxode2_rxbeta_`, shape1, shape2, n, ncores)
}

#' Simulate gamma variable from threefry generator
#'
#' @param shape The shape of the gamma random variable
#' @inheritParams stats::rgamma
#' @inheritParams rxnorm
#'
#' @template birthdayProblem
#'
#' @return gamma random deviates
#'
#' @examples
#'
#' \donttest{
#'
#' ## Use threefry engine
#'
#' rxgamma(0.5, n = 10) # with rxgamma you have to explicitly state n
#' rxgamma(5, n = 10, ncores = 2) # You can parallelize the simulation using openMP
#'
#' rxgamma(1)
#'
#'
#' ## This example uses `rxbeta` directly in the model
#'
#' rx <- function() {
#'   model({
#'     a <- rxgamma(2)
#'   })
#' }
#'
#' et <- et(1, id = 1:2)
#'
#' s <- rxSolve(rx, et)
#' }
#'
#' @export
rxgamma <- function(shape, rate = 1, n = 1L, ncores = 1L) {
  checkmate::assertNumeric(shape, len = 1, lower = 0)
  if (shape == 0) stop("'shape' cannot be 0", call. = FALSE)
  checkmate::assertNumeric(rate, len = 1, lower = 0)
  if (rate == 0) stop("'rate' cannot be 0", call. = FALSE)
  checkmate::assertCount(n)
  checkmate::assertCount(ncores)
  rxSeedEng(ncores)
  .Call(`_rxode2_rxgamma_`, shape, rate, n, ncores)
}

#' Simulate F variable from threefry generator
#'
#' @inheritParams stats::rf
#' @inheritParams rxnorm
#'
#' @template birthdayProblem
#'
#' @return f random deviates
#'
#' @examples
#' \donttest{
#'
#' ## Use threefry engine
#'
#' rxf(0.5, 0.5, n = 10) # with rxf you have to explicitly state n
#' rxf(5, 1, n = 10, ncores = 2) # You can parallelize the simulation using openMP
#'
#' rxf(1, 3)
#'
#'
#' ## This example uses `rxf` directly in the model
#'
#' rx <- function() {
#'   model({
#'     a <- rxf(2, 2)
#'   })
#' }
#'
#' et <- et(1, id = 1:2)
#'
#' s <- rxSolve(rx, et)
#' }
#'
#' @export
rxf <- function(df1, df2, n = 1L, ncores = 1L) {
  checkmate::assertNumeric(df1, len = 1, lower = 0)
  if (df1 == 0) stop("'df1' cannot be 0", call. = FALSE)
  checkmate::assertNumeric(df2, len = 1, lower = 0)
  if (df2 == 0) stop("'df2' cannot be 0", call. = FALSE)
  checkmate::assertCount(n)
  checkmate::assertCount(ncores)
  rxSeedEng(ncores)
  .Call(`_rxode2_rxf_`, df1, df2, n, ncores)
}

#' Simulate exponential variable from threefry generator
#'
#' @inheritParams stats::rexp
#' @inheritParams rxnorm
#'
#' @template birthdayProblem
#'
#' @return exponential random deviates
#'
#' @examples
#' \donttest{
#'
#' ## Use threefry engine
#'
#' rxexp(0.5, n = 10) # with rxexp you have to explicitly state n
#' rxexp(5, n = 10, ncores = 2) # You can parallelize the simulation using openMP
#'
#' rxexp(1)
#'
#'
#' ## This example uses `rxexp` directly in the model
#'
#' rx <- function() {
#'   model({
#'     a <- rxexp(2)
#'   })
#' }
#'
#' et <- et(1, id = 1:2)
#'
#' s <- rxSolve(rx, et)
#' }
#'
#' @export
rxexp <- function(rate, n = 1L, ncores = 1L) {
  checkmate::assertNumeric(rate, len = 1, lower = 0)
  if (rate == 0) stop("'rate' cannot be 0", call. = FALSE)
  checkmate::assertCount(n)
  checkmate::assertCount(ncores)
  rxSeedEng(ncores)
  .Call(`_rxode2_rxexp_`, rate, n, ncores)
}


#' Simulate chi-squared variable from threefry generator
#'
#' @inheritParams stats::rchisq
#' @inheritParams rxnorm
#'
#' @template birthdayProblem
#'
#' @return chi squared random deviates
#'
#' @examples
#' \donttest{
#'
#' ## Use threefry engine
#'
#' rxchisq(0.5, n = 10) # with rxchisq you have to explicitly state n
#' rxchisq(5, n = 10, ncores = 2) # You can parallelize the simulation using openMP
#'
#' rxchisq(1)
#'
#'
#' ## This example uses `rxchisq` directly in the model
#'
#' rx <- function() {
#'   model({
#'     a <- rxchisq(2)
#'   })
#' }
#'
#' et <- et(1, id = 1:2)
#'
#' s <- rxSolve(rx, et)
#' }
#'
#' @export
rxchisq <- function(df, n = 1L, ncores = 1L) {
  checkmate::assertNumeric(df, len = 1, lower = 0)
  if (df == 0) stop("'df' cannot be 0", call. = FALSE)
  checkmate::assertCount(n)
  checkmate::assertCount(ncores)
  rxSeedEng(ncores)
  .Call(`_rxode2_rxchisq_`, df, n, ncores)
}

#' Simulate Cauchy variable from threefry generator
#'
#' @inheritParams stats::rcauchy
#' @inheritParams rxnorm
#'
#' @template birthdayProblem
#'
#' @return Cauchy random deviates
#'
#' @examples
#' \donttest{
#'
#' ## Use threefry engine
#'
#' rxcauchy(0, 1, n = 10) # with rxcauchy you have to explicitly state n
#' rxcauchy(0.5, n = 10, ncores = 2) # You can parallelize the simulation using openMP
#'
#' rxcauchy(3)
#'
#'
#' ## This example uses `rxcauchy` directly in the model
#'
#' rx <- function() {
#'   model({
#'     a <- rxcauchy(2)
#'   })
#' }
#'
#' et <- et(1, id = 1:2)
#'
#' s <- rxSolve(rx, et)
#' }
#'
#' @export
rxcauchy <- function(location = 0, scale = 1, n = 1L, ncores = 1L) {
  checkmate::assertNumeric(location, len = 1)
  checkmate::assertNumeric(scale, len = 1, lower = 0)
  if (scale == 0) stop("'scale' cannot be 0", call. = FALSE)
  checkmate::assertCount(n)
  checkmate::assertCount(ncores)
  rxSeedEng(ncores)
  .Call(`_rxode2_rxcauchy_`, location, scale, n, ncores)
}
#' Simulate ordinal value
#'
#' @param ... the probabilities to be simulated.  These should sum up to a number below one.
#'
#'
#' @return A number from 1 to the (number of probabilities specified + 1)
#'
#' @details
#'
#' The values entered into the 'rxord' simulation will simulate the
#' probability of falling each group. If it falls outside of the
#' specified probabilities, it will simulate the group (number of
#' probabilities specified + 1)
#'
#' @author Matthew L. Fidler
#' @examples
#'
#' # This will give values 1, and 2
#' rxord(0.5)
#' rxord(0.5)
#' rxord(0.5)
#' rxord(0.5)
#'
#' # This will give values 1, 2 and 3
#' rxord(0.3, 0.3)
#' rxord(0.3, 0.3)
#' rxord(0.3, 0.3)
#'
#' @export
rxord <- function(...) {
  .args <- c(...)
  if (any(.args < 0)) return(NA_real_)
  if (any(.args > 1)) return(NA_real_)
  .sum <- sum(.args)
  if (.sum >= 1) return(NA_real_)
  .v <- rxunif()
  .args <- cumsum(.args)
  .Call(`_rxode2_rxordSelect`, .v, .args)
}

#' Simulate Binomial variable from threefry generator
#'
#' @inheritParams stats::rbinom
#' @inheritParams rxnorm
#'
#' @template birthdayProblem
#'
#' @return binomial random deviates
#'
#' @examples
#' \donttest{
#' ## Use threefry engine
#'
#' rxbinom(10, 0.9, n = 10) # with rxbinom you have to explicitly state n
#' rxbinom(3, 0.5, n = 10, ncores = 2) # You can parallelize the simulation using openMP
#'
#' rxbinom(4, 0.7)
#'
#'
#' ## This example uses `rxbinom` directly in the model
#'
#' rx <- function() {
#'   model({
#'     a <- rxbinom(1, 0.5)
#'   })
#' }
#'
#' et <- et(1, id = 1:2)
#'
#' s <- rxSolve(rx, et)
#' }
#'
#' @export
rxbinom <- function(size, prob, n = 1L, ncores = 1L) {
  checkmate::assertNumeric(prob, len = 1, lower = 0, upper = 1)
  checkmate::assertCount(size)
  checkmate::assertCount(n)
  checkmate::assertCount(ncores)
  rxSeedEng(ncores)
  .Call(`_rxode2_rxbinom_`, size, prob, n, ncores)
}

#' Simulate Binomial variable from threefry generator
#'
#' @inheritParams stats::rnbinom
#' @inheritParams rxbinom
#'
#' @template birthdayProblem
#'
#' @return negative binomial random deviates. Note that `rxbinom2`
#'   uses the `mu` parameterization an the `rxbinom` uses the `prob`
#'   parameterization (`mu=size/(prob+size)`)
#'
#' @examples
#' \donttest{
#' ## Use threefry engine
#'
#' rxnbinom(10, 0.9, n = 10) # with rxbinom you have to explicitly state n
#' rxnbinom(3, 0.5, n = 10, ncores = 2) # You can parallelize the simulation using openMP
#'
#' rxnbinom(4, 0.7)
#'
#' # use mu parameter
#' rxnbinomMu(40, 40, n=10)
#'
#' ## This example uses `rxbinom` directly in the model
#'
#' rx <- function() {
#'   model({
#'     a <- rxnbinom(10, 0.5)
#'   })
#' }
#'
#' et <- et(1, id = 1:100)
#'
#' s <- rxSolve(rx, et)
#'
#' rx <- function() {
#'   model({
#'     a <- rxnbinomMu(10, 40)
#'   })
#' }
#'
#' s <- rxSolve(rx, et)
#' }
#' @export
rxnbinom <- function(size, prob, n = 1L, ncores = 1L) {
  checkmate::assertNumeric(prob, len = 1, lower = 0, upper = 1)
  checkmate::assertCount(size)
  checkmate::assertCount(n)
  checkmate::assertCount(ncores)
  rxSeedEng(ncores)
  .Call(`_rxode2_rxnbinom_`, size, prob, n, ncores)
}

##' @rdname rxnbinom
##' @export
rxnbinomMu <- function(size, mu, n = 1L, ncores = 1L) {
  checkmate::assertNumeric(mu, len = 1, lower = 0)
  checkmate::assertCount(size)
  checkmate::assertCount(n)
  checkmate::assertCount(ncores)
  rxSeedEng(ncores)
  .Call(`_rxode2_rxnbinomMu_`, size, mu, n, ncores)
}

#' Simulate a from a Poisson process
#'
#' @param n Number of time points to simulate in the Poisson process
#'
#' @param lambda Rate of Poisson process
#'
#' @param gamma Asymmetry rate of Poisson process.  When gamma=1.0,
#'   this simulates a homogenous Poisson process.  When gamma<1.0,
#'   the Poisson process has more events early, when gamma > 1.0,
#'   the Poisson process has more events late in the process.
#'
#'   When gamma is non-zero, the tmax should not be infinite but indicate
#'   the end of the Poisson process to be simulated.  In most
#'   pharamcometric cases, this will be the end of the study.
#'   Internally this uses a rate of:
#'
#'   l(t) = lambda*gamma*(t/tmax)^(gamma-1)
#'
#'
#' @param prob When specified, this is a probability function with
#'   one argument, time, that gives the probability that a Poisson
#'   time t is accepted as a rejection time.
#'
#' @param t0 the starting time of the Poisson process
#'
#' @param tmax the maximum time of the Poisson process
#'
#' @param randomOrder when `TRUE` randomize the order of the Poisson
#'   events.  By default (`FALSE`) it returns the Poisson process is
#'   in order of how the events occurred.
#'
#' @return
#'
#' This returns a vector of the Poisson process times; If the dropout is >=
#' tmax, then all the rest of the times are = tmax to indicate the
#' dropout is equal to or after tmax.
#'
#' @author Matthew Fidler
#' @export
#' @examples
#'
#' ## Sample homogenous Poisson process of rate 1/10
#' rxPp(10, 1 / 10)
#'
#' ## Sample inhomogenous Poisson rate of 1/10
#'
#' rxPp(10, 1 / 10, gamma = 2, tmax = 100)
#'
#' ## Typically the Poisson process times are in a sequential order,
#' ## using randomOrder gives the Poisson process in random order
#'
#' rxPp(10, 1 / 10, gamma = 2, tmax = 10, randomOrder = TRUE)
#'
#' ## This uses an arbitrary function to sample a non-homogenous Poisson process
#'
#' rxPp(10, 1 / 10, prob = function(x) {
#'   1/(1+abs(x))
#' })
#'
rxPp <- function(n, lambda, gamma = 1.0, prob = NULL, t0 = 0.0, tmax = Inf, randomOrder = FALSE) {
  checkmate::assertNumeric(t0, len = 1, any.missing = FALSE)
  checkmate::assertNumeric(tmax, len = 1, any.missing = FALSE, lower = t0)
  checkmate::assertNumeric(gamma, len = 1, any.missing = FALSE, lower = .Machine$double.eps)
  checkmate::assertNumeric(lambda, len = 1, any.missing = FALSE, lower = .Machine$double.eps)
  checkmate::assertIntegerish(n, len = 1, any.missing = FALSE, lower = 1L)
  checkmate::assertLogical(randomOrder, len = 1, any.missing = FALSE)
  if (gamma != 1.0 && is.infinite(tmax)) {
    stop("when 'gamma' is not 1, 'tmax' cannot be infinite")
  }
  .Call(`_rxode2_rpp_`, n, lambda, gamma, prob, t0, tmax, randomOrder, PACKAGE = "rxode2")
}

#' cbind Ome
#'
#' @param et The theta data frame
#' @param mat The full matrix simulation from omegas
#' @param n number of subject simulated
#' @return data frame with et combined with simulated omega matrix values
#' @author Matthew Fidler
#' @export
.cbindOme <- function(et, mat, n) {
  .Call(`_cbindOme`, et, mat, as.integer(n)) # nolint
}


#' Cumulative distribution of standard normal
#'
#' @param q vector of quantiles
#'
#' @return cumulative distribution of standard normal distribution
#'
#' @author Matthew Fidler
#'
#' @examples
#'
#' # phi is equivalent to pnorm(x)
#' phi(3)
#'
#' # See
#' pnorm(3)
#'
#' # This is provided for NONMEM-like compatibility in rxode2 models
#' @export
phi <- function(q) {
  .Call(`_rxode2_phi`, q)
}

#' Convert numeric vector to repeated data.frame
#'
#' @param vec Named input vector
#' @param n Number of columns
#' @return Data frame with repeated vec
#' @author Matthew Fidler
#' @export
.vecDf <- function(vec, n) {
  .Call(`_vecDF`, vec, as.integer(n)) # nolint
}



#' Expand parameters
#'
#'
#' @param object rxode2 model variables object
#' @param params parameters to expand
#' @param events event table to help with the expansion
#' @param control control structure to help with the parameter generation
#' @return Expanded parameters for simulation
#' @export
#' @author Matthew L. Fidler
#' @keywords internal
.expandPars <- function(object, params, events, control) {
  .Call(`_rxode2_expandPars_`, object, params, events, control)
}


#' Sample a covariance Matrix from the Posterior Inverse Wishart
#' distribution.
#'
#' Note this Inverse wishart rescaled to match the original scale of
#' the covariance matrix.
#'
#' If your covariance matrix is a 1x1 matrix, this uses an scaled
#' inverse chi-squared which is equivalent to the Inverse Wishart
#' distribution in the uni-directional case.
#'
#' @param nu Degrees of Freedom (Number of Observations) for
#'        covariance matrix simulation.
#'
#' @param omega Either the estimate of covariance matrix or the
#'     estimated standard deviations in matrix form each row forming
#'     the standard deviation simulated values
#'
#' @param n Number of Matrices to sample.  By default this is 1.
#'     This is only useful when `omega` is a matrix.  Otherwise
#'     it is determined by the number of rows in the input
#'     `omega` matrix of standard deviations
#'
#' @param omegaIsChol is an indicator of if the omega matrix is in
#'   the Cholesky decomposition. This is only used when \code{type="invWishart"}
#'
#' @param returnChol Return the Cholesky decomposition of the
#'   covariance matrix sample. This is only used when \code{type="invWishart"}
#'
#' @param diagXformType Diagonal transformation type.  These could be:
#'
#' * `log` The standard deviations are log transformed, so the
#'   actual standard deviations are exp(omega)
#'
#' * `identity` The standard deviations are not transformed. The
#' standard deviations are not transformed;  They should be positive.
#'
#' * `variance` The variances are specified in the `omega`
#' matrix; They are transformed into standard deviations.
#'
#' * `nlmixrSqrt` These standard deviations come from an nlmixr
#' omega matrix where diag(chol(inv(omega))) = x^2
#'
#' * `nlmixrLog` These standard deviations come from a nlmixr
#' omega matrix omega matrix where diag(chol(solve(omega))) = exp(x)
#'
#' * `nlmixrIdentity` These standard deviations come from a nlmixr
#' omega matrix omega matrix where diag(chol(solve(omega))) = x
#'
#'
#'  The nlmixr transformations only make sense when there is no
#'  off-diagonal correlations modeled.
#'
#' @param type The type of covariance posterior that is being
#'     simulated.  This can be:
#'
#'
#' * `invWishart` The posterior is an inverse wishart; This allows
#' for correlations between parameters to be modeled.  All the
#' uncertainty in the parameter is captured in the degrees of freedom
#' parameter.
#'
#' * `lkj` The posterior separates the standard deviation
#' estimates (modeled outside and provided in the `omega`
#' argument) and the correlation estimates. The correlation estimate
#' is simulated with the [rLKJ1()].  This simulation uses
#' the relationship `eta=(nu-1)/2`.  This is relationship based
#' on the proof of the relationship between the restricted
#' LKJ-distribution and inverse wishart distribution (XXXXXX).  Once
#' the correlation posterior is calculated, the estimated standard
#' deviations are then combined with the simulated correlation matrix
#' to create the covariance matrix.
#'
#' * `separation` Like the `lkj` option, this separates out
#' the estimation of the correlation and standard deviation.  Instead
#' of using the `LKJ` distribution to simulate the correlation,
#' it simulates the inverse wishart of the identity matrix and
#' converts the result to a correlation matrix.  This correlation
#' matrix is then used with the standard deviation to calculate the
#' simulated covariance matrix.
#'
#'
#' @return a matrix (n=1) or a list of matrices  (n > 1)
#'
#' @details
#'
#' In general, the separation strategy is preferred for diagonal
#' matrices.  If the dimension of the matrix is below 10, `lkj`
#' is numerically faster than `separation` method.  However, the
#' `lkj` method has densities too close to zero (XXXX) when the
#' dimension is above 10.  In that case, though computationally more
#' expensive `separation` method performs better.
#'
#' For matrices with modeled covariances, the easiest method to use
#' is the inverse Wishart which allows the simulation of correlation
#' matrices (XXXX).  This method is more well suited for well behaved
#' matrices, that is the variance components are not too low or too
#' high.  When modeling nonlinear mixed effects modeling matrices
#' with too high or low variances are considered sub-optimal in
#' describing a system.  With these rules in mind, it is reasonable
#' to use the inverse Wishart.
#'
#' @author Matthew L.Fidler & Wenping Wang
#'
#' @references
#'
#' Alvarez I, Niemi J and Simpson M. (2014) *Bayesian Inference for a
#' Covariance Matrix*. Conference on Applied Statistics in Agriculture.
#'
#' Wang1 Z, Wu Y, and Chu H. (2018) *On Equivalence of the LKJ
#' distribution and the restricted Wishart
#' distribution*. <doi:10.48550/arXiv.1809.047463
#'
#' @examples
#'
#' ## Sample a single covariance.
#' draw1 <- cvPost(3, matrix(c(1, .3, .3, 1), 2, 2))
#'
#' ## Sample 3 covariances
#' set.seed(42)
#' draw3 <- cvPost(3, matrix(c(1, .3, .3, 1), 2, 2), n = 3)
#'
#' ## Sample 3 covariances, but return the cholesky decomposition
#' set.seed(42)
#' draw3c <- cvPost(3, matrix(c(1, .3, .3, 1), 2, 2), n = 3, returnChol = TRUE)
#'
#' ## Sample 3 covariances with lognormal standard deviations via LKJ
#' ## correlation sample
#' cvPost(3, sapply(1:3, function(...) {
#'   rnorm(10)
#' }), type = "lkj")
#'
#' ## or return cholesky decomposition
#' cvPost(3, sapply(1:3, function(...) {
#'   rnorm(10)
#' }),
#' type = "lkj",
#' returnChol = TRUE
#' )
#'
#' ## Sample 3 covariances with lognormal standard deviations via separation
#' ## strategy using inverse Wishart correlation sample
#' cvPost(3, sapply(1:3, function(...) {
#'   rnorm(10)
#' }), type = "separation")
#'
#' ## or returning the cholesky decomposition
#' cvPost(3, sapply(1:3, function(...) {
#'   rnorm(10)
#' }),
#' type = "separation",
#' returnChol = TRUE
#' )
#' @export
cvPost <- function(nu, omega, n = 1L, omegaIsChol = FALSE, returnChol = FALSE,
                   type = c("invWishart", "lkj", "separation"),
                   diagXformType = c("log", "identity", "variance", "nlmixrSqrt", "nlmixrLog", "nlmixrIdentity")) {
  if (is.null(nu) && n == 1L) {
    return(omega)
  }
  if (inherits(type, "numeric") || inherits(type, "integer")) {
    .type <- as.integer(type)
  } else {
    .type <- as.vector(c(
      "invWishart" = 1L, "lkj" = 2L,
      "separation" = 3L
    )[match.arg(type)])
  }
  if (.type == 1L) {
    .xform <- 1L
  } else if (inherits(diagXformType, "numeric") || inherits(diagXformType, "integer")) {
    .xform <- as.integer(diagXformType)
  } else {
    .xform <- setNames(
      c(
        "variance" = 6L, "log" = 5L,
        "identity" = 4L, "nlmixrSqrt" = 1L,
        "nlmixrLog" = 2L,
        "nlmixrIdentity" = 3L
      )[match.arg(diagXformType)],
      NULL
    )
  }
  .ret <- .Call(`_rxode2_cvPost_`, nu, omega, n,
                omegaIsChol, returnChol, .type, .xform)
  return(.ret)
}

#' Set the parallel seed for rxode2 random number generation
#'
#' This sets the seed for the rxode2 parallel random number generation.
#' If set, then whenever a seed is set for the threefry or
#' vandercorput simulation engine, it will use this seed, increment
#' for the number of seeds and continue with the sequence the next
#' time the random number generator is called.
#'
#' In contrast, when this is not called, the time that the
#' vandercorput or threefry simulation engines are seeded it comes
#' from a uniform random number generated from the standard R random
#' seed.  This may cause a duplicate seed based on the R seed state.
#' This means that there could be correlations between simulations
#' that do not exist This will avoid the birthday problem picking
#' exactly the same seed using the seed state of the R random number
#' generator.  The more times the seed is called, the more likely this
#' becomes.
#'
#' @param seed An integer that represents the rxode2 parallel and
#'   internal random number generator seed.  When positive, use this
#'   seed for random number generation and increment and reseed any
#'   parallel or new engines that are being called. When negative,
#'   turn off the rxode2 seed and generate a seed from the R's uniform
#'   random number generator.  Best practice is to set this seed.
#'
#' @return Nothing, called for its side effects
#'
#' @author Matthew Fidler
#'
#' @examples
#'
#' rxSetSeed(42)
#'
#' # seed with generator 42
#' rxnorm()
#'
#' # Use R's random number generator
#' rnorm(1)
#'
#' rxSetSeed(42)
#'
#' # reproduces the same number
#' rxnorm()
#'
#' # But R's random number is not the same
#'
#' rnorm(1)
#'
#' # If we reset this to use the R's seed
#' # (internally rxode2 uses a uniform random number to span seeds)
#' # This can lead to duplicate sequences and seeds
#'
#' rxSetSeed(-1)
#'
#' # Now set seed works for both.
#'
#' # This is not recommended, but illustrates the different types of
#' # seeds that can be generated.
#'
#' set.seed(42)
#'
#' rxnorm()
#'
#' rnorm(1)
#'
#' set.seed(42)
#'
#' rxnorm()
#'
#' rnorm(1)
#'
#' @seealso rxGetSeed, rxWithSeed, rxWithPreserveSeed
#'
#' @references
#'
#' JD Cook. (2016). Random number generator seed mistakes.
#' \url{https://www.johndcook.com/blog/2016/01/29/random-number-generator-seed-mistakes/}
#'
#' @export
rxSetSeed <- function(seed) {
  .Call(`_rxSetSeed`, seed)
  invisible()
}

.rmRseed <- function() {
  if (!exists(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)) {
    return(NULL)
  }
  set.seed(seed = NULL)
  rm(".Random.seed", envir = globalenv())
}

.rxGetSeed <- function() {
  if (!exists(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)) {
    return(list(seed=NULL, kind=NULL, rxseed=rxGetSeed()))
  }
  list(seed = get(".Random.seed", globalenv(), mode = "integer",
                  inherits = FALSE), kind = RNGkind(), rxseed=rxGetSeed())
}

.rxSetSeed <- function(seed) {
  if (is.null(seed$seed)) {
    .rxGetSeed()
  } else {
    do.call(RNGkind, args = as.list(seed$kind))
    set.seed(seed$seed)
  }
  rxSetSeed(seed$rxseed)
}
#' Preserved seed and possibly set the seed
#'
#' @param seed R seed to use for the session
#'
#' @param code Is the code to evaluate
#'
#' @param rxseed is the rxode2 seed that is being preserved
#'
#' @return returns whatever the code is returning
#'
#' @inheritParams base::RNGkind
#'
#' @seealso rxGetSeed, rxSetSeed
#'
#' @examples
#'
#' rxGetSeed()
#' rxWithSeed(1, {
#'    print(rxGetSeed())
#'    rxnorm()
#'    print(rxGetSeed())
#'    rxnorm()
#' }, rxseed=3)
#'
#' @export
rxWithSeed <- function(seed, code, rxseed=rxGetSeed(), kind = "default", normal.kind = "default",
                       sample.kind = "default") {
  force(seed)
  force(rxseed)
  force(kind)
  force(normal.kind)
  force(sample.kind)
  .rxSeed <- .rxGetSeed()
  .origSeed <- .rxGetSeed()
  .newSeed <- .origSeed
  .newSeed$seed <- seed
  .newSeed$kind <- c(kind, normal.kind, sample.kind)
  .newSeed$rxseed <- rxseed
  on.exit(.rxSetSeed(.origSeed), add = TRUE)
  .rxSetSeed(.newSeed)
  force(code)
}

#' @rdname rxWithSeed
#' @export
rxWithPreserveSeed <- function(code) {
  .origSeed <- .rxGetSeed()
  on.exit(.rxSetSeed(.origSeed), add = TRUE)
  force(code)
}


#' Simulate from a (truncated) multivariate normal
#'
#' This is simulated with the fast, thread-safe threefry simulator
#' and can use multiple cores to generate the random deviates.
#'
#' @param n Number of random row vectors to be simulated OR the
#'     matrix to use for simulation (faster).
#'
#' @param mu mean vector
#'
#' @param sigma Covariance matrix for multivariate normal or a list
#'   of covariance matrices. If a list of covariance matrix, each
#'   matrix will simulate `n` matrices and combine them to a full
#'   matrix
#'
#' @param lower is a vector of the lower bound for the truncated
#'     multivariate norm
#'
#' @param upper is a vector of the upper bound for the truncated
#'     multivariate norm
#'
#' @param ncores Number of cores used in the simulation
#'
#' @param isChol A boolean indicating if `sigma` is a cholesky
#'     decomposition of the covariance matrix.
#'
#' @param keepNames Keep the names from either the mean or covariance
#'     matrix.
#'
#' @param a threshold for switching between methods; They can be
#'   tuned for maximum speed;  There are three cases that are considered:
#'
#'  case 1: a < l < u
#'
#'  case 2: l < u < -a
#'
#'  case 3: otherwise
#'
#' where l=lower and u = upper
#'
#' @param tol When case 3 is used from the above possibilities, the
#'   tol value controls the acceptance rejection and
#'   inverse-transformation;
#'
#' When abs(u-l)>tol, uses accept-reject from randn
#'
#' @param nlTol Tolerance for newton line-search
#'
#' @param nlMaxiter Maximum iterations for newton line-search
#'
#' @return
#'
#' If `n==integer` (default) the output is an (n x d) matrix
#' where the i-th row is the i-th simulated vector.
#'
#' If `is.matrix(n)` then the random vector are store in `n`,
#' which is provided by the user, and the function returns
#' `NULL` invisibly.
#'
#' @references John K. Salmon, Mark A. Moraes, Ron O. Dror, and David
#'     E. Shaw (2011). Parallel Random Numbers: As Easy as 1, 2, 3.
#'     D. E. Shaw Research, New York, NY 10036, USA.
#'
#' @examples
#'
#' ## From mvnfast
#' ## Unlike mvnfast, uses threefry simulation
#'
#' d <- 5
#' mu <- 1:d
#'
#' # Creating covariance matrix
#' tmp <- matrix(rnorm(d^2), d, d)
#' mcov <- tcrossprod(tmp, tmp)
#'
#'
#' set.seed(414)
#' rxRmvn(4, 1:d, mcov)
#'
#' set.seed(414)
#' rxRmvn(4, 1:d, mcov)
#'
#' set.seed(414)
#' rxRmvn(4, 1:d, mcov, ncores = 2) # r.v. generated on the second core are different
#'
#' ###### Here we create the matrix that will hold the simulated
#' #  random variables upfront.
#' A <- matrix(NA, 4, d)
#' class(A) <- "numeric" # This is important. We need the elements of A to be of class "numeric".
#'
#' set.seed(414)
#' rxRmvn(A, 1:d, mcov, ncores = 2) # This returns NULL ...
#' A # ... but the result is here
#'
#' ## You can also simulate from a truncated normal:
#'
#' rxRmvn(10, 1:d, mcov, lower = 1:d - 1, upper = 1:d + 1)
#'
#'
#' # You can also simulate from different matrices (if they match
#' # dimensions) by using a list of matrices.
#'
#' matL <- lapply(1:4, function(...) {
#'   tmp <- matrix(rnorm(d^2), d, d)
#'   tcrossprod(tmp, tmp)
#' })
#'
#'
#' rxRmvn(4, setNames(1:d, paste0("a", 1:d)), matL)
#' @author Matthew Fidler, Zdravko Botev and some from Matteo Fasiolo
#'
#' @references The thread safe multivariate normal was inspired from the `mvnfast` package by Matteo Fasiolo <https://CRAN.R-project.org/package=mvnfast>
#'
#' @references The concept of the truncated multivariate normal was
#'   taken from Zdravko Botev Botev (2017) \doi{10.1111/rssb.12162}
#'   and Botev and L'Ecuyer (2015) \doi{10.1109/WSC.2015.7408180} and
#'   converted to thread safe simulation;
#'
#' @export
rxRmvn <- function(n, mu = NULL, sigma, lower = -Inf, upper = Inf, ncores = 1, isChol = FALSE,
                   keepNames = TRUE, a = 0.4, tol = 2.05, nlTol = 1e-10, nlMaxiter = 100L) {
  .ret <- .Call(
    `_rxode2_rxRmvnSEXP`, n, mu, sigma, lower, upper, ncores,
    isChol, keepNames, a, tol, nlTol, nlMaxiter
  )
  if (is.matrix(n)) {
    return(invisible())
  }
  return(.ret)
}
