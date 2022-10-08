#' @export
#' @rdname rxnormV
rxnorm <- function(mean = 0, sd = 1, n = 1L, ncores = 1L) {
  checkmate::assertNumeric(mean, len = 1)
  checkmate::assertNumeric(sd, lower = 0, len = 1)
  checkmate::assertCount(n)
  checkmate::assertCount(ncores)
  rxSeedEng(ncores)
  .Call(`_rxode2_rxnorm_`, mean, sd, n, ncores)
}

#' Simulate random normal variable from threefry/vandercorput generator
#'
#' @inheritParams stats::rnorm
#'
#' @param n number of observations
#'
#' @param ncores Number of cores for the simulation
#'
#' `rxnorm` simulates using the threefry sitmo generator; `rxnormV`
#' uses the vandercorput generator
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
#' rx <- rxode2({
#'   a <- rxnorm()
#' })
#'
#' et <- et(1, id = 1:2)
#'
#' s <- rxSolve(rx, et)
#'
#' ## Use vandercorput generator
#'
#' rxnormV(n = 10) # with rxnorm you have to explicitly state n
#' rxnormV(n = 10, ncores = 2) # You can parallelize the simulation using openMP
#'
#' rxnormV(2, 3) ## The first 2 arguments are the mean and standard deviation
#'
#'
#' ## This example uses `rxnormV` directly in the model
#'
#' rx <- rxode2({
#'   a <- rxnormV()
#' })
#'
#' et <- et(1, id = 1:2)
#'
#' s <- rxSolve(rx, et)
#' }
#' @export
rxnormV <- function(mean = 0, sd = 1, n = 1L, ncores = 1L) {
  checkmate::assertNumeric(mean, len = 1)
  checkmate::assertNumeric(sd, lower = 0, len = 1)
  checkmate::assertCount(n)
  checkmate::assertCount(ncores)
  rxSeedEng(ncores)
  .Call(`_rxode2_rxnormV_`, mean, sd, n, ncores)
}

#' Simulate random Poisson variable from threefry generator
#'
#' @inheritParams stats::rpois
#' @inheritParams rxnormV
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
#' rx <- rxode2({
#'   a <- rxpois(3)
#' })
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
#' @inheritParams rxnormV
#'
#' @template birthdayProblem
#' @return t-distribution random numbers
#' @examples
#' \donttest{
#'
#' ## Use threefry engine
#'
#' rxt(df = 3, n = 10) # with rxt you have to explicitly state n
#' rxt(df = 3, n = 10, ncores = 2) # You can parallelize the simulation using openMP
#'
#' rxt(4) ## The first argument is the df parameter
#'
#'
#' ## This example uses `rxt` directly in the model
#'
#' rx <- rxode2({
#'   a <- rxt(3)
#' })
#'
#' et <- et(1, id = 1:2)
#'
#' s <- rxSolve(rx, et)
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
#' @inheritParams rxnormV
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
#' rx <- rxode2({
#'   a <- rxunif(0, 3)
#' })
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
#' @inheritParams rxnormV
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
#' rx <- rxode2({
#'   a <- rxweibull(1, 3)
#' })
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
#' @inheritParams rxnormV
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
#' rx <- rxode2({
#'   a <- rxgeom(0.24)
#' })
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
#' @inheritParams rxnormV
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
#' rx <- rxode2({
#'   a <- rxbeta(2, 2)
#' })
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
#' @inheritParams rxnormV
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
#' rx <- rxode2({
#'   a <- rxgamma(2)
#' })
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
#' @inheritParams rxnormV
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
#' rx <- rxode2({
#'   a <- rxf(2, 2)
#' })
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
#' @inheritParams rxnormV
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
#' rx <- rxode2({
#'   a <- rxexp(2)
#' })
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
#' @inheritParams rxnormV
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
#' rx <- rxode2({
#'   a <- rxchisq(2)
#' })
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
#' @inheritParams rxnormV
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
#' rx <- rxode2({
#'   a <- rxcauchy(2)
#' })
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
#' @inheritParams rxnormV
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
#' rx <- rxode2({
#'   a <- rxbinom(1, 0.5)
#' })
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
#' rx <- rxode2({
#'   a <- rxnbinom(10, 0.5)
#' })
#'
#' et <- et(1, id = 1:100)
#'
#' s <- rxSolve(rx, et)
#'
#' rx <- rxode2({
#'   a <- rxnbinomMu(10, 40)
#' })
#'
#'  s <- rxSolve(rx, et)
#'
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
#'   1 / x
#' })
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

#' Create a random "normal" matrix using vandercorput generator
#'
#' @param nrow Number of rows
#'
#' @param ncol Number of Columns
#'
#' @return Matrix of random numbers
#'
#' @author Matthew Fidler
#' @export
#' @examples
#'
#' rxRandNV(1, 1)
#' rxRandNV(3, 2)
rxRandNV <- function(nrow = 1, ncol = 1) {
  checkmate::assertIntegerish(nrow, len = 1, any.missing = FALSE, lower = 1L)
  checkmate::assertIntegerish(ncol, len = 1, any.missing = FALSE, lower = 1L)
  .Call(`_rxode2_rxrandnV`, as.integer(nrow), as.integer(ncol))
}
