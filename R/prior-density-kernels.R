## Univariate prior kernels without a closed-form CDF, the full kernel
## table, and the truncation mass every univariate prior term is normalized
## by (nlmixr2/rxode2#1387).

#' Univariate kernels whose truncation mass is integrated numerically
#'
#' Same fields as `.rxPriorUniKernelsCdf`, with `p = NULL` and an R log
#' density `lpdf` for `stats::integrate()`.
#'
#' @noRd
.rxPriorUniKernelsIntegrated <- list(
  expModNormal = list(
    support = function(par) c(-Inf, Inf),
    logConst = function(par) log(par[3]) - log(2),
    p = NULL,
    lpdf = function(x, par) {
      .mu <- par[1]
      .sigma <- par[2]
      .lambda <- par[3]
      log(.lambda) -
        log(2) +
        .lambda * (.mu + 0.5 * .lambda * .sigma^2 - x) +
        stats::pnorm((.mu + .lambda * .sigma^2 - x) / .sigma, lower.tail = FALSE, log.p = TRUE) +
        log(2)
    }
  ),
  skewNormal = list(
    support = function(par) c(-Inf, Inf),
    logConst = function(par) -0.5 * log(2 * pi) - log(par[2]),
    p = NULL,
    lpdf = function(x, par) {
      .z <- (x - par[1]) / par[2]
      log(2) + stats::dnorm(.z, log = TRUE) - log(par[2]) + stats::pnorm(par[3] * .z, log.p = TRUE)
    }
  ),
  vonMises = list(
    support = function(par) c(-Inf, Inf),
    logConst = function(par) -log(2 * pi) - (log(besselI(par[2], 0, expon.scaled = TRUE)) + par[2]),
    p = NULL,
    lpdf = function(x, par) {
      par[2] * cos(x - par[1]) - log(2 * pi) - (log(besselI(par[2], 0, expon.scaled = TRUE)) + par[2])
    }
  )
)

#' C evaluators for the univariate families, one per density shape
#'
#' @noRd
.rxPriorUniKernels <- c(.rxPriorUniKernelsCdf, .rxPriorUniKernelsIntegrated)

#' log(F(upper) - F(lower)) of a univariate kernel, clipped to its support
#'
#' @param kernel element of `.rxPriorUniKernels`
#' @param par the kernel's hyperparameters
#' @param lower,upper the parameter's own `ini({})` bounds
#' @return `0` when the bounds do not cut the support, otherwise the log
#'   probability mass inside them; `-Inf` when they exclude it entirely
#' @noRd
#' @author Matthew L. Fidler
.rxPriorUniLogMass <- function(kernel, par, lower, upper) {
  .sup <- kernel$support(par)
  .lo <- max(lower, .sup[1])
  .hi <- min(upper, .sup[2])
  if (.lo <= .sup[1] && .hi >= .sup[2]) {
    return(0)
  }
  if (.lo >= .hi) {
    return(-Inf)
  }
  if (is.null(kernel$p)) {
    .f <- function(x) exp(kernel$lpdf(x, par))
    return(log(stats::integrate(.f, .lo, .hi, rel.tol = 1e-10)$value))
  }
  .fl <- kernel$p(.lo, par, TRUE)
  .fu <- kernel$p(.hi, par, TRUE)
  if (.fu <= 0.5) {
    return(log(.fu - .fl))
  }
  log(kernel$p(.lo, par, FALSE) - kernel$p(.hi, par, FALSE))
}
