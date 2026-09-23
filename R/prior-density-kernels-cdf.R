## Univariate prior kernels with a closed-form CDF (nlmixr2/rxode2#1387).
## Each C term type (`src/priorDensity.cpp`) evaluates only the part of the
## log density that depends on the parameter; this gives, per type, its
## support, the log of its normalizing constant and its CDF, which
## `.rxPriorUniLogMass()` uses to truncate it to the parameter's bounds.

#' Univariate kernels whose truncation mass comes from a closed-form CDF
#'
#' @noRd
.rxPriorUniKernelsCdf <- list(
  lognormal = list(
    support = function(par) c(0, Inf),
    logConst = function(par) -log(par[2]) - 0.5 * log(2 * pi),
    p = function(q, par, lower.tail) stats::plnorm(q, par[1], par[2], lower.tail = lower.tail)
  ),
  gamma = list(
    support = function(par) c(0, Inf),
    logConst = function(par) par[1] * log(par[2]) - lgamma(par[1]),
    p = function(q, par, lower.tail) stats::pgamma(q, par[1], par[2], lower.tail = lower.tail)
  ),
  invGamma = list(
    support = function(par) c(0, Inf),
    logConst = function(par) par[1] * log(par[2]) - lgamma(par[1]),
    p = function(q, par, lower.tail) stats::pgamma(1 / q, par[1], par[2], lower.tail = !lower.tail)
  ),
  weibull = list(
    support = function(par) c(0, Inf),
    logConst = function(par) log(par[1]) - par[1] * log(par[2]),
    p = function(q, par, lower.tail) stats::pweibull(q, par[1], par[2], lower.tail = lower.tail)
  ),
  frechet = list(
    support = function(par) c(0, Inf),
    logConst = function(par) log(par[1]) + par[1] * log(par[2]),
    p = function(q, par, lower.tail) {
      .e <- ifelse(q <= 0, Inf, (q / par[2])^(-par[1]))
      if (lower.tail) exp(-.e) else -expm1(-.e)
    }
  ),
  pareto = list(
    support = function(par) c(par[1], Inf),
    logConst = function(par) log(par[2]) + par[2] * log(par[1]),
    p = function(q, par, lower.tail) {
      .s <- ifelse(q <= par[1], 1, (par[1] / q)^par[2])
      if (lower.tail) 1 - .s else .s
    }
  ),
  paretoType2 = list(
    support = function(par) c(par[1], Inf),
    logConst = function(par) log(par[3]) - log(par[2]),
    p = function(q, par, lower.tail) {
      .s <- ifelse(q <= par[1], 1, (1 + (q - par[1]) / par[2])^(-par[3]))
      if (lower.tail) 1 - .s else .s
    }
  ),
  beta = list(
    support = function(par) c(0, 1),
    logConst = function(par) -lbeta(par[1], par[2]),
    p = function(q, par, lower.tail) stats::pbeta(q, par[1], par[2], lower.tail = lower.tail)
  ),
  uniform = list(
    support = function(par) c(par[1], par[2]),
    logConst = function(par) -log(par[2] - par[1]),
    p = function(q, par, lower.tail) stats::punif(q, par[1], par[2], lower.tail = lower.tail)
  ),
  studentT = list(
    support = function(par) c(-Inf, Inf),
    logConst = function(par) {
      lgamma((par[1] + 1) / 2) - lgamma(par[1] / 2) - 0.5 * log(par[1] * pi) - log(par[3])
    },
    p = function(q, par, lower.tail) stats::pt((q - par[2]) / par[3], par[1], lower.tail = lower.tail)
  ),
  doubleExponential = list(
    support = function(par) c(-Inf, Inf),
    logConst = function(par) -log(2 * par[2]),
    p = function(q, par, lower.tail) {
      .z <- (q - par[1]) / par[2]
      .l <- ifelse(.z < 0, 0.5 * exp(.z), 1 - 0.5 * exp(-.z))
      .u <- ifelse(.z < 0, 1 - 0.5 * exp(.z), 0.5 * exp(-.z))
      if (lower.tail) .l else .u
    }
  ),
  logistic = list(
    support = function(par) c(-Inf, Inf),
    logConst = function(par) -log(par[2]),
    p = function(q, par, lower.tail) stats::plogis(q, par[1], par[2], lower.tail = lower.tail)
  ),
  gumbel = list(
    support = function(par) c(-Inf, Inf),
    logConst = function(par) -log(par[2]),
    p = function(q, par, lower.tail) {
      .e <- exp(-(q - par[1]) / par[2])
      if (lower.tail) exp(-.e) else -expm1(-.e)
    }
  ),
  skewDoubleExponential = list(
    support = function(par) c(-Inf, Inf),
    logConst = function(par) log(2) + log(par[3]) + log1p(-par[3]) - log(par[2]),
    p = function(q, par, lower.tail) {
      .mu <- par[1]
      .sigma <- par[2]
      .tau <- par[3]
      .l <- ifelse(
        q < .mu,
        .tau * exp(-2 * (1 - .tau) * (.mu - q) / .sigma),
        1 - (1 - .tau) * exp(-2 * .tau * (q - .mu) / .sigma)
      )
      .u <- ifelse(
        q < .mu,
        1 - .tau * exp(-2 * (1 - .tau) * (.mu - q) / .sigma),
        (1 - .tau) * exp(-2 * .tau * (q - .mu) / .sigma)
      )
      if (lower.tail) .l else .u
    }
  )
)
