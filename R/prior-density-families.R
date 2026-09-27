## lotri's univariate continuous prior families (nlmixr2/rxode2#1387):
## which C kernel (`prior-density-kernels*.R`, `src/priorDensity.cpp`)
## evaluates each, its hyperparameters in that kernel's order, and the
## prior term `R/priorDensity.R` puts in the spec.  Everything constant is
## computed here, once, with R's own special functions, so the thread-safe
## C kernel only evaluates the terms that depend on the parameter.

#' 'lotri' univariate distributions and the C kernel that evaluates each
#'
#' `check` lists the hyperparameter conditions as `list(ok, why)` pairs;
#' `to` maps the arguments as written (in 'lotri' `parNames` order) to the
#' kernel's own hyperparameters, so eg `chi_square(nu)` is evaluated as
#' `gamma(nu/2, 1/2)`.
#'
#' @noRd
.rxPriorUniFamilies <- list(
  lognormal = list(
    nPar = 2L,
    check = function(a) list(list(a[2] > 0, "'sdlog' must be positive")),
    to = function(a) list(kernel = "lognormal", par = a)
  ),
  chi_square = list(
    nPar = 1L,
    check = function(a) list(list(a[1] > 0, "'df' must be positive")),
    to = function(a) list(kernel = "gamma", par = c(a[1] / 2, 0.5))
  ),
  inv_chi_square = list(
    nPar = 1L,
    check = function(a) list(list(a[1] > 0, "'nu' must be positive")),
    to = function(a) list(kernel = "invGamma", par = c(a[1] / 2, 0.5))
  ),
  scaled_inv_chi_square = list(
    nPar = 2L,
    check = function(a) list(list(a[1] > 0, "'nu' must be positive"), list(a[2] > 0, "'sigma' must be positive")),
    to = function(a) list(kernel = "invGamma", par = c(a[1] / 2, a[1] * a[2]^2 / 2))
  ),
  exponential = list(
    nPar = 1L,
    check = function(a) list(list(a[1] > 0, "'rate' must be positive")),
    to = function(a) list(kernel = "gamma", par = c(1, a[1]))
  ),
  gamma = list(
    nPar = 2L,
    check = function(a) list(list(a[1] > 0, "'shape' must be positive"), list(a[2] > 0, "'rate' must be positive")),
    to = function(a) list(kernel = "gamma", par = a)
  ),
  inv_gamma = list(
    nPar = 2L,
    check = function(a) list(list(a[1] > 0, "'alpha' must be positive"), list(a[2] > 0, "'beta' must be positive")),
    to = function(a) list(kernel = "invGamma", par = a)
  ),
  weibull = list(
    nPar = 2L,
    check = function(a) list(list(a[1] > 0, "'shape' must be positive"), list(a[2] > 0, "'scale' must be positive")),
    to = function(a) list(kernel = "weibull", par = a)
  ),
  frechet = list(
    nPar = 2L,
    check = function(a) list(list(a[1] > 0, "'alpha' must be positive"), list(a[2] > 0, "'sigma' must be positive")),
    to = function(a) list(kernel = "frechet", par = a)
  ),
  rayleigh = list(
    nPar = 1L,
    check = function(a) list(list(a[1] > 0, "'sigma' must be positive")),
    to = function(a) list(kernel = "weibull", par = c(2, sqrt(2) * a[1]))
  ),
  pareto = list(
    nPar = 2L,
    check = function(a) list(list(a[1] > 0, "'y_min' must be positive"), list(a[2] > 0, "'alpha' must be positive")),
    to = function(a) list(kernel = "pareto", par = a)
  ),
  pareto_type_2 = list(
    nPar = 3L,
    check = function(a) {
      list(list(a[2] > 0, "'lambda' must be positive"), list(a[3] > 0, "'alpha' must be positive"))
    },
    to = function(a) list(kernel = "paretoType2", par = a)
  ),
  beta = list(
    nPar = 2L,
    check = function(a) list(list(a[1] > 0, "'shape1' must be positive"), list(a[2] > 0, "'shape2' must be positive")),
    to = function(a) list(kernel = "beta", par = a)
  ),
  beta_proportion = list(
    nPar = 2L,
    check = function(a) {
      list(list(a[1] > 0 && a[1] < 1, "'mu' must be in (0, 1)"), list(a[2] > 0, "'kappa' must be positive"))
    },
    to = function(a) list(kernel = "beta", par = c(a[1] * a[2], (1 - a[1]) * a[2]))
  ),
  uniform = list(
    nPar = 2L,
    check = function(a) list(list(a[1] < a[2], "'min' must be less than 'max'")),
    to = function(a) list(kernel = "uniform", par = a)
  ),
  student_t = list(
    nPar = 3L,
    check = function(a) list(list(a[1] > 0, "'nu' must be positive"), list(a[3] > 0, "'sigma' must be positive")),
    to = function(a) list(kernel = "studentT", par = a)
  ),
  double_exponential = list(
    nPar = 2L,
    check = function(a) list(list(a[2] > 0, "'sigma' must be positive")),
    to = function(a) list(kernel = "doubleExponential", par = a)
  ),
  logistic = list(
    nPar = 2L,
    check = function(a) list(list(a[2] > 0, "'scale' must be positive")),
    to = function(a) list(kernel = "logistic", par = a)
  ),
  gumbel = list(
    nPar = 2L,
    check = function(a) list(list(a[2] > 0, "'beta' must be positive")),
    to = function(a) list(kernel = "gumbel", par = a)
  ),
  skew_double_exponential = list(
    nPar = 3L,
    check = function(a) {
      list(list(a[2] > 0, "'sigma' must be positive"), list(a[3] > 0 && a[3] < 1, "'tau' must be in (0, 1)"))
    },
    to = function(a) list(kernel = "skewDoubleExponential", par = a)
  ),
  exp_mod_normal = list(
    nPar = 3L,
    check = function(a) list(list(a[2] > 0, "'sigma' must be positive"), list(a[3] > 0, "'lambda' must be positive")),
    to = function(a) list(kernel = "expModNormal", par = a)
  ),
  skew_normal = list(
    nPar = 3L,
    check = function(a) list(list(a[2] > 0, "'omega' must be positive")),
    to = function(a) list(kernel = "skewNormal", par = a)
  ),
  von_mises = list(
    nPar = 2L,
    check = function(a) list(list(a[2] > 0, "'kappa' must be positive")),
    to = function(a) list(kernel = "vonMises", par = a)
  )
)

#' Build the prior term for a 'lotri' univariate family
#'
#' @param key the term's key (a theta name, or `om.<eta>`)
#' @param prior prior text, for error messages
#' @param p the parsed prior (`.rxPriorParse()`)
#' @param lower,upper the parameter's own `ini({})` bounds
#' @return a term list for `.rxPriorFlattenSpec()`
#' @noRd
#' @author Matthew L. Fidler
.rxPriorUniTerm <- function(key, prior, p, lower, upper) {
  .fam <- .rxPriorUniFamilies[[p$stanName]]
  .a <- try(
    vapply(p$args, function(x) as.double(eval(x, envir = .rxPriorEvalEnv())), double(1)),
    silent = TRUE
  )
  if (inherits(.a, "try-error") || length(.a) != .fam$nPar || any(!is.finite(.a))) {
    .rxPriorDensityStop(key, prior, "the parameters could not be read back")
  }
  for (.c in .fam$check(.a)) {
    if (!isTRUE(.c[[1]])) {
      .rxPriorDensityStop(key, prior, .c[[2]])
    }
  }
  .to <- .fam$to(.a)
  .kernel <- .rxPriorUniKernels[[.to$kernel]]
  if (identical(.to$kernel, "vonMises") && xor(is.finite(lower), is.finite(upper))) {
    .rxPriorDensityStop(key, prior, "a von Mises prior needs both bounds finite, or neither")
  }
  .logMass <- .rxPriorUniLogMass(.kernel, .to$par, lower, upper)
  if (!is.finite(.logMass)) {
    .rxPriorDensityStop(
      key,
      prior,
      paste0("the bounds (", lower, ", ", upper, ") leave no probability mass under this prior")
    )
  }
  list(
    type = .to$kernel,
    names = key,
    par = as.double(.to$par),
    logConst = .kernel$logConst(.to$par) - .logMass,
    lower = lower,
    upper = upper
  )
}
