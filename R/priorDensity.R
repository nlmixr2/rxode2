## Shared prior log-density kernel (nlmixr2/nlmixr2est#929)
##
## `prior-sim.R` turns the `ini({})` priors into a `thetaMat`/`omegaNu` for
## *simulating* study-level variability (NONMEM NWPRI): the prior mean must
## equal the model's own point estimate, because the draw is a deviation
## added to it.
##
## This is the other half: evaluating a prior as a Bayesian penalty during
## *estimation*, where the parameter moves away from its initial estimate
## and the prior mean is whatever the user wrote (`dnorm(0, 10)` really
## does mean centered at 0). It reuses the parsing helpers from
## `prior-sim.R` (`.rxPriorParse`/`.rxPriorCovNames`/`.rxPriorCovMat`/
## `.rxPriorEvalEnv`/`.rxPriorStop`) but never asserts the mean against the
## estimate.
##
## Four distribution families are implemented, matching what
## `assertRxUiNormalPriors()`/`assertRxUiNoOmegaDf()` (`R/assert.R`) already
## gate on population parameters and omega degrees of freedom, plus
## `cauchy` for the half-Cauchy case the truncation logic has to get right
## (`prior(add.sd) ~ dcauchy(0, 5)` with `add.sd`'s own `lower = 0`):
## `normal`/`std_normal`, `cauchy`, `multi_normal` (the joint theta+omega
## block) and `inv_wishart` (the omega-block degrees-of-freedom prior).
## Anything else is a clear error naming the parameter and the
## distribution, never a silently wrong number.

#' Distributions `rxPriorLogDensity()` can evaluate
#'
#' @noRd
.rxPriorDensityStanNames <- c("normal", "std_normal", "cauchy",
                              "multi_normal", "inv_wishart")

#' log density and derivative of a (possibly truncated) univariate normal
#'
#' @param x value
#' @param mean,sd normal parameters
#' @param lower,upper truncation bounds (the parameter's own, from `iniDf`)
#' @return list with `value` and `grad` (d/dx)
#' @noRd
#' @author Matthew L. Fidler
.rxPriorLLNormal <- function(x, mean, sd, lower=-Inf, upper=Inf) {
  z <- (x - mean) / sd
  .val <- -log(sd) - 0.5 * log(2 * pi) - 0.5 * z * z
  if (is.finite(lower) || is.finite(upper)) {
    .val <- .val - log(stats::pnorm(upper, mean, sd) - stats::pnorm(lower, mean, sd))
  }
  list(value=.val, grad=-(x - mean) / (sd * sd))
}

#' log density and derivative of a (possibly truncated) Cauchy
#'
#' @inheritParams .rxPriorLLNormal
#' @param location,scale Cauchy parameters
#' @return list with `value` and `grad` (d/dx)
#' @noRd
#' @author Matthew L. Fidler
.rxPriorLLCauchy <- function(x, location, scale, lower=-Inf, upper=Inf) {
  z <- (x - location) / scale
  .val <- -log(pi) - log(scale) - log1p(z * z)
  if (is.finite(lower) || is.finite(upper)) {
    .val <- .val - log(stats::pcauchy(upper, location, scale) -
                          stats::pcauchy(lower, location, scale))
  }
  list(value=.val, grad=-2 * (x - location) / (scale * scale * (1 + z * z)))
}

#' log density and gradient of a multivariate normal
#'
#' @param x,mu numeric vectors
#' @param Sigma covariance matrix
#' @return list with `value` and `grad` (d/dx, same length as `x`)
#' @noRd
#' @author Matthew L. Fidler
.rxPriorLLMvn <- function(x, mu, Sigma) {
  .d <- x - mu
  .Si <- solve(Sigma)
  .k <- length(x)
  .logdet <- as.numeric(determinant(Sigma, logarithm=TRUE)$modulus)
  .val <- -0.5 * .k * log(2 * pi) - 0.5 * .logdet - 0.5 * as.numeric(t(.d) %*% .Si %*% .d)
  list(value=.val, grad=-as.numeric(.Si %*% .d))
}

#' log of the multivariate gamma function, `log Gamma_p(a)`
#'
#' @noRd
.rxPriorLmvgamma <- function(a, p) {
  p * (p - 1) / 4 * log(pi) + sum(lgamma(a + (1 - seq_len(p)) / 2))
}

#' log density and gradient of an inverse Wishart
#'
#' There is no inverse-Wishart log-density anywhere in the ecosystem --
#' `rxode2::cvPost()` (`R/rxrandom.R`) only samples from one. `nu`/`Psi`
#' are the prior's own (fixed, as written in `ini({})`) degrees of freedom
#' and scale matrix; `Omega` is the current value being evaluated (the
#' block's estimate during optimization, not the prior's own scale).
#'
#' @param Omega current omega block (symmetric, positive definite)
#' @param nu degrees of freedom
#' @param Psi scale matrix (same dimension as `Omega`)
#' @return list with `value` and `grad` (d/dOmega, a matrix)
#' @noRd
#' @author Matthew L. Fidler
.rxPriorLLInvWishart <- function(Omega, nu, Psi) {
  .p <- nrow(Omega)
  .Oi <- solve(Omega)
  .logdetOmega <- as.numeric(determinant(Omega, logarithm=TRUE)$modulus)
  .logdetPsi <- as.numeric(determinant(Psi, logarithm=TRUE)$modulus)
  .logNormConst <- (nu * .p / 2) * log(2) + .rxPriorLmvgamma(nu / 2, .p)
  .val <- (nu / 2) * .logdetPsi - ((nu + .p + 1) / 2) * .logdetOmega -
    0.5 * sum(diag(Psi %*% .Oi)) - .logNormConst
  .grad <- -((nu + .p + 1) / 2) * .Oi + 0.5 * (.Oi %*% Psi %*% .Oi)
  list(value=.val, grad=.grad)
}

#' The omega block a diagonal `iniDf` row belongs to
#'
#' @param ui rxode2 ui model
#' @param name the row's own name (an eta)
#' @return named matrix (the block), or `NULL` when it could not be found
#' @noRd
#' @author Matthew L. Fidler
.rxPriorOmegaBlockFor <- function(ui, name) {
  .omega <- ui$omega
  .blks <- if (inherits(.omega, "lotri")) {
    unlist(lapply(.omega, lotri::lotriMatInv), recursive=FALSE)
  } else if (!is.matrix(.omega) || dim(.omega)[1] == 0L) {
    list()
  } else {
    lotri::lotriMatInv(.omega)
  }
  for (.b in .blks) {
    if (name %in% dimnames(.b)[[1]]) return(.b)
  }
  NULL
}

#' Build one prior term per `iniDf` row (or row group) that carries a prior
#'
#' Unlike `.rxPriorThetaMat()`/`.rxPriorOmegaNu()` (`prior-sim.R`), the
#' prior's own mean/scale is used exactly as written -- there is no
#' assertion that it matches the model's initial estimate, since an
#' estimation-time prior is a genuine Bayesian belief the parameter is
#' meant to move away from.
#'
#' @param ui rxode2 ui model
#' @return list of terms; each is `list(type=, names=, ...)` -- see
#'   `rxPriorLogDensity()` for what each `type` needs
#' @noRd
#' @author Matthew L. Fidler
.rxPriorDensityTerms <- function(ui) {
  ui <- rxode2::assertRxUi(ui)
  .iniDf <- ui$iniDf
  if (is.null(.iniDf) || !any(names(.iniDf) == "prior")) return(list())
  .w <- which(!is.na(.iniDf$prior))
  .terms <- list()
  .seen <- character(0)
  for (.i in .w) {
    .isOmega <- !is.na(.iniDf$neta1[.i])
    if (.isOmega && .iniDf$neta1[.i] != .iniDf$neta2[.i]) {
      stop("a prior on an off-diagonal omega element ('", .iniDf$name[.i],
           "') is not supported", call.=FALSE)
    }
    .name <- .iniDf$name[.i]
    .key <- if (.isOmega) paste0("om.", .name) else .name
    if (.key %in% .seen) next
    .prior <- .iniDf$prior[.i]
    .p <- .rxPriorParse(.prior)
    if (is.null(.p) || is.na(.p$stanName)) {
      .rxPriorStop(.key, .prior, "the distribution is not known to 'lotri'")
    }
    if (.p$stanName %in% .rxOmegaDfStanNames) {
      if (!identical(.p$stanName, "inv_wishart")) {
        .rxPriorStop(.key, .prior, paste0("'", .p$fn, "' is not yet evaluated by ",
                                          "rxPriorLogDensity(); only 'invWishart()' is"))
      }
      if (length(.p$args) > 1L) {
        .rxPriorStop(.key, .prior,
                     paste0("an explicit inverse-Wishart scale matrix argument is not ",
                            "yet supported; only 'invWishart(nu)', which uses the ",
                            "block's own values as the scale, is implemented"))
      }
      .blk <- .rxPriorOmegaBlockFor(ui, .name)
      if (is.null(.blk)) {
        .rxPriorStop(.key, .prior, "could not find the omega block this prior is on")
      }
      .nu <- try(eval(.p$args[[1]], envir=.rxPriorEvalEnv()), silent=TRUE)
      if (inherits(.nu, "try-error") || length(.nu) != 1L || !is.finite(.nu)) {
        .rxPriorStop(.key, .prior, "the degrees of freedom could not be read back")
      }
      .nm <- dimnames(.blk)[[1]]
      if (.nu <= length(.nm) - 1) {
        .rxPriorStop(.nm, .prior,
                     paste0("an inverse Wishart on a ", length(.nm), "x", length(.nm),
                            " block needs degrees of freedom greater than ",
                            length(.nm) - 1, ", but ", .nu, " was given"))
      }
      .terms[[length(.terms) + 1L]] <- list(type="invWishart", names=.nm,
                                            nu=as.double(.nu), Psi=.blk)
      .seen <- c(.seen, paste0("om.", .nm))
      next
    }
    if (identical(.p$stanName, "multi_normal")) {
      .nm <- .rxPriorCovNames(.prior)
      .cov <- .rxPriorCovMat(.prior)
      if (is.null(.nm) || is.null(.cov)) {
        .rxPriorStop(.key, .prior, "the covariance could not be read back")
      }
      .mu <- try(eval(.p$args[[1]], envir=.rxPriorEvalEnv()), silent=TRUE)
      if (inherits(.mu, "try-error")) {
        .rxPriorStop(.nm, .prior, "the mean vector could not be read back")
      }
      .mu <- rep_len(as.double(.mu), length(.nm))
      dimnames(.cov) <- list(.nm, .nm)
      ## a joint block member that is itself an omega element is spelled
      ## with the same `om.` prefix `.rxPriorThetaMat()` uses
      .nm <- vapply(.nm, function(n) {
        if (any(!is.na(.iniDf$neta1) & .iniDf$neta1 == .iniDf$neta2 & .iniDf$name == n)) {
          paste0("om.", n)
        } else n
      }, character(1), USE.NAMES=FALSE)
      dimnames(.cov) <- list(.nm, .nm)
      .terms[[length(.terms) + 1L]] <- list(type="multiNormal", names=.nm,
                                            mu=.mu, Sigma=.cov)
      .seen <- c(.seen, .nm)
      next
    }
    if (.p$stanName %in% c("normal", "std_normal", "cauchy")) {
      if (identical(.p$stanName, "std_normal")) {
        .mu <- 0; .sd <- 1
      } else {
        .mu <- try(eval(.p$args[[1]], envir=.rxPriorEvalEnv()), silent=TRUE)
        .sd <- try(eval(.p$args[[2]], envir=.rxPriorEvalEnv()), silent=TRUE)
        if (inherits(.mu, "try-error") || inherits(.sd, "try-error")) {
          .rxPriorStop(.key, .prior, "the parameters could not be read back")
        }
      }
      .terms[[length(.terms) + 1L]] <- list(
        type=if (identical(.p$stanName, "cauchy")) "cauchy" else "normal",
        names=.key, mu=as.double(.mu), sd=as.double(.sd),
        lower=.iniDf$lower[.i], upper=.iniDf$upper[.i])
      .seen <- c(.seen, .key)
      next
    }
    .rxPriorStop(.key, .prior,
                 paste0("'", .p$fn, "' is not yet evaluated by rxPriorLogDensity(); ",
                        "supported distributions are '",
                        paste(.rxPriorDensityStanNames, collapse="', '"), "'"))
  }
  .terms
}

#' The current value a prior term's name addresses
#'
#' @param name parameter name, or an `om.<eta>` omega element
#' @param theta named numeric of current population-parameter values
#' @param omega current omega matrix, or `NULL`
#' @param ui rxode2 ui model
#' @return scalar numeric
#' @noRd
#' @author Matthew L. Fidler
.rxPriorGetValue <- function(name, theta, omega, ui) {
  if (startsWith(name, "om.")) {
    .eta <- substring(name, 4)
    if (is.null(omega)) {
      stop("the model has a prior on the omega element '", .eta,
           "', so 'omega' must be given", call.=FALSE)
    }
    if (!(.eta %in% dimnames(omega)[[1]])) {
      stop("'", .eta, "' is not in 'omega'", call.=FALSE)
    }
    return(unname(omega[.eta, .eta]))
  }
  if (is.null(theta) || !(name %in% names(theta))) {
    stop("the model has a prior on '", name, "', so 'theta' must name it",
         call.=FALSE)
  }
  unname(theta[[name]])
}

#' Add a scalar gradient contribution to the right accumulator
#'
#' @param name parameter name, or an `om.<eta>` omega element
#' @param g gradient contribution
#' @param gThetaEnv environment holding the mutable `gTheta` named numeric;
#'   updated in place, since an environment is a mutable reference
#' @param gOmega the omega-gradient matrix so far (or `NULL`)
#' @return the (possibly updated) `gOmega`; `gThetaEnv$gTheta` is updated
#'   as a side effect
#' @noRd
#' @author Matthew L. Fidler
.rxPriorAddGrad <- function(name, g, gThetaEnv, gOmega) {
  if (startsWith(name, "om.")) {
    .eta <- substring(name, 4)
    gOmega[.eta, .eta] <- gOmega[.eta, .eta] + g
    return(gOmega)
  }
  .cur <- gThetaEnv$gTheta
  if (name %in% names(.cur)) {
    .cur[name] <- .cur[name] + g
  } else {
    .cur[name] <- g
  }
  gThetaEnv$gTheta <- .cur
  gOmega
}

#' Value and gradient of a model's prior log density, on the natural scale
#'
#' Every estimation method that implements a prior needs the same thing:
#' turn the `prior` column on `$iniDf` (`rxode2::rxUiPriors()`) into a
#' log-density and its gradient at the *current* parameter value -- as
#' opposed to `rxSolve()`'s use of the same priors, which draws study-level
#' variability around the model's initial estimate. No Jacobian for an
#' optimizer's own unconstrained-scale reparameterization is applied here;
#' that is specific to the estimation method's own parameterization and is
#' its caller's responsibility to add (chain-ruling `gradTheta`/`gradOmega`
#' through `d(natural)/d(unconstrained)`), the same way `adviJacLogDet()`
#' (`nlmixr2est/src/inner.cpp`) already does for the full-Bayes ADVI path.
#'
#' @param ui rxode2 ui model
#' @param theta named numeric vector of current population-parameter
#'   values; only the ones a prior is on need to be present
#' @param omega current omega matrix (named like `ui$omega`), needed only
#'   when a prior touches an omega element or block; `NULL` otherwise
#' @return list with `value` (scalar log density, summed over every prior
#'   term), `gradTheta` (named numeric, d/dtheta) and `gradOmega` (a matrix
#'   the same dimension as `omega`, d/dOmega, or `NULL` when `omega` was
#'   not given). `gradOmega` is entrywise, treating `omega[i, j]` and
#'   `omega[j, i]` as independent, the way `-Oi %*% Psi %*% Oi`-style matrix
#'   calculus is usually reported; a caller whose free parameter moves both
#'   symmetric entries together (a Cholesky or log-Cholesky
#'   parameterization, say) needs `gradOmega[i, j] + gradOmega[j, i]`, which
#'   is `2 * gradOmega[i, j]` since `gradOmega` is itself symmetric
#' @family Assertions
#' @author Matthew L. Fidler
#' @export
#' @examples
#'
#' \donttest{
#' one.cmt <- function() {
#'  ini({
#'    tka <- 0.45
#'    tcl <- 1
#'    tv <- 3.45
#'    eta.ka ~ 0.6
#'    add.sd <- c(0, 0.7)
#'    prior(tka) ~ dnorm(0, 10)
#'    prior(add.sd) ~ dcauchy(0, 5)
#'  })
#'  model({
#'    ka <- exp(tka + eta.ka)
#'    cl <- exp(tcl)
#'    v <- exp(tv)
#'    d/dt(depot) <- -ka*depot
#'    d/dt(center) <- ka*depot - cl/v*center
#'    cp <- center/v
#'    cp ~ add(add.sd)
#'  })
#' }
#'
#' rxPriorLogDensity(one.cmt, theta=c(tka=0.1, add.sd=0.5))
#' }
rxPriorLogDensity <- function(ui, theta=NULL, omega=NULL) {
  ui <- rxode2::assertRxUi(ui)
  .terms <- .rxPriorDensityTerms(ui)
  .val <- 0
  .gThetaEnv <- new.env(parent=emptyenv())
  .gThetaEnv$gTheta <- numeric(0)
  .gOmega <- NULL
  if (!is.null(omega)) {
    .gOmega <- matrix(0, nrow(omega), ncol(omega), dimnames=dimnames(omega))
  }
  for (.t in .terms) {
    if (.t$type %in% c("normal", "cauchy")) {
      .x <- .rxPriorGetValue(.t$names, theta, omega, ui)
      .r <- if (identical(.t$type, "normal")) {
        .rxPriorLLNormal(.x, .t$mu, .t$sd, .t$lower, .t$upper)
      } else {
        .rxPriorLLCauchy(.x, .t$mu, .t$sd, .t$lower, .t$upper)
      }
      .val <- .val + .r$value
      .gOmega <- .rxPriorAddGrad(.t$names, .r$grad, .gThetaEnv, .gOmega)
    } else if (identical(.t$type, "multiNormal")) {
      .x <- vapply(.t$names, .rxPriorGetValue, double(1),
                  theta=theta, omega=omega, ui=ui)
      .r <- .rxPriorLLMvn(.x, .t$mu, .t$Sigma)
      .val <- .val + .r$value
      for (.k in seq_along(.t$names)) {
        .gOmega <- .rxPriorAddGrad(.t$names[.k], .r$grad[.k], .gThetaEnv, .gOmega)
      }
    } else if (identical(.t$type, "invWishart")) {
      if (is.null(omega)) {
        stop("the model has an inverse-Wishart prior on the omega block '",
             paste(.t$names, collapse=", "), "', so 'omega' must be given",
             call.=FALSE)
      }
      .Om <- omega[.t$names, .t$names, drop=FALSE]
      .r <- .rxPriorLLInvWishart(.Om, .t$nu, .t$Psi)
      .val <- .val + .r$value
      .gOmega[.t$names, .t$names] <- .gOmega[.t$names, .t$names] + .r$grad
    }
  }
  list(value=.val, gradTheta=.gThetaEnv$gTheta, gradOmega=.gOmega)
}
