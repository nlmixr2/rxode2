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
## `.rxPriorEvalEnv`) but never asserts the mean against the estimate.
##
## The actual value/gradient math (`src/priorDensity.cpp`) is pure C++ with
## no R/Rcpp touch of any kind, so it is safe to call from inside an
## OpenMP-parallel objective evaluation -- this file only *builds* the spec
## (parses the `prior` column, an R-only, one-time, main-thread operation)
## and exposes a thin R convenience wrapper over the C entry points for
## direct use and testing.
##
## Three methods are supported, matching three genuinely different
## conventions:
##
## - `"general"`: a textbook Bayesian log density for whatever `lotri`
##   reports -- `dnorm()`/`stdNormal()`, `dcauchy()` (including the
##   half-Cauchy case, `prior(add.sd) ~ dcauchy(0, 5)` with `add.sd`'s own
##   `lower = 0`), the joint theta+omega `multiNormal()` block (an `om.<eta>`
##   member addresses the raw omega value), and a textbook inverse-Wishart
##   on an omega block.
## - `"nwpri"`: NONMEM's own `$PRIOR NWPRI` parameterization (NONMEM7
##   Technical Guide, "Three Stage EM Analysis", eq. 1.154-1.171). Theta
##   reuses the same multivariate-normal math (eq. 1.156 is the textbook MVN
##   density up to a dropped constant); the omega term does NOT reduce to
##   the textbook inverse-Wishart with `nu`/`Psi` substituted in -- NONMEM's
##   own degrees-of-freedom convention (eq. 1.170, the "modal"/non-BAYES
##   version every method but BAYES uses) is `d_W = rho + n + 1` with scale
##   `rho * Psi`, not `rho`/`Psi` directly, and gives a different value and
##   gradient (see the `type` comment on `rx_prior_term_t`,
##   `inst/include/rxode2prior.h`, for the derivation).
## - `"tnpri"`: the assumption Monolix's Bayesian estimation makes (all
##   estimated parameters are jointly normal), which NONMEM's own $SIML
##   TNPRI recipe ("Appendix I: Note on TNPRI") applies to omega on its
##   Cholesky scale ONLY BECAUSE it is sampling values that must stay
##   positive definite -- the PRIOR ITSELF is still specified, and
##   evaluated, on the raw omega value, exactly like `"general"` already
##   does for an `om.<eta>` member (same `type=0`/`type=2` terms; there is
##   no Cholesky decomposition anywhere in this kernel). `chol(Omega^-1)`
##   is entirely a CALLER's own affair: nlmixr2est's FOCEI varies
##   `op_focei.cholOmegaInv` internally (`src/inner.cpp`), so it has to
##   reconstruct Omega from that state before calling this kernel, and
##   chain-rule the Omega-space gradient this kernel returns back into its
##   own `cholOmegaInv`-space afterward -- this kernel never does that
##   conversion. A method that estimates Omega directly (SAEM) uses the
##   returned gradient as-is.
##
## Neither `"nwpri"` nor `"tnpri"` has a Cauchy analogue, so both refuse
## one; `"tnpri"` also refuses `invWishart()` (that is `"nwpri"`'s own
## mechanism, not `"tnpri"`'s).

#' Distributions `rxPriorLogDensity()` can evaluate
#'
#' @noRd
.rxPriorDensityStanNames <- c("normal", "std_normal", "cauchy",
                              "multi_normal", "inv_wishart")

#' Prior-term type codes shared with `src/priorDensity.cpp`
#'
#' Must match `rx_prior_term_t.type` in `inst/include/rxode2prior.h`.
#'
#' @noRd
.rxPriorTermTypeCode <- c(normal=0L, cauchy=1L, multiNormal=2L,
                          invWishart=3L, invWishartNwpri=4L)

#' The covariance matrix a `multiNormal()` prior carries, without going
#' through `lotri::lotri()`'s own validation
#'
#' `.rxPriorCovMat()` (`prior-sim.R`) evaluates the embedded `lotri(...)`
#' call via `lotri::lotri()` itself, which requires at least one name it
#' recognizes as an already-known parameter -- fine for a block anchored by
#' a real theta (`tcl + om.eta.ka ~ c(...)`), but `lotri::lotri()` refuses
#' a block whose members are ALL `om.<eta>` on their own (`om.eta1 +
#' om.eta2 ~ c(...)`), with "prior given for unknown parameter(s)" -- under
#' any method, since raw omega values are what a joint block always
#' addresses here. This reads the same embedded lower-
#' triangular vector directly and fills the matrix by hand (column-major
#' lower triangle including the diagonal -- the same convention every
#' `lotri`-built covariance in this ecosystem already uses, eg `eta.cl +
#' eta.v ~ c(0.3, 0.01, 0.1)`), so it never needs `lotri::lotri()` to
#' recognize the names at all.
#'
#' @param names character vector of the block's member names, in order
#'   (from `.rxPriorCovNames()`)
#' @param prior prior as stored in the `prior` column
#' @return numeric matrix (unnamed dimnames), or `NULL`
#' @noRd
#' @author Matthew L. Fidler
.rxPriorCovMatFromNames <- function(names, prior) {
  .p <- .rxPriorParse(prior)
  if (is.null(.p)) return(NULL)
  for (.a in .p$args) {
    if (!(is.call(.a) && identical(.a[[1]], quote(`lotri`)))) next
    .b <- .a[[2]]
    if (is.call(.b) && identical(.b[[1]], quote(`{`))) .b <- .b[[2]]
    if (!(is.call(.b) && identical(.b[[1]], quote(`~`)))) return(NULL)
    .vec <- try(eval(.b[[3]], envir=.rxPriorEvalEnv()), silent=TRUE)
    if (inherits(.vec, "try-error") || !is.numeric(.vec)) return(NULL)
    .n <- length(names)
    if (length(.vec) != .n * (.n + 1) / 2) return(NULL)
    .m <- matrix(0, .n, .n)
    .k <- 1L
    ## row-major lower triangle (row i's entries (i,1)..(i,i) in order),
    ## the NONMEM $OMEGA BLOCK / 'lotri' convention -- NOT column-major;
    ## the two coincide for n=2 but diverge from n=3 on (verified against
    ## lotri::lotri(a+b+c~c(1,2,3,4,5,6)), which places 3 at (2,2) and 4 at
    ## (3,1), not 4 at (2,2)/3 at (3,1))
    for (.i in seq_len(.n)) {
      for (.j in seq_len(.i)) {
        .m[.i, .j] <- .vec[.k]
        .m[.j, .i] <- .vec[.k]
        .k <- .k + 1L
      }
    }
    return(.m)
  }
  NULL
}

#' Complain about a prior this kernel cannot evaluate
#'
#' `prior-sim.R`'s `.rxPriorStop()` is worded for the simulation path
#' ("cannot simulate from..."); this is the same idea for the estimation
#' kernel, worded for it.
#'
#' @param name parameter name(s)
#' @param prior prior as stored in the `prior` column
#' @param why one line saying what is wrong
#' @return nothing, called for the error
#' @noRd
#' @author Matthew L. Fidler
.rxPriorDensityStop <- function(name, prior, why) {
  stop("cannot evaluate the prior on '", paste(name, collapse="', '"),
       "' (", paste(prior, collapse=", "), "): ", why,
       call.=FALSE)
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

#' Record that these keys now carry `prior`, erroring on a conflict
#'
#' A block prior (`multiNormal()`/`invWishart()`) can name a member whose
#' own `iniDf` row was already processed under a *different* prior text --
#' not reachable through real `ini()` syntax (`lotri` itself refuses two
#' priors on one parameter), but a hand-edited or piped `iniDf` can still
#' reach it, and it must be a clear error rather than one prior silently
#' overwriting or summing with the other.
#'
#' @param seenPrior named character accumulator so far (`key` -> prior text)
#' @param keys keys about to be recorded
#' @param prior the prior text they are being recorded under
#' @return the updated `seenPrior`
#' @noRd
#' @author Matthew L. Fidler
.rxPriorMarkSeen <- function(seenPrior, keys, prior) {
  .old <- unname(seenPrior[keys])
  .conflict <- !is.na(.old) & .old != prior
  if (any(.conflict)) {
    stop("'", paste(keys[.conflict], collapse="', '"), "' carries two ",
         "different priors ('", paste(unique(.old[.conflict]), collapse="', '"),
         "' and '", prior, "'); this can only happen on a hand-edited or ",
         "piped 'iniDf'", call.=FALSE)
  }
  seenPrior[keys] <- prior
  seenPrior
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
#' @param method `"general"`, `"nwpri"` or `"tnpri"`; see the file header
#' @return list of terms; each is `list(type=, names=, ...)`
#' @noRd
#' @author Matthew L. Fidler
.rxPriorDensityTerms <- function(ui, method=c("general", "nwpri", "tnpri")) {
  method <- match.arg(method)
  ui <- rxode2::assertRxUi(ui)
  .iniDf <- ui$iniDf
  if (is.null(.iniDf) || !any(names(.iniDf) == "prior")) return(list())
  .w <- which(!is.na(.iniDf$prior))
  .terms <- list()
  .seen <- character(0)
  .seenPrior <- character(0)
  for (.i in .w) {
    .isOmega <- !is.na(.iniDf$neta1[.i])
    if (.isOmega && .iniDf$neta1[.i] != .iniDf$neta2[.i]) {
      stop("a prior on an off-diagonal omega element ('", .iniDf$name[.i],
           "') is not supported", call.=FALSE)
    }
    .name <- .iniDf$name[.i]
    if (!.isOmega && startsWith(.name, "om.")) {
      ## `om.<eta>` is how this kernel spells an omega diagonal element
      ## internally (matching `.rxPriorThetaMat()`); a population
      ## parameter that happens to be named that way would be ambiguous
      stop("the population parameter '", .name, "' collides with this ",
           "kernel's internal 'om.<eta>' spelling of an omega element; ",
           "rename the parameter", call.=FALSE)
    }
    .key <- if (.isOmega) paste0("om.", .name) else .name
    .prior <- .iniDf$prior[.i]
    .seenPrior <- .rxPriorMarkSeen(.seenPrior, .key, .prior)
    if (.key %in% .seen) next
    .p <- .rxPriorParse(.prior)
    if (is.null(.p) || is.na(.p$stanName)) {
      .rxPriorDensityStop(.key, .prior, "the distribution is not known to 'lotri'")
    }
    if (.p$stanName %in% .rxOmegaDfStanNames) {
      if (!identical(.p$stanName, "inv_wishart")) {
        .rxPriorDensityStop(.key, .prior, paste0("'", .p$fn, "' is not yet evaluated by ",
                                          "rxPriorLogDensity(); only 'invWishart()' is"))
      }
      if (identical(method, "tnpri")) {
        .rxPriorDensityStop(.key, .prior,
                     paste0("'invWishart()' is not part of the TNPRI method; use ",
                            "method=\"nwpri\" for an omega degrees-of-freedom prior"))
      }
      if (length(.p$args) > 1L) {
        .rxPriorDensityStop(.key, .prior,
                     paste0("an explicit inverse-Wishart scale matrix argument is not ",
                            "yet supported; only 'invWishart(nu)', which uses the ",
                            "block's own values as the scale, is implemented"))
      }
      .blk <- .rxPriorOmegaBlockFor(ui, .name)
      if (is.null(.blk)) {
        .rxPriorDensityStop(.key, .prior, "could not find the omega block this prior is on")
      }
      .nu <- try(eval(.p$args[[1]], envir=.rxPriorEvalEnv()), silent=TRUE)
      if (inherits(.nu, "try-error") || length(.nu) != 1L || !is.finite(.nu)) {
        .rxPriorDensityStop(.key, .prior, "the degrees of freedom could not be read back")
      }
      .nm <- dimnames(.blk)[[1]]
      if (.nu <= length(.nm) - 1) {
        .rxPriorDensityStop(.nm, .prior,
                     paste0("an inverse Wishart on a ", length(.nm), "x", length(.nm),
                            " block needs degrees of freedom greater than ",
                            length(.nm) - 1, ", but ", .nu, " was given"))
      }
      .omKeys <- paste0("om.", .nm)
      .seenPrior <- .rxPriorMarkSeen(.seenPrior, .omKeys, .prior)
      .terms[[length(.terms) + 1L]] <- list(
        type=if (identical(method, "nwpri")) "invWishartNwpri" else "invWishart",
        names=.omKeys, nu=as.double(.nu), Psi=.blk)
      .seen <- c(.seen, .omKeys)
      next
    }
    if (identical(.p$stanName, "multi_normal")) {
      .nm <- .rxPriorCovNames(.prior)
      .cov <- .rxPriorCovMat(.prior)
      if (is.null(.cov) && !is.null(.nm)) {
        ## a block with no theta anchor (all om.<eta> members) trips
        ## lotri::lotri()'s own validation -- read the vector by hand
        .cov <- .rxPriorCovMatFromNames(.nm, .prior)
      }
      if (is.null(.nm) || is.null(.cov)) {
        .rxPriorDensityStop(.key, .prior, "the covariance could not be read back")
      }
      .mu <- try(eval(.p$args[[1]], envir=.rxPriorEvalEnv()), silent=TRUE)
      if (inherits(.mu, "try-error")) {
        .rxPriorDensityStop(.nm, .prior, "the mean vector could not be read back")
      }
      .mu <- rep_len(as.double(.mu), length(.nm))
      ## a joint block member that is itself an omega element is spelled
      ## with the same `om.` prefix `.rxPriorThetaMat()` uses
      .nm <- vapply(.nm, function(n) {
        if (any(!is.na(.iniDf$neta1) & .iniDf$neta1 == .iniDf$neta2 & .iniDf$name == n)) {
          paste0("om.", n)
        } else n
      }, character(1), USE.NAMES=FALSE)
      dimnames(.cov) <- list(.nm, .nm)
      .seenPrior <- .rxPriorMarkSeen(.seenPrior, .nm, .prior)
      ## "tnpri" builds the exact same term as "general" here -- the prior
      ## is on the raw omega value either way; see the file header comment
      .terms[[length(.terms) + 1L]] <- list(type="multiNormal", names=.nm,
                                            mu=.mu, Sigma=.cov)
      .seen <- c(.seen, .nm)
      next
    }
    if (.p$stanName %in% c("normal", "std_normal", "cauchy")) {
      if (identical(.p$stanName, "cauchy") && method %in% c("nwpri", "tnpri")) {
        .rxPriorDensityStop(.key, .prior,
                     paste0("'dcauchy()' is not part of NONMEM's ",
                            toupper(method), " prior machinery; use ",
                            "method=\"general\" for a Cauchy prior"))
      }
      if (identical(.p$stanName, "std_normal")) {
        .mu <- 0; .sd <- 1
      } else {
        .mu <- try(eval(.p$args[[1]], envir=.rxPriorEvalEnv()), silent=TRUE)
        .sd <- try(eval(.p$args[[2]], envir=.rxPriorEvalEnv()), silent=TRUE)
        if (inherits(.mu, "try-error") || inherits(.sd, "try-error")) {
          .rxPriorDensityStop(.key, .prior, "the parameters could not be read back")
        }
      }
      .terms[[length(.terms) + 1L]] <- list(
        type=if (identical(.p$stanName, "cauchy")) "cauchy" else "normal",
        names=.key, mu=as.double(.mu), sd=as.double(.sd),
        lower=.iniDf$lower[.i], upper=.iniDf$upper[.i])
      .seen <- c(.seen, .key)
      next
    }
    .rxPriorDensityStop(.key, .prior,
                 paste0("'", .p$fn, "' is not yet evaluated by rxPriorLogDensity(); ",
                        "supported distributions are '",
                        paste(.rxPriorDensityStanNames, collapse="', '"), "'"))
  }
  .terms
}

#' The canonical eta order this kernel's omega indices are numbered in
#'
#' Matches `iniDf$neta1` for the diagonal (variance) rows, which is also
#' the numbering nlmixr2est's own `op_focei` uses for its omega matrix.
#'
#' The C evaluator addresses `omega` *positionally* (`etaIdx - 1`) against
#' a dimension sized from this vector's length -- were `neta1` ever gapped
#' (not a dense `1:n`, eg from a hand-edited `iniDf`), a term referencing
#' the index past the gap would read/write past the end of that array. This
#' asserts the numbering is dense before that can happen, the same
#' "a piped model can dodge it" defense `prior-sim.R` already applies to
#' the inverse-Wishart degrees-of-freedom check.
#'
#' @param ui rxode2 ui model
#' @return character vector of eta names, in index order
#' @noRd
#' @author Matthew L. Fidler
.rxPriorEtaOrder <- function(ui) {
  .iniDf <- ui$iniDf
  .d <- which(!is.na(.iniDf$neta1) & .iniDf$neta1 == .iniDf$neta2)
  .idx <- .iniDf$neta1[.d]
  if (length(.idx) > 0L && !isTRUE(all.equal(sort(.idx), as.double(seq_along(.idx))))) {
    stop("the model's omega diagonal indices ('neta1') are not a dense ",
         "1:n sequence (", paste(sort(.idx), collapse=", "), "); this can ",
         "only happen on a hand-edited or piped 'iniDf'", call.=FALSE)
  }
  .iniDf$name[.d][order(.idx)]
}

#' The `(thetaIdx, etaIdx)` a term member's key addresses
#'
#' @param ui rxode2 ui model
#' @param key parameter name, or an `om.<eta>` omega element
#' @return list with `thetaIdx`/`etaIdx` (1-based; the other is `0L`)
#' @noRd
#' @author Matthew L. Fidler
.rxPriorKeyIndex <- function(ui, key) {
  .iniDf <- ui$iniDf
  if (startsWith(key, "om.")) {
    .eta <- substring(key, 4)
    .w <- which(!is.na(.iniDf$neta1) & .iniDf$neta1 == .iniDf$neta2 & .iniDf$name == .eta)
    if (length(.w) != 1L) {
      stop("could not find the omega element '", .eta, "'", call.=FALSE)
    }
    list(thetaIdx=0L, etaIdx=as.integer(.iniDf$neta1[.w]))
  } else {
    .w <- which(is.na(.iniDf$neta1) & .iniDf$name == key)
    if (length(.w) != 1L) {
      stop("could not find the population parameter '", key, "'", call.=FALSE)
    }
    list(thetaIdx=as.integer(.iniDf$ntheta[.w]), etaIdx=0L)
  }
}

#' Flatten `.rxPriorDensityTerms()`'s output into `_rxode2_rxPriorBuildSpec()`'s
#' plain-vector representation
#'
#' @param ui rxode2 ui model
#' @param terms list from `.rxPriorDensityTerms()`
#' @return list of parallel `type`/`n`/`thetaIdx`/`etaIdx`/`mu`/`scale`/
#'   `lower`/`upper`/`nu` vectors (see `src/priorDensity.cpp`)
#' @noRd
#' @author Matthew L. Fidler
.rxPriorFlattenSpec <- function(ui, terms) {
  .type <- integer(0); .n <- integer(0)
  .thetaIdx <- integer(0); .etaIdx <- integer(0)
  .mu <- numeric(0); .scale <- numeric(0)
  .lower <- numeric(0); .upper <- numeric(0); .nu <- numeric(0)
  ## validate the omega numbering is dense (see .rxPriorEtaOrder()) before
  ## computing any etaIdx from it -- only when a term actually needs it, so
  ## a model with unusual (but unused) omega structure stays a no-op
  if (any(vapply(terms, function(t) any(startsWith(t$names, "om.")), logical(1)))) {
    .rxPriorEtaOrder(ui)
  }
  for (.t in terms) {
    .type <- c(.type, .rxPriorTermTypeCode[[.t$type]])
    .n <- c(.n, length(.t$names))
    .idx <- lapply(.t$names, .rxPriorKeyIndex, ui=ui)
    .thetaIdx <- c(.thetaIdx, vapply(.idx, `[[`, integer(1), "thetaIdx"))
    .etaIdx <- c(.etaIdx, vapply(.idx, `[[`, integer(1), "etaIdx"))
    if (.t$type %in% c("normal", "cauchy")) {
      .mu <- c(.mu, .t$mu)
      .scale <- c(.scale, .t$sd)
      .lower <- c(.lower, .t$lower)
      .upper <- c(.upper, .t$upper)
      .nu <- c(.nu, 0)
    } else if (identical(.t$type, "multiNormal")) {
      .mu <- c(.mu, .t$mu)
      ## Sigma is symmetric, so the column-major flatten R gives is already
      ## the row-major flatten src/priorDensity.cpp expects
      .scale <- c(.scale, as.numeric(.t$Sigma))
      .lower <- c(.lower, -Inf)
      .upper <- c(.upper, Inf)
      .nu <- c(.nu, 0)
    } else {
      .mu <- c(.mu, rep(0, length(.t$names)))
      .scale <- c(.scale, as.numeric(.t$Psi))
      .lower <- c(.lower, -Inf)
      .upper <- c(.upper, Inf)
      .nu <- c(.nu, .t$nu)
    }
  }
  list(type=.type, n=.n, thetaIdx=.thetaIdx, etaIdx=.etaIdx, mu=.mu, scale=.scale,
       lower=.lower, upper=.upper, nu=.nu)
}

#' Build the C spec `rxPriorLogDensityEval()` (and the C API) evaluates
#'
#' The one-time, R-only (main-thread) half of the kernel: parses the
#' `prior` column and hands the result to `src/priorDensity.cpp` as a
#' heap-allocated `rx_prior_spec_t`, wrapped in an R external pointer with a
#' finalizer. `rxPriorLogDensityEval()` (`inst/include/rxode2prior.h`) --
#' reachable through rxode2's C function-pointer table -- then evaluates it
#' with no R/Rcpp call of any kind, safe to call from inside an
#' OpenMP-parallel objective/gradient evaluation.
#'
#' @param ui rxode2 ui model
#' @param method `"general"` (default), `"nwpri"` or `"tnpri"`; see this
#'   file's header comment for what differs
#' @return external pointer to a `rx_prior_spec_t`
#' @family Assertions
#' @author Matthew L. Fidler
#' @export
rxPriorBuildSpec <- function(ui, method=c("general", "nwpri", "tnpri")) {
  method <- match.arg(method)
  ui <- rxode2::assertRxUi(ui)
  .terms <- .rxPriorDensityTerms(ui, method=method)
  .spec <- .rxPriorFlattenSpec(ui, .terms)
  .Call(`_rxode2_rxPriorBuildSpec`, .spec)
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
#' This is a thin R-level convenience shim: the actual math is the pure
#' C++ `rxPriorLogDensityEval()` (`inst/include/rxode2prior.h`), exposed
#' through rxode2's C function-pointer table so a downstream package's own
#' C++ objective can call it directly (no R/Rcpp touch, safe from inside an
#' OpenMP-parallel region) instead of round-tripping through R for every
#' evaluation the way this function does.
#'
#' @param ui rxode2 ui model
#' @param theta named numeric vector of current population-parameter
#'   values; only the ones a prior is on need to be present
#' @param omega current omega matrix, needed only when a prior touches an
#'   omega element or block; `NULL` otherwise. Must carry every eta in the
#'   model (like `ui$omega` itself, in any name order) even when only one
#'   of its blocks has a prior -- the underlying C kernel addresses omega
#'   positionally (the model's own eta numbering), not by submatrix, so a
#'   smaller matrix containing only the referenced block is not enough
#' @param method `"general"` (default), `"nwpri"` or `"tnpri"`; see this
#'   file's header comment for what differs. `"nwpri"` implements NONMEM's
#'   own `$PRIOR NWPRI` omega parameterization; `"tnpri"` is the same
#'   raw-omega `om.<eta>` treatment as `"general"` (Monolix's
#'   Bayesian-estimation assumption is on the natural parameter, same as
#'   here -- `chol(Omega^-1)` is FOCEI's own internal affair, not this
#'   kernel's). Neither `"nwpri"` nor `"tnpri"` has a Cauchy analogue, so
#'   both refuse one.
#' @return list with `value` (scalar log density, summed over every prior
#'   term), `gradTheta` (named numeric, d/dtheta) and `gradOmega` (a matrix
#'   the same dimension as `omega`, d/dOmega, or `NULL` when `omega` was
#'   not given). `gradOmega` is entrywise, treating `omega[i, j]` and
#'   `omega[j, i]` as independent, the way `-Oi %*% Psi %*% Oi`-style matrix
#'   calculus is usually reported. A caller whose free parameter moves an
#'   *off-diagonal* pair together (a Cholesky or log-Cholesky
#'   parameterization, say) needs `gradOmega[i, j] + gradOmega[j, i]`, which
#'   is `2 * gradOmega[i, j]` for `i != j` since `gradOmega` is itself
#'   symmetric -- but NOT for a diagonal entry, whose own free parameter is
#'   `gradOmega[i, i]` alone; doubling it as well would be wrong
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
rxPriorLogDensity <- function(ui, theta=NULL, omega=NULL, method=c("general", "nwpri", "tnpri")) {
  method <- match.arg(method)
  ui <- rxode2::assertRxUi(ui)
  .iniDf <- ui$iniDf
  .terms <- .rxPriorDensityTerms(ui, method=method)
  if (length(.terms) == 0L) {
    return(list(value=0, gradTheta=numeric(0),
               gradOmega=if (is.null(omega)) NULL else {
                 .g <- matrix(0, nrow(omega), ncol(omega), dimnames=dimnames(omega)); .g
               }))
  }
  .spec <- .rxPriorFlattenSpec(ui, .terms)
  .specPtr <- .Call(`_rxode2_rxPriorBuildSpec`, .spec)

  .nTheta <- suppressWarnings(max(.iniDf$ntheta, na.rm=TRUE))
  if (!is.finite(.nTheta)) .nTheta <- 0L
  .thetaFull <- numeric(.nTheta)
  .thetaKeys <- unique(unlist(lapply(.terms, function(t) t$names[!startsWith(t$names, "om.")])))
  for (.k in .thetaKeys) {
    .thetaFull[.rxPriorKeyIndex(ui, .k)$thetaIdx] <- .rxPriorGetTheta(.k, theta)
  }

  .etaOrder <- .rxPriorEtaOrder(ui)
  .nEta <- length(.etaOrder)
  .omegaFull <- if (.nEta == 0L) matrix(0, 0, 0) else {
    if (is.null(omega)) {
      matrix(0, .nEta, .nEta)
    } else {
      .m <- omega[.etaOrder, .etaOrder, drop=FALSE]
      matrix(as.numeric(.m), .nEta, .nEta)
    }
  }
  .needsOmega <- any(vapply(.terms, function(t) any(startsWith(t$names, "om.")), logical(1)))
  if (.needsOmega && is.null(omega)) {
    .omKeys <- unlist(lapply(.terms, function(t) t$names[startsWith(t$names, "om.")]))
    stop("the model has a prior on the omega element(s) '",
         paste(unique(substring(.omKeys, 4)), collapse="', '"),
         "', so 'omega' must be given", call.=FALSE)
  }

  .r <- .Call(`_rxode2_rxPriorLogDensity`, .specPtr, .thetaFull, .omegaFull)
  .gradThetaFull <- .r[[2]]
  .gradTheta <- setNames(vapply(.thetaKeys, function(k) {
    .gradThetaFull[.rxPriorKeyIndex(ui, k)$thetaIdx]
  }, double(1)), .thetaKeys)
  if (length(.gradTheta) == 0L) .gradTheta <- numeric(0)

  .gradOmega <- NULL
  if (!is.null(omega)) {
    .gradOmegaCanon <- matrix(.r[[3]], .nEta, .nEta, dimnames=list(.etaOrder, .etaOrder))
    .gradOmega <- .gradOmegaCanon[rownames(omega), colnames(omega), drop=FALSE]
    dimnames(.gradOmega) <- dimnames(omega)
  }
  list(value=.r[[1]], gradTheta=.gradTheta, gradOmega=.gradOmega)
}

#' Chain-rule a natural-scale omega gradient into FOCEI's cholOmegaInv scale
#'
#' `rxPriorLogDensity()`'s `gradOmega` is always on the raw omega scale --
#' the prior itself is defined there, for every method (see this file's
#' header comment). A caller whose optimizer varies
#' `chol(Omega^-1)` internally instead (FOCEI's `op_focei.cholOmegaInv`,
#' `nlmixr2est/src/inner.cpp`) still needs that gradient chain-ruled into its
#' own parameterization; a method that estimates Omega directly (SAEM) uses
#' `gradOmega` as-is and does not need this. This is a thin shim over the
#' pure C++ `rxPriorOmegaToCholOmegaInvGrad()` (`inst/include/rxode2prior.h`),
#' exposed the same way for a downstream package's own C++ objective.
#'
#' @param omega a single omega block (p x p numeric matrix, positive
#'   definite)
#' @param gradOmega the corresponding block of `rxPriorLogDensity()`'s
#'   `gradOmega`, i.e. `gradOmega[block, block]` (p x p, symmetric)
#' @return p x p matrix, the gradient with respect to the upper-triangular
#'   free elements of `U = chol(solve(omega))` (`omega^-1 = t(U) %*% U`);
#'   its strict lower triangle is exactly 0 since those entries address no
#'   free parameter of `U`. `NULL` if `omega` is not positive definite.
#' @family Assertions
#' @author Matthew L. Fidler
#' @export
#' @examples
#'
#' \donttest{
#' omega <- lotri::lotri(eta.ka + eta.cl ~ c(0.1, 0.01, 0.1))
#' gradOmega <- matrix(c(1, 0.2, 0.2, -0.5), 2, 2)
#' rxPriorOmegaToCholOmegaInvGrad(omega, gradOmega)
#' }
rxPriorOmegaToCholOmegaInvGrad <- function(omega, gradOmega) {
  .p <- nrow(omega)
  if (.p != ncol(omega) || !identical(dim(omega), dim(gradOmega))) {
    stop("'omega' and 'gradOmega' must be square matrices of the same dimension",
         call.=FALSE)
  }
  .ret <- .Call(`_rxode2_rxPriorOmegaToCholOmegaInvGrad`, matrix(as.numeric(omega), .p, .p),
               matrix(as.numeric(gradOmega), .p, .p))
  if (is.null(.ret)) return(NULL)
  dimnames(.ret) <- dimnames(omega)
  .ret
}

#' The current value of a population-parameter prior term's key
#'
#' @param key parameter name
#' @param theta named numeric of current population-parameter values
#' @return scalar numeric
#' @noRd
#' @author Matthew L. Fidler
.rxPriorGetTheta <- function(key, theta) {
  if (is.null(theta) || !(key %in% names(theta))) {
    stop("the model has a prior on '", key, "', so 'theta' must name it",
         call.=FALSE)
  }
  unname(theta[[key]])
}
