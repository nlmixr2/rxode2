#' Options controlling a `confint()` summary of a solved object
#'
#' Reads the `...` of [confint.rxSolve()] into a validated list saying which
#' summary was asked for and how it should be tuned.
#'
#' @param .args list of the `...` arguments given to [confint.rxSolve()]
#' @param object solved rxode2 object, used to check `by=`
#' @param level requested interval width; the default for `ci`
#' @return list with the elements `doSim`, `by`, `ci`, `mean`, `binom`, `n`,
#'   `pred`, `useT`, `mM`, `tol` and `ciMethod`
#' @author Matthew L. Fidler
#' @noRd
.confintOptions <- function(.args, object, level) {
  .doSim <- TRUE
  if (any(names(.args) == "doSim")) {
    checkmate::assertLogical(.args$doSim, len=1,
                             any.missing=FALSE, .var.name="doSim")
    .doSim <- .args$doSim
  }
  .by <- NULL
  if (any(names(.args) == "by")) {
    .by <- .args$by
    checkmate::assertSubset(.by, names(object), .var.name="by")
  }
  .ci <- level
  if (any(names(.args) == "ci")) {
    .ci <- .args$ci
    if (inherits(.ci, "logical")) {
      checkmate::assertLogical(.ci, len=1, any.missing=FALSE, .var.name="ci")
      if (!.ci) {
        .ci <- 0.0
      }
    } else {
      checkmate::assertNumeric(.ci, lower=0, upper=1, finite=TRUE, any.missing=FALSE, .var.name="ci")
    }
  }
  .mean <- FALSE
  .binom <- FALSE
  .nC <- 0L
  .pred <- FALSE
  .useT <- TRUE
  .mM <- 500000
  .tol <- .Machine$double.eps^0.25
  if (any(names(.args) == "useT")) {
    .useT <- .args$useT
    checkmate::assertLogical(.useT, len=1, any.missing=FALSE, .var.name="useT")
  }
  if (any(names(.args) == "mean")) {
    .mean <- .args$mean
    if (inherits(.mean, "character") &&
          length(.mean) == 1L &&
          .mean == "binom") {
      .binom <- TRUE
      .mean <- FALSE
    } else {
      checkmate::assertLogical(.mean, len=1, any.missing=FALSE, .var.name="mean")
    }
  }
  if (any(names(.args) == "pred")) {
    .pred <- .args$pred
    checkmate::assertLogical(.pred, len=1, any.missing=FALSE, .var.name="pred")
  }
  if (any(names(.args) == "n")) {
    .nC <- unique(.args$n)
    checkmate::assertIntegerish(.nC, len=1, any.missing=FALSE, lower=0L, .var.name="n")
  }
  if (any(names(.args) == "m")) {
    .mM <- unique(.args$m)
    checkmate::assertIntegerish(.mM, len=1, any.missing=FALSE, lower=0L, .var.name="m")
  }
  if (any(names(.args) == "M")) {
    .mM <- unique(.args$M)
    checkmate::assertIntegerish(.mM, len=1, any.missing=FALSE, lower=1000L, .var.name="M")
  }
  if (any(names(.args) == "tol")) {
    .tol <- unique(.args$tol)
    checkmate::assertNumeric(.tol, len=1, any.missing=FALSE, lower=.Machine$double.eps, .var.name="tol")
  }
  .ciMethod <- "wald"
  if (any(names(.args) == "ciMethod")) {
    .ciMethod <- .args$ciMethod
  } else if (any(names(.args) == "method")) {
    # `ciMethod` was read out of `method` before rxode2 5.1.7
    .ciMethod <- .args$method
  }
  checkmate::assertChoice(.ciMethod,
                          c("wilson", "wilsonCorrect", "agrestiCoull",
                            "wald", "wc", "ac"),
                          .var.name="ciMethod")
  list(doSim=.doSim, by=.by, ci=.ci, mean=.mean, binom=.binom, n=.nC,
       pred=.pred, useT=.useT, mM=.mM, tol=.tol, ciMethod=.ciMethod)
}

#' Summarize simulated values the way `confint()` was asked to
#'
#' @param .value numeric vector of simulated values
#' @param .probs probabilities to report
#' @param .opt options list from `.confintOptions()`
#' @return numeric vector as long as `.probs`
#' @author Matthew L. Fidler
#' @noRd
.confintProbs <- function(.value, .probs, .opt) {
  if (.opt$mean) {
    rxode2::meanProbs(.value, probs=.probs, na.rm=TRUE, useT=.opt$useT,
                      n=.opt$n, pred=.opt$pred)
  } else if (.opt$binom) {
    rxode2::binomProbs(.value, probs=.probs, na.rm=TRUE, n=.opt$n,
                       m=.opt$mM, M=.opt$mM, tol=.opt$tol,
                       pred=.opt$pred, ciMethod=.opt$ciMethod)
  } else {
    stats::quantile(.value, probs=.probs, na.rm=TRUE)
  }
}

#' Did the solve actually simulate the `thetaMat` uncertainty?
#'
#' `rxSolve()` only draws from `thetaMat` when the variability is being
#' simulated, which is `nStud > 1` unless `simVariability` forces it.
#'
#' @param object solved rxode2 object
#' @param .nStud number of studies the object was solved with
#' @return `NA` when no `thetaMat` was given, otherwise `TRUE`/`FALSE` for
#'   whether it was drawn from
#' @author Matthew L. Fidler
#' @noRd
.confintThetaMatUsed <- function(object, .nStud) {
  if (is.null(object$env$.args$thetaMat)) return(NA)
  .simVar <- object$env$.args$simVariability
  if (!checkmate::testLogical(.simVar, len=1L, any.missing=FALSE)) {
    .simVar <- .nStud > 1L
  }
  isTRUE(.simVar)
}

#' Group a stacked solve into the replicates a confidence band is taken over
#'
#' The band needs replicate estimates of a percentile.  Separate studies supply
#' them directly; a single study large enough to sub-sample supplies them by
#' being split into `round(sqrt(n))` pieces.
#'
#' @param .stk stacked data from `rxStack()`, already a `data.table`
#' @param .ci requested band width; `0` asks for no band
#' @param .nStud,.nSub the studies and subjects the object was solved with
#' @return list with `stk` (the stacked data, labeled so `sim.id` identifies the
#'   replicate) and `n`, the number of replicates, `NA_integer_` when the
#'   simulation cannot supply any
#' @author Matthew L. Fidler
#' @noRd
.confintReplicates <- function(.stk, .ci, .nStud, .nSub) {
  if (!(.ci == 0 || !any(names(.stk) == "sim.id") || !isTRUE(.nStud > 1L))) {
    # each study is its own uncertainty draw
    return(list(stk=.stk, n=.nStud))
  }
  if (any(names(.stk) == "sim.id")) {
    .stk$id <- factor(paste(.stk$sim.id, .stk$id))
    .ntot <- length(levels(.stk$id))
    .stk$id <- as.integer(.stk$id)
  } else if (any(names(.stk) == "id")) {
    .ntot <- length(unique(.stk$id))
  } else {
    .ntot <- .nSub
    if (.ntot == 1L && .nStud > 1L) {
      .ntot <- .nStud
    }
  }
  if (.ci == 0) return(list(stk=.stk, n=NA_integer_))
  if (.ntot < 2500) {
    .mwarn("in order to put confidence bands around the intervals, you need at least 2500 simulations") # nolint
    return(list(stk=.stk, n=NA_integer_))
  }
  # one study, but enough individuals to sub-sample it
  if (!any(names(.stk) == "sim.id")) {
    # `id` can be a factor (character subject identifiers); densify to 1:.ntot
    # so the modulus taken by the caller splits it into equal sub-samples
    .stk$sim.id <- as.integer(factor(.stk$id))
  }
  list(stk=.stk, n=round(sqrt(.ntot)))
}
