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
