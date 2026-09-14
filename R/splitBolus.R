#' Split bolus doses across compartments inside an rxode2 model
#'
#' @description
#' `splitBolus()` is a model-only directive that rewrites bolus doses
#' aimed at `cmt` into parallel bolus doses to the target compartments.
#' Each target compartment receives the full original amount. The
#' source-compartment bolus is replaced, not retained.
#'
#' This rewrite applies both to event tables translated by [etTrans()]
#' and to future doses scheduled during solving with [evid_()].
#'
#' The source compartment may also appear in the target list, but the
#' target compartments in `...` must be unique.
#'
#' @param cmt Source compartment name.
#' @param ... Target compartment names. Provide at least one target
#'   compartment.
#'
#' @return This function is only meaningful inside an rxode2 model; it
#'   errors when called directly from R.
#'
#' @export
splitBolus <- function(cmt, ...) {
  stop("'splitBolus()' can only be used inside an rxode2 model block", call. = FALSE)
}

#' Split infusion doses across compartments inside an rxode2 model
#'
#' @description
#' `splitInfusion()` is a model-only directive that rewrites infusion
#' doses aimed at `cmt` into parallel infusion doses to the target
#' compartments.  Each target compartment receives the full original
#' amount (or rate); use compartment properties like `f()` to scale the
#' split (for example a Monolix-style double-absorption `F1`/`1-F1`
#' dose apportionment).  The source-compartment infusion is replaced,
#' not retained.
#'
#' Both data-driven infusions (`RATE` or `DUR` records) and modeled
#' `rate()`/`dur()` infusions are split.  The source and every start
#' or stop record of the infusion is re-targeted, so the pairings the
#' solver relies on are preserved.
#'
#' Unlike `splitBolus()`, this rewrite applies at [etTrans()]
#' translation time only; doses pushed while solving with [evid_()]
#' are not split.
#'
#' The source compartment may also appear in the target list, but the
#' target compartments in `...` must be unique.
#'
#' @param cmt Source compartment name.
#' @param ... Target compartment names. Provide at least one target
#'   compartment.
#'
#' @return This function is only meaningful inside an rxode2 model; it
#'   errors when called directly from R.
#'
#' @export
splitInfusion <- function(cmt, ...) {
  stop("'splitInfusion()' can only be used inside an rxode2 model block", call. = FALSE)
}

#' Split doses across compartments, bolus or infusion (model directive)
#'
#' @description
#' `split()` is a model-only directive that rewrites doses aimed at
#' `cmt` into parallel doses to the target compartments, regardless of
#' whether the dose record is a bolus or an infusion. Each target
#' compartment receives the full original amount; use `f()` to scale
#' the split.
#'
#' A plain bolus record targeting a compartment that declares a modeled
#' `dur()` or `rate()` property is promoted to a modeled infusion
#' start/stop pair for that compartment, so a single bolus dose record
#' can feed both an infusion path and a bolus path (Monolix-style
#' double absorption with mixed zero- and first-order routes).
#' Steady-state bolus records are not promoted; they are copied as
#' boluses with a warning when a target declares `dur()`/`rate()`.
#'
#' Infusion records (data `RATE`/`DUR` or modeled `rate()`/`dur()`) are
#' split preserving their type, exactly like [splitInfusion()].
#'
#' This rewrite applies at [etTrans()] translation time only; doses
#' pushed while solving with [evid_()] are not split. Only one
#' splitting directive (`splitBolus()`, `splitInfusion()` or `split()`)
#' is allowed per model.
#'
#' There is no R function named `split()` exported by rxode2 (it would
#' mask [base::split()]); the directive is parsed from the model text
#' directly, as shown below.
#'
#' @section Usage inside a model:
#' ```
#' model({
#'   split(depot, central, depot2)
#'   dur(central) <- tk0
#'   f(central) <- f1
#'   f(depot2) <- 1 - f1
#'   d/dt(depot2) <- -ka2 * depot2
#'   d/dt(central) <- ka2 * depot2 - cl / v * central
#'   ...
#' })
#' ```
#' A bolus dose recorded against `depot` is split so `central` receives
#' a modeled-duration infusion (zero-order input over `tk0`, scaled by
#' `f1`) and `depot2` receives the bolus (first-order input scaled by
#' `1 - f1`).
#'
#' @name split-directive
#' @aliases split split()
NULL
