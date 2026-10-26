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
#' @examples
#' \donttest{
#' # a depot bolus split into fast and slow absorption paths
#' f <- function() {
#'   ini({
#'     ka1 <- 1; ka2 <- 0.2; fr <- 0.6; cl <- 1; v <- 10
#'   })
#'   model({
#'     splitBolus(depot, depot1, depot2)
#'     f(depot1) <- fr
#'     f(depot2) <- 1 - fr
#'     d/dt(depot) <- 0
#'     d/dt(depot1) <- -ka1 * depot1
#'     d/dt(depot2) <- -ka2 * depot2
#'     d/dt(central) <- ka1 * depot1 + ka2 * depot2 - cl / v * central
#'   })
#' }
#' rxSolve(f, et(amt = 100, cmt = "depot") |> et(0:24))
#' }
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
#' @examples
#' \donttest{
#' # a depot infusion split between two compartments
#' f <- function() {
#'   ini({
#'     fr <- 0.6; k1 <- 0.5; k2 <- 0.05
#'   })
#'   model({
#'     splitInfusion(depot, c1, c2)
#'     f(c1) <- fr
#'     f(c2) <- 1 - fr
#'     d/dt(depot) <- 0
#'     d/dt(c1) <- -k1 * c1
#'     d/dt(c2) <- -k2 * c2
#'   })
#' }
#' rxSolve(f, et(amt = 100, rate = 20, cmt = "depot") |> et(0:24))
#' }
splitInfusion <- function(cmt, ...) {
  stop("'splitInfusion()' can only be used inside an rxode2 model block", call. = FALSE)
}

#' Split infusion doses into infusion and bolus paths (model directive)
#'
#' @description
#' `splitInfusionBolus()` is a model-only directive that rewrites doses
#' aimed at `cmt` into parallel doses to the target compartments,
#' regardless of whether the dose record is a bolus or an infusion.
#' Each target compartment receives the full original amount; use `f()`
#' to scale the split.
#'
#' A plain bolus record is split so the FIRST target compartment
#' receives a modeled infusion start/stop pair (it must declare a
#' modeled `dur()` or `rate()` property, otherwise its copies stay
#' boluses) while every other target receives a bolus copy. Use
#' [splitBolusInfusion()] to give the LAST target the infusion instead.
#' This feeds one bolus dose record into both an infusion path and a
#' bolus path (Monolix-style double absorption with mixed zero- and
#' first-order routes). Steady-state bolus records are not promoted;
#' they are copied as boluses with a warning when the designated
#' infusion target declares `dur()`/`rate()`.
#'
#' Infusion records (data `RATE`/`DUR` or modeled `rate()`/`dur()`) are
#' split preserving their type, exactly like [splitInfusion()].
#'
#' This rewrite applies at [etTrans()] translation time only; doses
#' pushed while solving with [evid_()] are not split. Only one
#' splitting directive (`splitBolus()`, `splitInfusion()`,
#' `splitInfusionBolus()` or `splitBolusInfusion()`) is allowed per
#' model.
#'
#' There is no R function named `splitInfusionBolus()` exported by
#' rxode2; the directive is parsed from the model text directly, as
#' shown in `?splitBolusInfusion-directive`.
#'
#' @section Usage inside a model:
#' ```
#' model({
#'   splitInfusionBolus(depot, central, depot2)
#'   dur(central) <- tk0
#'   f(central) <- f1
#'   f(depot2) <- 1 - f1
#'   d/dt(depot2) <- -ka2 * depot2
#'   d/dt(central) <- ka2 * depot2 - cl / v * central
#'   ...
#' })
#' ```
#' A bolus dose recorded against `depot` is split so `central` (first
#' target) receives a modeled-duration infusion (zero-order input over
#' `tk0`, scaled by `f1`) and `depot2` receives the bolus (first-order
#' input scaled by `1 - f1`).
#'
#' @examples
#' \donttest{
#' f <- function() {
#'   ini({
#'     tk0 <- 4; fr <- 0.7; ka2 <- 1.2; cl <- 0.1; v <- 10
#'   })
#'   model({
#'     splitInfusionBolus(depot, central, depot2)
#'     dur(central) <- tk0
#'     f(central) <- fr
#'     f(depot2) <- 1 - fr
#'     d/dt(depot) <- 0
#'     d/dt(depot2) <- -ka2 * depot2
#'     d/dt(central) <- ka2 * depot2 - cl / v * central
#'   })
#' }
#' # the depot bolus becomes a central infusion plus a depot2 bolus
#' rxSolve(f, et(amt = 100, cmt = "depot") |> et(0:24))
#' }
#' @name splitInfusionBolus-directive
#' @aliases splitInfusionBolus splitInfusionBolus()
NULL

#' Split bolus doses into bolus and infusion paths (model directive)
#'
#' @description
#' `splitBolusInfusion()` is a model-only directive that rewrites doses
#' aimed at `cmt` into parallel doses to the target compartments,
#' regardless of whether the dose record is a bolus or an infusion.
#' Each target compartment receives the full original amount; use `f()`
#' to scale the split.
#'
#' A plain bolus record is split so the LAST target compartment
#' receives a modeled infusion start/stop pair (it must declare a
#' modeled `dur()` or `rate()` property, otherwise its copies stay
#' boluses) while every other target receives a bolus copy. Use
#' [splitInfusionBolus()] to give the FIRST target the infusion
#' instead. This feeds one bolus dose record into both a bolus path
#' and an infusion path (Monolix-style double absorption with mixed
#' first- and zero-order routes). Steady-state bolus records are not
#' promoted; they are copied as boluses with a warning when the
#' designated infusion target declares `dur()`/`rate()`.
#'
#' Infusion records (data `RATE`/`DUR` or modeled `rate()`/`dur()`) are
#' split preserving their type, exactly like [splitInfusion()].
#'
#' This rewrite applies at [etTrans()] translation time only; doses
#' pushed while solving with [evid_()] are not split. Only one
#' splitting directive (`splitBolus()`, `splitInfusion()`,
#' `splitInfusionBolus()` or `splitBolusInfusion()`) is allowed per
#' model.
#'
#' There is no R function named `splitBolusInfusion()` exported by
#' rxode2; the directive is parsed from the model text directly, as
#' shown below.
#'
#' @section Usage inside a model:
#' ```
#' model({
#'   splitBolusInfusion(depot, depot2, central)
#'   dur(central) <- tk0
#'   f(depot2) <- 1 - f1
#'   f(central) <- f1
#'   d/dt(depot2) <- -ka2 * depot2
#'   d/dt(central) <- ka2 * depot2 - cl / v * central
#'   ...
#' })
#' ```
#' A bolus dose recorded against `depot` is split so `depot2` receives
#' the bolus (first-order input scaled by `1 - f1`) and `central`
#' (last target) receives a modeled-duration infusion (zero-order
#' input over `tk0`, scaled by `f1`).
#'
#' @examples
#' \donttest{
#' f <- function() {
#'   ini({
#'     tk0 <- 4; fr <- 0.7; ka2 <- 1.2; cl <- 0.1; v <- 10
#'   })
#'   model({
#'     splitBolusInfusion(depot, depot2, central)
#'     dur(central) <- tk0
#'     f(central) <- fr
#'     f(depot2) <- 1 - fr
#'     d/dt(depot) <- 0
#'     d/dt(depot2) <- -ka2 * depot2
#'     d/dt(central) <- ka2 * depot2 - cl / v * central
#'   })
#' }
#' # the depot bolus becomes a central infusion plus a depot2 bolus
#' rxSolve(f, et(amt = 100, cmt = "depot") |> et(0:24))
#' }
#' @name splitBolusInfusion-directive
#' @aliases splitBolusInfusion splitBolusInfusion()
NULL
