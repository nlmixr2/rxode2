#' Push a future dose or observation event from within an rxode2 model
#'
#' @description
#' `evid_()` is a model-only function that schedules a future dosing or
#' observation event during ODE solving.  It is evaluated at each output
#' time point and inserts the requested event into the individual's event
#' timeline for future processing.
#'
#' This function has no meaning outside an rxode2 model block and will
#' throw an error if called directly from R.
#'
#' @param time Numeric. The time at which the event should occur.  Must
#'   be greater than the current model time `t`; events in the past are
#'   silently counted and a warning is issued after solving.
#'
#' @param evid Integer event ID.  Follows NONMEM/rxode2 conventions:
#'   \describe{
#'     \item{`0`}{Observation (adds an output row, no dose).}
#'     \item{`1`}{Dose (bolus or infusion depending on `rate`).}
#'     \item{`2`}{Other type observation (passed through).}
#'     \item{`3`}{Reset all compartments.}
#'     \item{`4`}{Reset then dose.}
#'     \item{`5`}{Replace compartment amount.}
#'     \item{`6`}{Multiply compartment amount.}
#'     \item{`7`}{Phantom/transit dose.}
#'     \item{`>= 100`}{Classic rxode2 internal evid; passed through verbatim.}
#'   }
#'
#' @param amt Numeric dose amount (for dose events) or `0` for
#'   observations.  When `rate > 0` this is interpreted as the total
#'   infusion amount and the infusion duration is `amt / rate`.
#'
#' @param cmt Integer compartment number (1-based) to which the dose is
#'   applied.  Default is 1
#' @param rate Numeric infusion rate.
#'   \describe{
#'     \item{`0`}{Bolus dose (default).}
#'     \item{`> 0`}{Fixed infusion rate; duration = `amt / rate`.}
#'     \item{`-1`}{Rate defined by the model (`rate_<cmt>` variable).}
#'     \item{`-2`}{Duration defined by the model (`dur_<cmt>` variable).}
#'   }
#' @param ii Numeric inter-dose interval.  Used together with `addl` to
#'   schedule repeat doses at `time`, `time + ii`, `time + 2*ii`, ...,
#'   `time + addl*ii`.  Also required when `ss > 0`. Default 0
#' @param addl Integer number of *additional* doses beyond the first.
#'   The total number of doses pushed is `addl + 1`, spaced `ii` apart.
#'   Each dose is pushed as a standalone event (not as a periodic
#'   schedule in the event table).
#' @param ss Integer steady-state flag applied to the *first* dose only
#'   (`addl` repetitions always use `ss = 0`).
#'   \describe{
#'     \item{`0`}{No steady-state (default).}
#'     \item{`1`}{Steady-state additive: add SS solution to current state.}
#'     \item{`2`}{Steady-state replace: replace current state with SS solution.}
#'   }
#'
#' @return This function is only meaningful inside an rxode2 model; it
#'   returns `NULL` invisibly if called from R directly (after signaling
#'   an error).
#'
#' @details
#' ## Behavior inside a model
#'
#' `evid_()` is evaluated at every output time point (when the solver is
#' exactly at a scheduled event time).  The pushed event is inserted into
#' the individual's event timeline and the solver visits it at the
#' specified future time.
#'
#' The number of events that may be pushed per individual is limited by
#' the `maxExtra` argument of [rxSolve()].  When `maxExtra = 0`
#' (the default) there is no limit.  Exceeding the limit causes an
#' error.
#'
#' Past-time pushes (where `time <= t`) are silently ignored and counted;
#' a warning is issued after solving.
#'
#' ## Relationship to NONMEM event columns
#'
#' The argument order and names mirror the standard NONMEM dataset
#' columns: `TIME`, `EVID`, `AMT`, `CMT`, `RATE`, `II`, `ADDL`, `SS`.
#'
#' @seealso [rxSolve()] for the `maxExtra` control argument.
#'
#' @examples
#' \donttest{
#' # Push a single bolus of 50 mg to compartment 1 at t + 12
#' m <- rxode2({
#'   d/dt(depot)   <- -ka * depot
#'   d/dt(central) <- ka * depot - cl / vd * central
#'   cp <- central / vd
#'   if (t < 24) {
#'     evid_(t + 12, 1, 50, 1, 0, 0, 0, 0)
#'   }
#' })
#'
#' # Push three boluses (addl = 2) at t+6, t+18, t+30 (ii = 12)
#' m2 <- rxode2({
#'   d/dt(depot)   <- -ka * depot
#'   d/dt(central) <- ka * depot - cl / vd * central
#'   cp <- central / vd
#'   if (t < 1) {
#'     evid_(t + 6, 1, 50, 1, 0, 12, 2, 0)
#'   }
#' })
#' }
#'
#' @export
evid_ <- function(time, evid, amt, cmt=1, rate=0, ii=0, addl=0, ss=0.0) {
  stop("'evid_()' can only be used inside an rxode2 model block", call. = FALSE)
}

#' @export
#' @keywords internal
#' @rdname rxUdfUi
rxUdfUi.evid_ <- function(fun) {
  .dummy <- function(time, evid, amt, cmt, rate, ii, addl, ss) {}
  .mc <- match.call(.dummy, fun)
  .time    <- deparse1(.mc$time)
  .evid <- deparse1(.mc$evid)
  .amt   <- deparse1(.mc$amt)

  .cmt  <- deparse1(.mc$cmt)
  if (.cmt == "NULL") {
    .cmt <- "1"
  }

  .rate   <- deparse1(.mc$rate)
  if (.rate == "NULL") {
    .rate <- "0"
  }

  .ii   <- deparse1(.mc$ii)
  if (.ii == "NULL") {
    .ii <- "0"
  }

  .addl   <- deparse1(.mc$addl)
  if (.addl == "NULL") {
    .addl <- "0"
  }

  .ss   <- deparse1(.mc$ss)
  if (.ss == "NULL") {
    .ss <- "0"
  }

  list(replace = paste0("evid_(", .time, ",", .evid, ",", .amt, ",", .cmt,
                        ",", .rate, ",", .ii, ",", .addl, ",", .ss, ")"))
}

#' @export
#' @keywords internal
#' @rdname rxUdfUi
rxUdfUi.bolus <- function(fun) {
  .dummy <- function(amt, cmt, ii, addl, ss) {}
  .mc <- match.call(.dummy, fun)
  .amt   <- deparse1(.mc$amt)

  .cmt  <- deparse1(.mc$cmt)
  if (.cmt == "NULL") {
    .cmt <- "1"
  }

  .ii   <- deparse1(.mc$ii)
  if (.ii == "NULL") {
    .ii <- "0"
  }

  .addl   <- deparse1(.mc$addl)
  if (.addl == "NULL") {
    .addl <- "0"
  }

  .ss   <- deparse1(.mc$ss)
  if (.ss == "NULL") {
    .ss <- "0"
  }

  list(replace = paste0("bolus(", .amt, ",", .cmt, ",", .ii, ",", .addl, ",", .ss, ")"))
}
