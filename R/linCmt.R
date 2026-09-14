#' Calculate the lambdas and coefficients of the two compartment model
#'
#' @param k10 elimination rate
#' @param k12 rate from central to peripheral compartment
#' @param k21 rate from peripheral to central compartment
#' @return List with `L` vector and matrices `C1` and `C2`
#' @export
#' @keywords internal
#' @author Matthew L. Fidler based on `wnl` package/paper, implemented
#'   in C/C++
#' @examples
#' .solComp2(k10=0.1, k12=3, k21=1)
.solComp2 <- function(k10, k12, k21) {
  checkmate::assertNumeric(k10, lower=0, len=1, any.missing=FALSE)
  checkmate::assertNumeric(k12, lower=0, len=1, any.missing=FALSE)
  checkmate::assertNumeric(k21, lower=0, len=1, any.missing=FALSE)
  rxode2lincmt::solComp2(k10, k12, k21)
}
#' Calculate the lambdas and coefficients of the three compartment model
#'
#' @inheritParams .solComp2
#' @param k13 rate from central to peripheral compartment #2
#' @param k31 rate from peripheral compartment #2 to central
#' @return List with `L` vector and matrices `C1`, `C2` and `C3`
#' @export
#' @author Matthew L. Fidler
#' @keywords internal
#' @examples
#' .solComp3(k10=0.1, k12=3, k21=1, k13=2, k31=0.5)
.solComp3 <- function(k10, k12, k21, k13, k31) {
  checkmate::assertNumeric(k10, lower=0, len=1, any.missing=FALSE)
  checkmate::assertNumeric(k12, lower=0, len=1, any.missing=FALSE)
  checkmate::assertNumeric(k21, lower=0, len=1, any.missing=FALSE)
  checkmate::assertNumeric(k13, lower=0, len=1, any.missing=FALSE)
  checkmate::assertNumeric(k31, lower=0, len=1, any.missing=FALSE)
  rxode2lincmt::solComp3(k10, k12, k21, k13, k31)
}

#' Force the delta-keyed exponential memo on or off (tests/benchmarks)
#'
#' @param on integer: 1 forces the memo on, 0 forces it off, -1 (the
#'   default) follows the RX_LINCMT_DELTA_MEMO environment latch read at
#'   window-fill time
#' @return the previous setting, invisibly usable to restore it
#' @keywords internal
#' @export
linCmtDeltaMemo <- function(on = -1L) {
  rxode2lincmt::.linCmtDeltaMemo(on)
}

#' Read (and optionally reset) the amortized linCmt() sequential counters
#'
#' @param reset logical; when TRUE zero the counters after reading
#' @return named integer vector: windows (window-constant recomputations),
#'   seqTailRows (rows evaluated from the window's dt-dependent tail),
#'   seqFullRows (rows that fell back to the full forward evaluator),
#'   valueCompute (value executions that solved the row),
#'   valueRestore (value executions that restored an already-solved row),
#'   memoHit (value executions short-circuited by the last-row memo),
#'   valueLite (already-solved value re-executions served by the thin
#'   fx-plus-scaling path with the Jacobian restore skipped),
#'   expBuild (delta-keyed exponential-memo builds: one per distinct row
#'   gap per theta window), expHit (rows whose exponentials came from the
#'   delta memo; disable with RX_LINCMT_DELTA_MEMO=off), expSolo (of
#'   those builds, the ones that went to the within-row slot the guard
#'   keeps serving after it stops speculating), dualRows (rows
#'   whose tail took one multi-direction pass, linCmtSensType="ADm"),
#'   phiAnalyticRows (rows propagated through the closed-form transition
#'   matrix; RX_LINCMT_PHI=2)
#' @keywords internal
#' @export
linCmtSeqStats <- function(reset = FALSE) {
  rxode2lincmt::.linCmtSeqStats(reset)
}

#' Toggle the linCmt() carry-advance runtime fast path (test/benchmark hook)
#'
#' @param enable logical; new state
#' @return the previous state, invisibly
#' @keywords internal
#' @export
linCmtCarrySetFast <- function(enable) {
  rxode2lincmt::.linCmtCarrySetFast(enable)
}

#' Highest carry sentinel `linCmtB(which1 = -k)` this build understands
#'
#' nlmixr2est gates its carry codegen on this: `-8` (the fast-path pin an
#' event-modifier jump needs) is only emitted when the loaded rxode2 has it.
#' @return integer, the magnitude of the most negative carry sentinel
#' @keywords internal
#' @export
linCmtCarrySentinelMax <- function() {
  rxode2lincmt::.linCmtCarrySentinelMax()
}

#' Read (and optionally reset) the linCmt() carry-advance fast-path counters
#'
#' @param reset logical; when TRUE zero the counters after reading
#' @return named numeric vector: advCalls (every which1=-5 invocation),
#'   advFast (subset that took the constant-theta skip)
#' @keywords internal
#' @export
linCmtCarryFastStats <- function(reset = FALSE) {
  rxode2lincmt::.linCmtCarryFastStats(reset)
}

# Internal test/benchmark hooks into rxode2lincmt's per-row kernel
linCmtModelDouble <- function(dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0,
                              trans, deriv, type, tau, tinf, amt, bolusCmt, ndiff,
                              sensType = 3L, sensH = 0.001) {
  rxode2lincmt::linCmtModelDouble(dt, p1, v1, p2, p3, p4, p5, ka, alastNV, rateNV, ncmt, oral0,
                                  trans, deriv, type, tau, tinf, amt, bolusCmt, ndiff,
                                  sensType, sensH)
}

linCmtCarryLiveTest <- function(id, t, tPrior, theta, ncmt, oral0, trans, which1, which2,
                                addVal = NULL) {
  rxode2lincmt::.linCmtCarryLiveTest(id, t, tPrior, theta, ncmt, oral0, trans, which1, which2,
                                     addVal)
}

linCmtBSensTypesSeen <- function(reset) {
  rxode2lincmt::.linCmtBSensTypesSeen(reset)
}

linCmtBThreadsSeen <- function(reset) {
  rxode2lincmt::.linCmtBThreadsSeen(reset)
}
