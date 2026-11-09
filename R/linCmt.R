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
  checkmate::assertNumeric(k10, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assertNumeric(k12, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assertNumeric(k21, lower = 0, len = 1, any.missing = FALSE)
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
  checkmate::assertNumeric(k10, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assertNumeric(k12, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assertNumeric(k21, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assertNumeric(k13, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assertNumeric(k31, lower = 0, len = 1, any.missing = FALSE)
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
linCmtModelDouble <- function(
  dt,
  p1,
  v1,
  p2,
  p3,
  p4,
  p5,
  ka,
  alastNV,
  rateNV,
  ncmt,
  oral0,
  trans,
  deriv,
  type,
  tau,
  tinf,
  amt,
  bolusCmt,
  ndiff,
  sensType = 3L,
  sensH = 0.001
) {
  rxode2lincmt::linCmtModelDouble(
    dt,
    p1,
    v1,
    p2,
    p3,
    p4,
    p5,
    ka,
    alastNV,
    rateNV,
    ncmt,
    oral0,
    trans,
    deriv,
    type,
    tau,
    tinf,
    amt,
    bolusCmt,
    ndiff,
    sensType,
    sensH
  )
}

linCmtCarryLiveTest <- function(id, t, tPrior, theta, ncmt, oral0, trans, which1, which2, addVal = NULL) {
  rxode2lincmt::.linCmtCarryLiveTest(id, t, tPrior, theta, ncmt, oral0, trans, which1, which2, addVal)
}

linCmtBSensTypesSeen <- function(reset) {
  rxode2lincmt::.linCmtBSensTypesSeen(reset)
}

linCmtBThreadsSeen <- function(reset) {
  rxode2lincmt::.linCmtBThreadsSeen(reset)
}

#' Variables the linCmt() parameters depend on
#'
#' Walks the normalized model back from the `linCmtA()`/`linCmtB()` arguments
#' through the assignments (and the `if()` conditions around them) that feed
#' them.  rxSolve() refuses "linear" interpolation only for a time-varying
#' covariate in this set.  The walk ignores statement order, so it may include
#' a variable whose assignment is overwritten (refusing more, never less).
#'
#' @param mv model variables
#' @return character vector of variable names, or `NULL` when the model
#'   cannot be walked (every covariate is then treated as a dependency)
#' @noRd
.rxLinCmtDepends <- function(mv) {
  tryCatch(
    {
      # braces let a normalized `else` start its own line
      .exprs <- str2lang(paste0("{\n", mv$model["normModel"], "\n}"))
      .lhs <- character(0)
      .rhs <- list()
      .seed <- character(0)
      .walk <- function(e, cond) {
        if (is.call(e)) {
          .f <- e[[1]]
          if (identical(.f, quote(`{`))) {
            for (.i in seq_along(e)[-1]) {
              .walk(e[[.i]], cond)
            }
            return(invisible())
          }
          if (identical(.f, quote(`if`))) {
            .c <- c(cond, all.vars(e[[2]]))
            .walk(e[[3]], .c)
            if (length(e) == 4L) {
              .walk(e[[4]], .c)
            }
            return(invisible())
          }
          if (identical(.f, quote(`while`))) {
            # a break under any if() in the body ends every later assignment
            .walk(e[[3]], c(cond, all.vars(e[[2]]), .rxLinCmtIfVars(e[[3]])))
            return(invisible())
          }
          if (identical(.f, quote(`=`)) || identical(.f, quote(`<-`)) || identical(.f, quote(`~`))) {
            .l <- e[[2]]
            .r <- if (length(e) == 3L) e[[3]] else NULL
            if (is.call(.l) && identical(.l[[1]], quote(`/`)) && identical(.l[[2]], quote(d))) {
              .l <- .l[[3]][[2]]
            }
            .lin <- .rxLinCmtCallVars(.r)
            .seed <<- c(.seed, .lin)
            if (is.name(.l)) {
              .lhs <<- c(.lhs, as.character(.l))
              .rhs[[length(.lhs)]] <<- unique(c(cond, all.vars(.r)))
            }
            return(invisible())
          }
          .seed <<- c(.seed, .rxLinCmtCallVars(e))
        }
        invisible()
      }
      .walk(.exprs, character(0))
      .need <- unique(.seed)
      repeat {
        .add <- unique(unlist(.rhs[.lhs %in% .need]))
        .new <- setdiff(.add, .need)
        if (length(.new) == 0L) {
          break
        }
        .need <- c(.need, .new)
      }
      .need
    },
    error = function(e) NULL
  )
}

#' Variables in every if() condition in `e`
#' @noRd
.rxLinCmtIfVars <- function(e) {
  if (!is.call(e)) {
    return(character(0))
  }
  .ret <- unlist(lapply(as.list(e)[-1], .rxLinCmtIfVars))
  if (identical(e[[1]], quote(`if`)) || identical(e[[1]], quote(`while`))) {
    .ret <- c(all.vars(e[[2]]), .ret)
  }
  unique(.ret)
}

#' Variables in the arguments of every linCmtA()/linCmtB() call in `e`
#' @noRd
.rxLinCmtCallVars <- function(e) {
  if (!is.call(e)) {
    return(character(0))
  }
  if (identical(e[[1]], quote(linCmtA)) || identical(e[[1]], quote(linCmtB))) {
    return(all.vars(e))
  }
  unlist(lapply(as.list(e)[-1], .rxLinCmtCallVars))
}
