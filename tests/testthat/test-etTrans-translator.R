## Record-for-record cross-check of the two event translators.
##
## etTrans() (src/etTran.cpp) and _rxTranslateOneEvent()
## (inst/include/rxode2EventTranslate.h, used by the runtime evid_() push
## path) implement the same NONMEM event semantics independently.  This test
## drives both over the same single events and compares the records they
## produce, so a divergence is caught here rather than as a wrong solve.
##
## Every difference that exists today is CLASSIFIED below.  A class listed in
## .etTransXKnownDiff() is asserted to still differ, so that fixing it fails
## this test and forces the entry to be removed -- the list may only shrink.

rxTest({

  ## Rules that belong to the CALLER, not to the translator, and so are
  ## excluded rather than classified:
  ##   - splitBolus is a post-pass over finished records on both sides
  ##     (etTran.cpp and _rxPushDose() both call the shared helpers), so a
  ##     splitBolus model would compare expanded output against unexpanded.
  ##   - etTran.cpp's first-record guards (a leading EVID=3 is dropped, a
  ##     leading EVID=4 becomes a plain dose) need to know the record's
  ##     position within the subject, which a single-event translator cannot.
  .models <- c("plain", "alag", "rateDur", "lin1")

  ## There are no known divergences left.  The three that existed when this
  ## test was written -- the EVID0_ONDOSE(60) companion for evid=2, a phantom
  ## dose keeping a modeled rate, and a steady-state dose into an alag()
  ## compartment (rxode2#1349) -- are all closed, so every comparable case
  ## must now agree exactly.  Anything added here must be justified, and the
  ## list may only shrink.
  .etTransXKnownDiff <- character(0)
  .etTransXClass <- function(cell, alag, ssAtDoseTime) ""

  .canonEt <- function(d) {
    .keep <- d$EVID >= 100 | d$EVID == 3
    d <- d[.keep, , drop = FALSE]
    .o <- order(d$TIME, -d$EVID)
    data.frame(evid = as.integer(d$EVID[.o]), time = as.numeric(d$TIME[.o]),
               amt = ifelse(d$EVID[.o] == 3, NA_real_, as.numeric(d$AMT[.o])),
               ii = as.numeric(d$II[.o]), row.names = NULL)
  }
  .canonHook <- function(h) {
    if (nrow(h) == 1L && !is.na(h$n[1]) && h$n[1] < 0) return("REJECT")
    h <- h[!is.na(h$k), , drop = FALSE]
    ## compare the internal (dose-shaped) records only, the same filter
    ## applied to the etTrans() side
    h <- h[h$evid >= 100 | h$evid == 3, , drop = FALSE]
    if (nrow(h) == 0L) return("EMPTY")
    .o <- order(h$time, -h$evid)
    data.frame(evid = as.integer(h$evid[.o]), time = as.numeric(h$time[.o]),
               amt = ifelse(h$evid[.o] == 3, NA_real_, as.numeric(h$amt[.o])),
               ii = as.numeric(h$ii[.o]), row.names = NULL)
  }
  .dosingName <- function(cell) {
    if (!is.null(cell$dose$rate)) paste0("rate", cell$dose$rate)
    else if (!is.null(cell$dose$dur)) paste0("dur", cell$dose$dur)
    else "bolus"
  }

  test_that("etTrans() and the shared translator agree record-for-record", {
    .modelSet <- .etTransGoldenModels()
    ## addl is a caller-side policy on both sides, and the position-dependent
    ## first-record guards are excluded above
    .cells <- Filter(function(ce) ce$addl == 0 && is.null(ce$time) &&
                       !grepl("^first", ce$nm), .etTransGridCells())
    .nAgree <- 0L
    .nKnown <- 0L
    .nEtErr <- 0L
    for (.mn in .models) {
      .mv <- .modelSet[[.mn]]
      .alagCmt <- if (is.null(.mv$alag)) integer(0) else as.integer(.mv$alag)
      for (.sat in c(TRUE, FALSE)) {
        for (.ce in .cells) {
          .d <- .etTransGridData(.ce)
          .d$addl <- 0
          .got <- withCallingHandlers(
            tryCatch(.canonEt(as.data.frame(
              etTrans(.d, .mv, keepDosingOnly = TRUE, addCmt = TRUE,
                      ssAtDoseTime = .sat))),
              error = function(e) "ERROR"),
            warning = function(w) invokeRestart("muffleWarning"))
          if (identical(.got, "ERROR")) {
            ## etTran.cpp validates against the model variables (compartment
            ## supports an infusion, replace/multiply is not an infusion,
            ## ...); the translator has no model to validate against, so
            ## these are compared through the golden harness instead
            .nEtErr <- .nEtErr + 1L
            next
          }
          .cmt <- abs(.ce$cmt)
          .alag <- as.integer(.cmt %in% .alagCmt)
          .rate <- if (!is.null(.ce$dose$rate)) .ce$dose$rate else
            if (!is.null(.ce$dose$dur)) .ce$dose$dur else 0
          .hook <- .canonHook(rxTranslateOneEvent_(
            ## the SIGN matters: a negative compartment is the turn-off signal
            time = 2, evid = as.integer(.ce$evid), cmt = as.integer(.ce$cmt),
            amt = .ce$amt, ii = .ce$ii, ss = as.integer(.ce$ss),
            rate = .rate, isDur = as.integer(!is.null(.ce$dose$dur)),
            ## etTran.cpp only treats a compartment as lagged for steady
            ## state under ssAtDoseTime, and so does the solve path (see
            ## rxLoadAlagCmt() in src/rxData.cpp)
            hasAlag = as.integer(.alag == 1L && .sat)))
          .cell <- list(evid = .ce$evid, ss = .ce$ss, ii = .ce$ii,
                        dosing = .dosingName(.ce))
          .class <- .etTransXClass(.cell, .alag == 1L, .sat)
          .lab <- paste(.mn, ifelse(.sat, "sat1", "sat0"), .ce$nm, sep = "/")
          if (.class %in% .etTransXKnownDiff) {
            ## deliberately still different -- when the gap is closed this
            ## fails and the class must be dropped from .etTransXKnownDiff
            expect_false(identical(.got, .hook), label = paste0(.lab, " [",
                                                                .class, "]"))
            .nKnown <- .nKnown + 1L
          } else {
            expect_identical(.got, .hook, label = .lab)
            .nAgree <- .nAgree + 1L
          }
        }
      }
    }
    ## Pin the shape of the comparison so a silent change in how many cases
    ## reach each branch is visible: of 4832 comparisons, 3996 agree exactly
    ## and 836 are etTrans()-side model validation.
    expect_identical(.nAgree + .nKnown + .nEtErr, 4832L)
    expect_identical(.nKnown, 0L)
    expect_gte(.nAgree, 3996L)
    expect_gte(.nEtErr, 836L)
  })
})
