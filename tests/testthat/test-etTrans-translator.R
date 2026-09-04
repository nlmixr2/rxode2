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

  ## Known, deliberate divergences at the time of writing.
  ##   ondose          evid=2 with a real cmt gets an extra EVID0_ONDOSE(60)
  ##                   "turn on" record from etTran.cpp only.
  ##   phantom-modeled evid=7 with a modeled rate/duration: etTran.cpp keeps
  ##                   the modeled rateI, the translator forces rateI 0.
  ##   ss-alag         a steady-state dose into a compartment with a modeled
  ##                   alag(): etTran.cpp rewrites flg 10->9 / 20->19 and
  ##                   expands to 2/3/4 records (rxode2#1349).
  .etTransXClass <- function(cell, alag, ssAtDoseTime) {
    .modeled <- cell$dosing %in% c("rate-1", "rate-2", "dur-1", "dur-2")
    if (cell$evid == 2) return("ondose")
    if (cell$evid == 7 && .modeled) return("phantom-modeled")
    ## flg 10/20 -- and so the 10->9 / 20->19 alag rewrite -- needs ii > 0;
    ## ss with ii == 0 is either a constant infusion (flg 40) or a plain
    ## dose.  etTran.cpp only rewrites under ssAtDoseTime (its default).
    if (ssAtDoseTime && cell$ss %in% c(1, 2) && cell$ii > 0 && alag &&
          cell$evid %in% c(1, 4, 5, 6)) {
      return("ss-alag")
    }
    ""
  }
  .etTransXKnownDiff <- c("ondose", "phantom-modeled", "ss-alag")

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
            time = 2, evid = as.integer(.ce$evid), cmt = as.integer(.cmt),
            amt = .ce$amt, ii = .ce$ii, ss = as.integer(.ce$ss),
            rate = .rate, isDur = as.integer(!is.null(.ce$dose$dur)),
            hasAlag = .alag))
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
    ## reach each branch is visible.  At the time of writing, of 4832
    ## comparisons: 2888 agree, 1108 are known divergences and 836 are
    ## etTrans()-side validation errors.  Closing a known gap moves cases
    ## from .nKnown into .nAgree, so only their sum is pinned exactly.
    expect_identical(.nAgree + .nKnown + .nEtErr, 4832L)
    expect_gte(.nAgree, 2888L)
    expect_gte(.nEtErr, 836L)
  })
})
