# Dispatch/detection logic choosing among the linCmt() subject-AD prototypes
# in src/linCmt.cpp (see bench/lincmt_subject_ad_proto.R for the prototypes
# themselves and ~/.claude/plans/the-lincmt-solutions-are-calm-seahorse.md
# for the full narrative). Two independent decisions, both made from a
# subject's own event timeline -- nothing here is wired into production yet;
# it operates on the same canonical event-list shape a caller would build
# from event-table data.
#
# Canonical subject event list (a data.frame or equivalent list of columns):
#   time  -- absolute event time
#   evid  -- 0 = observation, 1 = dose (bolus or infusion start)
#   amt   -- dose amount (evid=1 rows only)
#   dur   -- dose duration, 0 = bolus, >0 = infusion (evid=1 rows only)
#   theta -- a matrix, one row per EVENT, columns = the linCmt() macro
#            parameters (p1, v1, ...) as they would be evaluated for that
#            row (post-covariate-formula). A subject with no time-varying
#            covariate has every row identical.
#
# Usage:
#   source("bench/lincmt_subject_ad_dispatch.R")
#   chooseLinCmtSubjectADStrategy(time, evid, thetaMat)                  # defaults
#   chooseLinCmtSubjectADStrategy(time, evid, thetaMat,
#     control = linCmtSubjectADControl(maxDosesInPhase2 = 8))            # tuned
#   chooseLinCmtSubjectADStrategy(time, evid, thetaMat,
#     interpMethod = "linear")                                          # errors if theta varies
#   runDispatchTests()   # validates the dispatcher's choices are both
#                         # sensible AND correct (matches the oracle) across
#                         # a battery of canonical subject shapes
#
# Every tunable threshold is bundled into linCmtSubjectADControl() (named
# arguments with defaults, matching this codebase's foceiControl()/
# rxControl() convention) rather than scattered as bare defaults on helper
# functions -- these are the knobs that should become real rxControl()/
# rxSolve() options once this dispatcher is wired into production, so a user
# can tune the phase-2 dose-count cap, the superposition-fallback ceiling, or
# the time-varying-covariate detection tolerance for their own data.
#
# Covariate interpolation ("locf"/"nocb"/"midpoint"/"linear", matching
# rxode2's own covsInterpolation option): only "linear" is unsupported for a
# covariate that actually varies -- see checkCovariateInterpolationSupported()
# for why locf/nocb/midpoint are all exactly representable (piecewise
# constant between covariate records) while linear is not (genuinely
# continuous, and linCmt() only ever samples once per dose/observation row).

source(file.path("bench", "lincmt_subject_ad_proto.R"))

# ---------------------------------------------------------------------------
# Control object for every tunable in this dispatcher, matching this
# codebase's foceiControl()/rxControl() convention: named arguments with
# defaults, bundled into one object threaded through the dispatch functions,
# rather than bare magic numbers buried in helper-function signatures. This
# is the shape these knobs should take as real rxControl()/rxSolve() options
# once the dispatcher is wired into production (e.g.
# rxControl(linCmtHybridMaxDosesPhase2 = 5, ...)) -- kept as a plain list
# here since no such option exists yet, but naming and grouping already
# anticipate that.
# ---------------------------------------------------------------------------
linCmtSubjectADControl <- function(maxDosesInPhase2 = 5,
                                   supersededDoseCountCeiling = 30,
                                   timeVaryingRelTol = 1e-8) {
  list(maxDosesInPhase2 = maxDosesInPhase2,
      supersededDoseCountCeiling = supersededDoseCountCeiling,
      timeVaryingRelTol = timeVaryingRelTol)
}

# ---------------------------------------------------------------------------
# Decision 1: does this subject have a time-varying covariate on a linCmt()
# parameter? Checked on the ACTUAL per-row theta values (post-covariate-
# formula), not the raw covariate column -- what matters is whether the
# parameter itself changes, not whether some covariate happens to move while
# canceling out in the formula.
# ---------------------------------------------------------------------------
detectTimeVaryingTheta <- function(thetaMat, control = linCmtSubjectADControl()) {
  thetaMat <- as.matrix(thetaMat)
  if (nrow(thetaMat) <= 1) {
    return(list(varies = FALSE, ranges = rep(0, ncol(thetaMat))))
  }
  ranges <- apply(thetaMat, 2, function(col) diff(range(col)))
  scale <- pmax(1e-12, apply(thetaMat, 2, function(col) max(abs(col))))
  varies <- any(ranges > control$timeVaryingRelTol * scale)
  list(varies = varies, ranges = ranges, whichCols = which(ranges > control$timeVaryingRelTol * scale))
}

# ---------------------------------------------------------------------------
# Covariate interpolation method vs. what a closed-form linCmt() solve can
# actually represent. rxode2 supports four methods (confirmed in
# ~/src/rxode2/src/approx.cpp's rx_approxP(), R/rxsolve.R:385-410,1186,1449):
# "linear"=0, "locf"=1 (default), "nocb"=2, "midpoint"=3.
#
# linCmt() only ever samples a covariate ONCE per dose/observation row, at
# that row's own exact instant (no solver sub-stepping the way an ODE model
# gets, which is why _update_par_ptr() being called continuously for an ODE
# model doesn't help linCmt() at all) -- so whatever single value comes back
# from that one sample is then treated as constant across the WHOLE elapsed
# interval to the next row.
#
# For "locf" (last recorded value), "nocb" (next recorded value), and
# "midpoint" (their average), that single per-row sample IS the exact,
# correct value for that row's own instant, AND the true covariate is
# genuinely piecewise-constant between covariate-recording times regardless
# of dose/observation row spacing -- so a single sample per row is exact, not
# an approximation, for all three. Only "linear" is a real problem: it means
# the covariate (and therefore theta, whenever the covariate enters theta's
# formula nonlinearly -- e.g. any allometric/power covariate effect, which is
# the overwhelmingly common case in practice) is supposed to vary
# CONTINUOUSLY between covariate-recording times. A single per-row sample
# then silently discretizes that continuous variation into a step function,
# with no warning today (confirmed: no gate exists, and no test checks
# linCmt() solve/gradient correctness under any interpolation method,
# tests/testthat/test-interp.R only checks parser-level tagging). A closed
# form for the resulting genuinely-time-varying-rate ODE does not exist in
# general (only in a narrow special case -- an additive, not power-law,
# covariate effect on a single-compartment model's rate constant, which
# reduces to a linearly-time-varying decay rate with its own closed form --
# not implemented here since it is not the common case).
#
# So: error clearly, rather than silently return a wrong gradient (or a wrong
# value), whenever a covariate that GENUINELY varies for this subject
# ("varies" from detectTimeVaryingTheta()) is configured for "linear"
# interpolation. "locf"/"nocb"/"midpoint" all proceed through the ordinary
# time-varying-covariate path with no special-casing needed.
# ---------------------------------------------------------------------------
checkCovariateInterpolationSupported <- function(interpMethod, varies) {
  interpMethod <- tolower(as.character(interpMethod))
  if (!varies) return(invisible(TRUE)) # constant theta -- interpolation method is moot
  if (interpMethod %in% c("linear", "0")) {
    stop(paste0(
      "linCmt() does not support 'linear' covariate interpolation for a ",
      "covariate that actually varies across this subject's records: ",
      "linCmt() samples a covariate once per dose/observation row and treats ",
      "it as constant across the whole elapsed interval to the next row, ",
      "which is exact for 'locf'/'nocb'/'midpoint' (all genuinely piecewise-",
      "constant) but not for 'linear' (genuinely continuous between ",
      "covariate records) -- this combination is not supported on the ",
      "rxode2 side. Use covsInterpolation='locf'/'nocb'/'midpoint', or solve ",
      "this model as an ODE (d/dt()) instead of linCmt()."),
      call. = FALSE)
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Decision 2 (only reached when theta is constant): dose-heavy vs
# observation-heavy phase structure -> which AD strategy to use.
#
# Strategy: push as many of the EARLIEST doses as possible into a forward-
# mode roll-through "phase 1" (its cost is flat regardless of dose count),
# leaving at most `maxDosesInPhase2` of the LATEST doses for a nested-
# superposition "phase 2" (whose cost scales with doses-per-observation, so
# it needs that count kept small). maxDosesInPhase2 is a count, not a time
# threshold, because superposition's cost is driven by how many independent
# terms sum per observation, not by elapsed time.
#
# Precondition the hybrid split needs and the naive version does NOT relax:
# linCmtSubjectHybridDoseObsADProto's phase 1 only returns the phase's FINAL
# state -- it does not (yet) extract gradients at observations that fall
# inside phase 1. So a split is only valid if no observation lands at or
# before the chosen split point. If one does, this falls back to
# superposition-only (still fine unless nDoses is large enough that
# superposition's own worst case -- doses accumulating as fast as
# observations -- would apply) or forward-only as the safe, always-correct
# baseline.
# ---------------------------------------------------------------------------
chooseDoseObsStrategy <- function(time, evid, control = linCmtSubjectADControl()) {
  doseIdx <- which(evid == 1)
  obsIdx <- which(evid == 0)
  nDoses <- length(doseIdx)
  maxDosesInPhase2 <- control$maxDosesInPhase2
  supersededDoseCountCeiling <- control$supersededDoseCountCeiling

  if (nDoses == 0) {
    return(list(strategy = "superposition", reason = "no doses -- nothing to roll through"))
  }
  if (nDoses <= maxDosesInPhase2) {
    return(list(strategy = "superposition",
               reason = sprintf("only %d doses total, already <= the phase-2 cap (%d)", nDoses, maxDosesInPhase2)))
  }

  k <- nDoses - maxDosesInPhase2 # doses 1..k -> phase 1; the rest -> phase 2
  splitTime <- time[doseIdx[k]]

  obsAtOrBeforeSplit <- obsIdx[time[obsIdx] <= splitTime + 1e-9]
  if (length(obsAtOrBeforeSplit) > 0) {
    if (nDoses <= supersededDoseCountCeiling) {
      return(list(strategy = "superposition",
                 reason = sprintf(
                   "%d observation(s) fall inside the would-be dose-heavy phase 1 -- hybrid's phase 1 can't extract gradients there yet; %d doses is still <= the ceiling (%d) for superposition alone",
                   length(obsAtOrBeforeSplit), nDoses, supersededDoseCountCeiling)))
    }
    return(list(strategy = "forward",
               reason = sprintf(
                 "%d observation(s) fall inside the would-be dose-heavy phase 1, AND %d doses exceeds the ceiling (%d) for superposition alone -- neither specialized strategy applies safely",
                 length(obsAtOrBeforeSplit), nDoses, supersededDoseCountCeiling)))
  }

  list(strategy = "hybrid", splitTime = splitTime, splitDoseIdx = k,
      reason = sprintf("%d doses collapse into a flat-cost phase-1 roll-through; %d remain for phase 2 (cap %d), with no phase-1 observations to worry about",
                       k, nDoses - k, maxDosesInPhase2))
}

# ---------------------------------------------------------------------------
# Combined dispatcher. `control` bundles every tunable (see
# linCmtSubjectADControl()) -- pass a customized one to tune the phase-2
# dose-count cap, the superposition-fallback ceiling, or the time-varying
# detection tolerance without touching this function's body.
#
# `interpMethod` (default "locf", rxode2's own default) is the covariate
# interpolation method in force for whichever covariate(s) feed thetaMat's
# varying column(s) -- pass the real per-model/per-covariate setting once
# this is wired to actual model metadata. Checked BEFORE any strategy is
# chosen: an unsupported "linear" combination errors immediately rather than
# silently returning a wrong gradient or value from any strategy below.
# ---------------------------------------------------------------------------
chooseLinCmtSubjectADStrategy <- function(time, evid, thetaMat,
                                          control = linCmtSubjectADControl(),
                                          interpMethod = "locf") {
  tv <- detectTimeVaryingTheta(thetaMat, control)
  checkCovariateInterpolationSupported(interpMethod, tv$varies)
  if (tv$varies) {
    return(list(strategy = "etaCovariate", timeVarying = tv,
               reason = "time-varying covariate detected on column(s) in thetaMat",
               caveat = paste("only the 1-cmt-IV single-eta prototype",
                              "(linCmtSubjectReverseADEtaCovariateProto /",
                              "linCmtSubjectForwardADEtaCovariateProto) currently",
                              "implements the correct cumulative-sensitivity fix;",
                              "other geometries have no shipped fix yet -- see",
                              "project_lincmt_timevarying_covariate_bug")))
  }
  chooseDoseObsStrategy(time, evid, control)
}

# ---------------------------------------------------------------------------
# Validation: build canonical subject shapes, dispatch, RUN whichever
# strategy was chosen, and confirm the result matches the sequential oracle.
# This is the check that matters -- picking a "sensible-looking" strategy
# name is not enough; the dispatched call has to actually reproduce the
# right numbers.
# ---------------------------------------------------------------------------
.runChosenStrategy <- function(cfg, time, evid, amt, dur, choice) {
  nAlast <- .linCmtNalast(cfg$ncmt, cfg$oral0)
  cfgR <- list(p1 = cfg$p1, v1 = cfg$v1, p2 = cfg$p2, p3 = cfg$p3, p4 = cfg$p4, p5 = cfg$p5,
              ka = cfg$ka, rate = rep(0, cfg$nstate),
              ncmt = cfg$ncmt, oral0 = cfg$oral0, trans = cfg$trans)
  obsIdx <- which(evid == 0)

  if (choice$strategy == "forward") {
    # Exactly .oracleWalk()'s own convention (evolve to time[i] first, then
    # apply a bolus AT time[i]; an infusion evolves WITH its rate active for
    # the whole step instead) -- just using the production forward default
    # (sensType=30) instead of the oracle's reverse reference.
    alast <- numeric(nAlast)
    tPrev <- 0
    out <- list()
    for (i in seq_along(time)) {
      if (evid[i] == 1 && dur[i] > 0) {
        rOn <- cfgR; rOn$rate <- { r <- rep(0, cfg$nstate); r[1] <- amt[i] / dur[i]; r }
        s <- .linCmtCall(time[i] - tPrev, rOn, alast, sensType = 30L)
        alast <- s$Alast; tPrev <- time[i]
      } else {
        s <- .linCmtCall(time[i] - tPrev, cfgR, alast, sensType = 30L)
        alast <- s$Alast
        if (evid[i] == 1) alast[1] <- alast[1] + amt[i]
        tPrev <- time[i]
        if (evid[i] == 0) out[[length(out) + 1]] <- s$J
      }
    }
    return(out)
  }

  if (choice$strategy == "superposition") {
    obsT <- time[obsIdx]
    doseIdx <- which(evid == 1)
    doseT <- time[doseIdx]; doseAmt <- amt[doseIdx]; doseDur <- dur[doseIdx]
    proto <- .Call(`_rxode2_linCmtSubjectSuperpositionADProto`,
                   obsT, doseT, doseAmt, doseDur,
                   cfg$p1, cfg$v1, cfg$p2, cfg$p3, cfg$p4, cfg$p5, cfg$ka,
                   cfg$ncmt, cfg$oral0, cfg$trans, 0L)
    return(lapply(proto, function(x) x$J))
  }

  if (choice$strategy == "hybrid") {
    doseIdx <- which(evid == 1)
    splitK <- choice$splitDoseIdx
    p1DoseIdx <- doseIdx[seq_len(splitK)]
    p2DoseIdx <- doseIdx[-seq_len(splitK)]
    splitTime <- choice$splitTime # == time[p1DoseIdx[splitK]] by construction

    # linCmtSubjectHybridDoseObsADProto's phase-1 loop ADDS phase1Amt[iv]
    # THEN evolves by phase1Dt[iv] -- so phase1Dt[iv] is the gap AFTER dose
    # iv to the NEXT recorded point, not before it. Pairing dose k with the
    # gap that precedes it (rather than follows it) is exactly the timing
    # bug this session hit twice already with non-uniform dose spacing; a
    # uniform-spacing test (as in .checkHybridDoseObs) can't detect it since
    # every dose/gap is numerically interchangeable there.
    phase1Amt <- amt[p1DoseIdx]
    phase1Dt <- diff(c(time[p1DoseIdx], splitTime)) # last entry is 0 since splitTime == time[p1DoseIdx[splitK]]
    phase1Rate <- ifelse(dur[p1DoseIdx] > 0, amt[p1DoseIdx] / pmax(dur[p1DoseIdx], 1e-12), 0)
    # (bolus-vs-infusion timing within phase 1 simplified the same way as
    # elsewhere in this file: infusion duration folded into the single dt
    # step to the next event -- adequate for this battery's bolus-only
    # scenarios; a phase-1 infusion through this GENERIC glue would need the
    # same rate-on/rate-off step-splitting .checkHybridDoseObsInfusion uses.)
    obsT <- time[obsIdx] - splitTime
    doseT <- time[p2DoseIdx] - splitTime
    doseAmt <- amt[p2DoseIdx]; doseDur <- dur[p2DoseIdx]

    proto <- .Call(`_rxode2_linCmtSubjectHybridDoseObsADProto`,
                   phase1Dt, phase1Amt, phase1Rate,
                   obsT, doseT, doseAmt, doseDur,
                   cfg$p1, cfg$v1, cfg$p2, cfg$p3, cfg$p4, cfg$p5, cfg$ka,
                   cfg$ncmt, cfg$oral0, cfg$trans, 0L)
    return(lapply(proto, function(x) x$J))
  }

  stop("unhandled strategy: ", choice$strategy)
}

.oracleWalk <- function(cfg, time, evid, amt, dur) {
  nAlast <- .linCmtNalast(cfg$ncmt, cfg$oral0)
  cfgR <- list(p1 = cfg$p1, v1 = cfg$v1, p2 = cfg$p2, p3 = cfg$p3, p4 = cfg$p4, p5 = cfg$p5,
              ka = cfg$ka, rate = rep(0, cfg$nstate),
              ncmt = cfg$ncmt, oral0 = cfg$oral0, trans = cfg$trans)
  alast <- numeric(nAlast)
  tPrev <- 0
  out <- list()
  for (i in seq_along(time)) {
    if (evid[i] == 1 && dur[i] > 0) {
      rOn <- cfgR; rOn$rate <- { r <- rep(0, cfg$nstate); r[1] <- amt[i] / dur[i]; r }
      s <- .linCmtCall(time[i] - tPrev, rOn, alast, sensType = 3L)
      alast <- s$Alast; tPrev <- time[i]
    } else {
      s <- .linCmtCall(time[i] - tPrev, cfgR, alast, sensType = 3L)
      alast <- s$Alast
      if (evid[i] == 1) alast[1] <- alast[1] + amt[i]
      tPrev <- time[i]
      if (evid[i] == 0) out[[length(out) + 1]] <- s$J
    }
  }
  out
}

.checkDispatchScenario <- function(cfg, name, time, evid, amt, dur, expectStrategy = NULL) {
  npars <- length(c(cfg$p1, cfg$v1, cfg$p2, cfg$p3, cfg$p4, cfg$p5, cfg$ka))
  thetaRow <- c(cfg$p1, cfg$v1, cfg$p2, cfg$p3, cfg$p4, cfg$p5, cfg$ka)
  thetaMat <- matrix(rep(thetaRow, length(time)), nrow = length(time), byrow = TRUE)

  choice <- chooseLinCmtSubjectADStrategy(time, evid, thetaMat)
  strategyOk <- is.null(expectStrategy) || identical(choice$strategy, expectStrategy)
  message(sprintf("  [%s/%s] dispatcher chose: %-13s (%s)%s",
                  name, cfg$name, choice$strategy, choice$reason,
                  if (strategyOk) "" else sprintf("  ** expected %s **", expectStrategy)))

  oracleJ <- .oracleWalk(cfg, time, evid, amt, dur)
  gotJ <- .runChosenStrategy(cfg, time, evid, amt, dur, choice)
  worst <- 0
  for (i in seq_along(oracleJ)) worst <- max(worst, max(abs(oracleJ[[i]] - gotJ[[i]])))
  r <- .report(sprintf("dispatch[%s/%s]", name, cfg$name), worst)
  r$strategyOk <- strategyOk
  r$strategy <- choice$strategy
  r
}

runDispatchTests <- function() {
  configs <- .linCmtConfigs()[c(2, 4)] # 1cmt-oral, 2cmt-oral -- enough variety, keeps this fast
  results <- list()
  add <- function(r) results[[length(results) + 1]] <<- r

  for (cfg in configs) {
    # Scenario A: canonical hybrid shape -- 20 doses to steady state, then
    # dense observations, no more dosing.
    doseT <- seq(0, by = 0.5, length.out = 20)
    obsT <- max(doseT) + 0.3 * seq_len(15)
    time <- c(doseT, obsT); evid <- c(rep(1, 20), rep(0, 15))
    amt <- c(rep(100, 20), rep(0, 15)); dur <- rep(0, 35)
    ord <- order(time)
    add(.checkDispatchScenario(cfg, "doseHeavyThenObsHeavy", time[ord], evid[ord], amt[ord], dur[ord],
                               expectStrategy = "hybrid"))

    # Scenario B: few doses, many observations -- superposition's own sweet spot.
    doseT <- c(0)
    obsT <- 0.3 * seq_len(20)
    time <- c(doseT, obsT); evid <- c(1, rep(0, 20))
    amt <- c(100, rep(0, 20)); dur <- rep(0, 21)
    ord <- order(time)
    add(.checkDispatchScenario(cfg, "fewDosesManyObs", time[ord], evid[ord], amt[ord], dur[ord],
                               expectStrategy = "superposition"))

    # Scenario C: dense multi-dosing with an observation after EVERY dose --
    # no clean split exists (observations pervade any candidate phase 1) and
    # dose count is large -- should fall back to forward.
    n <- 40
    doseT <- 0.5 * seq_len(n) - 0.5
    obsT <- doseT + 0.25
    time <- as.vector(rbind(doseT, obsT))
    evid <- as.vector(rbind(rep(1, n), rep(0, n)))
    amt <- as.vector(rbind(rep(100, n), rep(0, n)))
    dur <- rep(0, 2 * n)
    add(.checkDispatchScenario(cfg, "denseInterleaved", time, evid, amt, dur,
                               expectStrategy = "forward"))

    # Scenario D: a handful of doses (<= maxDosesInPhase2), no rich
    # observation tail either -- should still just pick superposition (too
    # few doses to bother with a hybrid split).
    doseT <- c(0, 1, 2)
    obsT <- c(0.5, 1.5, 2.5, 3.5)
    time <- c(doseT, obsT); evid <- c(rep(1, 3), rep(0, 4))
    amt <- c(rep(100, 3), rep(0, 4)); dur <- rep(0, 7)
    ord <- order(time)
    add(.checkDispatchScenario(cfg, "fewDosesFewObs", time[ord], evid[ord], amt[ord], dur[ord],
                               expectStrategy = "superposition"))
  }

  # Scenario E: time-varying covariate detection (1cmt-iv only, matching the
  # eta-covariate prototypes' hard-coded scope).
  cfg <- .linCmtConfigs()[[1]]
  thetaMat <- matrix(c(1.0, 20, 1.6, 20), nrow = 2, byrow = TRUE) # CL steps 1.0 -> 1.6
  choice <- chooseLinCmtSubjectADStrategy(c(0, 1), c(1, 0), thetaMat)
  ok <- identical(choice$strategy, "etaCovariate")
  message(sprintf("  [timeVaryingDetect/%s] dispatcher chose: %-13s %s",
                  cfg$name, choice$strategy, if (ok) "-- PASS" else "-- FAIL (expected etaCovariate)"))
  add(list(name = "timeVaryingDetect", pass = ok, strategyOk = ok))

  # Scenario F: covariate-interpolation-method gate. "linear" + genuinely
  # varying theta must error; "locf"/"nocb"/"midpoint" + varying, and
  # "linear" + CONSTANT theta (moot -- nothing to interpolate), must all
  # proceed normally.
  errorsOnLinear <- tryCatch({
    chooseLinCmtSubjectADStrategy(c(0, 1), c(1, 0), thetaMat, interpMethod = "linear")
    FALSE
  }, error = function(e) grepl("linear", conditionMessage(e), fixed = TRUE))
  message(sprintf("  [interpMethod=linear, varies] %s", if (errorsOnLinear) "errored as expected -- PASS" else "FAIL (should have errored)"))
  add(list(name = "interpLinearErrors", pass = errorsOnLinear, strategyOk = errorsOnLinear))

  okOthers <- TRUE
  for (m in c("locf", "nocb", "midpoint")) {
    ok_m <- tryCatch({
      chooseLinCmtSubjectADStrategy(c(0, 1), c(1, 0), thetaMat, interpMethod = m)
      TRUE
    }, error = function(e) FALSE)
    message(sprintf("  [interpMethod=%s, varies] %s", m, if (ok_m) "proceeded as expected -- PASS" else "FAIL (should not have errored)"))
    okOthers <- okOthers && ok_m
  }
  add(list(name = "interpOthersProceed", pass = okOthers, strategyOk = okOthers))

  constantTheta <- matrix(c(1.0, 20, 1.0, 20), nrow = 2, byrow = TRUE)
  okConstantLinear <- tryCatch({
    chooseLinCmtSubjectADStrategy(c(0, 1), c(1, 0), constantTheta, interpMethod = "linear")
    TRUE
  }, error = function(e) FALSE)
  message(sprintf("  [interpMethod=linear, constant theta] %s", if (okConstantLinear) "proceeded as expected -- PASS" else "FAIL (should not have errored, nothing to interpolate)"))
  add(list(name = "interpLinearMootWhenConstant", pass = okConstantLinear, strategyOk = okConstantLinear))

  pass <- vapply(results, function(r) isTRUE(r$pass) && isTRUE(r$strategyOk), logical(1))
  message(sprintf("\n%d/%d dispatch checks passed (correctness AND strategy choice)", sum(pass), length(pass)))
  invisible(list(results = results, allPass = all(pass)))
}
