# Phase 2 of the linCmt() sensitivity-carry subsystem (see
# ~/.claude/plans/the-lincmt-solutions-are-calm-seahorse.md, "New subsystem:
# linCmt() sensitivity-carry"). Phase 1 (lincmt_sensitivity_carry_poc.R)
# validated the carry recurrence's MATH against a real rxSolve() to
# 3.08e-11 relative error vs FD-on-eta, but drove it entirely from R using
# standalone C++ prototypes (linCmtAlastTransitionMatrixProto,
# linCmtModelDouble) -- not through linCmtB()'s actual which1 dispatch with
# a live ind/rx_solve context.
#
# This script closes that gap: it adds per-subject storage
# (ind->linCmtCarryT, rxode2parseStruct.h) and THREE new which1 sentinels in
# production linCmtB() (src/linCmt.cpp):
#   which1 = -5  mutating advance: T_i = M_i %*% T_{i-1}, M_i recomputed
#                fresh from THIS row's own theta (m forward-mode passes),
#                stored back into ind->linCmtCarryT. Must be called exactly
#                once per row.
#   which1 = -7  additive carry: adds a caller-supplied local contribution
#                (dPredDTheta_i * dThetaDEta_i, computed by the caller) into
#                the stored cumulative carry -- called once per row, right
#                after -5, to reproduce the full recurrence
#                T_i = M_i %*% T_{i-1} + J_i * dThetaDEta_i.
#   which1 = -6  pure read of the stored cumulative T, no mutation.
#
# linCmtCarryLiveTest() (also new, src/linCmt.cpp) calls the REAL production
# linCmtB(rx_solve*, id, ...) function using rx_solve*/ind obtained from
# getRxSolve_() -- the SAME mechanism every other post-solve accessor in
# this package uses (rxData.cpp, rxSerialize.cpp) -- right after a genuine
# rxSolve() of the Phase 1 model, in the same R session. ind->idx/tprior are
# set per row to mirror what rxode2_df.cpp's own real per-row output-pass
# loop (line ~727 of that file) does before calling calc_lhs for row i.
# This is NOT a fabricated ind/rx_solve struct -- it is the real one left
# behind by a real solve, with only its own idx/tprior bookkeeping fields
# advanced by hand (matching a genuine re-query).
#
# One real gap found and worked around here (not a production bug, a
# testing-methodology mismatch): the -3/-4/-5 rate cache
# (ind->linCmtRateHist) is only ever WRITTEN while an idx is genuinely being
# solved for the FIRST time (ind->solvedIdx < idx). A standalone re-query
# issued from R in a SEPARATE .Call, well after the whole subject already
# finished solving (ind->solvedIdx permanently at its max), can never
# satisfy that condition, so the cache lookup returns NULL for every row --
# this would happen to which1=-3/-4 too in this same scenario, not just the
# new -5. Fixed by making the -5 branch fall back to the live
# ind->InfusionRate when the cache lookup misses (same intent already
# documented on linCmtBRateSlot() itself: "NULL is a defensive fallback ...
# not a case expected to occur" -- now actually handled). In the REAL
# calling convention this subsystem targets (embedded in a model's own
# calc_lhs, called immediately after that row's own value computation,
# Phase 3's job), the cache is always populated in time and this fallback
# never triggers.
#
# What Phase 2 still leaves for Phase 3: composing dPredDTheta_i *
# dThetaDEta_i is done in R below (via which1=-7, an explicit per-row
# "add this value" call) rather than automatically from generated model
# code -- that automatic composition is model-build-time detection +
# codegen wiring, Phase 3's task.

if (requireNamespace("devtools", quietly = TRUE) &&
      file.exists("DESCRIPTION") && file.exists("src/linCmt.cpp")) {
  devtools::load_all(".", quiet = TRUE)
} else {
  library(rxode2)
}
library(symengine)

set.seed(1)

## ---- 1. Same real model/grid as Phase 1 ----------------------------------
mod <- rxode2({
  cl <- tcl * (wt / 70)^0.75 * exp(eta.cl)
  v  <- tv
  cp <- linCmt()
})

tclVal <- 2.0
tvVal  <- 20.0
etaVal <- 0.3

doseTimes <- c(0, 12, 24, 36)
obsTimes  <- c(6, 18, 30, 42)

buildEv <- function() {
  doseDf <- data.frame(time = doseTimes, amt = 100, evid = 1, cmt = 1,
                       wt = ifelse(doseTimes < 24, 70, 90))
  obsDf  <- data.frame(time = obsTimes, amt = 0, evid = 0, cmt = 1,
                       wt = ifelse(obsTimes < 24, 70, 90))
  ev <- rbind(doseDf, obsDf)
  ev <- ev[order(ev$time), ]
  ev$id <- 1
  ev
}
ev <- buildEv()

solveIt <- function(etaVal) {
  rxSolve(mod, params = c(tcl = tclVal, tv = tvVal, eta.cl = etaVal),
          events = ev, returnType = "data.frame", addDosing = TRUE)
}

real0 <- solveIt(etaVal)
grid <- real0[order(real0$time), c("time", "evid", "amt", "wt")]
nRows <- nrow(grid)

clAt <- function(wt_i) tclVal * (wt_i / 70)^0.75 * exp(etaVal)

## dTheta_i/dEta via real symbolic differentiation (symengine), same as
## Phase 1.
clSym      <- S(sprintf("%.15g*(wt/70)^0.75*exp(etacl)", tclVal))
dClDEtaSym <- D(clSym, "etacl")
dClDEta <- function(wt_i) {
  eval(parse(text = as.character(dClDEtaSym)), envir = list(wt = wt_i, etacl = etaVal))
}

## ---- 2. Drive T_i = M_i %*% T_{i-1} + J_i*dThetaDEta_i through the REAL
## just-completed solve, one row per .Call so -5 then -7 run strictly in
## order for that row before the next row's -5 runs. (getRxSolve_() returns
## THIS solve's rx_solve*, since nothing else has solved since.)
predSensLive <- rep(NA_real_, nRows)
rawAlast <- 0.0

for (i in seq_len(nRows)) {
  wt_i <- grid$wt[i]
  cl_i <- clAt(wt_i)
  amt_i <- if (grid$evid[i] != 0) grid$amt[i] else 0

  if (i == 1) {
    rawAlast <- rawAlast + amt_i
    ## T_0 = 0 (Alast starts at 0) -- iniSubject()'s own reset; nothing to do.
  } else {
    dtPrev <- grid$time[i] - grid$time[i - 1]

    ## J_i via real production forward-mode Jacobian (sensType=30), entering
    ## from a CLEAN (non-cumulative) raw Alast -- same convention as Phase 1.
    nAlast <- 1 + 1 * 2  # ncmt + oral0 + ncmt*npars, npars=2 for 1cmt-iv
    alast0 <- c(rawAlast, numeric(nAlast - 1))
    res <- linCmtModelDouble(dt = dtPrev,
                             p1 = cl_i, v1 = tvVal, p2 = 0, p3 = 0, p4 = 0, p5 = 0, ka = 0,
                             alastNV = alast0, rateNV = 0,
                             ncmt = 1L, oral0 = 0L, trans = 1L,
                             deriv = TRUE, type = 0L, tau = 0, tinf = 0, amt = 0,
                             bolusCmt = 0L, ndiff = 0L, sensType = 30L)
    Ji <- res$J[1, 1]
    valBeforeDose <- as.numeric(res$Alast)[1]
    localContrib <- Ji * dClDEta(wt_i)

    ## Single .Call: which1=-5 (multiply-carry) THEN which1=-6 THEN
    ## which1=-7 (add local contribution) for THIS row, strictly ordered.
    liveOut <- linCmtCarryLiveTest(
      id = 0L, t = c(grid$time[i], grid$time[i]),
      tPrior = c(grid$time[i - 1], grid$time[i - 1]),
      p1 = c(cl_i, cl_i), v1 = tvVal,
      ncmt = 1L, oral0 = 0L, trans = 1L,
      which1 = c(-5L, -7L), which2 = c(0L, 0L),
      addVal = c(0, localContrib))

    rawAlast <- valBeforeDose
  }

  if (amt_i != 0 && i > 1) rawAlast <- rawAlast + amt_i

  if (grid$evid[i] == 0) {
    sNow <- linCmtCarryLiveTest(id = 0L, t = grid$time[i], tPrior = grid$time[i],
                                p1 = cl_i, v1 = tvVal,
                                ncmt = 1L, oral0 = 0L, trans = 1L,
                                which1 = -6L, which2 = 0L)
    predSensLive[i] <- sNow / tvVal
  }
}

cat("Carried d(pred)/d(eta) at observation rows, via REAL which1=-5/-6/-7:\n")
print(data.frame(time = grid$time, predSensLive)[grid$evid == 0, ])

## ---- 3. Ground truth: FD on eta via a REAL re-solve ----------------------
h <- 1e-5
realP <- solveIt(etaVal + h)
realM <- solveIt(etaVal - h)
fdSens <- (realP$cp[realP$evid == 0] - realM$cp[realM$evid == 0]) / (2 * h)

liveAtObs <- predSensLive[grid$evid == 0]
cmp <- data.frame(time = grid$time[grid$evid == 0], live = liveAtObs, fd = fdSens,
                  absDiff = abs(liveAtObs - fdSens),
                  relDiff = abs(liveAtObs - fdSens) / (abs(fdSens) + 1e-8))
cat("\n=== Comparison: REAL which1=-5/-6/-7 carried recurrence vs FD-on-eta ===\n")
print(cmp)

worst <- max(cmp$relDiff)
cat(sprintf("\nWorst relative difference: %.3e -- %s\n", worst,
            if (worst < 1e-3) "PASS (live production carry path matches FD-on-eta)" else "FAIL"))
