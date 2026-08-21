# Phase 1 proof of concept for the linCmt() sensitivity-carry subsystem
# (see ~/.claude/plans/the-lincmt-solutions-are-calm-seahorse.md, "New
# subsystem: linCmt() sensitivity-carry").
#
# Claim under test: the discrete recurrence
#
#   s_i = T_i %*% s_{i-1} + J_i %*% dThetaDEta_i
#
# (s_i = d(Alast_i, raw state)/d(eta), T_i = the constant state-transition
# Jacobian d(Alast_i)/d(Alast_{i-1}) for THIS interval's own theta_i, J_i =
# d(Alast_i)/d(theta_i) holding the entering state fixed, dThetaDEta_i =
# d(theta_i)/d(eta) from the covariate formula) reconstructs the TRUE
# cumulative d(pred)/d(eta) for a linCmt() parameter driven by both an eta
# and a time-varying covariate -- the case production linCmtB() gets wrong
# today (see project_lincmt_timevarying_covariate_bug memory).
#
# Building blocks used, all real production/validated code, none invented:
#   - T_i:  linCmtAlastTransitionMatrixProto() -- the which1=-4 computation
#           (src/linCmt.cpp), already validated to machine epsilon against
#           linToOde() in the prior session. This is its FIRST use combined
#           with the other pieces below.
#   - J_i:  linCmtModelDouble(..., sensType=30) -- real production forward-
#           mode (fvar) Jacobian code, the actual sensType=30 default path.
#   - dThetaDEta_i: symengine's own symbolic differentiation (the same
#           engine nlmixr2est's R/d.R / R/symengine.R drive) of the
#           covariate formula, evaluated at each row's real covariate value.
#   - Ground truth: a REAL rxode2 solve (rxSolve()) of a linCmt()+covariate
#           model, perturbed on eta.cl via finite differences.
#
# Design note on why J_i is computed from a "clean" (non-cumulative) entering
# Alast rather than chaining linCmtModelDouble's own returned Alast forward:
# linCmtModelDouble's returned Alast packs BOTH the raw state AND internal
# reconstruction slots that production's own getAlastAD<T>() consumes on the
# NEXT call -- i.e. chaining it directly would silently re-introduce the very
# same-theta-assumed carry bug this experiment exists to bypass. Instead, the
# raw state trajectory (m values only) is propagated separately in R, and s_i
# (the sensitivity carry) is accumulated OUTSIDE via the recurrence above.

if (requireNamespace("devtools", quietly = TRUE) &&
      file.exists("DESCRIPTION") && file.exists("src/linCmt.cpp")) {
  devtools::load_all(".", quiet = TRUE)
} else {
  library(rxode2)
}
library(symengine)

set.seed(1)

## ---- 1. Real rxode2 model: linCmt() driven by eta.cl AND wt covariate ----
mod <- rxode2({
  cl <- tcl * (wt / 70)^0.75 * exp(eta.cl)
  v  <- tv
  cp <- linCmt()
})

tclVal <- 2.0
tvVal  <- 20.0
etaVal <- 0.3

## Dose/observation grid: bolus q12h into the central compartment, wt steps
## 70 -> 90 at t=24 (locf -- every row carries its OWN current wt, so locf is
## automatic/exact here, no interpolation ambiguity).
doseTimes <- c(0, 12, 24, 36)
obsTimes  <- c(6, 18, 30, 42)

buildEv <- function(etaVal) {
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
obsRows <- real0[real0$evid == 0, ]
cat("Real solve, obs rows (time, wt, cl, cp):\n")
print(obsRows[, c("time", "wt", "cl", "cp")])

## ---- 2. Discrete recurrence driven from real production building blocks --
## Merged event/interval grid: unique times, dose amt at dose rows, wt at
## each row read back from the REAL solve above (not hand-computed).
grid <- real0[, c("time", "evid", "amt", "wt")]
grid <- grid[order(grid$time), ]
nRows <- nrow(grid)

rawAlast <- 0.0        # raw physical central-compartment amount, scalar (1-cmt)
s <- 0.0               # carried sensitivity d(Alast_raw)/d(eta)
predSens <- rep(NA_real_, nRows)

for (i in seq_len(nRows)) {
  wt_i <- grid$wt[i]
  clExpr <- tclVal * (wt_i / 70)^0.75 * exp(etaVal)  # theta_i, real covariate value
  dtPrev <- if (i == 1) 0 else grid$time[i] - grid$time[i - 1]

  ## dTheta_i/dEta via REAL symbolic differentiation (symengine), evaluated
  ## at this row's real wt and eta.
  clSym <- S(sprintf("%.15g*(wt/70)^0.75*exp(etacl)", tclVal))
  dClDEtaSym <- D(clSym, "etacl")
  dClDEta_i <- eval(parse(text = as.character(dClDEtaSym)),
                    envir = list(wt = wt_i, etacl = etaVal))

  amt_i <- if (grid$evid[i] != 0) grid$amt[i] else 0

  if (i == 1) {
    ## First row: no prior interval to transition from. Just apply the dose
    ## (if any); s stays 0 (nothing upstream of eta yet).
    rawAlast <- rawAlast + amt_i
  } else {
    ## T_i: production which1=-4 equivalent, using THIS interval's own theta.
    Ti <- linCmtAlastTransitionMatrixProto(
      p1 = clExpr, v1 = tvVal, p2 = 0, p3 = 0, p4 = 0, p5 = 0, ka = 0,
      rateNV = 0, dt = dtPrev, ncmt = 1L, oral0 = 0L, trans = 1L)

    ## J_i: production which1=-2 equivalent (real forward-mode Jacobian),
    ## entering from a CLEAN (non-cumulative) raw Alast -- alast0 has only
    ## the raw state, all reconstruction slots zeroed.
    nAlast <- 1 + 1 * 2  # ncmt + oral0 + ncmt*npars + oral0, npars=2 for 1cmt-iv
    alast0 <- c(rawAlast, numeric(nAlast - 1))
    res <- linCmtModelDouble(dt = dtPrev,
                             p1 = clExpr, v1 = tvVal, p2 = 0, p3 = 0, p4 = 0, p5 = 0, ka = 0,
                             alastNV = alast0, rateNV = 0,
                             ncmt = 1L, oral0 = 0L, trans = 1L,
                             deriv = TRUE, type = 0L, tau = 0, tinf = 0, amt = 0,
                             bolusCmt = 0L, ndiff = 0L, sensType = 30L)
    Ji <- res$J[1, 1]  # d(Alast_i)/d(p1=CL), holding entering state fixed
    ## Raw physical state (compartment amount), NOT res$val (which is
    ## adjustF()'d to a concentration) -- Alast is the field the existing
    ## oracle (.linCmtCall()/lincmt_oracle.R) itself feeds forward.
    valBeforeDose <- as.numeric(res$Alast)[1]

    ## Recurrence: s_i = T_i * s_{i-1} + J_i * dTheta_i/dEta
    s <- as.numeric(Ti) * s + Ji * dClDEta_i

    ## Advance the raw physical state the same way (production value path).
    rawAlast <- valBeforeDose
  }

  if (amt_i != 0 && i > 1) rawAlast <- rawAlast + amt_i

  ## s is d(raw central amount)/d(eta); cp = central/v with v fixed (not a
  ## function of eta.cl here), so d(cp)/d(eta) = s / v.
  if (grid$evid[i] == 0) predSens[i] <- s / tvVal
}

carryResult <- data.frame(time = grid$time, evid = grid$evid, predSens = predSens)
cat("\nCarried d(pred)/d(eta) at observation rows:\n")
print(carryResult[carryResult$evid == 0, ])

## ---- 3. Ground truth: FD on eta via a REAL re-solve ----------------------
h <- 1e-5
realP <- solveIt(etaVal + h)
realM <- solveIt(etaVal - h)
fdSens <- (realP$cp[realP$evid == 0] - realM$cp[realM$evid == 0]) / (2 * h)

cat("\nFD-on-eta ground truth at observation rows:\n")
print(data.frame(time = obsRows$time, fdSens = fdSens))

carriedAtObs <- carryResult$predSens[carryResult$evid == 0]
cat("\n=== Comparison: carried recurrence vs FD-on-eta ===\n")
cmp <- data.frame(time = obsRows$time, carried = carriedAtObs, fd = fdSens,
                  absDiff = abs(carriedAtObs - fdSens),
                  relDiff = abs(carriedAtObs - fdSens) / (abs(fdSens) + 1e-8))
print(cmp)

worst <- max(cmp$relDiff)
cat(sprintf("\nWorst relative difference: %.3e  -- %s\n", worst,
            if (worst < 1e-3) "PASS (carry recurrence matches FD-on-eta)" else "FAIL"))
