# Phase 3b.2 of the linCmt() sensitivity-carry subsystem (see
# ~/.claude/plans/the-lincmt-solutions-are-calm-seahorse.md): multi-pair
# carry storage. Phase 2 validated ind->linCmtCarryT for exactly ONE
# carry-eligible (linCmt-parameter, eta) pair (m=1, 1-cmt IV); this script
# proves the extended storage (4 rows x RX_LINCMT_CARRY_MAXPAIRS=8 pair
# columns, each column an independent d(Alast)/d(eta) m-vector) keeps
# SIMULTANEOUS pairs genuinely independent:
#
#   A. 1-cmt IV, TWO pairs at once -- eta.cl on a wt-driven cl AND eta.v on
#      a wt-driven v -- each validated independently against FD on ITS eta
#      via a real re-solve. Non-uniform dose/observation spacing throughout
#      (uniform spacing has hidden real pairing bugs twice in this effort).
#   B. Cross-contamination: re-drive A with the per-row which1=-7 calls in
#      the OPPOSITE pair order; results must be bit-identical, and a pair's
#      stored value must be unchanged by the OTHER pair's -7.
#   C. 2-cmt IV (m=2), two pairs -- exercises the row stride
#      (row*RX_LINCMT_CARRY_MAXPAIRS + pair) with row > 0, exactly where an
#      indexing bug would hide in m=1-only tests.
#   D. Cap: pair index >= 8 must fail loudly (clear R error), not truncate
#      or crash.
#
# which2 encoding (the contract 3b.3's codegen must emit): row + m*pair,
# m = ncmt+oral0. which1=-5 advances EVERY pair column at once (one shared
# transition matrix per row -- M depends only on the row's theta, identical
# for every eta) and is called ONCE PER ROW, not once per pair.

if (requireNamespace("devtools", quietly = TRUE) &&
      file.exists("DESCRIPTION") && file.exists("src/linCmt.cpp")) {
  devtools::load_all(".", quiet = TRUE)
} else {
  library(rxode2)
}

nPass <- 0L
nFail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) {
    nPass <<- nPass + 1L
    cat(sprintf("  PASS %s\n", label))
  } else {
    nFail <<- nFail + 1L
    cat(sprintf("  FAIL %s\n", label))
  }
}

wtAt <- function(tm) ifelse(tm < 20, 70, ifelse(tm < 40, 85, 100))

## Deliberately NON-uniform dose and observation spacing.
doseTimes <- c(0, 7, 24, 41)
obsTimes  <- c(3, 15, 30, 50)

buildEv <- function() {
  doseDf <- data.frame(time = doseTimes, amt = 100, evid = 1, cmt = 1,
                       wt = wtAt(doseTimes))
  obsDf  <- data.frame(time = obsTimes, amt = 0, evid = 0, cmt = 1,
                       wt = wtAt(obsTimes))
  ev <- rbind(doseDf, obsDf)
  ev <- ev[order(ev$time), ]
  ev$id <- 1
  ev
}
ev <- buildEv()

## ---- Scenario A: 1-cmt IV, two simultaneous pairs -------------------------
cat("== A: 1-cmt IV, eta.cl on cl AND eta.v on v, both wt-driven ==\n")

mod1 <- rxode2({
  cl <- tcl * (wt / 70)^0.75 * exp(eta.cl)
  v  <- tv * (wt / 70) * exp(eta.v)
  cp <- linCmt()
})

tclVal <- 2.0
tvVal  <- 20.0
etaCl  <- 0.3
etaV   <- -0.2

solve1 <- function(eCl, eV) {
  rxSolve(mod1, params = c(tcl = tclVal, tv = tvVal, eta.cl = eCl, eta.v = eV),
          events = ev, returnType = "data.frame", addDosing = TRUE)
}

clAt <- function(wt_i) tclVal * (wt_i / 70)^0.75 * exp(etaCl)
vAt  <- function(wt_i) tvVal * (wt_i / 70) * exp(etaV)

## Drives BOTH pairs through the live carry for one full pass over the grid.
## pairOrder: the order the two which1=-7 contributions are issued within a
## row (c(0,1) or c(1,0)) -- results must not depend on it.
driveTwoPair1cmt <- function(grid, pairOrder = c(0L, 1L)) {
  nRows <- nrow(grid)
  out <- list(s0 = rep(NA_real_, nRows), s1 = rep(NA_real_, nRows),
              pred = rep(NA_real_, nRows))
  rawAlast <- 0.0
  for (i in seq_len(nRows)) {
    wt_i <- grid$wt[i]
    cl_i <- clAt(wt_i)
    v_i  <- vAt(wt_i)
    amt_i <- if (grid$evid[i] != 0) grid$amt[i] else 0
    thetaRow <- matrix(c(cl_i, v_i, 0, 0, 0, 0, 0), nrow = 1)
    if (i > 1) {
      dtPrev <- grid$time[i] - grid$time[i - 1]
      alast0 <- c(rawAlast, numeric(2))  # m + m*npars = 1 + 1*2
      res <- linCmtModelDouble(dt = dtPrev,
                               p1 = cl_i, v1 = v_i, p2 = 0, p3 = 0, p4 = 0, p5 = 0, ka = 0,
                               alastNV = alast0, rateNV = 0,
                               ncmt = 1L, oral0 = 0L, trans = 1L,
                               deriv = TRUE, type = 0L, tau = 0, tinf = 0, amt = 0,
                               bolusCmt = 0L, ndiff = 0L, sensType = 30L)
      contrib <- c(res$J[1, 1] * cl_i,   # pair 0: dA/dcl * d(cl)/d(eta.cl)
                   res$J[1, 2] * v_i)    # pair 1: dA/dv  * d(v)/d(eta.v)
      ## ONE -5 for the row (advances BOTH pair columns), then the two -7s
      ## in pairOrder. m=1, so which2 = row + m*pair = pair.
      invisible(linCmtCarryLiveTest(
        id = 0L, t = rep(grid$time[i], 3), tPrior = rep(grid$time[i - 1], 3),
        theta = thetaRow[c(1, 1, 1), , drop = FALSE],
        ncmt = 1L, oral0 = 0L, trans = 1L,
        which1 = c(-5L, -7L, -7L),
        which2 = c(0L, pairOrder),
        addVal = c(0, contrib[pairOrder + 1L])))
      rawAlast <- as.numeric(res$Alast)[1]
    }
    if (amt_i != 0 && i > 1) rawAlast <- rawAlast + amt_i
    if (i == 1 && amt_i != 0) rawAlast <- rawAlast + amt_i
    if (grid$evid[i] == 0) {
      s0 <- linCmtCarryLiveTest(id = 0L, t = grid$time[i], tPrior = grid$time[i],
                                theta = thetaRow,
                                ncmt = 1L, oral0 = 0L, trans = 1L,
                                which1 = -6L, which2 = 0L)
      s1 <- linCmtCarryLiveTest(id = 0L, t = grid$time[i], tPrior = grid$time[i],
                                theta = thetaRow,
                                ncmt = 1L, oral0 = 0L, trans = 1L,
                                which1 = -6L, which2 = 1L)
      out$s0[i] <- s0
      out$s1[i] <- s1
      out$pred[i] <- rawAlast / v_i
    }
  }
  out
}

real0 <- solve1(etaCl, etaV)
grid <- real0[order(real0$time), c("time", "evid", "amt", "wt")]
obsMask <- grid$evid == 0

resA <- driveTwoPair1cmt(grid)

## Sanity: my raw-value propagation must reproduce the real solve's cp
## (a wrong dose/interval pairing would show up here first).
cpReal <- real0$cp[order(real0$time)][obsMask]
check("raw-value propagation matches real solved cp",
      max(abs(resA$pred[obsMask] - cpReal)) < 1e-8)

## Compose d(pred)/d(eta) at obs rows and compare each pair against FD on
## ITS OWN eta (real re-solves).
v_obs <- vAt(grid$wt[obsMask])
sensCl <- resA$s0[obsMask] / v_obs
## v depends on eta.v: d(pred)/d(eta.v) = s_v/v - A/v^2 * dv/deta.v,
## dv/deta.v = v, so the direct term is -pred.
sensV  <- resA$s1[obsMask] / v_obs - resA$pred[obsMask]

h <- 1e-5
fdCl <- {p <- solve1(etaCl + h, etaV); m <- solve1(etaCl - h, etaV)
  (p$cp[p$evid == 0] - m$cp[m$evid == 0]) / (2 * h)}
fdV  <- {p <- solve1(etaCl, etaV + h); m <- solve1(etaCl, etaV - h)
  (p$cp[p$evid == 0] - m$cp[m$evid == 0]) / (2 * h)}

relCl <- max(abs(sensCl - fdCl) / (abs(fdCl) + 1e-8))
relV  <- max(abs(sensV - fdV) / (abs(fdV) + 1e-8))
cat(sprintf("  pair 0 (eta.cl): worst rel diff vs FD %.3e\n", relCl))
cat(sprintf("  pair 1 (eta.v):  worst rel diff vs FD %.3e\n", relV))
check("pair 0 (eta.cl) matches FD-on-eta.cl", relCl < 1e-6)
check("pair 1 (eta.v) matches FD-on-eta.v", relV < 1e-6)

## ---- Scenario B: permuted -7 order must not change anything ---------------
cat("== B: cross-contamination (permuted -7 order) ==\n")
invisible(solve1(etaCl, etaV))  # fresh solve -> fresh iniSubject() reset
resB <- driveTwoPair1cmt(grid, pairOrder = c(1L, 0L))
check("pair 0 bit-identical under permuted -7 order",
      identical(resA$s0[obsMask], resB$s0[obsMask]))
check("pair 1 bit-identical under permuted -7 order",
      identical(resA$s1[obsMask], resB$s1[obsMask]))

## Direct isolation: on a fresh solve, add ONLY into pair 1 and confirm
## pair 0 stays exactly 0 (and vice versa) after a -5 advance.
invisible(solve1(etaCl, etaV))
thetaRow <- matrix(c(clAt(70), vAt(70), 0, 0, 0, 0, 0), nrow = 1)
invisible(linCmtCarryLiveTest(id = 0L, t = c(3, 3), tPrior = c(0, 0),
                    theta = thetaRow[c(1, 1), , drop = FALSE],
                    ncmt = 1L, oral0 = 0L, trans = 1L,
                    which1 = c(-5L, -7L), which2 = c(0L, 1L),
                    addVal = c(0, 42.5)))
s0Iso <- linCmtCarryLiveTest(id = 0L, t = 3, tPrior = 3, theta = thetaRow,
                             ncmt = 1L, oral0 = 0L, trans = 1L,
                             which1 = -6L, which2 = 0L)
s1Iso <- linCmtCarryLiveTest(id = 0L, t = 3, tPrior = 3, theta = thetaRow,
                             ncmt = 1L, oral0 = 0L, trans = 1L,
                             which1 = -6L, which2 = 1L)
check("writing pair 1 leaves pair 0 exactly 0", s0Iso == 0)
check("pair 1 holds exactly the value written", s1Iso == 42.5)

## ---- Scenario C: 2-cmt IV (m=2), two pairs -- row-stride exercise ---------
cat("== C: 2-cmt IV (m=2), eta.cl on cl AND eta.v on v ==\n")

mod2 <- rxode2({
  cl <- tcl * (wt / 70)^0.75 * exp(eta.cl)
  v  <- tv * (wt / 70) * exp(eta.v)
  q  <- tq
  v2 <- tv2
  cp <- linCmt()
})

tqVal  <- 4.0
tv2Val <- 50.0

solve2 <- function(eCl, eV) {
  rxSolve(mod2, params = c(tcl = tclVal, tv = tvVal, tq = tqVal, tv2 = tv2Val,
                           eta.cl = eCl, eta.v = eV),
          events = ev, returnType = "data.frame", addDosing = TRUE)
}

real2 <- solve2(etaCl, etaV)
grid2 <- real2[order(real2$time), c("time", "evid", "amt", "wt")]
obsMask2 <- grid2$evid == 0

m2 <- 2L
rawAlast <- c(0, 0)
s0Mat <- matrix(NA_real_, nrow(grid2), m2)  # pair 0 carried vector per row
s1Mat <- matrix(NA_real_, nrow(grid2), m2)  # pair 1
pred2 <- rep(NA_real_, nrow(grid2))
for (i in seq_len(nrow(grid2))) {
  wt_i <- grid2$wt[i]
  cl_i <- clAt(wt_i)
  v_i  <- vAt(wt_i)
  amt_i <- if (grid2$evid[i] != 0) grid2$amt[i] else 0
  thetaRow <- matrix(c(cl_i, v_i, tqVal, tv2Val, 0, 0, 0), nrow = 1)
  if (i > 1) {
    dtPrev <- grid2$time[i] - grid2$time[i - 1]
    alast0 <- c(rawAlast, numeric(2 * 4))  # m + m*npars = 2 + 2*4
    res <- linCmtModelDouble(dt = dtPrev,
                             p1 = cl_i, v1 = v_i, p2 = tqVal, p3 = tv2Val,
                             p4 = 0, p5 = 0, ka = 0,
                             alastNV = alast0, rateNV = c(0, 0),
                             ncmt = 2L, oral0 = 0L, trans = 1L,
                             deriv = TRUE, type = 0L, tau = 0, tinf = 0, amt = 0,
                             bolusCmt = 0L, ndiff = 0L, sensType = 30L)
    ## J is m x npars (cols: cl, v, q, v2). Contributions into BOTH rows of
    ## each pair's carried vector. which2 = row + m*pair, m=2:
    ##   pair 0 rows 0,1 -> which2 0,1; pair 1 rows 0,1 -> which2 2,3.
    c0 <- res$J[, 1] * cl_i   # d(Alast)/d(eta.cl), local part, rows 1:2
    c1 <- res$J[, 2] * v_i    # d(Alast)/d(eta.v)
    linCmtCarryLiveTest(
      id = 0L, t = rep(grid2$time[i], 5), tPrior = rep(grid2$time[i - 1], 5),
      theta = thetaRow[rep(1, 5), , drop = FALSE],
      ncmt = 2L, oral0 = 0L, trans = 1L,
      which1 = c(-5L, -7L, -7L, -7L, -7L),
      which2 = c(0L, 0L, 1L, 2L, 3L),
      addVal = c(0, c0[1], c0[2], c1[1], c1[2]))
    rawAlast <- as.numeric(res$Alast)[1:2]
  }
  if (amt_i != 0) rawAlast[1] <- rawAlast[1] + amt_i
  if (grid2$evid[i] == 0) {
    for (r in 0:1) {
      s0Mat[i, r + 1] <- linCmtCarryLiveTest(id = 0L, t = grid2$time[i],
                                             tPrior = grid2$time[i], theta = thetaRow,
                                             ncmt = 2L, oral0 = 0L, trans = 1L,
                                             which1 = -6L, which2 = r + 0L)
      s1Mat[i, r + 1] <- linCmtCarryLiveTest(id = 0L, t = grid2$time[i],
                                             tPrior = grid2$time[i], theta = thetaRow,
                                             ncmt = 2L, oral0 = 0L, trans = 1L,
                                             which1 = -6L, which2 = r + 2L)
    }
    pred2[i] <- rawAlast[1] / v_i
  }
}

cpReal2 <- real2$cp[order(real2$time)][obsMask2]
check("2-cmt raw-value propagation matches real solved cp",
      max(abs(pred2[obsMask2] - cpReal2)) < 1e-8)

v_obs2 <- vAt(grid2$wt[obsMask2])
sensCl2 <- s0Mat[obsMask2, 1] / v_obs2
sensV2  <- s1Mat[obsMask2, 1] / v_obs2 - pred2[obsMask2]

fdCl2 <- {p <- solve2(etaCl + h, etaV); m <- solve2(etaCl - h, etaV)
  (p$cp[p$evid == 0] - m$cp[m$evid == 0]) / (2 * h)}
fdV2  <- {p <- solve2(etaCl, etaV + h); m <- solve2(etaCl, etaV - h)
  (p$cp[p$evid == 0] - m$cp[m$evid == 0]) / (2 * h)}

relCl2 <- max(abs(sensCl2 - fdCl2) / (abs(fdCl2) + 1e-8))
relV2  <- max(abs(sensV2 - fdV2) / (abs(fdV2) + 1e-8))
cat(sprintf("  pair 0 (eta.cl): worst rel diff vs FD %.3e\n", relCl2))
cat(sprintf("  pair 1 (eta.v):  worst rel diff vs FD %.3e\n", relV2))
check("2-cmt pair 0 (eta.cl) matches FD-on-eta.cl", relCl2 < 1e-6)
check("2-cmt pair 1 (eta.v) matches FD-on-eta.v", relV2 < 1e-6)

## ---- Scenario D: cap-exceeded fails loudly --------------------------------
cat("== D: pair index cap ==\n")
capErr <- tryCatch({
  linCmtCarryLiveTest(id = 0L, t = 1, tPrior = 0,
                      theta = matrix(c(1, 10, 0, 0, 0, 0, 0), nrow = 1),
                      ncmt = 1L, oral0 = 0L, trans = 1L,
                      which1 = -6L, which2 = 8L)  # m=1 -> pair 8, cap is 8
  NULL
}, error = function(e) conditionMessage(e))
check("pair index >= cap raises a clear R error",
      !is.null(capErr) && grepl("out of range", capErr) &&
        grepl("RX_LINCMT_CARRY_MAXPAIRS", capErr))
cat(sprintf("  error message: %s\n", if (is.null(capErr)) "<none>" else capErr))

cat(sprintf("\n%d/%d checks passed\n", nPass, nPass + nFail))
if (nFail > 0) stop("Phase 3b.2 multi-pair carry checks FAILED")
