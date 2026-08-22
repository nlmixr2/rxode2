# Phase 4.2 of the linCmt() speed plan (see
# ~/.claude/plans/the-lincmt-solutions-are-calm-seahorse.md): the
# superposition sensitivity strategy wired into production linCmtB()
# (rxControl(linCmtSensStrategy="superposition")).  Every check solves a
# hand-written gradient model (the linCmtB(which1=-2) reads nlmixr2est's
# inner model makes) through the REAL rxSolve() path twice -- sequential
# ("forward", today's default) and "superposition" -- and requires the
# concentration and every d(cp)/d(theta) column to agree to round-off, with
# the mechanism counters proving the strategy actually filled the rows.
#
#   A. all 6 compartment configs x {bolus, infusion, mixed} regimens,
#      multi-subject, non-uniform spacing
#   B. steady-state regimens (ss=1 infusion and bolus; ss=2 on top of a
#      running regimen)
#   C. dose count above linCmtSupersededDoseCeiling (consolidation)
#   D. threaded multi-subject solve == single-threaded
#   E. benchmark, few-dose / dense-observation shape, real rxSolve() path

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

.cfgs <- list(
  list(name = "1cmt-iv",   ncmt = 1L, oral0 = 0L),
  list(name = "1cmt-oral", ncmt = 1L, oral0 = 1L),
  list(name = "2cmt-iv",   ncmt = 2L, oral0 = 0L),
  list(name = "2cmt-oral", ncmt = 2L, oral0 = 1L),
  list(name = "3cmt-iv",   ncmt = 3L, oral0 = 0L),
  list(name = "3cmt-oral", ncmt = 3L, oral0 = 1L))

.parsFor <- function(cfg) {
  p <- c(cl = 2.1, v = 21, q = 3.3, vp = 43, q2 = 0.9, vp2 = 61, ka = 1.3)
  if (cfg$ncmt < 2) p[c("q", "vp")] <- 0
  if (cfg$ncmt < 3) p[c("q2", "vp2")] <- 0
  if (cfg$oral0 == 0) p["ka"] <- 0
  p
}

# Gradient model: concentration plus d(cp)/d(theta_k) for every parameter,
# the same linCmtB(which1 = -2, which2 = k) reads a FOCEi inner model makes.
.gradModel <- function(cfg) {
  npars <- 2L*cfg$ncmt + cfg$oral0
  args <- sprintf("rx__PTR__, t, 1, %d, %d, %%d, %%d, 1, cl, v, q, vp, q2, vp2, ka",
                  cfg$ncmt, cfg$oral0)
  lines <- c(sprintf("cp=linCmtB(%s)", sprintf(args, -1L, -1L)),
             vapply(seq_len(npars) - 1L, function(k) {
               sprintf("d%d=linCmtB(%s)", k, sprintf(args, -2L, k))
             }, ""))
  suppressWarnings(rxode2(paste(lines, collapse = "\n")))
}

.doseCmt <- function(cfg) 1L

.evBolus <- function(cfg, nSub = 3L) {
  do.call(rbind, lapply(seq_len(nSub), function(i) {
    sh <- 0.3 * (i - 1)
    dose <- data.frame(id = i, time = c(0, 7.5, 13, 24.25) + c(0, sh, 0, sh),
                       amt = c(100, 80, 120, 90) * (1 + 0.1 * i), evid = 1,
                       cmt = .doseCmt(cfg), rate = 0, ii = 0, ss = 0)
    obs <- data.frame(id = i, time = c(0.5, 1.7, 3.1, 6.2, 8.3, 11.9, 16.4,
                                        22.2, 27.7, 35.1, 48.8) + sh * 0.5,
                      amt = 0, evid = 0, cmt = .doseCmt(cfg), rate = 0, ii = 0, ss = 0)
    rbind(dose, obs)
  }))
}

.evInfusion <- function(cfg, nSub = 3L) {
  do.call(rbind, lapply(seq_len(nSub), function(i) {
    dose <- data.frame(id = i, time = c(0, 12.5, 30),
                       amt = c(100, 150, 80), evid = 1, cmt = .doseCmt(cfg),
                       rate = c(50, 30, 80) * (1 + 0.1 * i), ii = 0, ss = 0)
    obs <- data.frame(id = i, time = c(0.4, 1.1, 2.6, 3.9, 6.5, 10.2, 13.3,
                                        15.8, 21.4, 31.1, 33.7, 44.4, 58.3) + 0.1 * i,
                      amt = 0, evid = 0, cmt = .doseCmt(cfg), rate = 0, ii = 0, ss = 0)
    rbind(dose, obs)
  }))
}

.evMixed <- function(cfg, nSub = 3L) {
  do.call(rbind, lapply(seq_len(nSub), function(i) {
    dose <- data.frame(id = i, time = c(0, 5, 18.5, 26),
                       amt = c(100, 60, 140, 70), evid = 1, cmt = .doseCmt(cfg),
                       rate = c(40, 0, 70, 0), ii = 0, ss = 0)
    obs <- data.frame(id = i, time = c(0.6, 1.9, 2.4, 4.7, 7.1, 9.3, 14.6,
                                        19.5, 21.1, 24.9, 28.8, 36.6, 49.9) + 0.2 * i,
                      amt = 0, evid = 0, cmt = .doseCmt(cfg), rate = 0, ii = 0, ss = 0)
    rbind(dose, obs)
  }))
}

.evSs <- function(cfg, nSub = 2L) {
  do.call(rbind, lapply(seq_len(nSub), function(i) {
    dose <- data.frame(id = i, time = c(0, 48, 60),
                       amt = c(100, 100, 50), evid = 1, cmt = .doseCmt(cfg),
                       rate = c(if (i == 1) 25 else 0, 0, 0),
                       ii = c(12, 12, 0), ss = c(1, 2, 0))
    obs <- data.frame(id = i, time = c(0.7, 2.3, 5.9, 11.1, 13.4, 20.2, 30.5,
                                        47.5, 49.1, 53.3, 59.4, 61.2, 66.6, 80.1) + 0.15 * i,
                      amt = 0, evid = 0, cmt = .doseCmt(cfg), rate = 0, ii = 0, ss = 0)
    rbind(dose, obs)
  }))
}

.evManyDoses <- function(cfg, nDose = 45L, nSub = 2L) {
  do.call(rbind, lapply(seq_len(nSub), function(i) {
    dt <- c(0, cumsum(rep(c(5.5, 6.5, 4.9), length.out = nDose - 1)))
    dose <- data.frame(id = i, time = dt, amt = 50 + 7 * (seq_len(nDose) %% 4),
                       evid = 1, cmt = .doseCmt(cfg), rate = 0, ii = 0, ss = 0)
    obs <- data.frame(id = i, time = sort(c(dt[-1] - 1.3, max(dt) + c(2.2, 9.7, 26.3))),
                      amt = 0, evid = 0, cmt = .doseCmt(cfg), rate = 0, ii = 0, ss = 0)
    rbind(dose, obs)
  }))
}

.solve <- function(m, cfg, ev, strategy, ...) {
  rxSolve(m, params = .parsFor(cfg), events = ev, returnType = "data.frame",
          linCmtSensStrategy = strategy, ...)
}

.cmpCols <- function(a, b) {
  cols <- grep("^(cp|d[0-9]+)$", names(a), value = TRUE)
  worst <- 0
  for (cc in cols) {
    sc <- pmax(1e-8, abs(b[[cc]]))
    worst <- max(worst, max(abs(a[[cc]] - b[[cc]]) / sc))
  }
  worst
}

.runCase <- function(label, cfg, m, ev, tol = 1e-9, minRows = 1L, ...) {
  ref <- .solve(m, cfg, ev, "forward", ...)
  invisible(rxode2:::linCmtSupStats(TRUE))
  sup <- .solve(m, cfg, ev, "superposition", ...)
  st <- rxode2:::linCmtSupStats(TRUE)
  worst <- .cmpCols(sup, ref)
  check(sprintf("%-28s rel diff %.2e  rows=%d primes=%d doses=%d rates=%d cons=%d",
                label, worst, st[["rows"]], st[["primes"]], st[["doses"]],
                st[["rateSteps"]], st[["consolidations"]]),
        is.finite(worst) && worst < tol && st[["rows"]] >= minRows)
  invisible(list(ref = ref, sup = sup, st = st))
}

## ---- A: configs x regimens ------------------------------------------------
cat("== A: 6 configs x bolus/infusion/mixed, 3 subjects, non-uniform ==\n")
for (cfg in .cfgs) {
  m <- .gradModel(cfg)
  .runCase(paste(cfg$name, "bolus"), cfg, m, .evBolus(cfg), minRows = 10L)
  .runCase(paste(cfg$name, "infusion"), cfg, m, .evInfusion(cfg), minRows = 10L)
  .runCase(paste(cfg$name, "mixed"), cfg, m, .evMixed(cfg), minRows = 10L)
}

## ---- B: steady state -------------------------------------------------------
cat("== B: steady-state regimens (ss=1 infusion/bolus, then ss=2, then bolus) ==\n")
for (cfg in .cfgs) {
  m <- .gradModel(cfg)
  .runCase(paste(cfg$name, "ss"), cfg, m, .evSs(cfg), minRows = 5L)
}

## ---- C: ceiling overflow / consolidation ----------------------------------
cat("== C: 45 doses per subject against ceiling 30 and ceiling 4 ==\n")
for (cfg in .cfgs[c(1, 4, 6)]) {
  m <- .gradModel(cfg)
  r <- .runCase(paste(cfg$name, "45 doses, ceiling 30"), cfg, m, .evManyDoses(cfg),
                minRows = 40L)
  check(sprintf("%-28s consolidated at least once", cfg$name),
        r$st[["consolidations"]] >= 1L)
  r <- .runCase(paste(cfg$name, "45 doses, ceiling 4"), cfg, m, .evManyDoses(cfg),
                minRows = 40L, linCmtSupersededDoseCeiling = 4L)
  check(sprintf("%-28s consolidated many times", cfg$name),
        r$st[["consolidations"]] >= 10L)
}

## ---- D: threads ------------------------------------------------------------
cat("== D: threaded multi-subject solve vs single thread ==\n")
nThr <- getRxThreads()
for (cfg in .cfgs[c(1, 6)]) {
  m <- .gradModel(cfg)
  ev <- .evMixed(cfg, nSub = 24L)
  s1 <- .solve(m, cfg, ev, "superposition", cores = 1L)
  sN <- .solve(m, cfg, ev, "superposition", cores = nThr)
  ref <- .solve(m, cfg, ev, "forward", cores = 1L)
  check(sprintf("%-28s %d threads == 1 thread (rel diff %.2e), == forward (%.2e)",
                cfg$name, nThr, .cmpCols(sN, s1), .cmpCols(sN, ref)),
        .cmpCols(sN, s1) < 1e-12 && .cmpCols(sN, ref) < 1e-9)
}

## ---- E: benchmark ---------------------------------------------------------
cat("== E: benchmark, real rxSolve() path, few doses / dense observations ==\n")
.benchEv <- function(nObs, nSub = 40L) {
  do.call(rbind, lapply(seq_len(nSub), function(i) {
    dose <- data.frame(id = i, time = c(0, 24), amt = 100, evid = 1, cmt = 1,
                       rate = 0, ii = 0, ss = 0)
    obs <- data.frame(id = i, time = seq(0.25, 72, length.out = nObs), amt = 0,
                      evid = 0, cmt = 1, rate = 0, ii = 0, ss = 0)
    rbind(dose, obs)
  }))
}
# Whole rxSolve() wall clock (event translation and output assembly included,
# identical in both arms), so the ratio UNDERSTATES the kernel-level change.
.timeIt <- function(fn, reps = 7L) {
  tt <- vapply(seq_len(reps), function(i) system.time(fn())[["elapsed"]], 0)
  median(tt)
}
for (cfg in .cfgs[c(1, 4, 6)]) {
  m <- .gradModel(cfg)
  for (nObs in c(50L, 200L, 400L)) {
    ev <- .benchEv(nObs)
    evt <- rxode2::etTrans(ev, m)
    .solve(m, cfg, evt, "forward", cores = 1L)
    tF <- .timeIt(function() .solve(m, cfg, evt, "forward", cores = 1L))
    tS <- .timeIt(function() .solve(m, cfg, evt, "superposition", cores = 1L))
    cat(sprintf("  %-10s nObs=%4d x 40 subjects  forward %.4fs  superposition %.4fs  ratio %.2fx\n",
                cfg$name, nObs, tF, tS, tF / tS))
  }
}

cat(sprintf("\n%d passed, %d failed\n", nPass, nFail))
if (nFail > 0) stop("superposition production validation failed")
