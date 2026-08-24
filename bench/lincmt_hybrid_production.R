# HISTORICAL RECORD: this script exercises rxControl(linCmtSensStrategy=)
# and/or linCmtHybStats(), removed when the hybrid strategy was retired (the
# amortized sequential evaluator, linCmtSeqTailJac, subsumed it).  To re-run,
# check out the commit range 473c6c52c..6939902d8 of this branch; the saved
# bench/results/*.rds remain the evidence of record.
# Phase 4.3 of the linCmt() speed plan (see
# ~/.claude/plans/the-lincmt-solutions-are-calm-seahorse.md): the hybrid
# sensitivity strategy wired into production linCmtB()
# (rxControl(linCmtSensStrategy="hybrid"/"auto")).  Every check solves a
# hand-written gradient model (the linCmtB(which1=-2) reads nlmixr2est's
# inner model makes, for k requested directions) through the REAL rxSolve()
# path twice -- "sequential" and "hybrid" -- and requires the concentration
# and every d(cp)/d(theta) column to agree to round-off, with the mechanism
# counters proving the hybrid actually filled the trailing observation rows.
#
#   A. all 6 compartment configs x {bolus, infusion, mixed} x k = 1,2,3,
#      multi-subject, non-uniform spacing, doses then a trailing run
#   B. steady-state regimens (ss=1 infusion and bolus; ss=2 on top)
#   C. observations interleaved with doses (phase 2 = trailing run only)
#   D. threaded multi-subject solve == single-threaded
#   E. benchmark: the recheck grid (doses x observations, 2/3-cmt oral,
#      k = 2,3) against the count-based auto sequential path, 1 and N threads

if (requireNamespace("devtools", quietly = TRUE) &&
      file.exists("DESCRIPTION") && file.exists("src/linCmt.cpp")) {
  # compile = FALSE: build first with pkgbuild::compile_dll(".", debug = FALSE)
  # (or benchmark an installed package).  load_all()'s own debug build is
  # -O0, which inflates every ratio here (the fvar kernels far more than
  # the rest), so timings taken through it are not representative.
  devtools::load_all(".", quiet = TRUE, compile = FALSE)
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

# Gradient model reading k directions (cl, v, then q/ka...), like a FOCEi
# inner model with k etas.
.gradModel <- function(cfg, k) {
  npars <- 2L*cfg$ncmt + cfg$oral0
  dirs <- seq_len(min(k, npars)) - 1L
  args <- sprintf("rx__PTR__, t, 1, %d, %d, %%d, %%d, 1, cl, v, q, vp, q2, vp2, ka",
                  cfg$ncmt, cfg$oral0)
  lines <- c(sprintf("cp=linCmtB(%s)", sprintf(args, -1L, -1L)),
             vapply(dirs, function(d) {
               sprintf("d%d=linCmtB(%s)", d, sprintf(args, -2L, d))
             }, ""))
  suppressWarnings(rxode2(paste(lines, collapse = "\n")))
}

.obsTail <- function(t0, n, sh) {
  t0 + cumsum(rep(c(1.3, 2.9, 4.1, 0.7), length.out = n)) + sh
}

.evBolus <- function(nSub = 3L) {
  do.call(rbind, lapply(seq_len(nSub), function(i) {
    sh <- 0.3 * (i - 1)
    dose <- data.frame(id = i, time = c(0, 7.5, 13, 24.25) + c(0, sh, 0, sh),
                       amt = c(100, 80, 120, 90) * (1 + 0.1 * i), evid = 1,
                       cmt = 1, rate = 0, ii = 0, ss = 0)
    obs <- data.frame(id = i, time = .obsTail(25, 11L, sh), amt = 0, evid = 0,
                      cmt = 1, rate = 0, ii = 0, ss = 0)
    rbind(dose, obs)
  }))
}

.evInfusion <- function(nSub = 3L) {
  do.call(rbind, lapply(seq_len(nSub), function(i) {
    dose <- data.frame(id = i, time = c(0, 12.5, 30),
                       amt = c(100, 150, 80), evid = 1, cmt = 1,
                       rate = c(50, 30, 80) * (1 + 0.1 * i), ii = 0, ss = 0)
    obs <- data.frame(id = i, time = .obsTail(31.5, 13L, 0.1 * i), amt = 0,
                      evid = 0, cmt = 1, rate = 0, ii = 0, ss = 0)
    rbind(dose, obs)
  }))
}

.evMixed <- function(nSub = 3L) {
  do.call(rbind, lapply(seq_len(nSub), function(i) {
    dose <- data.frame(id = i, time = c(0, 5, 18.5, 26),
                       amt = c(100, 60, 140, 70), evid = 1, cmt = 1,
                       rate = c(40, 0, 70, 0), ii = 0, ss = 0)
    obs <- data.frame(id = i, time = .obsTail(27, 13L, 0.2 * i), amt = 0,
                      evid = 0, cmt = 1, rate = 0, ii = 0, ss = 0)
    rbind(dose, obs)
  }))
}

.evSs <- function(nSub = 2L) {
  do.call(rbind, lapply(seq_len(nSub), function(i) {
    dose <- data.frame(id = i, time = c(0, 48, 60),
                       amt = c(100, 100, 50), evid = 1, cmt = 1,
                       rate = c(if (i == 1) 25 else 0, 0, 0),
                       ii = c(12, 12, 0), ss = c(1, 2, 0))
    obs <- data.frame(id = i, time = c(0.7, 2.3, 5.9, 11.1, 13.4, 20.2, 30.5,
                                        47.5, 49.1, 53.3, 59.4, .obsTail(61, 6L, 0)) + 0.15 * i,
                      amt = 0, evid = 0, cmt = 1, rate = 0, ii = 0, ss = 0)
    rbind(dose, obs)
  }))
}

.evInterleaved <- function(nSub = 3L) {
  do.call(rbind, lapply(seq_len(nSub), function(i) {
    dose <- data.frame(id = i, time = c(0, 7.5, 13, 24.25),
                       amt = c(100, 80, 120, 90) * (1 + 0.1 * i), evid = 1,
                       cmt = 1, rate = 0, ii = 0, ss = 0)
    obs <- data.frame(id = i, time = c(0.5, 1.7, 3.1, 6.2, 8.3, 11.9, 16.4, 22.2,
                                        27.7, 35.1, 48.8, 60.2) + 0.1 * i,
                      amt = 0, evid = 0, cmt = 1, rate = 0, ii = 0, ss = 0)
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

.runCase <- function(label, cfg, m, ev, rows, tol = 1e-9, ...) {
  ref <- .solve(m, cfg, ev, "sequential", ...)
  invisible(rxode2:::linCmtHybStats(TRUE))
  hyb <- .solve(m, cfg, ev, "hybrid", ...)
  st <- rxode2:::linCmtHybStats(TRUE)
  worst <- .cmpCols(hyb, ref)
  check(sprintf("%-30s rel diff %.2e  subj=%d rows=%d rates=%d cons=%d flush=%d full=%d",
                label, worst, st[["subjects"]], st[["rows"]], st[["rateSteps"]],
                st[["consolidations"]], st[["flushes"]], st[["fullRows"]]),
        is.finite(worst) && worst < tol && st[["rows"]] == rows)
  invisible(list(ref = ref, hyb = hyb, st = st))
}

# RX_HYB_BENCH=validate runs A-D only; =bench runs E only; unset runs both.
.part <- Sys.getenv("RX_HYB_BENCH", "")
.doValidate <- .part != "bench"
.doBench <- .part != "validate"
nThr <- getRxThreads()

## ---- A: configs x regimens x k -------------------------------------------
if (.doValidate) {
cat("== A: 6 configs x bolus/infusion/mixed x k=1,2,3, 3 subjects ==\n")
for (cfg in .cfgs) {
  for (k in 1:3) {
    m <- .gradModel(cfg, k)
    .runCase(sprintf("%s bolus k=%d", cfg$name, k), cfg, m, .evBolus(), 3L * 11L)
    .runCase(sprintf("%s infusion k=%d", cfg$name, k), cfg, m, .evInfusion(), 3L * 13L)
    .runCase(sprintf("%s mixed k=%d", cfg$name, k), cfg, m, .evMixed(), 3L * 13L)
  }
}

## ---- B: steady state -------------------------------------------------------
cat("== B: steady-state regimens (ss=1 infusion/bolus, then ss=2, then bolus) ==\n")
# subject 1's steady-state infusion leaves its turn-off pending for the whole
# pass, so only subject 2 engages (6 trailing rows); subject 1 stays sequential
for (cfg in .cfgs) {
  m <- .gradModel(cfg, 2L)
  .runCase(paste(cfg$name, "ss k=2"), cfg, m, .evSs(), 6L)
}

## ---- C: interleaved --------------------------------------------------------
cat("== C: observations interleaved with doses: phase 2 = trailing run ==\n")
for (cfg in .cfgs[c(2, 4, 6)]) {
  m <- .gradModel(cfg, 3L)
  .runCase(paste(cfg$name, "interleaved k=3"), cfg, m, .evInterleaved(), 3L * 4L)
}

## ---- D: threads ------------------------------------------------------------
cat("== D: threaded multi-subject solve vs single thread ==\n")
for (cfg in .cfgs[c(3, 6)]) {
  m <- .gradModel(cfg, 3L)
  ev <- .evMixed(nSub = 24L)
  s1 <- .solve(m, cfg, ev, "hybrid", cores = 1L)
  ref <- .solve(m, cfg, ev, "sequential", cores = 1L)
  worstN <- 0
  for (rep in 1:5) {
    sN <- .solve(m, cfg, ev, "hybrid", cores = nThr)
    worstN <- max(worstN, .cmpCols(sN, s1))
  }
  check(sprintf("%-30s %d threads == 1 thread x5 (rel diff %.2e), == sequential (%.2e)",
                cfg$name, nThr, worstN, .cmpCols(s1, ref)),
        worstN == 0 && .cmpCols(s1, ref) < 1e-9)
}
}

## ---- E: benchmark ---------------------------------------------------------
if (.doBench) {
cat("== E: benchmark, real rxSolve() path, doses then a trailing observation run ==\n")
.benchEv <- function(nDose, nObs, nSub = 44L) {
  do.call(rbind, lapply(seq_len(nSub), function(i) {
    dt <- c(0, cumsum(rep(c(11.5, 12.5), length.out = nDose - 1)))
    dose <- data.frame(id = i, time = dt, amt = 100 + 5 * (seq_len(nDose) %% 3),
                       evid = 1, cmt = 1, rate = 0, ii = 0, ss = 0)
    t0 <- max(dt)
    obs <- data.frame(id = i, time = t0 + seq(0.25, 96, length.out = nObs),
                      amt = 0, evid = 0, cmt = 1, rate = 0, ii = 0, ss = 0)
    rbind(dose, obs)
  }))
}
.timeIt <- function(fn, reps = 5L) {
  tt <- vapply(seq_len(reps), function(i) system.time(fn())[["elapsed"]], 0)
  median(tt)
}
benchRows <- list()
for (cfg in .cfgs[c(4, 6)]) {
  for (k in c(2L, 3L)) {
    m <- .gradModel(cfg, k)
    for (nDose in c(20L, 100L, 200L)) {
      for (nObs in c(50L, 200L, 400L)) {
        ev <- .benchEv(nDose, nObs)
        evt <- rxode2::etTrans(ev, m)
        for (cores in unique(c(1L, nThr))) {
          # sequential under the auto sensType rule, sequential forced to
          # forward mode (the hybrid's own dose-phase mode), and the hybrid
          .solve(m, cfg, evt, "sequential", cores = cores)
          tS <- .timeIt(function() .solve(m, cfg, evt, "sequential", cores = cores))
          tF <- .timeIt(function() .solve(m, cfg, evt, "sequential", cores = cores,
                                          linCmtSensType = "AD"))
          tH <- .timeIt(function() .solve(m, cfg, evt, "hybrid", cores = cores))
          cat(sprintf("  %-10s k=%d doses=%3d obs=%3d cores=%2d  sequential %.4fs  forward %.4fs  hybrid %.4fs  ratio %.2fx  ratioFwd %.2fx\n",
                      cfg$name, k, nDose, nObs, cores, tS, tF, tH, tS / tH, tF / tH))
          benchRows[[length(benchRows) + 1L]] <-
            data.frame(config = cfg$name, k = k, nDose = nDose, nObs = nObs,
                       cores = cores, sequential = tS, forward = tF, hybrid = tH,
                       ratio = tS / tH, ratioFwd = tF / tH)
        }
      }
    }
  }
}
bench <- do.call(rbind, benchRows)
dir.create("bench/results", showWarnings = FALSE)
saveRDS(bench, "bench/results/lincmt_hybrid_production.rds")
}

cat(sprintf("\n%d passed, %d failed\n", nPass, nFail))
if (nFail > 0) stop("hybrid production validation failed")
