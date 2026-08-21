# Persisted correctness + performance tests for the persistent/per-subject AD
# prototypes added to src/linCmt.cpp during the linCmt() speed investigation
# (see ~/.claude/plans/the-lincmt-solutions-are-calm-seahorse.md for the full
# narrative). None of these prototype entry points are wired into production
# yet -- this file exists so every dead end and every validated design from
# that investigation stays reproducible instead of living only in shell
# history.
#
# Usage (from the package root, with the package loaded via devtools::load_all):
#   source("bench/lincmt_subject_ad_proto.R")
#   runLinCmtSubjectADProtoTests()   # correctness, all prototypes, all configs
#   benchLinCmtSubjectADProto()      # performance tables backing the design
#
# Prototypes covered (all in src/linCmt.cpp):
#  - linCmtSubjectReverseADProto            sequential reverse-mode AD, live
#                                            Alast carry (no double round-trip)
#  - linCmtSubjectReverseADBatchProto        same, extraction deferred to the
#                                            end of the subject (CONFIRMED
#                                            WORSE than interleaved -- kept as
#                                            a negative-result regression case)
#  - linCmtSubjectSuperpositionADProto       superposition + per-observation
#                                            nested_rev_autodiff; bolus AND
#                                            two-phase infusion handling
#  - linCmtSubjectReverseADTimeVaryingProto  independent theta per interval --
#                                            CONFIRMED INSUFFICIENT as a fix
#                                            (matches "hold history fixed"
#                                            sensitivity, not the true
#                                            cumulative one); kept as a
#                                            documented negative result
#  - linCmtSubjectReverseADEtaCovariateProto shared-eta-root reverse AD --
#                                            the correct, FD-validated fix
#                                            for a time-varying covariate
#  - linCmtSubjectForwardADEtaCovariateProto same fix, forward-mode -- matches
#                                            the reverse-mode result exactly
#                                            at a fraction of the cost
#  - linCmtSubjectHybridDoseObsADProto       phase-aware hybrid: forward-mode
#                                            roll-through for a dose-heavy
#                                            phase, bridged via the exact
#                                            constant-theta reconstruction
#                                            into nested superposition for a
#                                            dense observation phase
#
# All every-call comparisons below reuse bench/lincmt_oracle.R's .linCmtCall()
# sequential harness as ground truth (already validated against production
# sensType=3/30 elsewhere), so this file does not re-derive that trust -- it
# only checks the NEW prototypes against it.

source(file.path("bench", "lincmt_oracle.R"))

.protoTol <- 1e-8 # round-off-scale tolerance for AD-vs-AD comparisons

.report <- function(name, worst, tol = .protoTol) {
  pass <- is.finite(worst) && worst <= tol
  message(sprintf("%-55s worst|diff| = %.3e -> %s", name, worst,
                  if (pass) "PASS" else "FAIL"))
  invisible(list(name = name, worst = worst, pass = pass))
}

# ---------------------------------------------------------------------------
# 1. linCmtSubjectReverseADProto: sequential reverse AD, live Alast carry.
#    Ground truth: bench/lincmt_oracle.R's .linCmtCall(), chained by hand,
#    same convention this file's other checks reuse (dose added, then evolve
#    to reach the next call's elapsed time).
# ---------------------------------------------------------------------------
.checkReverseADProto <- function(cfg, nIv = 12, dt = 0.7) {
  alast <- cfg$alast0
  amt <- c(100, rep(0, nIv - 1))
  oracleJ <- vector("list", nIv)
  for (iv in seq_len(nIv)) {
    s <- .linCmtCall(dt, cfg, alast, sensType = 3L)
    oracleJ[[iv]] <- s$J
    alast <- s$Alast
  }
  proto <- .Call(`_rxode2_linCmtSubjectReverseADProto`,
                 rep(dt, nIv), amt,
                 cfg$p1, cfg$v1, cfg$p2, cfg$p3, cfg$p4, cfg$p5, cfg$ka,
                 cfg$rate, cfg$ncmt, cfg$oral0, cfg$trans, 0L)
  worst <- 0
  for (iv in seq_len(nIv)) worst <- max(worst, max(abs(oracleJ[[iv]] - proto[[iv]]$J)))
  .report(sprintf("reverseADProto[%s]", cfg$name), worst)
}

# ---------------------------------------------------------------------------
# 2. linCmtSubjectReverseADBatchProto: NEGATIVE RESULT regression case.
#    Correctness must still hold (it does), but this function exists to keep
#    documented that deferring extraction to the end of the subject is a
#    real, measured performance regression vs interleaved extraction, not an
#    improvement -- see benchLinCmtSubjectADProto()'s "batch-at-end" section.
# ---------------------------------------------------------------------------
.checkReverseADBatchProto <- function(cfg, nIv = 12, dt = 0.7) {
  alast <- cfg$alast0
  amt <- c(100, rep(0, nIv - 1))
  oracleJ <- vector("list", nIv)
  for (iv in seq_len(nIv)) {
    s <- .linCmtCall(dt, cfg, alast, sensType = 3L)
    oracleJ[[iv]] <- s$J
    alast <- s$Alast
  }
  proto <- .Call(`_rxode2_linCmtSubjectReverseADBatchProto`,
                 rep(dt, nIv), amt,
                 cfg$p1, cfg$v1, cfg$p2, cfg$p3, cfg$p4, cfg$p5, cfg$ka,
                 cfg$rate, cfg$ncmt, cfg$oral0, cfg$trans, 0L)
  worst <- 0
  for (iv in seq_len(nIv)) worst <- max(worst, max(abs(oracleJ[[iv]] - proto[[iv]]$J)))
  .report(sprintf("reverseADBatchProto[%s]", cfg$name), worst)
}

# ---------------------------------------------------------------------------
# 3. linCmtSubjectSuperpositionADProto: bolus-only multi-dose correctness.
#    Oracle walk marks each dose "given" once (a dose time can coincide with
#    an observation time, so a naive tPrev-equality check would double-apply
#    it -- this tripped an early version of this exact test).
# ---------------------------------------------------------------------------
.checkSuperpositionBolus <- function(cfg, nDose = 6, nObsPerDose = 2, dt = 0.7) {
  doseT <- seq(0, by = dt * nObsPerDose, length.out = nDose)
  obsT <- sort(unique(c(doseT, as.vector(outer(doseT, dt * seq_len(nObsPerDose - 1), "+")))))
  doseAmt <- rep(100, nDose)
  doseDur <- rep(0, nDose)

  alast <- numeric(length(cfg$alast0))
  tPrev <- 0
  given <- rep(FALSE, nDose)
  oracleJ <- vector("list", length(obsT))
  for (i in seq_along(obsT)) {
    doseIdx <- which(abs(doseT - tPrev) < 1e-9 & !given)
    for (di in doseIdx) { alast[1] <- alast[1] + doseAmt[di]; given[di] <- TRUE }
    s <- .linCmtCall(obsT[i] - tPrev, cfg, alast, sensType = 3L)
    oracleJ[[i]] <- s$J
    alast <- s$Alast
    tPrev <- obsT[i]
  }
  proto <- .Call(`_rxode2_linCmtSubjectSuperpositionADProto`,
                 obsT, doseT, doseAmt, doseDur,
                 cfg$p1, cfg$v1, cfg$p2, cfg$p3, cfg$p4, cfg$p5, cfg$ka,
                 cfg$ncmt, cfg$oral0, cfg$trans, 0L)
  worst <- 0
  for (i in seq_along(obsT)) worst <- max(worst, max(abs(oracleJ[[i]] - proto[[i]]$J)))
  .report(sprintf("superposition(bolus)[%s]", cfg$name), worst)
}

# ---------------------------------------------------------------------------
# 4. linCmtSubjectSuperpositionADProto: mixed infusion + bolus regimen.
#    Regimen: infusion of 100 over 2.0 starting t=0, bolus of 50 at t=3.0;
#    observations during the infusion, between infusion and bolus, and after
#    the bolus.
# ---------------------------------------------------------------------------
.checkSuperpositionMixed <- function(cfg, dt = 0.4) {
  nAlast <- .linCmtNalast(cfg$ncmt, cfg$oral0)
  cfgR <- list(p1 = cfg$p1, v1 = cfg$v1, p2 = cfg$p2, p3 = cfg$p3, p4 = cfg$p4, p5 = cfg$p5,
              ka = cfg$ka, rate = rep(0, cfg$nstate),
              ncmt = cfg$ncmt, oral0 = cfg$oral0, trans = cfg$trans)

  obsT <- c(0.5, 2.5, 3.5, 5.0)
  tinf <- 2.0; infAmt <- 100
  boluT <- 3.0; boluAmt <- 50

  alast <- numeric(nAlast)
  tPrev <- 0
  boluGiven <- FALSE
  oracleJ <- vector("list", length(obsT))
  for (i in seq_along(obsT)) {
    to <- obsT[i]
    steps <- sort(unique(c(tinf, boluT, to)))
    steps <- steps[steps > tPrev & steps <= to + 1e-9]
    for (st in steps) {
      thisRate <- cfgR
      thisRate$rate <- if (tPrev < tinf - 1e-9) {
        r <- rep(0, cfg$nstate); r[1] <- infAmt / tinf; r
      } else rep(0, cfg$nstate)
      s <- .linCmtCall(st - tPrev, thisRate, alast, sensType = 3L)
      alast <- s$Alast
      if (abs(st - boluT) < 1e-9 && !boluGiven) { alast[1] <- alast[1] + boluAmt; boluGiven <- TRUE }
      tPrev <- st
    }
    oracleJ[[i]] <- s$J
  }

  proto <- .Call(`_rxode2_linCmtSubjectSuperpositionADProto`,
                 obsT, c(0, boluT), c(infAmt, boluAmt), c(tinf, 0),
                 cfg$p1, cfg$v1, cfg$p2, cfg$p3, cfg$p4, cfg$p5, cfg$ka,
                 cfg$ncmt, cfg$oral0, cfg$trans, 0L)
  worst <- 0
  for (i in seq_along(obsT)) worst <- max(worst, max(abs(oracleJ[[i]] - proto[[i]]$J)))
  .report(sprintf("superposition(infusion+bolus)[%s]", cfg$name), worst)
}

# ---------------------------------------------------------------------------
# 5. linCmtSubjectReverseADTimeVaryingProto: DOCUMENTED NEGATIVE RESULT.
#    Confirms this prototype matches "d(pred_i)/d(theta_i), history held
#    fixed" -- a real, well-defined quantity, but NOT the cumulative
#    sensitivity a shared eta/covariate parameter needs (see check #6/#7).
#    This check intentionally asserts EQUALITY to the "own theta only" FD
#    quantity (not the cumulative one) so a future change that accidentally
#    makes this prototype start producing the cumulative answer instead (or
#    silently breaks the isolation it does provide) shows up as a failure.
# ---------------------------------------------------------------------------
.checkTimeVaryingOwnThetaOnly <- function(cfg, dt1 = 1.0, dt2 = 1.5, scale = 1.6) {
  base <- if (cfg$ncmt == 1 && cfg$oral0 == 0) c(cfg$p1, cfg$v1)
          else if (cfg$ncmt == 1 && cfg$oral0 == 1) c(cfg$p1, cfg$v1, cfg$ka)
          else if (cfg$ncmt == 2 && cfg$oral0 == 0) c(cfg$p1, cfg$v1, cfg$p2, cfg$p3)
          else if (cfg$ncmt == 2 && cfg$oral0 == 1) c(cfg$p1, cfg$v1, cfg$p2, cfg$p3, cfg$ka)
          else if (cfg$ncmt == 3 && cfg$oral0 == 0) c(cfg$p1, cfg$v1, cfg$p2, cfg$p3, cfg$p4, cfg$p5)
          else c(cfg$p1, cfg$v1, cfg$p2, cfg$p3, cfg$p4, cfg$p5, cfg$ka)
  npars <- length(base)
  theta1 <- base; theta2 <- base; theta2[1] <- base[1] * scale

  mkCfg <- function(th) {
    list(p1 = th[1], v1 = th[2],
        p2 = if (cfg$ncmt >= 2) th[3] else 0, p3 = if (cfg$ncmt >= 2) th[4] else 0,
        p4 = if (cfg$ncmt >= 3) th[5] else 0, p5 = if (cfg$ncmt >= 3) th[6] else 0,
        ka = if (cfg$oral0) th[npars] else 0,
        rate = cfg$rate, ncmt = cfg$ncmt, oral0 = cfg$oral0, trans = cfg$trans)
  }
  a <- .linCmtCall(dt1, mkCfg(theta1), cfg$alast0, sensType = 3L)
  fdStep <- function(t2) .linCmtCall(dt2, mkCfg(t2), a$Alast, sensType = 3L)$val
  h <- 1e-5
  fdGrad <- sapply(seq_len(npars), function(k) {
    tp <- theta2; tp[k] <- tp[k] + h
    tm <- theta2; tm[k] <- tm[k] - h
    (fdStep(tp) - fdStep(tm)) / (2 * h)
  })

  thetaMat <- matrix(c(theta1, theta2), nrow = 2, byrow = TRUE)
  tv <- .Call(`_rxode2_linCmtSubjectReverseADTimeVaryingProto`,
             c(dt1, dt2), c(100, 0), thetaMat, cfg$rate,
             cfg$ncmt, cfg$oral0, cfg$trans, 0L)
  centralIdx <- cfg$oral0 + 1
  fxCentral <- tv[[2]]$val[centralIdx]
  protoGrad <- tv[[2]]$J[centralIdx, ] / cfg$v1
  protoGrad[2] <- protoGrad[2] - fxCentral / cfg$v1^2 # quotient rule, v1 column

  worst <- max(abs(fdGrad - protoGrad))
  .report(sprintf("timeVarying(ownThetaOnly, NOT the fix)[%s]", cfg$name), worst, tol = 1e-6)
}

# ---------------------------------------------------------------------------
# 6/7. Shared-eta-root fix: reverse-mode and forward-mode, 1-cmt IV only
#    (hard-coded in the prototypes). Ground truth: FD on eta itself, rerunning
#    the whole 2-interval sequence -- this is the unambiguous "true cumulative
#    sensitivity" a continuously-integrated ODE sensitivity state would also
#    produce, unlike check #5 above.
# ---------------------------------------------------------------------------
.checkEtaCovariateFix <- function(dt1 = 1.0, dt2 = 1.5, tcl = 1.0, tv = 20,
                                  refCov = 70, covExp = 0.75,
                                  cov1 = 70, cov2 = 90, eta0 = 0.1) {
  runSeqVal <- function(eta) {
    .Call(`_rxode2_linCmtSubjectReverseADEtaCovariateProto`,
         c(dt1, dt2), c(100, 0), c(cov1, cov2), tcl, tv, refCov, covExp, eta)[[2]]$val
  }
  h <- 1e-6
  fdDeta <- (runSeqVal(eta0 + h) - runSeqVal(eta0 - h)) / (2 * h)

  rev <- .Call(`_rxode2_linCmtSubjectReverseADEtaCovariateProto`,
              c(dt1, dt2), c(100, 0), c(cov1, cov2), tcl, tv, refCov, covExp, eta0)
  fwd <- .Call(`_rxode2_linCmtSubjectForwardADEtaCovariateProto`,
              c(dt1, dt2), c(100, 0), c(cov1, cov2), tcl, tv, refCov, covExp, eta0)

  r1 <- .report("etaCovariate(reverse) vs FD-on-eta", abs(rev[[2]]$dEta - fdDeta), tol = 1e-5)
  r2 <- .report("etaCovariate(forward) vs FD-on-eta", abs(fwd[[2]]$dEta - fdDeta), tol = 1e-5)
  r3 <- .report("etaCovariate(forward) vs (reverse)", abs(fwd[[2]]$dEta - rev[[2]]$dEta), tol = 1e-10)
  list(r1, r2, r3)
}

# ---------------------------------------------------------------------------
# 8. linCmtSubjectHybridDoseObsADProto: phase-aware hybrid correctness.
#    Phase 1: nDosesP1 boluses dtDose apart (a loading regimen). Phase 2:
#    nObsP2 pure observations dtObs apart, no more dosing -- exactly the
#    "many doses to steady state, then a rich observation history" shape.
# ---------------------------------------------------------------------------
.checkHybridDoseObs <- function(cfg, nDosesP1 = 10, dtDose = 0.5, nObsP2 = 8, dtObs = 0.4) {
  nAlast <- .linCmtNalast(cfg$ncmt, cfg$oral0)
  cfgR <- list(p1 = cfg$p1, v1 = cfg$v1, p2 = cfg$p2, p3 = cfg$p3, p4 = cfg$p4, p5 = cfg$p5,
              ka = cfg$ka, rate = rep(0, cfg$nstate),
              ncmt = cfg$ncmt, oral0 = cfg$oral0, trans = cfg$trans)

  phase1Dt <- rep(dtDose, nDosesP1)
  phase1Amt <- rep(100, nDosesP1)

  alast <- numeric(nAlast)
  for (iv in seq_len(nDosesP1)) {
    alast[1] <- alast[1] + phase1Amt[iv]
    s <- .linCmtCall(phase1Dt[iv], cfgR, alast, sensType = 3L)
    alast <- s$Alast
  }
  obsT <- dtObs * seq_len(nObsP2)
  oracleJ <- vector("list", nObsP2)
  aTmp <- alast; tPrev <- 0
  for (i in seq_len(nObsP2)) {
    s <- .linCmtCall(obsT[i] - tPrev, cfgR, aTmp, sensType = 3L)
    oracleJ[[i]] <- s$J
    aTmp <- s$Alast
    tPrev <- obsT[i]
  }

  hyb <- .Call(`_rxode2_linCmtSubjectHybridDoseObsADProto`,
              phase1Dt, phase1Amt,
              obsT, numeric(0), numeric(0), numeric(0),
              cfg$p1, cfg$v1, cfg$p2, cfg$p3, cfg$p4, cfg$p5, cfg$ka,
              cfg$ncmt, cfg$oral0, cfg$trans, 0L)
  worst <- 0
  for (i in seq_len(nObsP2)) worst <- max(worst, max(abs(oracleJ[[i]] - hyb[[i]]$J)))
  .report(sprintf("hybridDoseObs[%s]", cfg$name), worst)
}

# ---------------------------------------------------------------------------
# Top-level runner: every correctness check, every config.
# ---------------------------------------------------------------------------
runLinCmtSubjectADProtoTests <- function() {
  configs <- .linCmtConfigs()
  results <- list()
  add <- function(r) results[[length(results) + 1]] <<- r

  for (cfg in configs) add(.checkReverseADProto(cfg))
  for (cfg in configs) add(.checkReverseADBatchProto(cfg))
  for (cfg in configs) add(.checkSuperpositionBolus(cfg))
  for (cfg in configs) add(.checkSuperpositionMixed(cfg))
  for (cfg in configs) add(.checkTimeVaryingOwnThetaOnly(cfg))
  for (r in .checkEtaCovariateFix()) add(r)
  for (cfg in configs) add(.checkHybridDoseObs(cfg))

  pass <- vapply(results, function(r) isTRUE(r$pass), logical(1))
  message(sprintf("\n%d/%d checks passed", sum(pass), length(pass)))
  invisible(list(results = results, allPass = all(pass)))
}

# ---------------------------------------------------------------------------
# Performance: the numbers that justify each design decision above. Not a
# pass/fail gate (wall-clock is noisy) -- prints tables backing the plan's
# claims so a future change can be checked against them by eye.
# ---------------------------------------------------------------------------
benchLinCmtSubjectADProto <- function() {
  cfg <- .linCmtConfigs()[[6]] # 3cmt-oral, worst case for forward mode (p=7)
  dt <- 0.3

  timeIt <- function(callExpr, reps = 10) {
    t <- system.time(for (r in seq_len(reps)) eval(callExpr))["elapsed"]
    t / reps
  }

  cat("\n=== forward (production default) vs reverse-seq vs naive persistent-tape superposition ===\n")
  cat("(few doses, many observations -- the case the naive persistent-tape design was built for)\n")
  for (n in c(20, 50, 100, 200)) {
    obsT <- dt * seq_len(n)
    tf <- timeIt(quote({
      alast <- cfg$alast0
      for (iv in seq_len(n)) { s <- .linCmtCall(dt, cfg, alast, sensType = 30L); alast <- s$Alast }
    }))
    ts <- timeIt(quote({
      amt <- c(100, rep(0, n - 1))
      .Call(`_rxode2_linCmtSubjectReverseADProto`, rep(dt, n), amt,
            cfg$p1, cfg$v1, cfg$p2, cfg$p3, cfg$p4, cfg$p5, cfg$ka,
            cfg$rate, cfg$ncmt, cfg$oral0, cfg$trans, 0L)
    }))
    tsup <- timeIt(quote({
      .Call(`_rxode2_linCmtSubjectSuperpositionADProto`, obsT, 0, 100, 0,
            cfg$p1, cfg$v1, cfg$p2, cfg$p3, cfg$p4, cfg$p5, cfg$ka,
            cfg$ncmt, cfg$oral0, cfg$trans, 0L)
    }))
    cat(sprintf("  n=%4d  forward=%.5fs  reverse-seq(O(n^2))=%.5fs  superposition=%.5fs\n",
                n, tf, ts, tsup))
  }

  cat("\n=== superposition worst case: one new dose per observation (dense multi-dosing) ===\n")
  for (n in c(20, 50, 100)) {
    obsT <- dt * seq_len(n); doseT <- obsT - dt; doseAmt <- rep(100, n)
    tf <- timeIt(quote({
      alast <- cfg$alast0
      for (iv in seq_len(n)) { s <- .linCmtCall(dt, cfg, alast, sensType = 30L); alast <- s$Alast }
    }))
    tsup <- timeIt(quote({
      .Call(`_rxode2_linCmtSubjectSuperpositionADProto`, obsT, doseT, doseAmt, rep(0, n),
            cfg$p1, cfg$v1, cfg$p2, cfg$p3, cfg$p4, cfg$p5, cfg$ka,
            cfg$ncmt, cfg$oral0, cfg$trans, 0L)
    }))
    cat(sprintf("  n=%4d  forward=%.5fs  superposition(worst case)=%.5fs  %s\n",
                n, tf, tsup, if (tsup > tf) "-- forward wins here, as expected" else ""))
  }

  cat("\n=== phase-aware hybrid: scaling doses and observations independently ===\n")
  dtDose <- 0.5; dtObs <- 0.2
  cat("-- scaling doses (nObs fixed=20) --\n")
  for (nDoses in c(10, 50, 100, 200)) {
    th <- timeIt(quote({
      phase1Dt <- rep(dtDose, nDoses); phase1Amt <- rep(100, nDoses)
      obsT <- dtObs * seq_len(20)
      .Call(`_rxode2_linCmtSubjectHybridDoseObsADProto`,
            phase1Dt, phase1Amt, obsT, numeric(0), numeric(0), numeric(0),
            cfg$p1, cfg$v1, cfg$p2, cfg$p3, cfg$p4, cfg$p5, cfg$ka,
            cfg$ncmt, cfg$oral0, cfg$trans, 0L)
    }))
    tf <- timeIt(quote({
      alast <- numeric(.linCmtNalast(cfg$ncmt, cfg$oral0))
      for (iv in seq_len(nDoses)) { alast[1] <- alast[1] + 100; s <- .linCmtCall(dtDose, cfg, alast, sensType = 30L); alast <- s$Alast }
      for (iv in seq_len(20)) { s <- .linCmtCall(dtObs, cfg, alast, sensType = 30L); alast <- s$Alast }
    }))
    cat(sprintf("  nDoses=%4d  hybrid=%.5fs  forward-prod=%.5fs\n", nDoses, th, tf))
  }
  cat("-- scaling observations (nDoses fixed=20) --\n")
  for (nObs in c(10, 50, 100, 200)) {
    th <- timeIt(quote({
      phase1Dt <- rep(dtDose, 20); phase1Amt <- rep(100, 20)
      obsT <- dtObs * seq_len(nObs)
      .Call(`_rxode2_linCmtSubjectHybridDoseObsADProto`,
            phase1Dt, phase1Amt, obsT, numeric(0), numeric(0), numeric(0),
            cfg$p1, cfg$v1, cfg$p2, cfg$p3, cfg$p4, cfg$p5, cfg$ka,
            cfg$ncmt, cfg$oral0, cfg$trans, 0L)
    }))
    tf <- timeIt(quote({
      alast <- numeric(.linCmtNalast(cfg$ncmt, cfg$oral0))
      for (iv in seq_len(20)) { alast[1] <- alast[1] + 100; s <- .linCmtCall(dtDose, cfg, alast, sensType = 30L); alast <- s$Alast }
      for (iv in seq_len(nObs)) { s <- .linCmtCall(dtObs, cfg, alast, sensType = 30L); alast <- s$Alast }
    }))
    cat(sprintf("  nObs=%4d  hybrid=%.5fs  forward-prod=%.5fs\n", nObs, th, tf))
  }

  cat("\n=== eta-covariate fix: forward vs reverse cost, time-varying covariate ===\n")
  tcl <- 1.0; tv <- 20; refCov <- 70; covExp <- 0.75
  for (n in c(20, 50, 100, 200, 400)) {
    obsT <- dt * seq_len(n)
    cov <- ifelse(obsT < obsT[length(obsT)] / 2, 70, 90)
    amt <- c(100, rep(0, n - 1))
    tfwd <- timeIt(quote(.Call(`_rxode2_linCmtSubjectForwardADEtaCovariateProto`,
                              rep(dt, n), amt, cov, tcl, tv, refCov, covExp, 0.1)))
    trev <- timeIt(quote(.Call(`_rxode2_linCmtSubjectReverseADEtaCovariateProto`,
                              rep(dt, n), amt, cov, tcl, tv, refCov, covExp, 0.1)))
    cat(sprintf("  n=%4d  forward-eta=%.5fs  reverse-eta=%.5fs  ratio(rev/fwd)=%.1fx\n",
                n, tfwd, trev, trev / tfwd))
  }
  invisible(NULL)
}
