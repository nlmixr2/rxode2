# HISTORICAL RECORD: the prototype exports this script calls were removed from
# src/linCmt.cpp in fb37eabe8; check out 73f2e13d0 (or earlier) to re-run it.
# RECORD OF A DECISION, not a live benchmark.  The superposition strategy it
# measured (rxControl(linCmtSensStrategy="superposition"), phase 4.2) was
# removed from the package after this gate's verdict, so the "sup" arm and
# rxode2:::linCmtSupStats() below no longer exist in the current tree; the
# measured results are kept in bench/results/lincmt_strategy_gate_1cmt_3cmt.rds.
# To re-run it as written, check out eac2a66d2..f1bb62bc2.
#
# Measurement gate before phase 4.3 of the linCmt() speed plan (see
# ~/.claude/plans/the-lincmt-solutions-are-calm-seahorse.md).  Phase 4.2 found
# that plain sequential reverse-mode AD (linCmtSensType="ADr", threaded since
# the guard relaxation) beats the production superposition kernel, and the
# speed prototypes were only ever benchmarked against sequential FORWARD
# mode.  This bench re-measures the phase-aware hybrid's target shape -- many
# closely spaced doses, then a rich observation history -- against the new
# cheap baseline:
#
#   (a) sequential forward  (linCmtSensType="AD",  today's default)  real rxSolve()
#   (b) sequential reverse  (linCmtSensType="ADr")                    real rxSolve()
#   (d) superposition strategy (phase 4.2)                           real rxSolve()
#   (c) hybrid prototype (linCmtSubjectHybridDoseObsADProto), one subject,
#       prototype level.  Its production cost is bracketed two ways: a LOWER
#       bound of prototype time x nSub (zero wrapper overhead), and an
#       ESTIMATE scaled by the measured production-superposition /
#       prototype-superposition overhead factor on the few-dose shape where
#       both do the same per-row work (phase 4.2's benchmark shape).  The
#       prototype sequential reverse kernel is NOT used as a comparator: it is
#       the persistent-tape O(n^2) design, unlike production ADr's flat
#       per-row nest, so a ratio against it is meaningless.
#
# Usage (package root):  Rscript bench/lincmt_strategy_gate.R
# or source() it and call benchLinCmtStrategyGate().
#
# Measured 2026-08-21 (44 subjects, 11 threads; bench/results/
# lincmt_strategy_gate_1cmt_3cmt.rds): ADr beats forward 1.4-1.6x (2cmt-oral)
# and 1.7-2.1x (3cmt-oral), parity at 1cmt-iv.  The hybrid is within +-10% of
# ADr at 2cmt-oral and 1.1-1.7x SLOWER at 3cmt-oral even before wrapper
# overhead (its phase-1 forward roll-through costs npars passes per dose row
# where ADr pays one reverse nest); it only wins at 1cmt-iv (1.2-2.3x, the
# least reliable estimate, on solves already under 0.1s).  Superposition is
# 6-11x slower than ADr on this dose-heavy shape, as designed.  Verdict:
# sequential ADr is the production default; the hybrid is not worth a
# production port.

if (requireNamespace("devtools", quietly = TRUE) &&
      file.exists("DESCRIPTION") && file.exists("src/linCmt.cpp")) {
  devtools::load_all(".", quiet = TRUE)
} else {
  library(rxode2)
}
source(file.path("bench", "lincmt_oracle.R"))

.gateGradModel <- function(cfg) {
  npars <- 2L*cfg$ncmt + cfg$oral0
  args <- sprintf("rx__PTR__, t, 1, %d, %d, %%d, %%d, 1, cl, v, q, vp, q2, vp2, ka",
                  cfg$ncmt, cfg$oral0)
  lines <- c(sprintf("cp=linCmtB(%s)", sprintf(args, -1L, -1L)),
             vapply(seq_len(npars) - 1L, function(k) {
               sprintf("d%d=linCmtB(%s)", k, sprintf(args, -2L, k))
             }, ""))
  suppressWarnings(rxode2(paste(lines, collapse = "\n")))
}

.gatePars <- function(cfg) {
  c(cl = cfg$p1, v = cfg$v1, q = cfg$p2, vp = cfg$p3, q2 = cfg$p4, vp2 = cfg$p5,
    ka = cfg$ka)
}

# nDoses boluses dtDose apart with no observations in between, then nObs
# observations dtObs apart -- the hybrid's precondition shape.
.gateEv <- function(nDoses, nObs, nSub, dtDose = 0.5, dtObs = 0.2) {
  doseT <- (seq_len(nDoses) - 1) * dtDose
  obsT <- max(doseT) + dtDose + dtObs * seq_len(nObs)
  do.call(rbind, lapply(seq_len(nSub), function(i) {
    rbind(data.frame(id = i, time = doseT, amt = 100, evid = 1, cmt = 1,
                     rate = 0, ii = 0, ss = 0),
          data.frame(id = i, time = obsT, amt = 0, evid = 0, cmt = 1,
                     rate = 0, ii = 0, ss = 0))
  }))
}

# inner > 1 batches sub-millisecond prototype calls inside one measurement so
# the result is not quantized by system.time()'s resolution
.gateTime <- function(fn, reps, inner = 1L) {
  median(vapply(seq_len(reps), function(i) {
    system.time(for (k in seq_len(inner)) fn())[["elapsed"]] / inner
  }, 0))
}

.gateSolve <- function(m, cfg, evt, arm, cores) {
  sens <- if (arm == "adr") "ADr" else "AD"
  strat <- if (arm == "sup") "superposition" else "forward"
  rxSolve(m, params = .gatePars(cfg), events = evt, returnType = "data.frame",
          linCmtSensType = sens, linCmtSensStrategy = strat, cores = cores)
}

.gateProtoHybrid <- function(cfg, nDoses, nObs, dtDose = 0.5, dtObs = 0.2) {
  .Call(`_rxode2_linCmtSubjectHybridDoseObsADProto`,
        rep(dtDose, nDoses), rep(100, nDoses), rep(0, nDoses),
        dtObs * seq_len(nObs), numeric(0), numeric(0), numeric(0),
        cfg$p1, cfg$v1, cfg$p2, cfg$p3, cfg$p4, cfg$p5, cfg$ka,
        cfg$ncmt, cfg$oral0, cfg$trans, 0L)
}

# Wrapper-overhead factor: production superposition strategy vs the
# superposition prototype on phase 4.2's few-dose / dense-observation shape
# (2 boluses, nObs observations), where both evaluate the same 1-2 term list
# per row.  Returned as production seconds per subject / prototype seconds.
.gateSupOverhead <- function(m, cfg, nObs = 200L, nSub = 44L, repsProd = 5L,
                             repsProto = 10L) {
  ev <- do.call(rbind, lapply(seq_len(nSub), function(i) {
    rbind(data.frame(id = i, time = c(0, 24), amt = 100, evid = 1, cmt = 1,
                     rate = 0, ii = 0, ss = 0),
          data.frame(id = i, time = seq(0.25, 72, length.out = nObs), amt = 0,
                     evid = 0, cmt = 1, rate = 0, ii = 0, ss = 0))
  }))
  evt <- rxode2::etTrans(ev, m)
  .gateSolve(m, cfg, evt, "sup", 1L)
  tS <- .gateTime(function() .gateSolve(m, cfg, evt, "sup", 1L), repsProd)
  obsT <- seq(0.25, 72, length.out = nObs)
  tP <- .gateTime(function() {
    .Call(`_rxode2_linCmtSubjectSuperpositionADProto`, obsT, c(0, 24), c(100, 100),
          c(0, 0), cfg$p1, cfg$v1, cfg$p2, cfg$p3, cfg$p4, cfg$p5, cfg$ka,
          cfg$ncmt, cfg$oral0, cfg$trans, 0L)
  }, repsProto, inner = 20L)
  (tS / nSub) / tP
}

benchLinCmtStrategyGate <- function(cfgIdx = c(1L, 4L, 6L),
                                    doseGrid = c(20L, 100L, 200L),
                                    obsGrid = c(50L, 200L, 400L),
                                    nSub = 44L, repsProd = 5L, repsProto = 10L,
                                    nThr = getRxThreads()) {
  cfgs <- .linCmtConfigs()[cfgIdx]
  rows <- list()
  for (cfg in cfgs) {
    m <- .gateGradModel(cfg)
    ov <- .gateSupOverhead(m, cfg, nSub = nSub, repsProd = repsProd,
                           repsProto = repsProto)
    cat(sprintf("\n=== %s (npars=%d)  production/prototype overhead factor %.2fx ===\n",
                cfg$name, 2L*cfg$ncmt + cfg$oral0, ov))
    for (nDoses in doseGrid) for (nObs in obsGrid) {
      evt <- rxode2::etTrans(.gateEv(nDoses, nObs, nSub), m)
      .gateSolve(m, cfg, evt, "forward", 1L) # warm the model cache
      tF1 <- .gateTime(function() .gateSolve(m, cfg, evt, "forward", 1L), repsProd)
      tR1 <- .gateTime(function() .gateSolve(m, cfg, evt, "adr", 1L), repsProd)
      invisible(utils::getFromNamespace("linCmtSupStats", "rxode2")(TRUE))
      tS1 <- .gateTime(function() .gateSolve(m, cfg, evt, "sup", 1L), repsProd)
      st <- utils::getFromNamespace("linCmtSupStats", "rxode2")(TRUE)
      tFN <- .gateTime(function() .gateSolve(m, cfg, evt, "forward", nThr), repsProd)
      tRN <- .gateTime(function() .gateSolve(m, cfg, evt, "adr", nThr), repsProd)
      tSN <- .gateTime(function() .gateSolve(m, cfg, evt, "sup", nThr), repsProd)
      tH <- .gateTime(function() .gateProtoHybrid(cfg, nDoses, nObs), repsProto,
                      inner = 20L)
      tHlb1 <- tH * nSub           # zero-overhead lower bound
      tHest1 <- tHlb1 * ov         # with the measured wrapper overhead
      tHestN <- tHest1 * (tRN / tR1) # assume the same thread scaling as ADr
      adrPerSub <- tR1 / nSub
      best1 <- c(forward = tF1, adr = tR1, sup = tS1, hybridEst = tHest1)
      bestN <- c(forward = tFN, adr = tRN, sup = tSN, hybridEst = tHestN)
      rows[[length(rows) + 1L]] <- data.frame(
        cfg = cfg$name, nDoses = nDoses, nObs = nObs,
        fwd1 = tF1, adr1 = tR1, sup1 = tS1, hybLB1 = tHlb1, hybEst1 = tHest1,
        fwdN = tFN, adrN = tRN, supN = tSN, hybEstN = tHestN,
        protoHybPerSub = tH, adrPerSub = adrPerSub, protoHybOverAdr = tH / adrPerSub,
        overhead = ov,
        supPrimes = st[["primes"]], supDoses = st[["doses"]],
        supCons = st[["consolidations"]], supRows = st[["rows"]],
        best1 = names(best1)[which.min(best1)],
        bestN = names(bestN)[which.min(bestN)],
        bestOverAdr1 = tR1 / min(best1), bestOverAdrN = tRN / min(bestN),
        stringsAsFactors = FALSE)
      cat(sprintf(paste0("  doses=%3d obs=%3d | 1thr fwd %.3f adr %.3f sup %.3f hybLB %.3f hyb~%.3f",
                         " | %dthr fwd %.3f adr %.3f sup %.3f hyb~%.3f",
                         " | proto-hyb/prod-adr per subj %.2fx | best1 %s (%.2fx adr) bestN %s (%.2fx adr)\n"),
                  nDoses, nObs, tF1, tR1, tS1, tHlb1, tHest1, nThr, tFN, tRN, tSN, tHestN,
                  tH / adrPerSub, names(best1)[which.min(best1)], tR1 / min(best1),
                  names(bestN)[which.min(bestN)], tRN / min(bestN)))
    }
  }
  out <- do.call(rbind, rows)
  attr(out, "nThr") <- nThr
  attr(out, "nSub") <- nSub
  invisible(out)
}

if (sys.nframe() == 0L) {
  res <- benchLinCmtStrategyGate()
  dir.create(file.path("bench", "results"), showWarnings = FALSE)
  saveRDS(res, file.path("bench", "results", "lincmt_strategy_gate.rds"))
  print(res[, c("cfg", "nDoses", "nObs", "fwd1", "adr1", "sup1", "hybLB1", "hybEst1",
                "adrN", "supN", "hybEstN", "protoHybOverAdr", "best1", "bestN",
                "bestOverAdr1", "bestOverAdrN")], digits = 3)
}
