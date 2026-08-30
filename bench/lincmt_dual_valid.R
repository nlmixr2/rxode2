# Validation for the multi-direction forward-mode Jacobian
# (linCmtSensType = "ADm", sensType 32) against the shipped per-direction
# forward mode (sensType 3) and reverse mode (sensType 31).
#
# The claim being tested is BITWISE identity with sensType 3, not agreement
# to round-off: dualN reproduces the operation order of every stan/math/fwd
# rule it replaces and drives the identical templated kernels, so slot si of
# the single pass must compute exactly what the fvar pass for that direction
# computes.  Any difference is a defect in a dualN rule, not a summation
# order choice.
#
# Sweeps ncmt x oral x trans x steady-state type x dt x requested-direction
# mask, so every kernel in linCmt.h is exercised -- including the
# steady-state kernels, which have no constants/tail factorization and so
# are served by the full dual evaluator rather than the window path.
#
# Usage: Rscript bench/lincmt_dual_valid.R
suppressMessages(devtools::load_all(".", compile = FALSE, quiet = TRUE))
rxode2::setRxThreads(1L)

diffKa <- 1L; diffP1 <- 2L; diffV1 <- 4L; diffP2 <- 8L
diffP3 <- 16L; diffP4 <- 32L; diffP5 <- 64L

.nAlast <- function(ncmt, oral0) {
  npars <- 2L * ncmt + oral0
  ncmt + oral0 + ncmt * npars + oral0
}

.call1 <- function(dt, cfg, alast, sensType, ndiff) {
  .Call(`_rxode2_linCmtModelDouble`,
        dt, cfg$p1, cfg$v1, cfg$p2, cfg$p3, cfg$p4, cfg$p5, cfg$ka,
        as.double(alast), as.double(cfg$rate),
        cfg$ncmt, cfg$oral0, cfg$trans, TRUE,
        cfg$type, cfg$tau, cfg$tinf, cfg$amt, cfg$bolusCmt,
        as.integer(ndiff), as.integer(sensType), 0.001)
}

# Two intervals so the carry-forward sensitivity (getAlastAD) is exercised.
.record <- function(cfg, dt, sensType, ndiff) {
  s1 <- .call1(dt, cfg, cfg$alast0, sensType, ndiff)
  s2 <- .call1(dt, cfg, s1$Alast, sensType, ndiff)
  c(as.numeric(s1$val), as.numeric(s1$J), as.numeric(s1$Jg),
    as.numeric(s2$val), as.numeric(s2$J), as.numeric(s2$Jg))
}

.mkCfg <- function(ncmt, oral0, trans, type, rate0 = 0) {
  nstate <- ncmt + oral0
  pars <- switch(as.character(trans),
    # trans 1: CL, V, Q, Vp, Q2, Vp2
    "1" = list(p1 = 1.0, v1 = 20, p2 = 2.0, p3 = 40, p4 = 0.5, p5 = 60),
    # trans 2: micro rate constants
    "2" = list(p1 = 0.05, v1 = 20, p2 = 0.1, p3 = 0.05, p4 = 0.025, p5 = 0.0083),
    # trans 3/4/5 (2-cmt only) and 10/11 (macro A/alpha) use values that keep
    # the eigen-decomposition real and the compartments positive.
    "3" = list(p1 = 1.0, v1 = 20, p2 = 2.0, p3 = 60, p4 = 0, p5 = 0),
    "4" = list(p1 = 0.2, v1 = 20, p2 = 0.03, p3 = 0.05, p4 = 0, p5 = 0),
    "5" = list(p1 = 0.2, v1 = 20, p2 = 0.03, p3 = 2.0, p4 = 0, p5 = 0),
    "10" = list(p1 = 0.2, v1 = 0.03, p2 = 0.03, p3 = 0.01, p4 = 0.005, p5 = 0.004),
    "11" = list(p1 = 0.2, v1 = 20, p2 = 0.03, p3 = 0.01, p4 = 0.005, p5 = 0.004))
  amounts <- numeric(nstate); amounts[1] <- 100
  rate <- numeric(nstate)
  if (rate0 > 0) rate[1L + oral0] <- rate0
  c(list(name = sprintf("%dcmt-%s-trans%d-type%d%s", ncmt,
                        if (oral0 == 1L) "oral" else "iv", trans, type,
                        if (rate0 > 0) "-inf" else ""),
         ncmt = ncmt, oral0 = oral0, trans = trans, ka = 1.1,
         rate = rate, nstate = nstate,
         alast0 = c(amounts, numeric(.nAlast(ncmt, oral0) - nstate)),
         type = type,
         tau = if (type > 0L) 12.0 else 0.0,
         tinf = if (type == 2L) 2.0 else 0.0,
         amt = if (type == 3L) 100.0 else 0.0,
         bolusCmt = 0L),
    pars)
}

.configs <- function() {
  out <- list()
  for (ncmt in 1:3) {
    tr <- c(1L, 2L)
    if (ncmt == 2L) tr <- c(tr, 3L, 4L, 5L, 10L, 11L)
    if (ncmt == 3L) tr <- c(tr, 10L, 11L)
    if (ncmt == 1L) tr <- c(tr, 10L, 11L)
    for (trans in tr) {
      for (oral0 in c(0L, 1L)) {
        for (type in 0:3) {
          out[[length(out) + 1L]] <- .mkCfg(ncmt, oral0, trans, as.integer(type))
        }
        # normal row carrying a zero-order infusion
        out[[length(out) + 1L]] <- .mkCfg(ncmt, oral0, trans, 0L, rate0 = 25)
      }
    }
  }
  out
}

# Requested-direction masks: all directions, plus subsets that drive
# dualJacN<1> .. <7>.
.masks <- function(ncmt, oral0) {
  full <- diffP1 + diffV1
  if (ncmt >= 2L) full <- full + diffP2 + diffP3
  if (ncmt >= 3L) full <- full + diffP4 + diffP5
  if (oral0 == 1L) full <- full + diffKa
  ms <- c(0L, full, diffP1, diffP1 + diffV1)
  if (ncmt >= 2L) ms <- c(ms, diffP1 + diffV1 + diffP2)
  if (oral0 == 1L) ms <- c(ms, diffKa, diffP1 + diffKa)
  unique(ms)
}

nCase <- 0L; nSkip <- 0L; bad <- list(); worstRev <- 0
for (cfg in .configs()) {
  for (dt in c(0.25, 1.0, 4.0)) {
    for (nd in .masks(cfg$ncmt, cfg$oral0)) {
      ref <- try(.record(cfg, dt, 3L, nd), silent = TRUE)
      if (inherits(ref, "try-error")) { nSkip <- nSkip + 1L; next }
      cand <- try(.record(cfg, dt, 32L, nd), silent = TRUE)
      if (inherits(cand, "try-error")) {
        bad[[length(bad) + 1L]] <- sprintf("%s dt=%g nd=%d: ADm errored", cfg$name, dt, nd)
        next
      }
      nCase <- nCase + 1L
      # identical() on the raw doubles: NA/NaN patterns must match too.
      if (!identical(ref, cand)) {
        w <- which(!(ref == cand | (is.na(ref) & is.na(cand))))
        bad[[length(bad) + 1L]] <-
          sprintf("%s dt=%g nd=%d: %d/%d entries differ, worst |d| = %.3e",
                  cfg$name, dt, nd, length(w), length(ref),
                  suppressWarnings(max(abs(ref[w] - cand[w]), na.rm = TRUE)))
      }
      rev <- try(.record(cfg, dt, 31L, nd), silent = TRUE)
      if (!inherits(rev, "try-error")) {
        ok <- is.finite(ref) & is.finite(rev)
        if (any(ok)) {
          rd <- abs(ref[ok] - rev[ok]) / pmax(abs(ref[ok]), 1e-8)
          worstRev <- max(worstRev, max(rd))
        }
      }
    }
  }
}

message(sprintf("== ADm (32) vs AD (3): %d cases, %d skipped, %d MISMATCHED",
                nCase, nSkip, length(bad)))
if (length(bad)) {
  for (b in utils::head(bad, 20)) message("  ", b)
} else {
  message("  all bitwise identical")
}
message(sprintf("== AD (3) vs ADr (31) worst relative difference: %.3e", worstRev))
quit(status = if (length(bad)) 1L else 0L)
