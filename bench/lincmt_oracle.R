# linCmt() AD-Jacobian numeric oracle harness.
#
# Phase 1 of the forward-mode-AD linCmt plan (~/src/rxode2-linCmt-stan-plan.md).
#
# Captures the reverse-mode AD reference (sensType = 3) value `val`, Jacobian
# `J`, and concentration-gradient `Jg` from `linCmtModelDouble` for a panel of
# linear-compartment configurations, over a two-interval solve so the
# carry-forward sensitivity (getAlast on an AD type) is exercised. The captured
# record is the regression oracle that the forward-mode path (linCmtFwdJac,
# sensType = 30) is checked against in phase 2.
#
# Usage (from the package root, with the package loaded via devtools::load_all):
#   source("bench/lincmt_oracle.R")
#   writeLinCmtOracle()                 # capture reverse-AD reference -> JSON
#   compareLinCmtSens(30L)              # diff a candidate sensType vs the oracle

.linCmtCall <- function(dt, cfg, alast, sensType = 3L, type = 0L,
                        tau = 0, tinf = 0, amt = 0, bolusCmt = 0L) {
  .Call(`_rxode2_linCmtModelDouble`,
        dt,
        cfg$p1, cfg$v1, cfg$p2, cfg$p3, cfg$p4, cfg$p5, cfg$ka,
        as.double(alast), as.double(cfg$rate),
        cfg$ncmt, cfg$oral0, cfg$trans,
        TRUE,            # deriv
        type, tau, tinf, amt, bolusCmt,
        0L,              # ndiff (0 -> use sensType path)
        as.integer(sensType), 0.001)
}

# getNalast() for the deriv path: ncmt + oral0 + ncmt*npars + oral0,
# npars = 2*ncmt + oral0.
.linCmtNalast <- function(ncmt, oral0) {
  npars <- 2L * ncmt + oral0
  ncmt + oral0 + ncmt * npars + oral0
}

# Panel of configurations: 1/2/3-compartment, IV and oral, trans = 1 (CL/V).
.linCmtConfigs <- function() {
  mk <- function(name, ncmt, oral0,
                 p1, v1, p2 = 0, p3 = 0, p4 = 0, p5 = 0, ka = 0) {
    nstate <- ncmt + oral0
    rate <- rep(0, nstate)
    # Fresh bolus of 100 into the first solved compartment (depot for oral,
    # central for IV); remaining states start empty. Sensitivity carry begins
    # at zero, then interval 2 exercises a non-zero carry.
    amounts <- numeric(nstate)
    amounts[1] <- 100
    nAlast <- .linCmtNalast(ncmt, oral0)
    alast0 <- c(amounts, numeric(nAlast - nstate))
    list(name = name, ncmt = ncmt, oral0 = oral0, trans = 1L,
         p1 = p1, v1 = v1, p2 = p2, p3 = p3, p4 = p4, p5 = p5, ka = ka,
         rate = rate, alast0 = alast0, nstate = nstate)
  }
  list(
    mk("1cmt-iv",   1L, 0L, p1 = 1.0, v1 = 20),
    mk("1cmt-oral", 1L, 1L, p1 = 1.0, v1 = 20, ka = 1.1),
    mk("2cmt-iv",   2L, 0L, p1 = 1.0, v1 = 20, p2 = 2.0, p3 = 40),
    mk("2cmt-oral", 2L, 1L, p1 = 1.0, v1 = 20, p2 = 2.0, p3 = 40, ka = 1.1),
    mk("3cmt-iv",   3L, 0L, p1 = 1.0, v1 = 20, p2 = 2.0, p3 = 40, p4 = 0.5, p5 = 60),
    mk("3cmt-oral", 3L, 1L, p1 = 1.0, v1 = 20, p2 = 2.0, p3 = 40, p4 = 0.5, p5 = 60, ka = 1.1)
  )
}

.linCmtDtGrid <- function() c(0.25, 1.0, 4.0)

# Run a two-interval solve for one config at one dt; return the captured
# value/Jacobian/Jg for each interval as plain numeric vectors.
.linCmtSolveRecord <- function(cfg, dt, sensType) {
  s1 <- .linCmtCall(dt, cfg, cfg$alast0, sensType = sensType)
  # Reconstruct an alast for interval 2 from interval 1's returned Alast
  # (carry-forward sensitivity is encoded there).
  alast2 <- s1$Alast
  s2 <- .linCmtCall(dt, cfg, alast2, sensType = sensType)
  list(
    interval1 = list(val = as.numeric(s1$val), J = as.numeric(s1$J),
                     Jg = as.numeric(s1$Jg)),
    interval2 = list(val = as.numeric(s2$val), J = as.numeric(s2$J),
                     Jg = as.numeric(s2$Jg))
  )
}

# Build the full record across the panel for a given sensType.
buildLinCmtRecord <- function(sensType = 3L) {
  out <- list()
  for (cfg in .linCmtConfigs()) {
    for (dt in .linCmtDtGrid()) {
      key <- sprintf("%s@dt=%g", cfg$name, dt)
      out[[key]] <- .linCmtSolveRecord(cfg, dt, sensType)
    }
  }
  out
}

.linCmtResultsDir <- function() {
  d <- file.path("bench", "results")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  d
}

.linCmtOraclePath <- function() {
  file.path(.linCmtResultsDir(), "lincmt_oracle.json")
}

# Capture the reverse-AD reference and write it to JSON.
writeLinCmtOracle <- function(sensType = 3L) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite required to write the oracle")
  }
  rec <- buildLinCmtRecord(sensType)
  jsonlite::write_json(rec, .linCmtOraclePath(),
                       digits = 16, auto_unbox = TRUE, pretty = TRUE)
  message("wrote ", length(rec), " cases to ", .linCmtOraclePath())
  invisible(rec)
}

# Compare a candidate sensType against the stored oracle; returns the worst
# absolute and relative difference across every value/J/Jg entry.
compareLinCmtSens <- function(sensType, tol = 1e-6) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite required to read the oracle")
  }
  oracle <- jsonlite::read_json(.linCmtOraclePath(), simplifyVector = TRUE)
  cand   <- buildLinCmtRecord(sensType)
  worstAbs <- 0; worstRel <- 0; worstWhere <- NA_character_
  nonFinite <- character(0)
  for (key in names(oracle)) {
    for (iv in c("interval1", "interval2")) {
      for (fld in c("val", "J", "Jg")) {
        o <- as.numeric(oracle[[key]][[iv]][[fld]])
        c0 <- as.numeric(cand[[key]][[iv]][[fld]])
        if (length(o) != length(c0)) {
          stop(sprintf("length mismatch %s/%s/%s: %d vs %d",
                       key, iv, fld, length(o), length(c0)))
        }
        # Any non-finite candidate entry is an automatic failure -- never let
        # NaN/Inf slip past which.max(), which silently ignores NaN.
        if (any(!is.finite(c0))) {
          nonFinite <- c(nonFinite,
                         sprintf("%s/%s/%s", key, iv, fld))
        }
        ad <- abs(o - c0)
        ad[!is.finite(ad)] <- Inf
        rd <- ad / pmax(abs(o), 1e-8)
        mi <- which.max(ad)
        if (length(mi) && is.finite(ad[mi]) && ad[mi] > worstAbs) {
          worstAbs <- ad[mi]; worstRel <- rd[mi]
          worstWhere <- sprintf("%s/%s/%s[%d]", key, iv, fld, mi)
        }
      }
    }
  }
  pass <- worstAbs <= tol && length(nonFinite) == 0L
  message(sprintf("sensType %d vs oracle: worst |diff| = %.3e (rel %.3e) at %s -> %s",
                  sensType, worstAbs, worstRel, worstWhere,
                  if (pass) "PASS" else "FAIL"))
  if (length(nonFinite) > 0L) {
    message(sprintf("  %d non-finite candidate field(s), e.g. %s",
                    length(nonFinite), paste(utils::head(nonFinite, 3), collapse = ", ")))
  }
  invisible(list(pass = pass, worstAbs = worstAbs, worstRel = worstRel,
                 where = worstWhere, nonFinite = nonFinite))
}
