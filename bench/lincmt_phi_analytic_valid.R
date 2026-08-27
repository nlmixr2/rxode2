# Validation for the closed-form transition matrix (linCmtSensPhi = 2).
#
# Unlike "ADm", this is a SUMMATION-ORDER change -- Phi is assembled and then
# applied, where the row tail accumulates the same products as it goes -- so
# the bar is the one the probe-built matrix already ships under: agreement
# with the row tail to a few units in the last place, and agreement with
# reverse mode (an independent code path) to round-off.
#
# The probe-built matrix (phi = 1) is exact by construction, so where it
# engages it is also a direct check on the closed-form algebra itself.
#
# Usage: Rscript bench/lincmt_phi_analytic_valid.R
suppressMessages(devtools::load_all(".", compile = FALSE, quiet = TRUE))
rxode2::setRxThreads(1L)

.gradModel <- function(ncmt, oral0, dirs) {
  args <- sprintf("rx__PTR__, t, 1, %d, %d, %%d, %%d, %%s, cl, v, q, vp, q2, vp2, ka",
                  ncmt, oral0)
  function(trans) {
    a <- sprintf(args, -1L, -1L, trans)
    lines <- c(sprintf("cp=linCmtB(%s)", a),
               vapply(dirs, function(k) {
                 sprintf("d%d=linCmtB(%s)", k, sprintf(args, -2L, k, trans))
               }, ""))
    suppressWarnings(rxode2::rxode2(paste(lines, collapse = "\n")))
  }
}
.parsFor <- function(ncmt, oral0) {
  p <- c(cl = 2.1, v = 21, q = 3.3, vp = 43, q2 = 0.9, vp2 = 61, ka = 1.3)
  if (ncmt < 2) p[c("q", "vp")] <- 0
  if (ncmt < 3) p[c("q2", "vp2")] <- 0
  if (oral0 == 0) p["ka"] <- 0
  p
}
# irregular bolus + infusion doses, then an irregular observation run, so the
# delta memo misses and both paths build their own exponentials
.ev <- function(nSub = 3L) {
  do.call(rbind, lapply(seq_len(nSub), function(i) {
    sh <- 0.37 * (i - 1)
    dose <- data.frame(id = i, time = c(0, 5.5, 12, 18.25, 26) + c(0, sh, 0, sh, 0),
                       amt = c(100, 80, 120, 90, 110) * (1 + 0.1 * i), evid = 1,
                       cmt = 1, rate = c(0, 40, 0, 60, 0), ii = 0, ss = 0)
    obs <- data.frame(id = i, time = sort(c(0.4, 1.1, 2.7, 4.9, 7.3, 9.9, 13.2,
                                            15.8, 19.4, 22.1, 27.3, 31.9, 38.2,
                                            44.7, 52.1) + sh),
                      amt = 0, evid = 0, cmt = 1, rate = 0, ii = 0, ss = 0)
    rbind(dose, obs)
  }))
}
.cols <- function(a) grep("^(cp|d[0-9]+)$", names(a), value = TRUE)
.relDiff <- function(a, b) {
  max(vapply(.cols(a), function(cc) {
    max(abs(a[[cc]] - b[[cc]]) / pmax(1e-8, abs(b[[cc]])))
  }, 0))
}
# Scaled by the column's own magnitude, not by each entry's: a sensitivity
# that crosses zero has no meaningful relative error at the crossing, and
# per-entry scaling reports that noise as a failure.
.colScaled <- function(a, b) {
  max(vapply(.cols(a), function(cc) {
    x <- a[[cc]]; y <- b[[cc]]
    sc <- max(abs(y), na.rm = TRUE)
    if (sc == 0) 0 else max(abs(x - y)) / sc
  }, 0))
}

worstTail <- 0; worstRev <- 0; worstTailRev <- 0; where <- ""
for (ncmt in 1:3) {
  for (oral0 in c(0L, 1L)) {
    npars <- 2L * ncmt + oral0
    mk <- .gradModel(ncmt, oral0, seq_len(npars) - 1L)
    for (trans in if (ncmt == 2L) c(1L, 2L, 3L, 4L, 5L) else c(1L, 2L)) {
      m <- mk(trans)
      pars <- .parsFor(ncmt, oral0)
      ev <- .ev()
      sol <- function(phi, sens = "AD") {
        rxode2::rxSolve(m, params = pars, events = ev, returnType = "data.frame",
                        cores = 1L, linCmtSensType = sens, linCmtSensPhi = phi)
      }
      tail0 <- sol(0L)
      ana <- sol(2L)
      rev <- sol(0L, "ADr")
      dT <- .colScaled(ana, tail0); dR <- .colScaled(ana, rev)
      dRT <- .colScaled(tail0, rev)
      if (dT > worstTail) { worstTail <- dT; where <- sprintf("%dcmt oral%d trans%d", ncmt, oral0, trans) }
      worstRev <- max(worstRev, dR)
      worstTailRev <- max(worstTailRev, dRT)
      st <- rxode2:::linCmtSeqStats(TRUE)
    }
  }
}
message(sprintf("phi=2 vs row tail       : %.3e (column-scaled) at %s", worstTail, where))
message(sprintf("phi=2 vs reverse mode    : %.3e", worstRev))
message(sprintf("row tail vs reverse mode : %.3e   <- the existing spread", worstTailRev))

# Sharpest check on the closed-form algebra itself: where the probe-built
# matrix engages it is exact by construction (its entries ARE the kernel's
# response to unit-basis prior states), so the two matrices must agree.
message("-- against the probe-built matrix, on a design where it engages --")
worstProbe <- 0; whereP <- ""
for (ncmt in 2:3) {
  npars <- 2L * ncmt + 1L
  m <- .gradModel(ncmt, 1L, seq_len(npars) - 1L)(1L)
  evU <- rxode2::et(rxode2::et(amt = 100, time = 0, cmt = 1), seq(0.5, 60, by = 0.5))
  evU <- rxode2::et(evU, id = 1:3)
  sol <- function(phi) rxode2::rxSolve(m, params = .parsFor(ncmt, 1L), events = evU,
                                       returnType = "data.frame", cores = 1L,
                                       linCmtSensType = "AD", linCmtSensPhi = phi)
  invisible(rxode2:::linCmtSeqStats(TRUE))
  p1 <- sol(1L); s1 <- rxode2:::linCmtSeqStats(TRUE)
  p2 <- sol(2L); s2 <- rxode2:::linCmtSeqStats(TRUE)
  d <- .colScaled(p2, p1)
  message(sprintf("  %dcmt oral: %.3e   probeRows=%d analyticRows=%d",
                  ncmt, d, s1[["phiRows"]], s2[["phiAnalyticRows"]]))
  if (d > worstProbe) { worstProbe <- d; whereP <- sprintf("%dcmt", ncmt) }
}
ok <- worstRev < 1e-6 && worstTail < 1e-9 && worstProbe < 1e-9
message(if (ok) "PASS" else sprintf("FAIL (probe %.3e at %s)", worstProbe, whereP))
quit(status = if (ok) 0L else 1L)
