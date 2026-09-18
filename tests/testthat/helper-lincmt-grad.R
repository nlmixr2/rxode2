# Shared by the test-lincmt-*.R gradient tests: a linCmtB() model returning
# the value and the requested sensitivity directions, its parameters, and
# dosing designs.  Definitions only, like every helper file.

.linCmtTestModel <- function(ncmt, oral0, dirs) {
  args <- sprintf("rx__PTR__, t, 1, %d, %d, %%d, %%d, 1, cl, v, q, vp, q2, vp2, ka", ncmt, oral0)
  lines <- c(
    sprintf("cp=linCmtB(%s)", sprintf(args, -1L, -1L)),
    vapply(
      dirs,
      function(k) {
        sprintf("d%d=linCmtB(%s)", k, sprintf(args, -2L, k))
      },
      ""
    )
  )
  suppressWarnings(rxode2(paste(lines, collapse = "\n")))
}
.linCmtTestPars <- function(ncmt, oral0) {
  p <- c(cl = 2.1, v = 21, q = 3.3, vp = 43, q2 = 0.9, vp2 = 61, ka = 1.3)
  if (ncmt < 2) {
    p[c("q", "vp")] <- 0
  }
  if (ncmt < 3) {
    p[c("q2", "vp2")] <- 0
  }
  if (oral0 == 0) {
    p["ka"] <- 0
  }
  p
}
# bolus and infusion doses at irregular times (so the delta memo misses and
# the kernel computes its own exponentials) then a trailing observation run
.linCmtTestEvDoseThenObs <- function(nSub = 3L, nObs = 9L) {
  do.call(
    rbind,
    lapply(seq_len(nSub), function(i) {
      sh <- 0.3 * (i - 1)
      dose <- data.frame(
        id = i,
        time = c(0, 5.5, 12, 18.25, 26) + c(0, sh, 0, sh, 0),
        amt = c(100, 80, 120, 90, 110) * (1 + 0.1 * i),
        evid = 1,
        cmt = 1,
        rate = c(0, 40, 0, 60, 0),
        ii = 0,
        ss = 0
      )
      obs <- data.frame(
        id = i,
        time = 28 + cumsum(rep(c(1.3, 2.9, 4.1), length.out = nObs)) + sh,
        amt = 0,
        evid = 0,
        cmt = 1,
        rate = 0,
        ii = 0,
        ss = 0
      )
      rbind(dose, obs)
    })
  )
}
.linCmtTestEvSs <- function(nSub = 2L) {
  do.call(
    rbind,
    lapply(seq_len(nSub), function(i) {
      dose <- data.frame(
        id = i,
        time = c(0, 48, 60),
        amt = c(100, 100, 50),
        evid = 1,
        cmt = 1,
        rate = c(if (i == 1) 25 else 0, 0, 0),
        ii = c(12, 12, 0),
        ss = c(1, 2, 0)
      )
      obs <- data.frame(
        id = i,
        time = c(0.7, 2.3, 5.9, 11.1, 13.4, 20.2, 30.5, 47.5, 49.1, 53.3, 59.4, 61.2, 66.6, 80.1, 91.3, 104.9) +
          0.15 * i,
        amt = 0,
        evid = 0,
        cmt = 1,
        rate = 0,
        ii = 0,
        ss = 0
      )
      rbind(dose, obs)
    })
  )
}
.linCmtTestStats <- function() rxode2::linCmtSeqStats(TRUE)
