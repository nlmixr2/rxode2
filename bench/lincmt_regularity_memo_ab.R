# What does an IRREGULAR sampling schedule cost a linCmt() sensitivity fit,
# and where does that cost live?
#
# Two FOCEi fits differing only in the observation schedule -- one uniform
# gap repeated, or a hundred distinct gaps -- crossed with the three
# linCmtSensPhi routes and with the delta-keyed exponential memo forced on
# and off.  The ratio irregular/uniform within one route is the quantity;
# the memo-off cells are the control that says whether the ratio is
# arithmetic or caching.  It is caching: with the memo off the two designs
# cost the same to within a percent under every route, so the per-row work
# does not depend on the schedule at all.  What the ratio measures is a
# discount only a regular design can collect.
#
# Optimized build only -- devtools::load_all() WITH compilation builds at
# -O0 and has inverted comparisons on this code before (NEWS.md 5.1.7).
# Pinned single core; load recorded.
#
# Usage: REPS=3 taskset -c <idle core> Rscript bench/lincmt_regularity_memo_ab.R
message("== lincmt_regularity_memo_ab ==")
suppressMessages(devtools::load_all(".", compile = FALSE, quiet = TRUE))
suppressMessages(devtools::load_all("~/src/nlmixr2est",
                                    helpers = FALSE, quiet = TRUE))
rxode2::setRxThreads(1L)
REPS <- as.integer(Sys.getenv("REPS", "3"))
MAXIT <- as.integer(Sys.getenv("MAXITER", "20"))
loadAvg <- function() as.numeric(strsplit(readLines("/proc/loadavg"), " ")[[1]][1])

mod <- function() {
  ini({
    tka <- log(1.3); tcl <- log(2.1); tv <- log(21)
    tq <- log(3.3); tvp <- log(43)
    eta.ka ~ fix(0.1); eta.cl ~ fix(0.1); eta.v ~ fix(0.1)
    prop.sd <- fix(0.2)
  })
  model({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    q <- exp(tq)
    vp <- exp(tvp)
    cp <- linCmt(cl, v, q, vp, ka)
    cp ~ prop(prop.sd)
  })
}

# One distinct gap against a hundred of them, over a comparable window and
# the same observation count, so only the SPACING differs.  The uniform
# gaps are exact binary fractions: a schedule built as seq(0, 96, by=0.96)
# looks uniform and is not, and would be silently measured as irregular.
nSub <- 40L
set.seed(20260828)
schedules <- list(uniform = seq_len(100) * 1,
                  irregular = cumsum(round(runif(100, 0.2, 1.8), 6)))
mkDat <- function(tim) {
  ev <- do.call(rbind, lapply(seq_len(nSub), function(i)
    rbind(data.frame(ID = i, TIME = 0, AMT = 100, EVID = 1L, DV = NA_real_),
          data.frame(ID = i, TIME = tim, AMT = NA_real_, EVID = 0L,
                     DV = NA_real_))))
  sim <- rxode2::rxSolve(mod(), ev, cores = 1L, addDosing = FALSE)
  ev$DV[ev$EVID == 0L] <- sim$cp * (1 + rnorm(sum(ev$EVID == 0L), 0, 0.2))
  ev
}
dat <- lapply(schedules, mkDat)

fitOnce <- function(d, phi, memo) {
  rxode2::linCmtDeltaMemo(if (memo) -1L else 0L)
  on.exit(rxode2::linCmtDeltaMemo(-1L))
  ctl <- nlmixr2est::foceiControl(
    print = 0, calcTables = FALSE, covMethod = "",
    maxOuterIterations = MAXIT,
    rxControl = rxode2::rxControl(linCmtSensType = "AD", cores = 1L,
                                  linCmtSensPhi = phi))
  rxode2::linCmtSeqStats(TRUE)
  f <- suppressWarnings(suppressMessages(
    nlmixr2est::nlmixr2(mod(), d, est = "focei", control = ctl)))
  st <- rxode2::linCmtSeqStats(TRUE)
  list(t = as.numeric(f$time$optimize), objf = f$objf,
       nEval = f$env$optReturn$feval, st = st)
}

invisible(fitOnce(dat$uniform, 2L, TRUE))   # warm-up (compiles), not timed
res <- list()
for (phi in 0:2) {
  for (memo in c(TRUE, FALSE)) {
    for (dn in names(dat)) {
      z <- lapply(seq_len(REPS), function(r) fitOnce(dat[[dn]], phi, memo))
      st <- z[[REPS]]$st
      res[[length(res) + 1L]] <- data.frame(
        phi = phi, memo = memo, design = dn,
        sec = median(vapply(z, function(x) x$t, 0)),
        objf = z[[1]]$objf, nEval = z[[1]]$nEval,
        expBuild = st[["expBuild"]], expSolo = st[["expSolo"]],
        expHit = st[["expHit"]], load = loadAvg())
    }
  }
}
res <- do.call(rbind, res)
res$secPerEval <- res$sec / res$nEval
print(res, row.names = FALSE, digits = 5)

cat("\nirregular / uniform, per route:\n")
for (phi in 0:2) {
  for (memo in c(TRUE, FALSE)) {
    d <- res[res$phi == phi & res$memo == memo, ]
    cat(sprintf("  linCmtSensPhi = %d, memo %-3s  %.3fx\n", phi,
                if (memo) "on" else "off",
                d$secPerEval[d$design == "irregular"] /
                  d$secPerEval[d$design == "uniform"]))
  }
}
attr(res, "provenance") <- list(
  when = format(Sys.time(), tz = "UTC"), reps = REPS, maxOuterIterations = MAXIT,
  commit = system("git rev-parse --short HEAD", intern = TRUE),
  note = paste("FOCEi 40x100, 2-cmt oral, 3 fixed-omega etas; uniform vs",
               "irregular schedule x linCmtSensPhi 0/1/2 x delta memo on/off"))
saveRDS(res, "bench/results/lincmt_regularity_memo_ab.rds")
message("saved bench/results/lincmt_regularity_memo_ab.rds")
