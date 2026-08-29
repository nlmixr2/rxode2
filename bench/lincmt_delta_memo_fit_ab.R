# Fit-level A/B for the delta-keyed exponential memo: one FOCEi fit
# (2-cmt oral, 3 etas, 40 subjects x 100 uniform obs) with the memo ON vs
# OFF on the same optimized binary.  Forward mode forced through
# rxControl(linCmtSensType = "AD") for the same reason as the solve A/B.
# Pinned single core; run idle; load recorded.
#
# Usage: taskset -c <idle core> Rscript bench/lincmt_delta_memo_fit_ab.R
message("== lincmt_delta_memo_fit_ab ==")
suppressMessages(devtools::load_all(".", compile = FALSE, quiet = TRUE))
suppressMessages(devtools::load_all("~/src/nlmixr2est",
                                    helpers = FALSE, quiet = TRUE))
rxode2::setRxThreads(1L)
REPS <- as.integer(Sys.getenv("REPS", "3"))
loadAvg <- function() as.numeric(strsplit(readLines("/proc/loadavg"), " ")[[1]][1])

mod <- function() {
  ini({
    tka <- log(1.3); tcl <- log(2.1); tv <- log(21)
    tq <- log(3.3); tvp <- log(43)
    eta.ka ~ 0.1; eta.cl ~ 0.1; eta.v ~ 0.1
    add.sd <- 0.7
  })
  model({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    q <- exp(tq)
    vp <- exp(tvp)
    cp <- linCmt(cl, v, q, vp, ka)
    cp ~ add(add.sd)
  })
}

set.seed(42)
nSub <- 40L
obsT <- seq(0.5, 50, by = 0.5) # 100 uniform obs
ev <- do.call(rbind, lapply(seq_len(nSub), function(i) {
  rbind(data.frame(ID = i, TIME = 0, AMT = 100, EVID = 1, DV = 0),
        data.frame(ID = i, TIME = obsT, AMT = 0, EVID = 0, DV = 0))
}))
simTruth <- c(tka = log(1.3), tcl = log(2.1), tv = log(21),
              tq = log(3.3), tvp = log(43))
sim <- rxode2::rxSolve(mod(), ev, cores = 1L, addDosing = FALSE,
                       useLinCmt = TRUE)
ev$DV[ev$EVID == 0] <- sim$cp * (1 + rnorm(sum(ev$EVID == 0), 0, 0.1))
dat <- ev[ev$EVID == 0 | ev$EVID == 1, ]

ctl <- nlmixr2est::foceiControl(print = 0, calcTables = FALSE,
                                covMethod = "",
                                rxControl = rxode2::rxControl(
                                  linCmtSensType = "AD", cores = 1L))

fitOnce <- function(memo) {
  rxode2::linCmtDeltaMemo(if (memo) 1L else 0L)
  on.exit(rxode2::linCmtDeltaMemo(-1L))
  t0 <- proc.time()[["elapsed"]]
  f <- suppressWarnings(suppressMessages(
    nlmixr2est::nlmixr2(mod(), dat, est = "focei", control = ctl)))
  el <- proc.time()[["elapsed"]] - t0
  list(t = el, objf = f$objective)
}

# warm-up (compiles) not timed
invisible(fitOnce(TRUE))
res <- do.call(rbind, lapply(seq_len(REPS), function(r) {
  off <- fitOnce(FALSE)
  on <- fitOnce(TRUE)
  data.frame(rep = r, tOff = off$t, tOn = on$t,
             objfOff = off$objf, objfOn = on$objf, load = loadAvg())
}))
print(res, digits = 6)
cat(sprintf("median fit: off %.2f s, on %.2f s, gain %.3fx; objf diff %.2e\n",
            median(res$tOff), median(res$tOn), median(res$tOff)/median(res$tOn),
            max(abs(res$objfOff - res$objfOn))))
attr(res, "provenance") <- list(
  when = format(Sys.time(), tz = "UTC"), reps = REPS,
  commit = system("git rev-parse --short HEAD", intern = TRUE),
  note = "FOCEi 40x100 uniform; memo ON vs OFF, same binary, pinned")
saveRDS(res, "bench/results/lincmt_delta_memo_fit_ab.rds")
message("saved bench/results/lincmt_delta_memo_fit_ab.rds")
