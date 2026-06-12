#!/usr/bin/env Rscript
# rxode2 performance benchmark harness
#
# Usage (from the package root):
#   NOT_CRAN=true Rscript bench/bench.R            # run, print, save results
#   NOT_CRAN=true Rscript bench/bench.R --baseline # also save as the baseline
#
# Loads the package with devtools::load_all() so the installed library copy
# is untouched.  Results are written to bench/results/<date>-<sha>.csv and
# compared against bench/baseline.csv when it exists.

args <- commandArgs(trailingOnly = TRUE)
.saveBaseline <- "--baseline" %in% args

message("Loading package with devtools::load_all() ...")
suppressMessages(devtools::load_all(quiet = TRUE))

.sha <- tryCatch(system("git rev-parse --short HEAD", intern = TRUE),
                 error = function(e) "unknown")

# median wall time of n evaluations of f(), in seconds
.timeIt <- function(f, n = 5L) {
  f() # warm up
  .times <- vapply(seq_len(n), function(i) {
    .t0 <- Sys.time()
    f()
    as.numeric(Sys.time() - .t0, units = "secs")
  }, numeric(1))
  stats::median(.times)
}

# ---------------------------------------------------------------------------
# models and events shared by the scenarios
# ---------------------------------------------------------------------------

.modOde <- function() {
  ini({
    tka <- 0.45; tcl <- 1; tv <- 3.45
    eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1
    add.sd <- 0.7
  })
  model({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    d/dt(depot) <- -ka * depot
    d/dt(center) <- ka * depot - cl / v * center
    cp <- center / v
    cp ~ add(add.sd)
  })
}

.modLinCmt <- function() {
  ini({
    tka <- 0.45; tcl <- 1; tv <- 3.45
    eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1
    add.sd <- 0.7
  })
  model({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    cp <- linCmt()
    cp ~ add(add.sd)
  })
}

.modCov <- function() {
  ini({
    tka <- 0.45; tcl <- 1; tv <- 3.45
    eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1
    add.sd <- 0.7
  })
  model({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl) * (wt / 70)^0.75
    v <- exp(tv + eta.v) * (wt / 70)
    d/dt(depot) <- -ka * depot
    d/dt(center) <- ka * depot - cl / v * center
    cp <- center / v
    cp ~ add(add.sd)
  })
}

message("Compiling benchmark models ...")
.mOde <- rxode2(.modOde)
.mLin <- rxode2(.modLinCmt)
.mCov <- rxode2(.modCov)

.ev <- et(amt = 300, ii = 12, addl = 13)
.ev <- et(.ev, seq(0, 168, by = 0.5))

# time-varying covariate event table (wt changes over time per subject)
.evCov <- as.data.frame(et(.ev, id = 1:200))
set.seed(101)
.evCov$wt <- 70 * exp(0.1 * sin(.evCov$time / 24) +
                        stats::rnorm(nrow(.evCov), 0, 0.01))

set