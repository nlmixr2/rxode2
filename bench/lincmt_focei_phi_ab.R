# Fit-level A/B for the closed-form transition matrix (linCmtSensPhi = 2,
# the new default) against the probe-built one (1), across a UNIFORM and an
# IRREGULAR observation design.
#
# Why the design is the variable.  The probe-built matrix can only be paid
# for on an interval shown to recur, so on a uniform design it already
# serves nearly every row and the closed form has nothing to add; the
# gradient-slope kit's dataset is uniform (a single 0.5 h gap across all 99
# intervals), which is why its per-direction slope does not move.  The
# closed form's whole point is the design the probe declines.  This measures
# whether the solve-level difference survives into a fit, where a large part
# of the per-iteration cost is inner-optimizer effort rather than solve
# arithmetic.
#
# Both routes evaluate the same exact closed form, so the objective must
# agree to round-off; that is checked here as well as the time.
#
# Uses the INSTALLED nlmixr2est against an rxode2 given by RXLIB (an
# installed build -- devtools::load_all() WITH compilation is -O0).
#
# Usage: RXLIB=<libpath> taskset -c <idle core> Rscript bench/lincmt_focei_phi_ab.R
RXLIB <- Sys.getenv("RXLIB", "")
if (nzchar(RXLIB)) .libPaths(c(RXLIB, .libPaths()))
suppressMessages({library(nlmixr2); library(nlmixr2data)})
rxode2::setRxThreads(1L)
REPS <- as.integer(Sys.getenv("REPS", "3"))
ITER <- as.integer(Sys.getenv("ITER", "20"))
loadAvg <- function() as.numeric(strsplit(readLines("/proc/loadavg"), " ")[[1]][1])
cat("rxode2 from:", dirname(system.file(package = "rxode2")), "\n")

set.seed(42)
mkDat <- function(design, nSub = 40L, nObs = 100L) {
  tim <- if (design == "uniform") seq(0.5, by = 0.5, length.out = nObs) else
    cumsum(runif(nObs, 0.15, 0.85))
  sim <- function() {
    do.call(rbind, lapply(seq_len(nSub), function(i) {
      d <- data.frame(ID = i, TIME = c(0, tim), DV = NA_real_,
                      AMT = c(100, rep(NA_real_, length(tim))),
                      EVID = c(1L, rep(0L, length(tim))), CMT = c(1L, rep(2L, length(tim))))
      d
    }))
  }
  d <- sim()
  m <- rxode2::rxode2({
    ka <- exp(0.45 + eta.ka); cl <- exp(1 + eta.cl); v <- exp(3.45 + eta.v)
    q <- exp(1.2); vp <- exp(3.9)
    cp <- linCmt()
  })
  s <- rxode2::rxSolve(m, d[, c("ID", "TIME", "AMT", "EVID", "CMT")] |>
                         setNames(c("id", "time", "amt", "evid", "cmt")),
                       params = c(eta.ka = 0, eta.cl = 0, eta.v = 0),
                       omega = lotri::lotri(eta.ka ~ 0.4, eta.cl ~ 0.3, eta.v ~ 0.2),
                       addDosing = FALSE, returnType = "data.frame", cores = 1L)
  d$DV[d$EVID == 0L] <- s$cp * (1 + 0.15 * rnorm(sum(d$EVID == 0L)))
  names(d) <- c("id", "time", "dv", "amt", "evid", "cmt")
  d
}

mod <- function() {
  ini({
    tka <- 0.45; tcl <- 1; tv <- 3.45; tq <- 1.2; tvp <- 3.9
    eta.ka ~ 0.4; eta.cl ~ 0.3; eta.v ~ 0.2
    prop.sd <- 0.15
  })
  model({
    ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v <- exp(tv + eta.v)
    q <- exp(tq); vp <- exp(tvp)
    linCmt() ~ prop(prop.sd)
  })
}

res <- list()
for (design in c("uniform", "irregular")) {
  dat <- mkDat(design)
  for (phi in c(1L, 2L)) {
    fitOnce <- function() {
      t0 <- proc.time()[["elapsed"]]
      f <- suppressWarnings(suppressMessages(
        nlmixr2(mod, dat, "focei",
                control = foceiControl(print = 0L, calcTables = FALSE,
                                       maxOuterIterations = ITER,
                                       rxControl = rxode2::rxControl(
                                         linCmtSensType = "AD",
                                         linCmtSensPhi = phi)))))
      list(sec = proc.time()[["elapsed"]] - t0, objf = f$objDf$OBJF)
    }
    fitOnce()
    r <- lapply(seq_len(REPS), function(i) fitOnce())
    res[[length(res) + 1L]] <-
      data.frame(design = design, phi = phi,
                 sec = median(vapply(r, function(x) x$sec, 0)),
                 objf = r[[1]]$objf, load = loadAvg())
  }
}
res <- do.call(rbind, res)
res$gain <- NA_real_
for (d in unique(res$design)) {
  i <- res$design == d
  res$gain[i] <- res$sec[i & res$phi == 1L] / res$sec[i]
}
print(res, row.names = FALSE, digits = 5)
cat("\nobjective agreement per design (max |delta|):\n")
for (d in unique(res$design)) {
  o <- res$objf[res$design == d]
  cat(sprintf("  %-10s %.3e\n", d, max(abs(o - o[1]))))
}
saveRDS(res, "bench/results/focei_phi_ab.rds")
