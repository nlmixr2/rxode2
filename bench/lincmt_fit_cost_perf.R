# NOT YET RUN: skipped 2026-08-24 -- another agent was rebuilding this
# worktree's .so during the measurement window (perf CPU-%% needs a stable
# symbolized build). Run against a settled optimized build.
# CPU attribution for ONE 40x100 linCmt FOCEi fit (companion to
# bench/lincmt_fit_cost_breakdown.R -- run AFTER it, not concurrently).
# Usage:
#   taskset -c 19 perf record -g --call-graph dwarf -o /tmp/lincmt_fit.perf \
#     Rscript bench/lincmt_fit_cost_perf.R
#   perf report -i /tmp/lincmt_fit.perf --stdio --percent-limit 1 | head -80
# Buckets when reading the report:
#   linCmt kernel: linCmtB, linCmtSeqTailJac, linCmtWinFill, linCmtStan*,
#     macros2micros*, computeSolComp*, getJacCp, adjustF, sensTheta
#   solver machinery: linSolve, solveWith1Pt, iniSubject, handle_evid,
#     par_solve, copyLinCmt
#   inner problem (nlmixr2est): likInner0, innerCost/innerEval, RcppTrust,
#     n1qn1
#   outer bobyqa: bobyqa_, trsbox_, rescue_, prelim_, altmov_ (minqa-style)
#   R interpreter/setup: Rf_eval, bcEval, R_alloc/gc, do_*
#   allocator: malloc, free, operator new
# The first (warm-up) fit re-uses the model cache primed by the breakdown
# script run, so compile time does not pollute the profile.
suppressMessages({
  devtools::load_all(Sys.getenv("RXTREE", "~/src/rxode2-lincmt-analytic"),
                     compile = FALSE, quiet = TRUE)
  devtools::load_all("~/src/nlmixr2est", helpers = FALSE, quiet = TRUE)
})
rxode2::setRxThreads(1L)
trueTheta <- c(ka = 1.2, cl = 4, v = 30, q = 8, vp = 60)
odeLines <- paste0("d/dt(depot) <- -ka*depot\n",
  "d/dt(central) <- ka*depot - cl/v*central - q/v*central + q/vp*periph\n",
  "d/dt(periph) <- q/v*central - q/vp*periph")
iniBlock <- paste(c(sprintf("l%s <- log(%.6g)", names(trueTheta), unname(trueTheta)*1.2),
                    "eta.ka ~ 0.1", "eta.cl ~ 0.1", "eta.v ~ 0.1",
                    "prop.sd <- 0.2"), collapse = "\n")
parBlock <- paste(vapply(names(trueTheta), function(p) {
  e <- if (p %in% c("ka","cl","v")) sprintf("*exp(eta.%s)", p) else ""
  sprintf("%s <- exp(l%s)%s", p, p, e)
}, ""), collapse = "\n")
mkUi <- function(body, pred) eval(parse(text = sprintf(
  "function() {\n ini({\n%s\n })\n model({\n%s\n%s\n cp <- %s\n cp ~ prop(prop.sd)\n })\n}",
  iniBlock, parBlock, body, pred)))
ui <- mkUi("", "linCmt()")
set.seed(1002003)
nSub <- 40L; nObs <- 100L
obsT <- sort(unique(round(exp(seq(log(0.25), log(32), length.out = nObs)), 3)))
simMod <- rxode2::rxode2(paste0("cp = central/v\n", odeLines))
eta <- matrix(rnorm(nSub*3L, 0, 0.3), nSub, 3L, dimnames = list(NULL, c("ka","cl","v")))
pars <- data.frame(row.names = seq_len(nSub))
for (p in names(trueTheta)) {
  pars[[p]] <- if (p %in% colnames(eta)) trueTheta[[p]]*exp(eta[,p]) else trueTheta[[p]]
}
ev <- rxode2::et(amt = 100, time = 0, cmt = "depot") |> rxode2::et(obsT)
sim <- rxode2::rxSolve(simMod, pars, ev, cores = 1L, addDosing = FALSE, useLinCmt = FALSE)
set.seed(2003004)
dat <- rbind(data.frame(ID = seq_len(nSub), TIME = 0, DV = NA_real_, AMT = 100,
                        EVID = 1, CMT = "depot"),
             data.frame(ID = rep(seq_len(nSub), each = nObs), TIME = sim$time,
                        DV = sim$cp*(1 + rnorm(nrow(sim), 0, 0.15)), AMT = 0,
                        EVID = 0, CMT = "central"))
dat <- dat[order(dat$ID, dat$TIME, -dat$EVID), ]
ctl <- nlmixr2est::foceiControl(calcTables = FALSE, print = 0L, covMethod = "",
  rxControl = rxode2::rxControl(cores = 1L, linCmtSensType = "AD"))
fit <- suppressWarnings(suppressMessages(
  nlmixr2est::nlmixr2(ui, dat, est = "focei", control = ctl)))
cat(sprintf("perf fit objf %.4f\n", fit$objective))
