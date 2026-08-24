# Decompose the 40x100 linCmt() FOCEi fit's wall clock into solve vs
# non-solve components (user question: "what are the fit level non solve
# costs"). Same model/data as bench/lincmt_amort_fit.R; nlmixr2est from
# the lincmt-speed worktree. Outer optimizer is bobyqa (derivative-free:
# NO outer gradient) so the arithmetic is
#   wall = setup + nOuterEval x (inner pass over 40 subjects) + R overhead
# and the inner pass splits into solve proper vs everything else.
# Counters: rxode2:::linCmtSeqStats() valueCompute counts SOLVED rows, so
#   subject inner evaluations = valueCompute / rowsPerSubject.
# Run pinned: taskset -c 21 Rscript bench/lincmt_fit_cost_breakdown.R
suppressMessages({
  devtools::load_all(Sys.getenv("RXTREE", "~/src/rxode2-lincmt-carry-jump"),
                     compile = FALSE, quiet = TRUE)
  devtools::load_all("~/src/nlmixr2est-lincmt-speed", helpers = FALSE, quiet = TRUE)
})
rxode2::setRxThreads(1L)
loadAvg <- function() as.numeric(strsplit(readLines("/proc/loadavg"), " ")[[1]][1])

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
cat(sprintf("controls: outerOpt=%s innerOpt=%s hessianMethod=%s fast=%s\n",
            ctl$outerOpt, if (is.null(ctl$innerOpt)) "NA" else ctl$innerOpt,
            if (is.null(ctl$hessianMethod)) "NA" else ctl$hessianMethod,
            as.character(isTRUE(ctl$fast))))

fitOne <- function(control) {
  suppressWarnings(suppressMessages(
    nlmixr2est::nlmixr2(ui, dat, est = "focei", control = control)))
}
res <- list()
# warm-up (compiles); untimed
invisible(fitOne(ctl))
# (1) full fit, counters around it
invisible(rxode2:::linCmtSeqStats(TRUE))
t0 <- proc.time()[["elapsed"]]
fit <- fitOne(ctl)
res$wallFull <- proc.time()[["elapsed"]] - t0
res$stats <- rxode2:::linCmtSeqStats(TRUE)
res$objf <- fit$objective
res$time <- as.data.frame(fit$time)
saveRDS(res, "bench/results/lincmt_fit_cost_breakdown.rds")
cat("stage1 full fit:", res$wallFull, "s objf", res$objf, "\n"); print(res$stats); print(res$time)
res$parHistRows <- tryCatch(nrow(fit$parHistData), error = function(e) NA_integer_)
cand <- grep("n[A-Z]|Eval|eval|iter|Iter", ls(fit$env), value = TRUE)
res$envCounters <- sapply(cand, function(n) {
  v <- tryCatch(get(n, envir = fit$env), error = function(e) NULL)
  if (is.numeric(v) && length(v) == 1L) v else NA_real_
})
# (2) posthoc (maxOuter=0): setup + ONE inner optimization over subjects
ctlPost <- nlmixr2est::foceiControl(calcTables = FALSE, print = 0L, covMethod = "",
  maxOuterIterations = 0L,
  rxControl = rxode2::rxControl(cores = 1L, linCmtSensType = "AD"))
invisible(rxode2:::linCmtSeqStats(TRUE))
t0 <- proc.time()[["elapsed"]]
fitP <- fitOne(ctlPost)
res$wallPost <- proc.time()[["elapsed"]] - t0
res$statsPost <- rxode2:::linCmtSeqStats(TRUE)
saveRDS(res, "bench/results/lincmt_fit_cost_breakdown.rds")
cat("stage2 posthoc:", res$wallPost, "s\n"); print(res$statsPost)
# (3) setup-only-ish: maxOuter=0 AND maxInner=0 (one eta=0 evaluation/subject)
ctlSetup <- nlmixr2est::foceiControl(calcTables = FALSE, print = 0L, covMethod = "",
  maxOuterIterations = 0L, maxInnerIterations = 0L,
  rxControl = rxode2::rxControl(cores = 1L, linCmtSensType = "AD"))
invisible(rxode2:::linCmtSeqStats(TRUE))
t0 <- proc.time()[["elapsed"]]
fitS <- fitOne(ctlSetup)
res$wallSetup <- proc.time()[["elapsed"]] - t0
res$statsSetup <- rxode2:::linCmtSeqStats(TRUE)
saveRDS(res, "bench/results/lincmt_fit_cost_breakdown.rds")
cat("stage3 setupish:", res$wallSetup, "s\n"); print(res$statsSetup)
res$solvePassSec <- NA_real_
res$rowsPerSubject <- nObs + 1L
res$load <- loadAvg()
print(res$stats); print(res$time)
cat(sprintf("wall full %.2f s; posthoc %.2f; setupish %.2f; solve pass %.4f s; parHist %s; objf %.4f (load %.2f)\n",
    res$wallFull, res$wallPost, res$wallSetup,
    ifelse(is.na(res$solvePassSec), -1, res$solvePassSec),
    res$parHistRows, res$objf, res$load))
cat("env counters:\n"); print(res$envCounters[!is.na(res$envCounters)])
saveRDS(res, "bench/results/lincmt_fit_cost_breakdown.rds")
