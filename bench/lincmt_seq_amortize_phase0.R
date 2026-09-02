# HISTORICAL RECORD: this script exercises rxControl(linCmtSensStrategy=)
# and/or linCmtHybStats(), removed when the hybrid strategy was retired (the
# amortized sequential evaluator, linCmtSeqTailJac, subsumed it).  To re-run,
# check out the commit range 473c6c52c..6939902d8 of this branch; the saved
# bench/results/*.rds remain the evidence of record.
# Phase 0 of the sequential sensitivity-amortization project
# (plans/snazzy-mapping-kettle.md): measure the missing hybrid arm's
# us/obs on the three-arm cells, and prepare the artifacts the callgrind
# profile script consumes.  Optimized build only; run pinned:
#   taskset -c <idle> Rscript bench/lincmt_seq_amortize_phase0.R
# Env: CONFIG=2cmt (default), REPS=3.
# RXTREE picks which rxode2 build to time (the quiet-machine A/B loads the
# baseline / post-A throwaway worktrees); the model text and data are
# identical across trees.
suppressMessages({
  devtools::load_all(Sys.getenv("RXTREE", "~/src/rxode2-lincmt-analytic"),
                     compile = FALSE, quiet = TRUE)
  devtools::load_all("~/src/nlmixr2est", helpers = FALSE, quiet = TRUE)
})
rxode2::setRxThreads(1L)

cfg  <- Sys.getenv("CONFIG", "2cmt")
nRep <- as.integer(Sys.getenv("REPS", "3"))
outDir <- "~/src/rxode2-lincmt-analytic/bench/results"

loadAvg <- function() as.numeric(strsplit(readLines("/proc/loadavg"), " ")[[1]][1])
# guard against a busy machine; a small persistent baseline from other
# pinned work is acceptable for pinned single-core cells (load recorded
# per row).  MAXLOAD overrides for an explicitly contended, provenance-
# marked run (the results carry the load per row either way).
stopifnot(loadAvg() < as.numeric(Sys.getenv("MAXLOAD", "3")))

trueTheta <- list(
  `1cmt` = c(ka = 1.2, cl = 4, v = 30),
  `2cmt` = c(ka = 1.2, cl = 4, v = 30, q = 8, vp = 60),
  `3cmt` = c(ka = 1.2, cl = 4, v = 30, q = 8, vp = 60, q2 = 3, vp2 = 200))[[cfg]]
odeLines <- list(
  `1cmt` = "d/dt(depot) <- -ka*depot\nd/dt(central) <- ka*depot - cl/v*central",
  `2cmt` = paste0("d/dt(depot) <- -ka*depot\n",
                  "d/dt(central) <- ka*depot - cl/v*central - q/v*central + q/vp*periph\n",
                  "d/dt(periph) <- q/v*central - q/vp*periph"),
  `3cmt` = paste0("d/dt(depot) <- -ka*depot\n",
                  "d/dt(central) <- ka*depot - cl/v*central - q/v*central + q/vp*periph - q2/v*central + q2/vp2*periph2\n",
                  "d/dt(periph) <- q/v*central - q/vp*periph\n",
                  "d/dt(periph2) <- q2/v*central - q2/vp2*periph2"))[[cfg]]
iniBlock <- paste(c(sprintf("l%s <- log(%.6g)", names(trueTheta), unname(trueTheta) * 1.2),
                    "eta.ka ~ 0.1", "eta.cl ~ 0.1", "eta.v ~ 0.1",
                    "prop.sd <- 0.2"), collapse = "\n")
parBlock <- paste(vapply(names(trueTheta), function(p) {
  e <- if (p %in% c("ka", "cl", "v")) sprintf("*exp(eta.%s)", p) else ""
  sprintf("%s <- exp(l%s)%s", p, p, e)
}, ""), collapse = "\n")
uiLin <- eval(parse(text = sprintf(
  "function() {\n ini({\n%s\n })\n model({\n%s\n cp <- linCmt()\n cp ~ prop(prop.sd)\n })\n}",
  iniBlock, parBlock)))

mkDat <- function(nSub, nObs, tmax = 32) {
  set.seed(1002003)
  obsT <- sort(unique(round(exp(seq(log(0.25), log(tmax), length.out = nObs)), 3)))
  simMod <- rxode2::rxode2(paste0("cp = central/v\n", odeLines))
  eta <- matrix(rnorm(nSub * 3L, 0, 0.3), nSub, 3L,
                dimnames = list(NULL, c("ka", "cl", "v")))
  pars <- data.frame(row.names = seq_len(nSub))
  for (p in names(trueTheta)) {
    pars[[p]] <- if (p %in% colnames(eta)) trueTheta[[p]] * exp(eta[, p]) else trueTheta[[p]]
  }
  ev <- rxode2::et(amt = 100, time = 0, cmt = "depot") |> rxode2::et(obsT)
  sim <- rxode2::rxSolve(simMod, pars, ev, cores = 1L, addDosing = FALSE,
                         useLinCmt = FALSE)
  set.seed(2003004)
  simDat <- data.frame(ID = rep(seq_len(nSub), each = length(obsT)),
                       TIME = sim$time,
                       DV = sim$cp * (1 + rnorm(nrow(sim), 0, 0.15)),
                       AMT = 0, EVID = 0, CMT = "central")
  doseRows <- data.frame(ID = seq_len(nSub), TIME = 0, DV = NA_real_,
                         AMT = 100, EVID = 1, CMT = "depot")
  dat <- rbind(doseRows, simDat)
  dat[order(dat$ID, dat$TIME, -dat$EVID), ]
}

ctl <- nlmixr2est::foceiControl(
  calcTables = FALSE, print = 0L, covMethod = "",
  maxOuterIterations = 0L,
  rxControl = rxode2::rxControl(cores = 1L, linCmtSensType = "AD"))
fit <- suppressWarnings(suppressMessages(
  nlmixr2est::nlmixr2(uiLin, mkDat(40L, 10L), est = "focei", control = ctl)))
im <- fit$env$innerModel
stopifnot(grepl("linCmtB", rxode2::rxNorm(im)))

pn <- rxode2::rxModelVars(im)$params
th <- fit$env$fullTheta
mkPars <- function(nSub) {
  pars <- setNames(as.data.frame(matrix(0.05, nSub, length(pn))), pn)
  thetaCols <- grepl("^THETA_", pn)
  if (sum(thetaCols) == length(th)) pars[, thetaCols] <- rep(th, each = nSub)
  pars
}

cells <- list(c(40L, 1000L), c(400L, 1000L), c(40L, 200L))
rows <- list()
for (cell in cells) {
  nSub <- cell[1]; nObs <- cell[2]
  obsT <- sort(unique(round(exp(seq(log(0.25), log(32), length.out = nObs)), 4)))
  ev <- rxode2::et(amt = 100, time = 0, cmt = 1) |> rxode2::et(obsT)
  pars <- mkPars(nSub)
  for (strat in c("sequential", "hybrid")) {
    tset <- numeric(nRep)
    ref <- NULL
    for (r in seq_len(nRep)) {
      t0 <- proc.time()[["elapsed"]]
      # this tree's auto sensType still carries the pre-#1280 count rule
      # (reverse at nreq >= max(m,3)); force forward, the shipped default
      s <- rxode2::rxSolve(im, pars, ev, cores = 1L, addDosing = FALSE,
                           useLinCmt = FALSE, linCmtSensStrategy = strat,
                           linCmtSensType = "AD")
      tset[r] <- proc.time()[["elapsed"]] - t0
      if (r == 1L) ref <- s
    }
    us <- median(tset) / (nSub * nObs) * 1e6
    rows[[length(rows) + 1L]] <- data.frame(
      cfg = cfg, strat = strat, nSub = nSub, nObs = nObs,
      sec = median(tset), usPerObs = us, load = loadAvg())
    cat(sprintf("%s %s nSub=%d nObs=%d: %.4f s = %.3f us/obs (load %.2f)\n",
                cfg, strat, nSub, nObs, median(tset), us, loadAvg()))
    if (strat == "hybrid") {
      st <- utils::getFromNamespace("linCmtHybStats", "rxode2")(TRUE)
      cat("  hybrid counters:", paste(names(st), st, sep = "=", collapse = " "), "\n")
    }
  }
}
res <- do.call(rbind, rows)
attr(res, "provenance") <- list(
  script = "bench/lincmt_seq_amortize_phase0.R", cfg = cfg,
  date = format(Sys.time()), build = "pkgbuild::compile_dll(debug=FALSE)")
saveRDS(res, file.path(path.expand(outDir),
                       sprintf("seq_amortize_phase0_%s.rds", cfg)))

# artifacts for the callgrind profile run (light script, rxode2 only)
saveRDS(list(modelText = rxode2::rxNorm(im), pars = mkPars(40L),
             cfg = cfg),
        file.path(path.expand(outDir), sprintf("phase0_prep_%s.rds", cfg)))
cat("phase0 done\n")
