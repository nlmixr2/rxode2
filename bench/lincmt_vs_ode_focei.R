# lincmt_vs_ode_focei.R -- how much faster is a linCmt() FOCEi fit than the
# SAME model written as d/dt() ODEs?  (The NONMEM ADVAN-vs-ODE question.)
#
# Protocol (see plan file "OPTIMIZED, UNCONTENDED SWEEP" block):
#   - rxode2 from THIS worktree, OPTIMIZED (-O3 verified in the linCmt.o
#     debug producer; loaded with compile = FALSE so nothing is rebuilt).
#   - nlmixr2est from ~/src/nlmixr2est (load_all, helpers=FALSE).
#   - Single-thread (rxode2 cores = 1); run the whole Rscript pinned:
#       CONFIG=1cmt REPS=3 taskset -c <idle core> Rscript bench/lincmt_vs_ode_focei.R
#   - linCmtSensType = "AD" (forward) is FORCED: this tree still carries the
#     old count rule (auto -> reverse at nreq >= max(m, 3), i.e. reverse for
#     every arm here), while the decided default (rxode2#1280) is forward.
#   - Warm-up fit per arm (compiles models), then REPS timed fits; medians.
#   - Load average recorded per rep; saemLL job (if any) untouched.
#
# One config per invocation (CONFIG env: 1cmt / 2cmt / 3cmt) so each run fits
# a foreground timeout; partial results are saved per config and merged when
# all three exist.

suppressMessages({
  devtools::load_all("~/src/rxode2-lincmt-analytic", compile = FALSE, quiet = TRUE)
  devtools::load_all("~/src/nlmixr2est", helpers = FALSE, quiet = TRUE)
})
rxode2::setRxThreads(1L)

cfg   <- Sys.getenv("CONFIG", "1cmt")
nRep  <- as.integer(Sys.getenv("REPS", "3"))
core  <- Sys.getenv("CORE", "unpinned")
outDir <- "~/src/rxode2-lincmt-analytic/bench/results"

loadAvg <- function() as.numeric(strsplit(readLines("/proc/loadavg"), " ")[[1]][1])

simTimes <- c(0.25, 0.75, 1.5, 3, 5.5, 8, 12.5, 17, 24, 32)

trueTheta <- list(
  `1cmt` = c(ka = 1.2, cl = 4, v = 30),
  `2cmt` = c(ka = 1.2, cl = 4, v = 30, q = 8, vp = 60),
  `3cmt` = c(ka = 1.2, cl = 4, v = 30, q = 8, vp = 60, q2 = 3, vp2 = 200))[[cfg]]

odeLines <- list(
  `1cmt` = "d/dt(depot) = -ka*depot; d/dt(central) = ka*depot - cl/v*central",
  `2cmt` = paste0("d/dt(depot) = -ka*depot;",
                  "d/dt(central) = ka*depot - cl/v*central - q/v*central + q/vp*periph;",
                  "d/dt(periph) = q/v*central - q/vp*periph"),
  `3cmt` = paste0("d/dt(depot) = -ka*depot;",
                  "d/dt(central) = ka*depot - cl/v*central - q/v*central + q/vp*periph - q2/v*central + q2/vp2*periph2;",
                  "d/dt(periph) = q/v*central - q/vp*periph;",
                  "d/dt(periph2) = q2/v*central - q2/vp2*periph2"))[[cfg]]

## ---- simulate one dataset (shared by both arms) --------------------------
set.seed(1002003)
nSub <- 40L
etaSd <- 0.3
eta <- matrix(rnorm(nSub * 3L, 0, etaSd), nSub, 3L,
              dimnames = list(NULL, c("ka", "cl", "v")))
simMod <- rxode2::rxode2(paste0("cp = central/v;", odeLines))
pars <- data.frame(row.names = seq_len(nSub))
for (p in names(trueTheta)) {
  pars[[p]] <- if (p %in% colnames(eta)) trueTheta[[p]] * exp(eta[, p]) else trueTheta[[p]]
}
# one-subject event table; a params data.frame with nSub rows replicates it
ev <- rxode2::et(amt = 100, cmt = "depot") |> rxode2::et(simTimes)
sim <- rxode2::rxSolve(simMod, pars, ev, cores = 1L, addDosing = FALSE)
stopifnot(nrow(sim) == nSub * length(simTimes))
set.seed(2003004)
simDat <- data.frame(ID = rep(seq_len(nSub), each = length(simTimes)),
                     TIME = sim$time,
                     DV = sim$cp * (1 + rnorm(nrow(sim), 0, 0.15)),
                     AMT = 0, EVID = 0, CMT = "central")
doseRows <- data.frame(ID = seq_len(nSub), TIME = 0, DV = NA_real_,
                       AMT = 100, EVID = 1, CMT = "depot")
dat <- rbind(doseRows, simDat)
dat <- dat[order(dat$ID, dat$TIME, -dat$EVID), ]

## ---- model pair ----------------------------------------------------------
iniBlock <- function() {
  ini <- c(sprintf("l%s <- log(%.6g)", names(trueTheta), unname(trueTheta) * 1.2),
           "eta.ka ~ 0.1", "eta.cl ~ 0.1", "eta.v ~ 0.1",
           "prop.sd <- 0.2")
  paste(ini, collapse = "\n")
}
parBlock <- paste(vapply(names(trueTheta), function(p) {
  e <- if (p %in% c("ka", "cl", "v")) sprintf("*exp(eta.%s)", p) else ""
  sprintf("%s <- exp(l%s)%s", p, p, e)
}, ""), collapse = "\n")

uiLin <- eval(parse(text = sprintf(
  "function() {\n ini({\n%s\n })\n model({\n%s\n cp <- linCmt()\n cp ~ prop(prop.sd)\n })\n}",
  iniBlock(), parBlock)))
uiOde <- eval(parse(text = sprintf(
  "function() {\n ini({\n%s\n })\n model({\n%s\n%s\n cp <- central/v\n cp ~ prop(prop.sd)\n })\n}",
  iniBlock(), parBlock, gsub(";", "\n", odeLines))))

## ---- verify the two arms agree before timing -----------------------------
linChk <- rxode2::rxode2(paste0(
  "param(", paste(names(trueTheta), collapse = ", "), "); cp = linCmt();"))
li <- rxode2::rxSolve(linChk, pars, ev, cores = 1L)
od <- rxode2::rxSolve(simMod, pars, ev, cores = 1L, useLinCmt = FALSE,
                      atol = 1e-12, rtol = 1e-12)
relDiff <- max(abs(li$cp - od$cp) / pmax(abs(od$cp), 1e-10))
if (relDiff > 1e-6) stop(sprintf("linCmt vs ODE prediction mismatch: %.3g", relDiff))

ctl <- nlmixr2est::foceiControl(
  calcTables = FALSE, print = 0L, covMethod = "",
  rxControl = rxode2::rxControl(cores = 1L, linCmtSensType = "AD"))
ctlPost <- nlmixr2est::foceiControl(
  calcTables = FALSE, print = 0L, covMethod = "", maxOuterIterations = 0L,
  rxControl = rxode2::rxControl(cores = 1L, linCmtSensType = "AD"))

fitOne <- function(ui, control) {
  t0 <- proc.time()[["elapsed"]]
  fit <- suppressWarnings(suppressMessages(
    nlmixr2est::nlmixr2(ui, dat, est = "focei", control = control)))
  list(sec = proc.time()[["elapsed"]] - t0, fit = fit)
}
nIter <- function(fit) {
  n <- tryCatch(nrow(fit$parHistData), error = function(e) NA_integer_)
  if (is.null(n) || is.na(n)) n <- tryCatch(nrow(fit$parHist), error = function(e) NA_integer_)
  n
}

rows <- list()
runArm <- function(arm, ui) {
  warm <- fitOne(ui, ctl)                      # untimed: compiles models
  noLin <- tryCatch({
    im <- warm$fit$env$innerModel
    !any(grepl("linCmtB", paste(deparse(im), collapse = "")))
  }, error = function(e) NA)
  for (r in seq_len(nRep)) {
    z <- fitOne(ui, ctl)
    rows[[length(rows) + 1L]] <<- data.frame(
      cfg = cfg, arm = arm, what = "fit", rep = r, sec = z$sec,
      objf = z$fit$objective, nIter = nIter(z$fit),
      load = loadAvg(), odeHasNoLinCmt = noLin)
  }
  for (r in seq_len(nRep)) {
    z <- fitOne(ui, ctlPost)
    rows[[length(rows) + 1L]] <<- data.frame(
      cfg = cfg, arm = arm, what = "posthoc", rep = r, sec = z$sec,
      objf = z$fit$objective, nIter = NA_integer_,
      load = loadAvg(), odeHasNoLinCmt = noLin)
  }
}
runArm("linCmt", uiLin)
runArm("ode", uiOde)

res <- do.call(rbind, rows)
attr(res, "provenance") <- list(
  date = format(Sys.time()), config = cfg, reps = nRep, core = core,
  flags = "linCmt.o DW_AT_producer ends -O3 (after -O2); pkgbuild::compile_dll(debug=FALSE)",
  sensType = "AD (forward) forced; this tree's auto still carries the pre-#1280 count rule",
  rxode2 = system("git -C ~/src/rxode2-lincmt-analytic rev-parse --short HEAD", intern = TRUE),
  nlmixr2est = system("git -C ~/src/nlmixr2est rev-parse --short HEAD", intern = TRUE),
  predAgreement = relDiff)
dir.create(outDir, showWarnings = FALSE)
saveRDS(res, file.path(outDir, sprintf("lincmt_vs_ode_focei_%s.rds", cfg)))

parts <- file.path(outDir, sprintf("lincmt_vs_ode_focei_%s.rds", c("1cmt", "2cmt", "3cmt")))
if (all(file.exists(parts))) {
  all3 <- do.call(rbind, lapply(parts, readRDS))
  attr(all3, "provenance") <- lapply(parts, function(f) attr(readRDS(f), "provenance"))
  saveRDS(all3, file.path(outDir, "lincmt_vs_ode_focei.rds"))
}
print(aggregate(sec ~ cfg + arm + what, res, median))
cat("pred agreement:", relDiff, "\n")
