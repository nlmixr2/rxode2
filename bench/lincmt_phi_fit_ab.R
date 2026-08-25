# lincmt_phi_fit_ab.R -- the fit-cell A/B for the transition-matrix (Phi)
# path.  Solve-level gains were measured in lincmt_transition_matrix_ab.R
# (2cmt uniform 1.22/1.12x, 3cmt uniform 1.44/1.21x, 3cmt multidose
# 1.59/1.39x); this asks what survives at the FOCEi fit level, where the
# project's own breakdown says the sensitivity solve is only ~a tenth of
# the wall clock.
#
# Protocol (project benchmark discipline):
#   - rxode2 from THIS worktree, built the ORDINARY optimized way:
#     pkgbuild::compile_dll(debug=FALSE), loaded compile=FALSE, taking
#     whatever optimization that yields.  src/Makevars lists -O3 before
#     R's -O2, so -O2 wins -- and that is what an installed package
#     actually runs, so it is what a benchmark must use.  Do NOT force
#     -O3 (R_MAKEVARS_USER or otherwise): a package may not override the
#     user's optimization flags, and numbers taken that way do not
#     represent what anyone runs.  Never benchmark through a plain
#     load_all() (-O0).  A/B ratios are valid only with both arms built
#     identically; absolute numbers never cross build flag sets.
#   - nlmixr2est from ~/src/nlmixr2est-lincmt-speed (load_all, helpers=FALSE).
#   - Single-thread (cores = 1); run pinned:
#       CELL=2cmt-uniform ROUNDS=3 CORE=21 taskset -c 21 Rscript bench/lincmt_phi_fit_ab.R
#   - linCmtSensType = "AD" (forward) FORCED: this tree still carries the
#     pre-#1280 count rule (auto -> reverse here), and Phi only serves the
#     forward tail path, so without this the A/B would measure nothing.
#   - ONE binary, control flipped (linCmtSensPhi TRUE/FALSE), arms
#     ALTERNATED within each round, medians over rounds, load per rep.
#   - et() events, not a large data.frame (etTrans otherwise dominates).
#
# One cell per invocation so each run fits a foreground timeout.
#
# RESULTS (2026-08-25, DEFAULT optimization, pinned core 21, load ~1.3,
# 2 rounds, arms alternated; medians) -- after the engage rule was made
# row-aware:
#
#   cell              arm     sec   nIter  ms/iter  phiBuild  reuse
#   2cmt-uniform      phiOn  28.40   1167    24.34     75895    495
#   2cmt-uniform      phiOff 45.36   1302    34.84         0      -
#   2cmt-uniform      ode    23.05   1263    18.25         0      -
#   2cmt-nonuniform   phiOn  37.86   1086    34.86         0      -
#   2cmt-nonuniform   phiOff 37.98   1086    34.97         0      -
#
# phiOff/phiOn:            wall     per-iteration
#   2cmt uniform          1.60x         1.431x
#   2cmt nonuniform       1.00x         1.003x
#
# The uniform per-iteration gain is LARGER at the default -O2 (1.43x)
# than it was at the forced -O3 (1.22x): less aggressive optimization
# makes the fvar tail the matrix replaces relatively more expensive.
# The non-uniform cell is now exactly inert -- no matrix is built in
# either arm, so the two arms run identical code, and they agree to the
# last digit AND take the same number of iterations (1086).  The 0.80x
# wall and the 32% iteration divergence recorded for that cell below
# were both artifacts of the spurious building, not properties of the
# design.  Against the integrated arm the closed form remains 1.33x
# more expensive per unit work (24.34 vs 18.25 ms/iter).
#
# RESULTS BELOW ARE SUPERSEDED in two ways, kept for the reasoning:
#   (a) they were taken at a forced -O3, which is not what an installed
#       package runs (see the protocol note above); and
#   (b) the 2cmt-nonuniform row records a BUG since fixed -- the engage
#       rule counted a row re-querying its own gap as evidence that the
#       interval recurs, so a strictly non-repeating design built 8.6M
#       matrices at 4:1 "reuse" inside a fit.  Evidence is now row-aware
#       and that cell builds none.  Re-measured numbers at the default
#       optimization are in RESULTS (2026-08-25) further below.
#
# RESULTS (2026-08-24, forced -O3, pinned core 21, load 0.4-2.0, 3 rounds,
# arms alternated; medians).  Read the PER-ITERATION column, not wall
# clock: the arms converge in different numbers of outer iterations, and
# that difference swamps the timing.
#
#   cell              arm     sec   nIter  ms/iter  phiBuild  reuse
#   2cmt-uniform      phiOn  21.66    936    23.14     60936    499
#   2cmt-uniform      phiOff 36.60   1302    28.11         0      -
#   2cmt-uniform      ode    22.69   1263    17.96         0      -
#   3cmt-uniform      phiOn  46.26   1800    25.70    114856    499
#   3cmt-uniform      phiOff 55.47   1410    39.34         0      -
#   2cmt-nonuniform   phiOn  38.92   1434    27.14   8594700      4
#   2cmt-nonuniform   phiOff 31.00   1086    28.54         0      -
#
# phiOff/phiOn:            wall     per-iteration
#   2cmt uniform          1.69x         1.215x
#   3cmt uniform          1.20x         1.531x
#   2cmt nonuniform       0.80x         1.052x
#
# 1. The per-iteration gains (1.22 / 1.53) track the solve-level bench
#    (1.22 / 1.44) closely; wall clock does not, because nIter differs by
#    up to 32% between arms.  A last-bit change in the gradient moves
#    bobyqa's trajectory, so it stops after a different number of steps --
#    an optimizer-path lottery, not a performance property.  With one
#    dataset per cell the wall-clock direction is not evidence either way.
# 2. NON-UNIFORM IS NOT A REGRESSION IN WORK: per iteration it is 1.05x,
#    i.e. marginally faster.  The 0.80x wall clock is entirely the 32%
#    extra iterations.
# 3. BUT the engage rule does NOT hold in the fit path: this same design
#    builds ZERO matrices in a plain rxSolve() (verified: phiBuild=0, with
#    or without per-subject theta variation) yet builds 8.6M in the fit,
#    at 4:1 reuse instead of 499:1.  Something in the inner problem's row
#    walk re-arms the interval cache that a single solve leaves disarmed.
#    Worth a look: it is wasted work, even though it is not costing
#    measurable time here.
# 4. vs the integrated arm (2cmt-uniform, same build/data): linCmt+Phi is
#    21.66 s against the ODE's 22.69 s -- parity in wall clock -- but
#    23.14 vs 17.96 ms/iter, so the integrator is still 1.29x cheaper per
#    unit work.  The wall-clock parity leans on 936 vs 1263 iterations.
#    (Not comparable to the project's older 28.5 vs 14.3 s figures: a
#    different build, dataset and observation count.)

suppressMessages({
  devtools::load_all("~/src/rxode2-lincmt-carry-jump", compile = FALSE, quiet = TRUE)
  devtools::load_all("~/src/nlmixr2est-lincmt-speed", helpers = FALSE, quiet = TRUE)
})
rxode2::setRxThreads(1L)

cell   <- Sys.getenv("CELL", "2cmt-uniform")
nRound <- as.integer(Sys.getenv("ROUNDS", "3"))
core   <- Sys.getenv("CORE", "unpinned")
outDir <- "~/src/rxode2-lincmt-carry-jump/bench/results"

loadAvg <- function() as.numeric(strsplit(readLines("/proc/loadavg"), " ")[[1]][1])

parts   <- strsplit(cell, "-")[[1]]
cfg     <- parts[1]                      # 2cmt / 3cmt
spacing <- parts[2]                      # uniform / nonuniform
withOde <- identical(cell, "2cmt-uniform")   # the canonical vs-ODE cell

nSub <- 40L
nObs <- 100L
## uniform: exactly repeating gaps (Phi's regime).  nonuniform: never
## repeating, so the engage rule must build nothing at all.
simTimes <- if (spacing == "uniform") {
  seq(0.5, 50, length.out = nObs)
} else {
  cumsum(seq(0.30, 0.70, length.out = nObs))
}

trueTheta <- list(
  `2cmt` = c(ka = 1.2, cl = 4, v = 30, q = 8, vp = 60),
  `3cmt` = c(ka = 1.2, cl = 4, v = 30, q = 8, vp = 60, q2 = 3, vp2 = 200))[[cfg]]

odeLines <- list(
  `2cmt` = paste0("d/dt(depot) = -ka*depot;",
                  "d/dt(central) = ka*depot - cl/v*central - q/v*central + q/vp*periph;",
                  "d/dt(periph) = q/v*central - q/vp*periph"),
  `3cmt` = paste0("d/dt(depot) = -ka*depot;",
                  "d/dt(central) = ka*depot - cl/v*central - q/v*central + q/vp*periph - q2/v*central + q2/vp2*periph2;",
                  "d/dt(periph) = q/v*central - q/vp*periph;",
                  "d/dt(periph2) = q2/v*central - q2/vp2*periph2"))[[cfg]]

## ---- simulate one dataset (shared by every arm) --------------------------
set.seed(1002003)
etaSd <- 0.3
eta <- matrix(rnorm(nSub * 3L, 0, etaSd), nSub, 3L,
              dimnames = list(NULL, c("ka", "cl", "v")))
simMod <- rxode2::rxode2(paste0("cp = central/v;", odeLines))
pars <- data.frame(row.names = seq_len(nSub))
for (p in names(trueTheta)) {
  pars[[p]] <- if (p %in% colnames(eta)) trueTheta[[p]] * exp(eta[, p]) else trueTheta[[p]]
}
ev <- rxode2::et(amt = 100, cmt = "depot") |> rxode2::et(simTimes)
sim <- rxode2::rxSolve(simMod, pars, ev, cores = 1L, addDosing = FALSE)
stopifnot(nrow(sim) == nSub * nObs)
set.seed(2003004)
simDat <- data.frame(ID = rep(seq_len(nSub), each = nObs), TIME = sim$time,
                     DV = sim$cp * (1 + rnorm(nrow(sim), 0, 0.15)),
                     AMT = 0, EVID = 0, CMT = "central")
doseRows <- data.frame(ID = seq_len(nSub), TIME = 0, DV = NA_real_,
                       AMT = 100, EVID = 1, CMT = "depot")
dat <- rbind(doseRows, simDat)
dat <- dat[order(dat$ID, dat$TIME, -dat$EVID), ]

## ---- model pair ----------------------------------------------------------
iniBlock <- paste(c(sprintf("l%s <- log(%.6g)", names(trueTheta),
                            unname(trueTheta) * 1.2),
                    "eta.ka ~ 0.1", "eta.cl ~ 0.1", "eta.v ~ 0.1",
                    "prop.sd <- 0.2"), collapse = "\n")
parBlock <- paste(vapply(names(trueTheta), function(p) {
  e <- if (p %in% c("ka", "cl", "v")) sprintf("*exp(eta.%s)", p) else ""
  sprintf("%s <- exp(l%s)%s", p, p, e)
}, ""), collapse = "\n")

uiLin <- eval(parse(text = sprintf(
  "function() {\n ini({\n%s\n })\n model({\n%s\n cp <- linCmt()\n cp ~ prop(prop.sd)\n })\n}",
  iniBlock, parBlock)))
uiOde <- eval(parse(text = sprintf(
  "function() {\n ini({\n%s\n })\n model({\n%s\n%s\n cp <- central/v\n cp ~ prop(prop.sd)\n })\n}",
  iniBlock, parBlock, gsub(";", "\n", odeLines))))

ctlFor <- function(phi) nlmixr2est::foceiControl(
  calcTables = FALSE, print = 0L, covMethod = "",
  rxControl = rxode2::rxControl(cores = 1L, linCmtSensType = "AD",
                                linCmtSensPhi = phi))
## the ODE arm never touches linCmt; its control just mirrors the rest
ctlOde <- nlmixr2est::foceiControl(
  calcTables = FALSE, print = 0L, covMethod = "",
  rxControl = rxode2::rxControl(cores = 1L))

fitOne <- function(ui, control) {
  rxode2:::linCmtSeqStats(TRUE)
  t0 <- proc.time()[["elapsed"]]
  fit <- suppressWarnings(suppressMessages(
    nlmixr2est::nlmixr2(ui, dat, est = "focei", control = control)))
  sec <- proc.time()[["elapsed"]] - t0
  st <- rxode2:::linCmtSeqStats(TRUE)
  list(sec = sec, objf = fit$objective, st = st,
       nIter = tryCatch(nrow(fit$parHistData), error = function(e) NA_integer_))
}

arms <- list(list(nm = "phiOn",  ui = uiLin, ctl = ctlFor(TRUE)),
             list(nm = "phiOff", ui = uiLin, ctl = ctlFor(FALSE)))
if (withOde) arms <- c(arms, list(list(nm = "ode", ui = uiOde, ctl = ctlOde)))

## warm-up per arm (compiles models; untimed)
for (a in arms) invisible(fitOne(a$ui, a$ctl))

rows <- list()
for (r in seq_len(nRound)) {
  ord <- if (r %% 2L == 1L) seq_along(arms) else rev(seq_along(arms))
  for (i in ord) {
    a <- arms[[i]]
    z <- fitOne(a$ui, a$ctl)
    rows[[length(rows) + 1L]] <- data.frame(
      cell = cell, arm = a$nm, round = r, sec = z$sec,
      objf = z$objf, nIter = z$nIter,
      phiBuild = unname(z$st[["phiBuild"]]), phiRows = unname(z$st[["phiRows"]]),
      tailRows = unname(z$st[["seqTailRows"]]), load = loadAvg(),
      stringsAsFactors = FALSE)
  }
}
res <- do.call(rbind, rows)

prod <- system(paste("readelf --debug-dump=info",
                     "~/src/rxode2-lincmt-carry-jump/src/linCmt.o",
                     "2>/dev/null | grep -m1 -o 'DW_AT_producer.*'"), intern = TRUE)
attr(res, "provenance") <- list(
  date = format(Sys.time()), cell = cell, rounds = nRound, core = core,
  nSub = nSub, nObs = nObs, spacing = spacing,
  producer = substr(paste(prod, collapse = " "), 1, 300),
  flags = "pkgbuild::compile_dll(debug=FALSE) with R_MAKEVARS_USER forcing -O3 last",
  sensType = "AD (forward) forced; Phi serves only the forward tail path",
  rxode2 = system("git -C ~/src/rxode2-lincmt-carry-jump rev-parse --short HEAD", intern = TRUE),
  nlmixr2est = system("git -C ~/src/nlmixr2est-lincmt-speed rev-parse --short HEAD", intern = TRUE))
dir.create(outDir, showWarnings = FALSE)
saveRDS(res, file.path(outDir, sprintf("lincmt_phi_fit_ab_%s.rds", cell)))

cat("\n== ", cell, " ==\n", sep = "")
print(aggregate(cbind(sec, phiBuild, phiRows, load) ~ arm, res, median),
      row.names = FALSE)
cat("\nobjectives (all reps):\n")
print(unique(res[, c("arm", "objf", "nIter")]), row.names = FALSE)
