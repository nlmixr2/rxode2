# Re-measure the fit-cost breakdown for the paper, on the CURRENT tree at
# the DEFAULT optimization, and settle the contradiction between:
#   (a) bench/lincmt_fit_cost_breakdown.R's "~56 us per computed row, so the
#       sensitivity solve is roughly a tenth of a fit" -- taken on a build
#       where the value memo did NOT engage (its own provenance says so), and
#   (b) the Phi fit-cell A/B's 1.43x PER-ITERATION gain from a solve-side-only
#       change, which a 10%-solve model says should have been ~1.02x.
# Inverting (b) says the solve is close to ALL of the per-iteration cost.
# Both cannot be right.
#
# BUILD DISCIPLINE (CRAN: a package may not override the user's optimization):
#   Never force -O3 (no R_MAKEVARS_USER, no Makevars edit).  src/Makevars puts
#   -O3 in PKG_CXXFLAGS, R appends its own -O2 AFTER it, so -O2 wins and IS
#   what a real installation runs.  Verified here from the DWARF producer
#   string.  Never benchmark through a plain load_all() (that is -O0).
#
# Two INDEPENDENT determinations of the solve fraction f:
#   REPLAY   -- solve the fit's own inner model standalone over the same
#               subjects/rows, scale one pass to the fit's total computed
#               rows, divide by the fit's optimize time.
#   DIFFERENTIAL -- measure the Phi on/off gain at the SOLVE level (g) and at
#               the FIT level per iteration (R) in the same session, then
#               f = (1 - 1/R) / (1 - 1/g), from
#               R = 1 / ((1 - f) + f/g).
# They isolate different things (see the caveats printed at the end), so
# agreement between them is the evidence, not either one alone.
#
# Run pinned on a quiet machine:
#   ROUNDS=3 CORE=21 taskset -c 21 Rscript bench/lincmt_fit_solve_fraction.R
suppressMessages({
  devtools::load_all(Sys.getenv("RXTREE", "~/src/rxode2-lincmt-carry-jump"),
                     compile = FALSE, quiet = TRUE)
  devtools::load_all(Sys.getenv("NLTREE", "~/src/nlmixr2est-lincmt-speed"),
                     compile = FALSE, helpers = FALSE, quiet = TRUE)
})
rxode2::setRxThreads(1L)

nRound <- as.integer(Sys.getenv("ROUNDS", "3"))
core   <- Sys.getenv("CORE", "unpinned")
cfg    <- Sys.getenv("CFG", "2cmt")
outDir <- "~/src/rxode2-lincmt-carry-jump/bench/results"
loadAvg <- function() as.numeric(strsplit(readLines("/proc/loadavg"), " ")[[1]][1])

## ---- provenance: the flags actually used ---------------------------------
optFlags <- tryCatch({
  o <- file.path(Sys.getenv("RXTREE", "~/src/rxode2-lincmt-carry-jump"),
                 "src", "linCmt.o")
  p <- system(sprintf("readelf --debug-dump=info %s 2>/dev/null | grep -m1 -o 'GNU C++17.*'",
                      path.expand(o)), intern = TRUE)
  if (length(p)) sub(".*?(-g .*?-std)", "\\1", p[1]) else NA_character_
}, error = function(e) NA_character_)
cat("producer flags:", optFlags, "\n")
cat("effective -O: ", {
  m <- regmatches(optFlags, gregexpr("-O[0-3g]", optFlags))[[1]]
  if (length(m)) tail(m, 1) else "?"
}, " (last -O on the command line wins)\n", sep = "")

## ---- data: the paper's cell, 2cmt oral 3 etas, 40 x 100 UNIFORM ----------
nSub <- 40L; nObs <- 100L
simTimes <- seq(0.5, 50, length.out = nObs)   # exactly repeating gaps
trueTheta <- list(
  `2cmt` = c(ka = 1.2, cl = 4, v = 30, q = 8, vp = 60),
  `3cmt` = c(ka = 1.2, cl = 4, v = 30, q = 8, vp = 60, q2 = 3, vp2 = 200))[[cfg]]
odeLines <- list(
  `2cmt` = paste0("d/dt(depot) = -ka*depot;",
                  "d/dt(central) = ka*depot - cl/v*central - q/v*central + q/vp*periph;",
                  "d/dt(periph) = q/v*central - q/vp*periph"),
  `3cmt` = paste0("d/dt(depot) = -ka*depot;",
                  "d/dt(central) = ka*depot - cl/v*central - q/v*central + q/vp*periph",
                  " - q2/v*central + q2/vp2*periph2;",
                  "d/dt(periph) = q/v*central - q/vp*periph;",
                  "d/dt(periph2) = q2/v*central - q2/vp2*periph2"))[[cfg]]

set.seed(1002003)
eta <- matrix(rnorm(nSub * 3L, 0, 0.3), nSub, 3L,
              dimnames = list(NULL, c("ka", "cl", "v")))
simMod <- rxode2::rxode2(paste0("cp = central/v;", odeLines))
pars0 <- data.frame(row.names = seq_len(nSub))
for (p in names(trueTheta)) {
  pars0[[p]] <- if (p %in% colnames(eta)) trueTheta[[p]] * exp(eta[, p]) else trueTheta[[p]]
}
ev <- rxode2::et(amt = 100, cmt = "depot") |> rxode2::et(simTimes)
sim <- rxode2::rxSolve(simMod, pars0, ev, cores = 1L, addDosing = FALSE)
stopifnot(nrow(sim) == nSub * nObs)
set.seed(2003004)
dat <- rbind(
  data.frame(ID = seq_len(nSub), TIME = 0, DV = NA_real_, AMT = 100,
             EVID = 1, CMT = "depot"),
  data.frame(ID = rep(seq_len(nSub), each = nObs), TIME = sim$time,
             DV = sim$cp * (1 + rnorm(nrow(sim), 0, 0.15)), AMT = 0,
             EVID = 0, CMT = "central"))
dat <- dat[order(dat$ID, dat$TIME, -dat$EVID), ]
rowsPerSubject <- nObs + 1L

iniBlock <- paste(c(sprintf("l%s <- log(%.6g)", names(trueTheta),
                            unname(trueTheta) * 1.2),
                    "eta.ka ~ 0.1", "eta.cl ~ 0.1", "eta.v ~ 0.1",
                    "prop.sd <- 0.2"), collapse = "\n")
parBlock <- paste(vapply(names(trueTheta), function(p) {
  e <- if (p %in% c("ka", "cl", "v")) sprintf("*exp(eta.%s)", p) else ""
  sprintf("%s <- exp(l%s)%s", p, p, e)
}, ""), collapse = "\n")
ui <- eval(parse(text = sprintf(
  "function() {\n ini({\n%s\n })\n model({\n%s\n cp <- linCmt()\n cp ~ prop(prop.sd)\n })\n}",
  iniBlock, parBlock)))

ctlOf <- function(phi) nlmixr2est::foceiControl(
  calcTables = FALSE, print = 0L, covMethod = "",
  rxControl = rxode2::rxControl(cores = 1L, linCmtSensType = "AD",
                                linCmtSensPhi = phi))
fitOne <- function(phi) {
  invisible(rxode2:::linCmtSeqStats(TRUE))
  t0 <- proc.time()[["elapsed"]]
  fit <- suppressWarnings(suppressMessages(
    nlmixr2est::nlmixr2(ui, dat, est = "focei", control = ctlOf(phi))))
  wall <- proc.time()[["elapsed"]] - t0
  st <- rxode2:::linCmtSeqStats(TRUE)
  list(wall = wall, stats = st, objf = fit$objective,
       time = as.data.frame(fit$time),
       nIter = tryCatch(nrow(fit$parHistData), error = function(e) NA_integer_),
       fit = fit, load = loadAvg())
}

## ---- warm-up (compiles the inner models); untimed ------------------------
cat("warm-up fit (untimed, compiles)...\n")
warm <- fitOne(TRUE)
innerModel <- warm$fit$env$innerModel
fullTheta  <- warm$fit$env$fullTheta
stopifnot(grepl("linCmtB", rxode2::rxNorm(innerModel)))

## ---- standalone replay of the fit's own inner model ----------------------
# A whole rxSolve of a 4000-row inner model is dominated by FIXED per-call
# cost (etTrans, event setup, allocation): measured 11.5 us/row against the
# fit's own 0.74 us/row, which also diluted the Phi gain to 1.02x.  So take
# the MARGINAL per-row cost from the slope between two sizes with the SAME
# gap (so Phi engages identically in both): rows and seconds both grow, the
# fixed call cost cancels.
pn   <- rxode2::rxModelVars(innerModel)$params
sPar <- setNames(as.data.frame(matrix(0.05, nSub, length(pn))), pn)
thC  <- grepl("^THETA_", pn)
if (sum(thC) == length(fullTheta)) sPar[, thC] <- rep(fullTheta, each = nSub)
gap    <- simTimes[2] - simTimes[1]
evSmall <- ev
evBig   <- rxode2::et(amt = 100, cmt = "depot") |>
  rxode2::et(seq(simTimes[1], by = gap, length.out = nObs * 10L))
timeSolve <- function(evx, phi, nRep) {
  invisible(rxode2:::linCmtSeqStats(TRUE))
  tt <- numeric(nRep)
  for (r in seq_len(nRep)) {
    t0 <- proc.time()[["elapsed"]]
    s <- rxode2::rxSolve(innerModel, sPar, evx, cores = 1L, addDosing = FALSE,
                         useLinCmt = FALSE, linCmtSensPhi = phi)
    tt[r] <- proc.time()[["elapsed"]] - t0
    stopifnot(nrow(s) > 0)
  }
  st <- rxode2:::linCmtSeqStats(TRUE)
  list(sec = median(tt), rows = as.numeric(st[["valueCompute"]]) / nRep)
}
onePass <- function(phi, nRep = 15L) {
  a <- timeSolve(evSmall, phi, nRep)
  b <- timeSolve(evBig,   phi, max(3L, nRep %/% 3L))
  marginalUs <- 1e6 * (b$sec - a$sec) / (b$rows - a$rows)
  list(sec = a$sec, rows = a$rows, bigSec = b$sec, bigRows = b$rows,
       marginalUs = marginalUs)
}

rows <- list(); passes <- list()
for (rd in seq_len(nRound)) {
  # alternate the arm order so drift cannot favour one consistently
  arms <- if (rd %% 2L == 1L) c(TRUE, FALSE) else c(FALSE, TRUE)
  for (phi in arms) {
    z <- fitOne(phi)
    st <- z$stats
    rows[[length(rows) + 1L]] <- data.frame(
      round = rd, phi = phi, wall = z$wall,
      setup = z$time$setup, optimize = z$time$optimize,
      other = z$wall - z$time$setup - z$time$optimize,
      nIter = z$nIter, objf = z$objf,
      valueCompute = as.numeric(st[["valueCompute"]]),
      phiBuild = as.numeric(st[["phiBuild"]]),
      phiRows  = as.numeric(st[["phiRows"]]),
      expBuild = as.numeric(st[["expBuild"]]),
      expHit   = as.numeric(st[["expHit"]]),
      load = z$load)
    cat(sprintf("round %d phi=%-5s wall %.2f s optimize %.2f nIter %s rows %.3g phiBuild %.3g (load %.2f)\n",
                rd, phi, z$wall, z$time$optimize, z$nIter,
                as.numeric(st[["valueCompute"]]), as.numeric(st[["phiBuild"]]),
                z$load))
  }
  for (phi in arms) {
    p <- onePass(phi)
    passes[[length(passes) + 1L]] <- data.frame(
      round = rd, phi = phi, sec = p$sec, rows = p$rows,
      bigSec = p$bigSec, bigRows = p$bigRows,
      wholeCallUs = 1e6 * p$sec / p$rows, marginalUs = p$marginalUs,
      load = loadAvg())
    cat(sprintf("   replay phi=%-5s whole-call %.2f us/row | MARGINAL %.3f us/row (%.4f->%.4f s, %.0f->%.0f rows)\n",
                phi, 1e6 * p$sec / p$rows, p$marginalUs, p$sec, p$bigSec,
                p$rows, p$bigRows))
  }
  saveRDS(list(fits = do.call(rbind, rows), passes = do.call(rbind, passes)),
          file.path(path.expand(outDir), "lincmt_fit_solve_fraction.rds"))
}

fits <- do.call(rbind, rows); pass <- do.call(rbind, passes)
agg <- function(d, k) {
  s <- split(d, d$phi)
  vapply(s, function(x) median(x[[k]]), 0)
}
optOn  <- agg(fits, "optimize")[["TRUE"]];  optOff  <- agg(fits, "optimize")[["FALSE"]]
itOn   <- agg(fits, "nIter")[["TRUE"]];     itOff   <- agg(fits, "nIter")[["FALSE"]]
rowsOn <- agg(fits, "valueCompute")[["TRUE"]]; rowsOff <- agg(fits, "valueCompute")[["FALSE"]]
msIterOn  <- 1e3 * optOn  / itOn
msIterOff <- 1e3 * optOff / itOff
usRowOn   <- 1e6 * optOn  / rowsOn
usRowOff  <- 1e6 * optOff / rowsOff
R <- msIterOff / msIterOn                      # fit-level per-iteration gain
passOn  <- agg(pass, "sec")[["TRUE"]]; passOff <- agg(pass, "sec")[["FALSE"]]
margOn  <- agg(pass, "marginalUs")[["TRUE"]]
margOff <- agg(pass, "marginalUs")[["FALSE"]]
g <- margOff / margOn        # solve-level gain, marginal (fixed cost removed)
fDiff   <- (1 - 1/R) / (1 - 1/g)
# REPLAY: the fit's own computed rows x the MARGINAL per-row solve cost
solveSecOn <- margOn * 1e-6 * rowsOn
fReplay    <- solveSecOn / optOn
# Both of the above are INVALID here -- see the printout.  What IS rigorous:
# R = 1/((1-f) + f/g) with g >= 1 (Phi cannot slow the solve) gives
#   f >= 1 - 1/R,  and f -> 1 as g -> R.  No independent g needed.
fLower <- 1 - 1/R

summ <- list(
  provenance = list(commit = system("git -C ~/src/rxode2-lincmt-carry-jump rev-parse --short HEAD",
                                    intern = TRUE),
                    nlCommit = system("git -C ~/src/nlmixr2est-lincmt-speed rev-parse --short HEAD",
                                      intern = TRUE),
                    producerFlags = optFlags, core = core, rounds = nRound,
                    cfg = cfg, nSub = nSub, nObs = nObs, spacing = "uniform",
                    loadRange = range(c(fits$load, pass$load)),
                    date = format(Sys.time(), "%Y-%m-%d %H:%M")),
  fits = fits, passes = pass,
  derived = c(optOn = optOn, optOff = optOff, nIterOn = itOn, nIterOff = itOff,
              msIterOn = msIterOn, msIterOff = msIterOff,
              usRowOn = usRowOn, usRowOff = usRowOff,
              fitPerIterGain = R, solveGain = g,
              passSecOn = passOn, passSecOff = passOff,
              marginalUsOn = margOn, marginalUsOff = margOff,
              solveSecOn = solveSecOn,
              fLowerBound = fLower,
              fDifferentialINVALID = fDiff, fReplayINVALID = fReplay))
saveRDS(summ, file.path(path.expand(outDir), "lincmt_fit_solve_fraction.rds"))

cat("\n================ paper table ================\n")
cat(sprintf("cell: %s oral, 3 etas, %d subjects x %d uniform obs; FOCEi/bobyqa; %s; %s\n",
            cfg, nSub, nObs, core, summ$provenance$date))
cat(sprintf("%-28s %12s %12s\n", "", "phi ON", "phi OFF"))
cat(sprintf("%-28s %12.2f %12.2f\n", "optimize (s)", optOn, optOff))
cat(sprintf("%-28s %12.0f %12.0f\n", "outer iterations", itOn, itOff))
cat(sprintf("%-28s %12.2f %12.2f\n", "ms / iteration", msIterOn, msIterOff))
cat(sprintf("%-28s %12.2f %12.2f\n", "us / computed row", usRowOn, usRowOff))
cat(sprintf("%-28s %12.3f %12.3f\n", "marginal us/row (solve)", margOn, margOff))
cat(sprintf("\nfit per-iteration gain R = %.3fx (solve-side-only change)\n", R))
cat(sprintf("SOLVE FRACTION: f >= 1 - 1/R = %.2f, and f -> 1 as g -> R.\n", fLower))
cat("  Rigorous: R = 1/((1-f) + f/g) and g >= 1 (Phi cannot slow the solve),\n")
cat("  so no independent g is needed for the lower bound.\n")
cat(sprintf("\nBOTH standalone-replay estimates are INVALID and are reported only to\n"))
cat(sprintf("document why: whole-call %.1f us/row, marginal %.1f us/row, against the\n",
            1e6 * passOn / agg(pass, "rows")[["TRUE"]], margOn))
cat(sprintf("fit's own %.2f us/row -- an rxSolve is ~%.0fx more expensive per row than\n",
            usRowOn, margOn / usRowOn))
cat("the fit's inner solve (which uses ind_solve: no output data.frame, no\n")
cat("etTrans per evaluation).  At 7 us/row resolution Phi's saving is invisible\n")
cat(sprintf("(g reads %.3fx), so rxSolve cannot serve as a proxy for the inner solve.\n", g))
cat(sprintf("\nold '56 us/row' figure re-taken on this build: %.1f us/row (phi ON) / %.1f (phi OFF)\n",
            usRowOn, usRowOff))
cat(sprintf("load range over the run: %.2f - %.2f\n", summ$provenance$loadRange[1],
            summ$provenance$loadRange[2]))
cat("\ncaveats: the REPLAY figure includes the solve's own per-call entry and\n")
cat("event handling (it is a whole rxSolve of the inner model), so it bounds\n")
cat("the solve's share from above rather than isolating kernel arithmetic;\n")
cat("the DIFFERENTIAL figure assumes Phi changes only the solve, which is\n")
cat("true by construction.  Single cell -- not a general claim.\n")
