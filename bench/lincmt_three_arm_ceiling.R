# lincmt_three_arm_ceiling.R -- three-arm speed comparison on the SAME linear
# model: (A) linCmt() closed form (forward AD, the decided default), (B) ODE
# integrator with generated sensitivity equations, (C) matrix exponential via
# nlmixr2est PR #998's native path (method = "indLin", rxSensMatExp).
#
# Purpose (plan file "USER HYPOTHESIS" block): test whether the matrix
# exponential explains the 3-10x NONMEM ADVAN-vs-ODE folklore, and establish
# the SOLVE-level ceiling the fit-level ratio can approach as problem size
# grows.  The solve-level arm times each method's GENERATED INNER MODEL (the
# object a FOCEi fit actually evaluates: value + eta sensitivities) through
# rxSolve, so all three arms compute the same quantities.
#
# Protocol (matches bench/lincmt_vs_ode_focei.R):
#   - rxode2 from THIS worktree, optimized .so, loaded compile = FALSE.
#   - nlmixr2est from ~/src/nlmixr2est-matexp-bench (origin/main detached
#     worktree -- the lincmt-speed branch predates PR #998).
#   - Single-thread, whole Rscript pinned:
#       CONFIG=1cmt MODE=solve REPS=3 taskset -c <idle> Rscript bench/lincmt_three_arm_ceiling.R
#   - linCmtSensType = "AD" forced (this tree's auto predates rxode2#1280).
#   - Timed runs only when load < 2; load recorded per row.
# MODE=solve  : solve-level sweep over problem sizes (fast, many cells)
# MODE=fit    : FOCEi fits, 40x10 (3 reps) + one larger 2cmt fit (1 rep)

suppressMessages({
  devtools::load_all("~/src/rxode2-lincmt-carry-jump", compile = FALSE, quiet = TRUE)
  devtools::load_all("~/src/nlmixr2est-matexp-bench", helpers = FALSE, quiet = TRUE)
})
rxode2::setRxThreads(1L)

cfg    <- Sys.getenv("CONFIG", "1cmt")
mode   <- Sys.getenv("MODE", "solve")
nRep   <- as.integer(Sys.getenv("REPS", "3"))
core   <- Sys.getenv("CORE", "unpinned")
outDir <- "~/src/rxode2-lincmt-carry-jump/bench/results"

loadAvg <- function() as.numeric(strsplit(readLines("/proc/loadavg"), " ")[[1]][1])
stopifnot(loadAvg() < 2)

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

# matExp() block: first-order rate constants k_from_to (PR #998 convention).
matLines <- list(
  `1cmt` = paste0("matExp()\nk_depot_central <- ka\nk_central_output <- cl/v"),
  `2cmt` = paste0("matExp()\nk_depot_central <- ka\nk_central_output <- cl/v\n",
                  "k_central_periph <- q/v\nk_periph_central <- q/vp"),
  `3cmt` = paste0("matExp()\nk_depot_central <- ka\nk_central_output <- cl/v\n",
                  "k_central_periph <- q/v\nk_periph_central <- q/vp\n",
                  "k_central_periph2 <- q2/v\nk_periph2_central <- q2/vp2"))[[cfg]]

iniBlock <- paste(c(sprintf("l%s <- log(%.6g)", names(trueTheta), unname(trueTheta) * 1.2),
                    "eta.ka ~ 0.1", "eta.cl ~ 0.1", "eta.v ~ 0.1",
                    "prop.sd <- 0.2"), collapse = "\n")
parBlock <- paste(vapply(names(trueTheta), function(p) {
  e <- if (p %in% c("ka", "cl", "v")) sprintf("*exp(eta.%s)", p) else ""
  sprintf("%s <- exp(l%s)%s", p, p, e)
}, ""), collapse = "\n")

mkUi <- function(body, pred) eval(parse(text = sprintf(
  "function() {\n ini({\n%s\n })\n model({\n%s\n%s\n cp <- %s\n cp ~ prop(prop.sd)\n })\n}",
  iniBlock, parBlock, body, pred)))

uiLin <- mkUi("", "linCmt()")
uiOde <- mkUi(odeLines, "central/v")
uiMat <- mkUi(matLines, "central/v")

## ---- shared simulation helpers ------------------------------------------
# NOTE (found while building this bench): the native matExp sens path
# (nlmixr2est PR #998) silently mis-solves the INNER problem when the data
# carries a STRING CMT column ("depot"/"central"): the value differs and the
# eta gradient is dead (etas stay 0).  Numeric CMT (or none) is exact.  The
# matExp arm therefore gets numeric CMT (1 = depot, 2 = central, source-first
# order); the other arms keep strings.  Same physical regimen in all arms,
# checked by the posthoc equivalence gate below.
mkDat <- function(nSub, nObs, nDose = 1L, tmax = 32) {
  set.seed(1002003)
  obsT <- sort(unique(round(exp(seq(log(0.25), log(tmax), length.out = nObs)), 3)))
  doseT <- if (nDose > 1L) seq(0, tmax * 0.4, length.out = nDose) else 0
  simMod <- rxode2::rxode2(paste0("cp = central/v\n", odeLines))
  eta <- matrix(rnorm(nSub * 3L, 0, 0.3), nSub, 3L,
                dimnames = list(NULL, c("ka", "cl", "v")))
  pars <- data.frame(row.names = seq_len(nSub))
  for (p in names(trueTheta)) {
    pars[[p]] <- if (p %in% colnames(eta)) trueTheta[[p]] * exp(eta[, p]) else trueTheta[[p]]
  }
  ev <- rxode2::et(amt = 100, time = doseT, cmt = "depot") |> rxode2::et(obsT)
  sim <- rxode2::rxSolve(simMod, pars, ev, cores = 1L, addDosing = FALSE,
                         useLinCmt = FALSE)
  set.seed(2003004)
  simDat <- data.frame(ID = rep(seq_len(nSub), each = length(obsT)),
                       TIME = sim$time,
                       DV = sim$cp * (1 + rnorm(nrow(sim), 0, 0.15)),
                       AMT = 0, EVID = 0, CMT = "central")
  doseRows <- data.frame(ID = rep(seq_len(nSub), each = length(doseT)),
                         TIME = rep(doseT, nSub), DV = NA_real_,
                         AMT = 100, EVID = 1, CMT = "depot")
  dat <- rbind(doseRows, simDat)
  dat[order(dat$ID, dat$TIME, -dat$EVID), ]
}

ctlOf <- function(maxOuter = 500L) nlmixr2est::foceiControl(
  calcTables = FALSE, print = 0L, covMethod = "",
  maxOuterIterations = maxOuter,
  rxControl = rxode2::rxControl(cores = 1L, linCmtSensType = "AD"))

fitOne <- function(ui, dat, maxOuter = 500L) {
  t0 <- proc.time()[["elapsed"]]
  fit <- suppressWarnings(suppressMessages(
    nlmixr2est::nlmixr2(ui, dat, est = "focei", control = ctlOf(maxOuter))))
  list(sec = proc.time()[["elapsed"]] - t0, fit = fit)
}

## ---- build each arm's inner model once (40x10 posthoc fit) ---------------
datSmall <- mkDat(40L, 10L)
numCmt <- function(d) { d$CMT <- ifelse(d$CMT == "depot", 1L, 2L); d }
armDat <- function(a, d) if (a == "matExp") numCmt(d) else d
arms <- list(linCmt = uiLin, ode = uiOde, matExp = uiMat)
inner <- list(); innerFit <- list()
for (a in names(arms)) {
  z <- fitOne(arms[[a]], armDat(a, datSmall), maxOuter = 0L)
  inner[[a]] <- z$fit$env$innerModel
  innerFit[[a]] <- z$fit
}
# structural sanity: the ODE arm must be linCmtB-free; matExp arm must carry
# the native matExp sens (no rx__sens_ d/dt lines of the flattened form is not
# required -- what matters is it is NOT the linCmt kernel and NOT plain ODE).
modTxt <- function(im) tryCatch(rxode2::rxNorm(im),
                                error = function(e) paste(deparse(im), collapse = "\n"))
stopifnot(!grepl("linCmtB", modTxt(inner$ode)))
stopifnot(grepl("linCmtB", modTxt(inner$linCmt)))

## ---- arm equivalence: posthoc objf + eta gradients -----------------------
# posthoc objective at identical starting values must agree; the inner models'
# sens columns are consumed identically by focei, so matching objf + matching
# per-subject etas is the aligned end-to-end check.
objs <- vapply(innerFit, function(f) f$objective, 0)
etaMax <- max(abs(as.matrix(innerFit$linCmt$eta[-1]) - as.matrix(innerFit$ode$eta[-1])),
              abs(as.matrix(innerFit$matExp$eta[-1]) - as.matrix(innerFit$ode$eta[-1])))
cat("posthoc objf:", paste(sprintf("%s=%.6f", names(objs), objs), collapse = " "),
    " max |eta diff| vs ode:", format(etaMax, digits = 3), "\n")
if (max(abs(objs - objs[["ode"]])) > 0.05 || etaMax > 2e-2) {
  stop("arm disagreement -- fix before timing")
}

if (mode == "floor") {
  ## ---- value-only (k = 0) per-arm floor at scale ---------------------------
  # plain prediction models, no sensitivities: the per-observation floor each
  # arm's sens machinery sits on.  Per-observation microseconds is the number
  # to compare against NONMEM-solved in absolute terms.
  plainTxt <- list(
    linCmt = paste0("param(", paste(names(trueTheta), collapse = ", "), ")\ncp = linCmt()"),
    ode    = paste0("cp = central/v\n", gsub(" <- ", " = ", odeLines)),
    matExp = paste0(gsub(" <- ", " = ", matLines), "\ncp = central/v"))
  rows <- list()
  for (cell in list(c(40L, 1000L), c(400L, 1000L))) {
    nSub <- cell[1]; nObs <- cell[2]
    obsT <- sort(unique(round(exp(seq(log(0.25), log(32), length.out = nObs)), 4)))
    ev <- rxode2::et(amt = 100, time = 0, cmt = 1) |> rxode2::et(obsT)
    pars <- as.data.frame(t(replicate(nSub, trueTheta)))
    for (a in names(plainTxt)) {
      m <- rxode2::rxode2(plainTxt[[a]])
      xtr <- list()
      if (a == "matExp") xtr$method <- "indLin"
      if (a == "ode") { xtr$atol <- 1e-8; xtr$rtol <- 1e-8; xtr$useLinCmt <- FALSE }
      tset <- numeric(nRep)
      for (r in seq_len(nRep)) {
        t0 <- proc.time()[["elapsed"]]
        s <- do.call(rxode2::rxSolve, c(list(m, pars, ev, cores = 1L,
                                             addDosing = FALSE), xtr))
        tset[r] <- proc.time()[["elapsed"]] - t0
        stopifnot(nrow(s) > 0)
      }
      us <- median(tset) / (nSub * nObs) * 1e6
      rows[[length(rows) + 1L]] <- data.frame(
        cfg = cfg, arm = a, nSub = nSub, nObs = nObs, sec = median(tset),
        usPerObs = us, load = loadAvg())
      cat(sprintf("%s %s floor nSub=%d nObs=%d: %.4f s = %.3f us/obs (load %.2f)\n",
                  cfg, a, nSub, nObs, median(tset), us, loadAvg()))
    }
  }
  res <- do.call(rbind, rows)
  fn <- sprintf("three_arm_floor_%s.rds", cfg)
} else if (mode == "solve") {
  ## ---- solve-level sweep ---------------------------------------------------
  # time each arm's inner model through rxSolve on shared event grids.
  # params: the inner models share the THETA_/ETA_ naming; take them from the
  # posthoc fit so every arm evaluates at the same point.
  cells <- expand.grid(nSub = c(40L, 400L), nObs = c(10L, 200L, 1000L),
                       nDose = 1L)
  cells <- rbind(cells, data.frame(nSub = 40L, nObs = 50L, nDose = 100L))
  rows <- list()
  for (ci in seq_len(nrow(cells))) {
    nSub <- cells$nSub[ci]; nObs <- cells$nObs[ci]; nDose <- cells$nDose[ci]
    obsT <- sort(unique(round(exp(seq(log(0.25), log(32), length.out = nObs)), 4)))
    doseT <- if (nDose > 1L) seq(0, 12.8, length.out = nDose) else 0
    ev <- rxode2::et(amt = 100, time = doseT, cmt = 1) |> rxode2::et(obsT)
    for (a in names(arms)) {
      # matExp solves scale badly (2cmt 400x1000 took 126 s vs ode 0.30 s);
      # cap its biggest cells so a config fits one foreground window.  The
      # skipped cells are strictly worse for matExp than the ones kept.
      if (a == "matExp" && Sys.getenv("CAP_MATEXP", "1") == "1" &&
          nSub * nObs > 40L * 1000L) {
        rows[[length(rows) + 1L]] <- data.frame(
          cfg = cfg, arm = a, nSub = nSub, nObs = nObs, nDose = nDose,
          sec = NA_real_, load = loadAvg())
        cat(sprintf("%s %s nSub=%d nObs=%d nDose=%d: SKIPPED (cap)\n",
                    cfg, a, nSub, nObs, nDose))
        next
      }
      im <- inner[[a]]
      pn <- rxode2::rxModelVars(im)$params
      pars <- setNames(as.data.frame(matrix(0.05, nSub, length(pn))), pn)
      thetaCols <- grepl("^THETA_", pn)
      f <- innerFit[[a]]
      th <- f$env$fullTheta
      if (sum(thetaCols) == length(th)) pars[, thetaCols] <- rep(th, each = nSub)
      xtr <- list()
      if (a == "matExp") xtr$method <- "indLin"
      if (a == "ode") { xtr$atol <- 1e-8; xtr$rtol <- 1e-8 }
      tset <- numeric(nRep)
      for (r in seq_len(nRep)) {
        t0 <- proc.time()[["elapsed"]]
        s <- do.call(rxode2::rxSolve,
                     c(list(im, pars, ev, cores = 1L, addDosing = FALSE,
                            useLinCmt = FALSE), xtr))
        tset[r] <- proc.time()[["elapsed"]] - t0
        stopifnot(nrow(s) > 0)
      }
      rows[[length(rows) + 1L]] <- data.frame(
        cfg = cfg, arm = a, nSub = nSub, nObs = nObs, nDose = nDose,
        sec = median(tset), load = loadAvg())
      cat(sprintf("%s %s nSub=%d nObs=%d nDose=%d: %.4f s (load %.2f)\n",
                  cfg, a, nSub, nObs, nDose, median(tset), loadAvg()))
    }
  }
  res <- do.call(rbind, rows)
  fn <- sprintf("three_arm_solve_%s.rds", cfg)
} else {
  ## ---- fit-level -----------------------------------------------------------
  rows <- list()
  fitArms <- names(arms)
  # a 2/3-cmt matExp FIT is infeasible in one window: its inner solve is
  # already 30-400x slower than the other arms (see three_arm_solve_*), and a
  # focei fit multiplies that by hundreds of evaluations.  The solve-level
  # data carries the verdict; keep the matExp fit arm for 1cmt only.
  if (cfg != "1cmt" && Sys.getenv("FIT_MATEXP", "0") != "1") {
    fitArms <- setdiff(fitArms, "matExp")
  }
  for (a in fitArms) {
    warm <- fitOne(arms[[a]], armDat(a, datSmall))  # warm (compile) -- untimed
    for (r in seq_len(nRep)) {
      z <- fitOne(arms[[a]], armDat(a, datSmall))
      rows[[length(rows) + 1L]] <- data.frame(
        cfg = cfg, arm = a, size = "40x10", rep = r, sec = z$sec,
        objf = z$fit$objective, load = loadAvg())
      cat(sprintf("%s %s 40x10 rep %d: %.2f s objf %.4f\n", cfg, a, r, z$sec,
                  z$fit$objective))
    }
  }
  if (cfg == "2cmt") {
    datBig <- mkDat(40L, 100L)
    for (a in fitArms) {
      z <- fitOne(arms[[a]], armDat(a, datBig))
      rows[[length(rows) + 1L]] <- data.frame(
        cfg = cfg, arm = a, size = "40x100", rep = 1L, sec = z$sec,
        objf = z$fit$objective, load = loadAvg())
      cat(sprintf("%s %s 40x100: %.2f s objf %.4f\n", cfg, a, z$sec,
                  z$fit$objective))
    }
  }
  res <- do.call(rbind, rows)
  fn <- sprintf("three_arm_fit_%s.rds", cfg)
}

attr(res, "provenance") <- list(
  date = format(Sys.time()), config = cfg, mode = mode, reps = nRep, core = core,
  posthocObjf = objs, etaMax = etaMax,
  sensType = "AD (forward) forced; matExp arm method=indLin",
  rxode2 = system("git -C ~/src/rxode2-lincmt-carry-jump rev-parse --short HEAD", intern = TRUE),
  nlmixr2est = system("git -C ~/src/nlmixr2est-matexp-bench rev-parse --short HEAD", intern = TRUE))
dir.create(outDir, showWarnings = FALSE)
saveRDS(res, file.path(outDir, fn))
cat("saved", fn, "\n")
