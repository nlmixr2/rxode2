# lincmt_inner_scaling.R -- does the cost of an extra eta come from the
# sensitivity solve, or from the inner problem re-solving more often?
#
# WHY.  The gradient-slope sweep measures a per-direction slope of about
# 3.03 us per observation in a fit.  The sequential kernel is entered ONCE
# PER ROW whatever the direction count, and its per-direction arithmetic is
# 0.020 us (transition matrix) to 0.133 us (tail) -- 23 to 150 times less.
# So the slope is not per-direction solve work.  The candidate already on
# record as that sweep's binding limitation is that a higher-dimensional
# inner problem takes more inner iterations, each re-solving the subject.
#
# THE TEST.  Count kernel entries per objective evaluation against the eta
# count, everything else fixed.  Entries per evaluation is
#   (rows per subject) x (subjects) x (inner solves per subject),
# so if the first two are constant and entries/evaluation grows with eta
# count, the growth IS inner re-solving and the sweep's slope is mostly
# that rather than sensitivity arithmetic.
#
#   RXODE2_LINCMT_PROF=1 taskset -c <idle> Rscript bench/lincmt_inner_scaling.R
#
# Optimized build, loaded without recompiling, pinned, single thread.

## NMLIB points at a library holding an INSTALLED nlmixr2est; empty uses
## the default library.  INNEROPT selects the inner optimizer.  Running
## both optimizers against ONE installed build is the comparison that
## isolates them -- same code, same flags, one knob.
##
## INSTALL, DO NOT load_all().  devtools::load_all() builds at -O0, and
## `compile = FALSE` merely loads whatever .so is already there, whose
## provenance is then unknown.  This is not only a timing hazard: -O0
## changes floating-point contraction and precision, which moves where
## the inner convergence test falls, which changes HOW MANY TIMES the
## inner problem iterates.  An earlier version of this header claimed
## counts were build-independent and used that to compare across trees.
## They are not.  Measured: an -O0 tree matched an installed one exactly
## at one and two random effects and then jumped 3.1x at three -- a
## convergence criterion missed at higher dimension, not a uniform
## slowdown.  Nothing here is comparable across builds unless both were
## built the same way, counts included.
NMLIB    <- Sys.getenv("NMLIB", "")
INNEROPT <- Sys.getenv("INNEROPT", "")
if (nzchar(NMLIB)) .libPaths(c(NMLIB, .libPaths()))
suppressMessages({
  devtools::load_all(Sys.getenv("RXTREE", "~/src/rxode2-lincmt-carry-jump"),
                     compile = FALSE, quiet = TRUE)
  library(nlmixr2est)
})
cat(sprintf("nlmixr2est %s from %s\n",
            as.character(utils::packageVersion("nlmixr2est")),
            dirname(system.file(package = "nlmixr2est"))))

rxode2::setRxThreads(1L)
loadAvg <- function() as.numeric(strsplit(readLines("/proc/loadavg"), " ")[[1]][1])

nSub <- 40L; nObs <- 100L
budget <- as.integer(Sys.getenv("MAXITER", "40"))
## Which parameters carry the random effects, in order.  The default puts
## v third, so "dimension reaches 3" and "an eta lands on v" are confounded
## at exactly the cell where the jump appears.  ETAORDER separates them.
etaPars <- strsplit(Sys.getenv("ETAORDER", "ka,cl,v,q,vp"), ",")[[1]]
stopifnot(all(etaPars %in% c("ka", "cl", "v", "q", "vp")))
trueTheta <- c(ka = 1.2, cl = 4, v = 30, q = 8, vp = 60)

set.seed(1002003)
obsT <- sort(unique(round(exp(seq(log(0.25), log(32), length.out = nObs)), 3)))
simMod <- rxode2::rxode2("cp = central/v
d/dt(depot) <- -ka*depot
d/dt(central) <- ka*depot - cl/v*central - q/v*central + q/vp*periph
d/dt(periph) <- q/v*central - q/vp*periph")
eta <- matrix(rnorm(nSub*3, 0, 0.3), nSub, 3,
              dimnames = list(NULL, c("ka", "cl", "v")))
pars <- data.frame(row.names = seq_len(nSub))
for (p in names(trueTheta))
  pars[[p]] <- if (p %in% colnames(eta)) trueTheta[[p]]*exp(eta[, p]) else trueTheta[[p]]
ev <- rxode2::et(amt = 100, time = 0, cmt = "depot") |> rxode2::et(obsT)
sim <- rxode2::rxSolve(simMod, pars, ev, cores = 1L, addDosing = FALSE, useLinCmt = FALSE)
set.seed(2003004)
dat <- rbind(
  data.frame(ID = rep(seq_len(nSub), each = length(obsT)), TIME = sim$time,
             DV = sim$cp*(1 + rnorm(nrow(sim), 0, 0.15)), AMT = 0, EVID = 0,
             CMT = "central"),
  data.frame(ID = seq_len(nSub), TIME = 0, DV = NA_real_, AMT = 100, EVID = 1,
             CMT = "depot"))
dat <- dat[order(dat$ID, dat$TIME, -dat$EVID), ]

mkUi <- function(nEta) {
  ini <- vapply(names(trueTheta), function(p)
    sprintf("l%s <- %.9g", p, log(unname(trueTheta[[p]]))), character(1))
  om <- vapply(seq_along(etaPars), function(j)
    sprintf("eta.%s ~ fix(%s)", etaPars[j], if (j <= nEta) "0.1" else "0"), character(1))
  par <- vapply(names(trueTheta), function(p) {
    e <- if (p %in% etaPars) sprintf("*exp(eta.%s)", p) else ""
    sprintf("%s <- exp(l%s)%s", p, p, e) }, character(1))
  eval(parse(text = sprintf(paste0(
    "function() {\n ini({\n%s\n%s\n prop.sd <- fix(0.2)\n })\n",
    " model({\n%s\n cp <- linCmt()\n cp ~ prop(prop.sd)\n })\n}"),
    paste(ini, collapse = "\n"), paste(om, collapse = "\n"),
    paste(par, collapse = "\n"))))
}

rows <- list()
for (nEta in 1:5) {
  invisible(linCmtSeqProf(reset = TRUE)); invisible(linCmtSeqStats(reset = TRUE))
  t0 <- proc.time()[["elapsed"]]
  fit <- suppressWarnings(suppressMessages(nlmixr2(
    mkUi(nEta), dat, est = "focei",
    control = do.call(foceiControl, c(
      list(calcTables = FALSE, print = 0L, covMethod = "",
           maxOuterIterations = budget, rxControl = rxControl(cores = 1L)),
      if (nzchar(INNEROPT)) list(innerOpt = INNEROPT) else list())))))
  wall <- proc.time()[["elapsed"]] - t0
  p <- linCmtSeqProf(); k <- linCmtSeqStats()
  fe <- tryCatch(fit$env$optReturn$feval, error = function(e) NA_real_)
  rows[[length(rows)+1L]] <- data.frame(
    nEta = nEta, etas = paste(etaPars[seq_len(nEta)], collapse="+"),
    feval = as.numeric(fe), sec = unname(fit$time$optimize),
    kernelRows = p[["rows"]], dirs = p[["phiDirs"]] + p[["tailDirs"]],
    secKernel = p[["secAll"]], load = loadAvg())
  cat(sprintf("[%s] nEta=%d  feval %s  %.2f s  kernel entries %s\n",
              if (nzchar(INNEROPT)) INNEROPT else "default", nEta, format(fe),
              unname(fit$time$optimize), format(p[["rows"]])))
}
r <- do.call(rbind, rows)
obsRows <- nSub*nObs
r$entriesPerEval <- r$kernelRows / r$feval
r$innerSolvesPerSubj <- r$entriesPerEval / obsRows
r$usPerEvalPerDir <- 1e6*r$sec/r$feval/r$nEta/obsRows

cat("\n== kernel entries per objective evaluation vs eta count ==\n")
print(r[, c("nEta", "etas", "feval", "sec", "kernelRows", "entriesPerEval",
            "innerSolvesPerSubj", "usPerEvalPerDir")], row.names = FALSE, digits = 4)
b <- stats::coef(stats::lm(entriesPerEval ~ nEta, r))
cat(sprintf("\nentries/evaluation ~ %.0f + %.0f * nEta   (relative slope %.2f per eta)\n",
            b[1], b[2], b[2]/b[1]))
cat(sprintf("ratio 5 eta / 1 eta: entries %.2fx, time %.2fx\n",
            r$entriesPerEval[5]/r$entriesPerEval[1],
            (r$sec[5]/r$feval[5])/(r$sec[1]/r$feval[1])))
tag <- paste0(if (nzchar(INNEROPT)) INNEROPT else "default",
              if (nzchar(Sys.getenv("ETATAG"))) paste0("_", Sys.getenv("ETATAG")) else "")
attr(r, "arm") <- list(nmLib = NMLIB, innerOpt = tag, budget = budget,
                       nmVersion = as.character(utils::packageVersion("nlmixr2est")),
                       nmPath = dirname(system.file(package = "nlmixr2est")))
saveRDS(r, sprintf("bench/results/inner_scaling_%s.rds", tag))
cat(sprintf("wrote bench/results/inner_scaling_%s.rds\n", tag))
