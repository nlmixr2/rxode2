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

suppressMessages({
  devtools::load_all(Sys.getenv("RXTREE", "~/src/rxode2-lincmt-carry-jump"),
                     compile = FALSE, quiet = TRUE)
  library(nlmixr2est)
})
rxode2::setRxThreads(1L)
loadAvg <- function() as.numeric(strsplit(readLines("/proc/loadavg"), " ")[[1]][1])

nSub <- 40L; nObs <- 100L
budget <- as.integer(Sys.getenv("MAXITER", "40"))
etaPars <- c("ka", "cl", "v", "q", "vp")
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
    control = foceiControl(calcTables = FALSE, print = 0L, covMethod = "",
                           maxOuterIterations = budget,
                           rxControl = rxControl(cores = 1L)))))
  wall <- proc.time()[["elapsed"]] - t0
  p <- linCmtSeqProf(); k <- linCmtSeqStats()
  fe <- tryCatch(fit$env$optReturn$feval, error = function(e) NA_real_)
  rows[[length(rows)+1L]] <- data.frame(
    nEta = nEta, feval = as.numeric(fe), sec = unname(fit$time$optimize),
    kernelRows = p[["rows"]], dirs = p[["phiDirs"]] + p[["tailDirs"]],
    secKernel = p[["secAll"]], load = loadAvg())
  cat(sprintf("nEta=%d  feval %s  %.2f s  kernel entries %s\n",
              nEta, format(fe), unname(fit$time$optimize), format(p[["rows"]])))
}
r <- do.call(rbind, rows)
obsRows <- nSub*nObs
r$entriesPerEval <- r$kernelRows / r$feval
r$innerSolvesPerSubj <- r$entriesPerEval / obsRows
r$usPerEvalPerDir <- 1e6*r$sec/r$feval/r$nEta/obsRows

cat("\n== kernel entries per objective evaluation vs eta count ==\n")
print(r[, c("nEta", "feval", "sec", "kernelRows", "entriesPerEval",
            "innerSolvesPerSubj", "usPerEvalPerDir")], row.names = FALSE, digits = 4)
b <- stats::coef(stats::lm(entriesPerEval ~ nEta, r))
cat(sprintf("\nentries/evaluation ~ %.0f + %.0f * nEta   (relative slope %.2f per eta)\n",
            b[1], b[2], b[2]/b[1]))
cat(sprintf("ratio 5 eta / 1 eta: entries %.2fx, time %.2fx\n",
            r$entriesPerEval[5]/r$entriesPerEval[1],
            (r$sec[5]/r$feval[5])/(r$sec[1]/r$feval[1])))
saveRDS(r, "bench/results/inner_scaling.rds")
cat("wrote bench/results/inner_scaling.rds\n")
