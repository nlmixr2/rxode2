## Fit-path check for the transition-matrix engage rule.
##
## The rule is that a matrix is assembled only on evidence that a row's
## interval RECURS in the design, so a design whose gaps never repeat must
## build none at all.  That held in rxSolve() but not in a fit: one row
## reaches the row-Jacobian several times there (the generated model runs
## the value line from dydt and calc_lhs, and the inner problem re-walks a
## subject many times), and each of those executions looked its own gap up
## again.  Counting a re-execution as reuse made every design look regular
## -- a strictly non-repeating design built 8.6M matrices at 4:1 "reuse".
##
## Why this lives in bench/ rather than tests/testthat/: a plain solve
## queries each row exactly once (expHit == 0 below), so the fault is not
## reachable from rxode2's own test suite -- reproducing it needs a fit,
## and rxode2 does not depend on nlmixr2est.  The definitive regression
## test belongs in nlmixr2est's suite; this script is the reproducer.
##
## Build: the ordinary optimized path (pkgbuild::compile_dll(debug=FALSE)
## then load_all(compile=FALSE)); no optimization flags are forced.
## Usage: Rscript bench/lincmt_phi_fit_engage_check.R
suppressMessages(devtools::load_all("~/src/rxode2-lincmt-carry-jump", quiet = TRUE,
                                    compile = FALSE))
suppressMessages(devtools::load_all("~/src/nlmixr2est-lincmt-speed", quiet = TRUE,
                                    helpers = FALSE))
rxode2::setRxThreads(1L)

nSub <- 8L
nObs <- 40L
trueTheta <- c(ka = 1.2, cl = 4, v = 30, q = 8, vp = 60)
odeLines <- paste0("d/dt(depot) = -ka*depot;",
                   "d/dt(central) = ka*depot - cl/v*central - q/v*central + q/vp*periph;",
                   "d/dt(periph) = q/v*central - q/vp*periph")

uiLin <- function() {
  ini({
    lka <- log(1.44); lcl <- log(4.8); lv <- log(36)
    lq <- log(9.6); lvp <- log(72)
    eta.ka ~ 0.1; eta.cl ~ 0.1; eta.v ~ 0.1
    prop.sd <- 0.2
  })
  model({
    ka <- exp(lka)*exp(eta.ka)
    cl <- exp(lcl)*exp(eta.cl)
    v <- exp(lv)*exp(eta.v)
    q <- exp(lq); vp <- exp(lvp)
    cp <- linCmt()
    cp ~ prop(prop.sd)
  })
}

simData <- function(simTimes) {
  set.seed(1002003)
  eta <- matrix(rnorm(nSub*3L, 0, 0.3), nSub, 3L,
                dimnames = list(NULL, c("ka", "cl", "v")))
  simMod <- rxode2::rxode2(paste0("cp = central/v;", odeLines))
  pars <- data.frame(row.names = seq_len(nSub))
  for (p in names(trueTheta)) {
    pars[[p]] <- if (p %in% colnames(eta)) trueTheta[[p]]*exp(eta[, p]) else trueTheta[[p]]
  }
  ev <- rxode2::et(amt = 100, cmt = "depot") |> rxode2::et(simTimes)
  sim <- rxode2::rxSolve(simMod, pars, ev, cores = 1L, addDosing = FALSE)
  set.seed(2003004)
  obs <- data.frame(ID = rep(seq_len(nSub), each = length(simTimes)),
                    TIME = sim$time, DV = sim$cp*(1 + rnorm(nrow(sim), 0, 0.15)),
                    AMT = 0, EVID = 0, CMT = "central")
  dose <- data.frame(ID = seq_len(nSub), TIME = 0, DV = NA_real_,
                     AMT = 100, EVID = 1, CMT = "depot")
  d <- rbind(dose, obs)
  d[order(d$ID, d$TIME, -d$EVID), ]
}

fitStats <- function(simTimes) {
  dat <- simData(simTimes)
  invisible(rxode2:::linCmtSeqStats(TRUE))
  f <- nlmixr2est::nlmixr2(
    uiLin, dat, "focei",
    nlmixr2est::foceiControl(maxOuterIterations = 0L, print = 0L,
                             calcTables = FALSE, covMethod = "",
                             rxControl = rxode2::rxControl(
                               cores = 1L, linCmtSensType = "AD",
                               linCmtSensPhi = TRUE)))
  list(stats = rxode2:::linCmtSeqStats(TRUE), objf = f$objf)
}

## Gaps strictly increasing: no interval ever recurs.
nonUnif <- fitStats(cumsum(seq(0.30, 0.70, length.out = nObs)))
## One repeating gap: the regime the matrix exists for.
unif <- fitStats(seq(0.5, 50, length.out = nObs))

report <- function(tag, r) {
  s <- r$stats
  cat(sprintf("%-12s phiBuild=%-7d phiRows=%-8d reuse=%-6s objf=%.4f\n", tag,
              s[["phiBuild"]], s[["phiRows"]],
              if (s[["phiBuild"]] > 0) sprintf("%.1f", s[["phiRows"]]/s[["phiBuild"]]) else "-",
              r$objf))
}
report("nonuniform", nonUnif)
report("uniform", unif)

ok <- TRUE
if (nonUnif$stats[["phiBuild"]] != 0L) {
  cat("FAIL: a design whose intervals never repeat built a transition matrix\n")
  ok <- FALSE
}
if (unif$stats[["phiBuild"]] <= 0L ||
      unif$stats[["phiRows"]] < 5L*unif$stats[["phiBuild"]]) {
  cat("FAIL: a repeating design did not engage, or reuse collapsed\n")
  ok <- FALSE
}
cat(if (ok) "OK\n" else "CHECK FAILED\n")
quit(status = if (ok) 0L else 1L)
