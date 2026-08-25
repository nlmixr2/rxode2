# The 40x100 FOCEi fit cell (the dense-design inversion): linCmt arm on the
# RXTREE build vs the ODE arm; part of the quiet-machine A/B.
suppressMessages({
  devtools::load_all(Sys.getenv("RXTREE", "~/src/rxode2-lincmt-carry-jump"),
                     compile = FALSE, quiet = TRUE)
  devtools::load_all("~/src/nlmixr2est-matexp-bench", helpers = FALSE, quiet = TRUE)
})
rxode2::setRxThreads(1L)
arm <- Sys.getenv("ARM", "linCmt")
nRep <- as.integer(Sys.getenv("REPS", "3"))
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
ui <- if (arm == "ode") mkUi(odeLines, "central/v") else mkUi("", "linCmt()")
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
tset <- numeric(nRep); objf <- NA_real_
for (r in seq_len(nRep)) {
  t0 <- proc.time()[["elapsed"]]
  fit <- suppressWarnings(suppressMessages(
    nlmixr2est::nlmixr2(ui, dat, est = "focei", control = ctl)))
  tset[r] <- proc.time()[["elapsed"]] - t0
  objf <- fit$objective
}
cat(sprintf("FIT40x100 %s: %.2f s (median of %d), objf %.4f (load %.2f)\n",
            arm, median(tset), nRep, objf, loadAvg()))
