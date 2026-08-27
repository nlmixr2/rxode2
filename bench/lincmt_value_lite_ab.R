# CONTENTION NOTE: every wall-clock run of this script (2026-08-24) executed
# under load 2.4-6.4 from concurrent sessions (loads recorded per cell/fit);
# the solve and posthoc A/B showed no difference resolvable above that noise,
# and the objective was identical.  The mechanism evidence (linCmtSeqStats()
# valueLite counters; see test-lincmt-value-memo.R) is the primary evidence
# for the thin value path.
# A/B for the thin value path ("lite": dydt/calc_lhs consolidation).
# Baseline = 298b0ec4b (pre-lite) built in ~/src/rxode2-lite-base;
# after = this tree.  Pinned externally via taskset; load recorded.
# Usage: TREE=<path> OUT=<rds> Rscript bench/lincmt_value_lite_ab.R
tree <- Sys.getenv("TREE", ".")
out <- Sys.getenv("OUT", "bench/results/lincmt_value_lite_ab_after.rds")
mode <- Sys.getenv("MODE", "all")
suppressMessages(devtools::load_all(tree, quiet = TRUE, compile = FALSE))
suppressMessages(devtools::load_all("~/src/nlmixr2est",
                                    quiet = TRUE, helpers = FALSE))
rxode2::setRxThreads(1L)
loadAvg <- function() as.numeric(strsplit(readLines("/proc/loadavg"), " ")[[1]][1])
res <- list()
# --- solve cells: 2-cmt oral and 3-cmt oral sens models, uniform rich
solveCell <- function(ncmt, nSub, nObs) {
  oral0 <- 1L
  pn <- c("CL","V","Q","V2","Q2","V3","KA")
  m <- rxode2(sprintf(
    "cp = linCmtB(rx__PTR__, t, 0, %d, 1, -1, -1, 1, CL, V, Q, V2, Q2, V3, KA)
g1 = rx__sens_central_BY_p1/V
g2 = -(central)/((V)*(V))+(rx__sens_central_BY_v1)/(V)
g3 = rx__sens_central_BY_ka/V", ncmt))
  p <- c(CL=4, V=20, Q=6, V2=40, Q2=3, V3=90, KA=1.1)
  if (ncmt < 3) p[c("Q2","V3")] <- 0
  ev <- et(amt=100, ii=24, addl=3) |> et(seq(0.1, 96, length.out=nObs))
  ed <- as.data.frame(ev$get.EventTable())
  evm <- do.call(rbind, lapply(seq_len(nSub), function(i) { d <- ed; d$id <- i; d }))
  ts <- vapply(seq_len(as.integer(Sys.getenv("SREPS","3"))), function(r) {
    t0 <- proc.time()[["elapsed"]]
    invisible(rxSolve(m, p, evm, cores=1L, returnType="data.frame",
                      linCmtSensType="AD"))
    proc.time()[["elapsed"]] - t0
  }, 0)
  data.frame(cell=sprintf("%dcmt %dx%d", ncmt, nSub, nObs),
             usObs=median(ts)/(nSub*nObs)*1e6, load=loadAvg())
}
if (mode %in% c("all","solve")) res$solve <- rbind(solveCell(2L, 400L, 1000L), solveCell(2L, 40L, 200L),
                   solveCell(3L, 400L, 1000L), solveCell(3L, 40L, 200L))
if (mode %in% c("all","fit")) {
# --- fit cell: 2-cmt oral, 3 etas, 40x100 (protocol of the earlier fit A/Bs)
set.seed(1234)
mod <- function() {
  ini({ tka <- 0.45; tcl <- 1; tv <- 3.45; tq <- 1.2; tv2 <- 3.9
    eta.ka ~ 0.4; eta.cl ~ 0.3; eta.v ~ 0.2
    prop.sd <- 0.15 })
  model({ ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v <- exp(tv + eta.v)
    q <- exp(tq); v2 <- exp(tv2)
    linCmt() ~ prop(prop.sd) })
}
evF <- et(amt=100, ii=24, addl=3) |> et(seq(0.25, 96, length.out=100L)) |> et(id=1:40)
simP <- c(tka=0.45, tcl=1, tv=3.45, tq=1.2, tv2=3.9)
sim <- suppressWarnings(rxSolve(mod, evF, params=c(simP, prop.sd=0.15),
  omega=lotri::lotri(eta.ka ~ 0.4, eta.cl ~ 0.3, eta.v ~ 0.2), addDosing=TRUE, seed=42))
dat <- as.data.frame(sim)[, c("id","time","sim")]
names(dat)[3] <- "dv"
d0 <- as.data.frame(evF$get.EventTable()); d0 <- d0[d0$evid != 0, c("id","time","amt","evid","ii","addl")]
dat$amt <- NA; dat$evid <- 0; dat$ii <- 0; dat$addl <- 0
dat <- rbind(d0[, c("id","time","amt","evid","ii","addl")] |> transform(dv=NA),
             dat[, c("id","time","amt","evid","ii","addl","dv")])
dat <- dat[order(dat$id, dat$time, -dat$evid), ]
tf <- vapply(1:2, function(r) {
  t0 <- proc.time()[["elapsed"]]
  f <- suppressWarnings(suppressMessages(nlmixr2(mod, dat, est="focei",
    control=foceiControl(calcTables=FALSE, print=0L))))
  el <- proc.time()[["elapsed"]] - t0
  attr(el, "objf") <- f$objective
  el
}, 0)
res$fit <- data.frame(secWarm=tf[length(tf)], secAll=paste(round(tf,1),collapse="/"), load=loadAvg())
}
res$tree <- tree; res$time <- format(Sys.time())
dir.create("bench/results", showWarnings = FALSE)
saveRDS(res, out)
cat("tree:", tree, "\n"); if (!is.null(res$solve)) print(res$solve); if (!is.null(res$fit)) print(res$fit)
