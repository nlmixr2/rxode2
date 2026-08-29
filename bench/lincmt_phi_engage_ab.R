# Transition-matrix propagation under the engage rule: what it buys where
# row intervals repeat, and -- the non-negotiable half -- that it costs
# nothing where they do not.
#
# TREE=<path> selects the build.  The baseline tree (51d740c6d) carries the
# same code but never engages it; this tree engages it under the rule, so
# the pair measures both the gain and the regression gate on the paths that
# decline (never-repeating intervals, infusion rate rows, steady state).
#
# Usage: TREE=<path> Rscript bench/lincmt_phi_engage_ab.R
suppressMessages(devtools::load_all(Sys.getenv("TREE", "."), compile = FALSE,
                                    quiet = TRUE))
rxode2::setRxThreads(1L)
TREE <- basename(normalizePath(Sys.getenv("TREE", ".")))
REPS <- as.integer(Sys.getenv("REPS", "5"))

loadAvg <- function() as.numeric(strsplit(readLines("/proc/loadavg")[1], " ")[[1]][1])

gradModel <- function(ncmt, oral0, dirs) {
  args <- sprintf("rx__PTR__, t, 1, %d, %d, %%d, %%d, 1, p1, v1, p2, p3, p4, p5, ka",
                  ncmt, oral0)
  suppressWarnings(rxode2::rxode2(paste(c(
    sprintf("cp=linCmtB(%s)", sprintf(args, -1L, -1L)),
    vapply(dirs, function(k) sprintf("d%d=linCmtB(%s)", k, sprintf(args, -2L, k)), "")),
    collapse = "\n")))
}

P2 <- c(p1=2.1, v1=21, p2=3.3, p3=43, p4=0,   p5=0,  ka=1.3)
P3 <- c(p1=2.1, v1=21, p2=3.3, p3=43, p4=0.9, p5=61, ka=1.3)
P1 <- c(p1=2.1, v1=21, p2=0,   p3=0,  p4=0,   p5=0,  ka=1.3)

NOBS <- 1000L
mkEv <- list(
  uniform  = function() rxode2::et(rxode2::et(amt=100, time=0, cmt=1),
                                   seq(0.05, 0.05*NOBS, by=0.05)),
  multi    = function() rxode2::et(rxode2::et(amt=100, time=0, cmt=1, ii=12, addl=9),
                                   seq(0.12, 0.12*NOBS, by=0.12)),
  nonunif  = function() rxode2::et(rxode2::et(amt=100, time=0, cmt=1),
                                   cumsum(seq(0.02, 0.4, length.out=NOBS))),
  infusion = function() rxode2::et(rxode2::et(amt=100, time=0, cmt=1, rate=2),
                                   seq(0.05, 0.05*NOBS, by=0.05)),
  ss       = function() rxode2::et(rxode2::et(amt=100, time=0, cmt=1, ii=12, ss=1),
                                   seq(0.05, 0.05*NOBS, by=0.05))
)
mods <- list(
  `1cmt` = list(m=gradModel(1L, 1L, 0:2), p=P1),
  `2cmt` = list(m=gradModel(2L, 1L, 0:4), p=P2),
  `3cmt` = list(m=gradModel(3L, 1L, 0:6), p=P3)
)

NSUB <- 40L
res <- list()
for (cfg in names(mods)) {
  for (rn in names(mkEv)) {
    ev <- mkEv[[rn]]()
    ev <- do.call(rbind, lapply(seq_len(NSUB), function(i) {
      d <- as.data.frame(ev); d$id <- i; d
    }))
    md <- mods[[cfg]]
    invisible(rxode2::rxSolve(md$m, md$p, ev, cores=1L, addDosing=FALSE,
                              linCmtSensType="AD"))         # warm
    rxode2::linCmtSeqStats(TRUE)
    tv <- numeric(REPS); ld <- numeric(REPS)
    for (r in seq_len(REPS)) {
      ld[r] <- loadAvg()
      t0 <- proc.time()[["elapsed"]]
      invisible(rxode2::rxSolve(md$m, md$p, ev, cores=1L, addDosing=FALSE,
                                linCmtSensType="AD"))
      tv[r] <- proc.time()[["elapsed"]] - t0
    }
    st <- rxode2::linCmtSeqStats(TRUE)
    nrows <- NSUB*NOBS
    res[[length(res)+1L]] <- data.frame(
      tree=TREE, cfg=cfg, regimen=rn, sec=median(tv),
      usPerObs=1e6*median(tv)/nrows,
      phiRows=unname(st[["phiRows"]]), phiBuild=unname(st[["phiBuild"]]),
      tailRows=unname(st[["seqTailRows"]]), load=median(ld),
      stringsAsFactors=FALSE)
  }
}
res <- do.call(rbind, res)
saveRDS(res, sprintf("bench/results/phi_engage_ab_%s.rds", TREE))
print(res, row.names=FALSE)
