# Value-only (linCmtA) floor per tree: RXTREE env picks the build.
# Part of the quiet-machine A/B for the sensitivity-amortization project.
#
# CORRECTION 2026-08-24: the floors saved inside seq_amortize_ab.rds
# (2.4-3.4 us/obs) were measured in the contended/cold window and do NOT
# reproduce -- this script re-run on the identical HEAD tree on a quiet
# machine gives ~0.25-0.63 us/obs (warm reps ~0.25), agreeing with the
# three-arm floors (bench/results/three_arm_floor_*.rds, 0.28-0.45).
# The earlier version also recorded no per-rep load, which is how the
# contamination went unnoticed; every rep now carries load, and the first
# rep (model build + first solve) is reported separately from the warm
# median.  Use the WARM median as "the linCmt value floor" for a plain
# rxode2 value solve; results are saved to
# bench/results/seq_amortize_floor_requiet.rds by RUN=save.
suppressMessages(devtools::load_all(Sys.getenv("RXTREE", "~/src/rxode2-lincmt-analytic"),
                                    compile = FALSE, quiet = TRUE))
rxode2::setRxThreads(1L)
cfg <- Sys.getenv("CONFIG", "2cmt")
nRep <- as.integer(Sys.getenv("REPS", "7"))
loadAvg <- function() as.numeric(strsplit(readLines("/proc/loadavg"), " ")[[1]][1])
thn <- list(`1cmt` = c("ka","cl","v"), `2cmt` = c("ka","cl","v","q","vp"),
            `3cmt` = c("ka","cl","v","q","vp","q2","vp2"))[[cfg]]
tt <- list(`1cmt` = c(1.2,4,30), `2cmt` = c(1.2,4,30,8,60),
           `3cmt` = c(1.2,4,30,8,60,3,200))[[cfg]]
m <- rxode2::rxode2(paste0("param(", paste(thn, collapse=", "), ")\ncp = linCmt()"))
obsT <- sort(unique(round(exp(seq(log(0.25), log(32), length.out = 1000L)), 4)))
ev <- rxode2::et(amt = 100, time = 0, cmt = 1) |> rxode2::et(obsT)
pars <- setNames(as.data.frame(t(replicate(400L, tt))), thn)
tset <- numeric(nRep); lset <- numeric(nRep)
for (r in seq_len(nRep)) {
  t0 <- proc.time()[["elapsed"]]
  s <- rxode2::rxSolve(m, pars, ev, cores = 1L, addDosing = FALSE)
  tset[r] <- proc.time()[["elapsed"]] - t0
  lset[r] <- loadAvg()
  stopifnot(nrow(s) > 0)
}
warm <- tset[-1]
cat(sprintf("FLOOR %s 400x1000: first %.4f s; warm median %.4f s = %.3f us/obs (loads %s)\n",
            cfg, tset[1], median(warm), median(warm)/(400*1000)*1e6,
            paste(sprintf("%.2f", lset), collapse=",")))
if (nzchar(Sys.getenv("RUN"))) {
  fn <- "~/src/rxode2-lincmt-analytic/bench/results/seq_amortize_floor_requiet.rds"
  old <- if (file.exists(fn)) readRDS(fn) else NULL
  row <- data.frame(cfg = cfg, tree = Sys.getenv("RXTREE", "HEAD"),
                    firstSec = tset[1], warmMedianSec = median(warm),
                    usPerObs = median(warm)/(400*1000)*1e6,
                    reps = nRep, maxLoad = max(lset), date = as.character(Sys.Date()))
  saveRDS(rbind(old, row), fn)
}
