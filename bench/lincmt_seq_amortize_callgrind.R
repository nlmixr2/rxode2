# HISTORICAL RECORD: this script exercises rxControl(linCmtSensStrategy=)
# and/or linCmtHybStats(), removed when the hybrid strategy was retired (the
# amortized sequential evaluator, linCmtSeqTailJac, subsumed it).  To re-run,
# check out the commit range 473c6c52c..6939902d8 of this branch; the saved
# bench/results/*.rds remain the evidence of record.
# Callgrind target for the sequential-kernel profile (phase 0 of the
# amortization project).  Run AFTER lincmt_seq_amortize_phase0.R:
#   valgrind --tool=callgrind --callgrind-out-file=/tmp/cg.lincmt.%p \
#     Rscript bench/lincmt_seq_amortize_callgrind.R
# Loads rxode2 only; the model compile happens before the hot region and
# in child processes callgrind does not trace.
suppressMessages(
  devtools::load_all("~/src/rxode2-lincmt-analytic", compile = FALSE, quiet = TRUE))
rxode2::setRxThreads(1L)
cfg <- Sys.getenv("CONFIG", "2cmt")
prep <- readRDS(path.expand(sprintf(
  "~/src/rxode2-lincmt-analytic/bench/results/phase0_prep_%s.rds", cfg)))
im <- rxode2::rxode2(prep$modelText)
obsT <- sort(unique(round(exp(seq(log(0.25), log(32), length.out = 1000L)), 4)))
ev <- rxode2::et(amt = 100, time = 0, cmt = 1) |> rxode2::et(obsT)
# one warm solve outside the measured region compiles/caches everything;
# linCmtSensType forced: this tree's auto still has the pre-#1280 rule.
# 10 hot solves give the sampler (perf) enough hits in the kernel.
strat <- Sys.getenv("STRAT", "sequential")
# MODEL=inner (linCmtB sens path, default) or MODEL=plain (linCmtA value
# path); NSOLVE hot solves after one warm solve, so an LD_PRELOAD malloc
# counter can difference two NSOLVE settings to get allocations per solve.
mdl <- Sys.getenv("MODEL", "inner")
nSolve <- as.integer(Sys.getenv("NSOLVE", "10"))
if (mdl == "plain") {
  pn <- names(prep$pars)
  thn <- c("ka", "cl", "v", "q", "vp", "q2", "vp2")
  thn <- thn[seq_len(c(`1cmt` = 3L, `2cmt` = 5L, `3cmt` = 7L)[[prep$cfg]])]
  im <- rxode2::rxode2(paste0("param(", paste(thn, collapse = ", "),
                              ")\ncp = linCmt()"))
  tt <- list(`1cmt` = c(1.2, 4, 30), `2cmt` = c(1.2, 4, 30, 8, 60),
             `3cmt` = c(1.2, 4, 30, 8, 60, 3, 200))[[prep$cfg]]
  prep$pars <- setNames(as.data.frame(t(replicate(nrow(prep$pars), tt))), thn)
}
s <- rxode2::rxSolve(im, prep$pars, ev, cores = 1L, addDosing = FALSE,
                     useLinCmt = FALSE, linCmtSensStrategy = strat,
                     linCmtSensType = "AD")
stopifnot(nrow(s) > 0)
if (nSolve > 0) for (r in seq_len(nSolve)) {
  s <- rxode2::rxSolve(im, prep$pars, ev, cores = 1L, addDosing = FALSE,
                       useLinCmt = FALSE, linCmtSensStrategy = strat,
                       linCmtSensType = "AD")
  stopifnot(nrow(s) > 0)
}
cat("callgrind target done\n")
