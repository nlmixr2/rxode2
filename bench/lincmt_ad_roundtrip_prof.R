# lincmt_ad_roundtrip_prof.R -- Phase 0 of the AD round-trip plan.
#
# QUESTION.  Of the time spent evaluating one sequential sensitivity row,
# how much is work done ONCE PER ROW (the carry reconstruction the plan
# proposes to make persistent) and how much is work done PER DIRECTION?
# The plan's premise is that the per-row round trip dominates.  Reading
# the code suggests otherwise -- restoreJacTo and the Alast rebuild sit
# ABOVE the direction loop -- so this measures it.
#
#   RXODE2_LINCMT_PROF=1 taskset -c <idle> Rscript bench/lincmt_ad_roundtrip_prof.R
#
# Protocol: optimized build (compile_dll(debug = FALSE)), loaded WITHOUT
# recompiling, pinned, single-threaded, load recorded.  Never through a
# plain load_all -- that builds at -O0 and has produced a 49x artifact on
# this code before.

suppressMessages(devtools::load_all("~/src/rxode2-lincmt-carry-jump",
                                    compile = FALSE, quiet = TRUE))
setRxThreads(1L)
stopifnot(nzchar(Sys.getenv("RXODE2_LINCMT_PROF")))
loadAvg <- function() as.numeric(strsplit(readLines("/proc/loadavg"), " ")[[1]][1])

## The model has to REQUEST directions or the sequential sensitivity kernel
## is never entered.  Same construction the other benches in this directory
## use: one value call and one linCmtB direction call per parameter, which is
## what nlmixr2est's generated inner model emits.
gradModel <- function(ncmt, oral0, dirs) {
  args <- sprintf("rx__PTR__, t, 1, %d, %d, %%d, %%d, 1, cl, v, q, vp, q2, vp2, ka",
                  ncmt, oral0)
  lines <- c(sprintf("cp=linCmtB(%s)", sprintf(args, -1L, -1L)),
             vapply(dirs, function(k) sprintf("d%d=linCmtB(%s)",
                                              k, sprintf(args, -2L, k)), ""))
  suppressWarnings(rxode2(paste(lines, collapse = "\n")))
}
## 2-compartment oral, five directions -- the cell the NONMEM sweep used.
mod  <- gradModel(2L, 1L, 0:4)
pars <- c(cl = 4, v = 30, q = 8, vp = 60, q2 = 0, vp2 = 0, ka = 1.2)

## Two designs: a uniform grid, where the interval repeats and the
## transition matrix engages, and a log-spaced grid, where no interval
## repeats and every row takes the tail kernel.  The plan's target -- the
## per-row carry reconstruction -- is shared by both.
designs <- list(
  uniform = seq(0.5, 100, by = 0.5),
  nonUnif = sort(unique(round(exp(seq(log(0.25), log(100), length.out = 200)), 4))))

nSub <- 40L
out <- list()
for (nm in names(designs)) {
  ev <- et(amt = 100, cmt = 1) |> et(designs[[nm]]) |> et(id = seq_len(nSub))
  invisible(linCmtSeqProf(reset = TRUE)); invisible(linCmtSeqStats(reset = TRUE))
  t0 <- proc.time()[["elapsed"]]
  s <- rxSolve(mod, pars, ev, cores = 1L, addDosing = FALSE,
               linCmtSensType = "AD", returnType = "data.frame")
  wall <- proc.time()[["elapsed"]] - t0
  p <- linCmtSeqProf(); k <- linCmtSeqStats()
  out[[nm]] <- list(prof = p, stats = k, wall = wall, rows = nrow(s),
                    load = loadAvg())
}

fmt <- function(x) formatC(x, format = "f", digits = 3)
cat("\nPhase 0: where a sequential sensitivity row goes\n")
cat(sprintf("(%d subjects; profiler enabled = %s)\n\n", nSub,
            out[[1]]$prof[["enabled"]] == 1))
for (nm in names(out)) {
  o <- out[[nm]]; p <- o$prof
  tot <- p[["secAll"]]
  cat(sprintf("== %s design: %d solve rows, wall %.2f s, load %.2f\n",
              nm, o$rows, o$wall, o$load))
  cat(sprintf("   rows through the sequential kernel : %s\n", format(p[["rows"]])))
  cat(sprintf("   directions via transition matrix   : %s\n", format(p[["phiDirs"]])))
  cat(sprintf("   directions via tail kernel         : %s\n", format(p[["tailDirs"]])))
  if (tot > 0) {
    for (seg in c("secWinFill", "secRowShared", "secPhiDir", "secTailDir", "secOther")) {
      cat(sprintf("   %-14s %8s s  %5.1f%% of the kernel\n",
                  sub("^sec", "", seg), fmt(p[[seg]]), 100 * p[[seg]] / tot))
    }
    cat(sprintf("   %-14s %8s s  %5.1f%% of wall clock\n", "TOTAL kernel",
                fmt(tot), 100 * tot / o$wall))
    perRow <- 1e6 * p[["secRowShared"]] / max(p[["rows"]], 1)
    nd <- p[["phiDirs"]] + p[["tailDirs"]]
    perDir <- 1e6 * (p[["secPhiDir"]] + p[["secTailDir"]]) / max(nd, 1)
    cat(sprintf("   -> per-row shared %.3f us/row; per-direction %.3f us/dir\n",
                perRow, perDir))
  }
  cat("\n")
}
saveRDS(out, "bench/results/ad_roundtrip_prof.rds")
cat("wrote bench/results/ad_roundtrip_prof.rds\n")
