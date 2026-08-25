# Quiet-machine A/B for the last-row value memo (P4 of the dedup project,
# plans/snazzy-mapping-kettle.md).  RXTREE selects the rxode2 build:
#   baseline: RXTREE=~/src/rxode2-memo-base   (39aa7f58d, counters only)
#   memo:     RXTREE=~/src/rxode2-lincmt-carry-jump
# MODE=solve (default) times the sens-path cells on the saved inner-model
# text (bench/results/phase0_prep_<cfg>.rds); MODE=fit runs the 40x100
# FOCEi fit cell (loads nlmixr2est).  Optimized builds only; run pinned:
#   taskset -c <idle> Rscript bench/lincmt_value_memo_ab.R
suppressMessages(devtools::load_all(Sys.getenv("RXTREE", "~/src/rxode2-lincmt-carry-jump"),
                                    compile = FALSE, quiet = TRUE))
rxode2::setRxThreads(1L)
mode <- Sys.getenv("MODE", "solve")
cfg  <- Sys.getenv("CONFIG", "2cmt")
nRep <- as.integer(Sys.getenv("REPS", "3"))
outDir <- path.expand("~/src/rxode2-lincmt-carry-jump/bench/results")
loadAvg <- function() as.numeric(strsplit(readLines("/proc/loadavg"), " ")[[1]][1])
stopifnot(loadAvg() < as.numeric(Sys.getenv("MAXLOAD", "3")))
tag <- basename(path.expand(Sys.getenv("RXTREE", "~/src/rxode2-lincmt-carry-jump")))

if (mode == "solve") {
  prep <- readRDS(file.path(outDir, sprintf("phase0_prep_%s.rds", cfg)))
  im <- rxode2::rxode2(prep$modelText)
  cells <- list(c(400L, 1000L), c(40L, 200L))
  rows <- list()
  for (cell in cells) {
    nSub <- cell[1]; nObs <- cell[2]
    obsT <- sort(unique(round(exp(seq(log(0.25), log(32), length.out = nObs)), 4)))
    ev <- rxode2::et(amt = 100, time = 0, cmt = 1) |> rxode2::et(obsT)
    pars <- prep$pars[rep(seq_len(nrow(prep$pars)), length.out = nSub), , drop = FALSE]
    tset <- numeric(nRep)
    for (r in seq_len(nRep)) {
      t0 <- proc.time()[["elapsed"]]
      s <- rxode2::rxSolve(im, pars, ev, cores = 1L, addDosing = FALSE,
                           useLinCmt = FALSE, linCmtSensType = "AD")
      tset[r] <- proc.time()[["elapsed"]] - t0
    }
    us <- median(tset) / (nSub * nObs) * 1e6
    rows[[length(rows) + 1L]] <- data.frame(
      tree = tag, cfg = cfg, nSub = nSub, nObs = nObs,
      sec = median(tset), usPerObs = us, load = loadAvg())
    cat(sprintf("%s %s nSub=%d nObs=%d: %.4f s = %.3f us/obs (load %.2f)\n",
                tag, cfg, nSub, nObs, median(tset), us, loadAvg()))
  }
  res <- do.call(rbind, rows)
  f <- file.path(outDir, sprintf("value_memo_ab_%s_%s.rds", cfg, tag))
  attr(res, "provenance") <- list(script = "bench/lincmt_value_memo_ab.R",
                                  tree = tag, cfg = cfg,
                                  date = format(Sys.time()),
                                  build = "pkgbuild::compile_dll(debug=FALSE)")
  saveRDS(res, f)
} else {
  suppressMessages(devtools::load_all("~/src/nlmixr2est-matexp-bench",
                                      helpers = FALSE, quiet = TRUE))
  # same model family as the phase-0 fit; 40 subjects x 100 obs
  source(file.path(path.expand("~/src/rxode2-lincmt-carry-jump/bench"),
                   "lincmt_value_memo_fit_model.R"))
  tset <- numeric(nRep); obj <- NA_real_
  for (r in seq_len(nRep)) {
    t0 <- proc.time()[["elapsed"]]
    fit <- runMemoFitCell()
    tset[r] <- proc.time()[["elapsed"]] - t0
    obj <- fit$objective
  }
  cat(sprintf("%s fit 40x100: median %.2f s (reps %s) objf %.6f load %.2f\n",
              tag, median(tset), paste(round(tset, 2), collapse = "/"),
              obj, loadAvg()))
  saveRDS(data.frame(tree = tag, sec = median(tset), objf = obj,
                     load = loadAvg()),
          file.path(outDir, sprintf("value_memo_ab_fit_%s.rds", tag)))
}
cat("done\n")
