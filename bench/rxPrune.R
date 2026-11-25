## Benchmark rxPrune(): the C walker against the R walker (.rxPrune()).
##
##   Rscript bench/rxPrune.R           # loads installed rxode2
##   Rscript bench/rxPrune.R load_all  # loads the working tree
##
## Models come from tests/testthat/helper-rxPrune-models.R (the same set the
## equality test uses).  `walk` times only the if/else rewrite of the
## already-parsed model; `total` adds rxModelVars()/rxNorm() and the R parse
## that both walkers share.

.args <- commandArgs(trailingOnly = TRUE)
if (length(.args) && .args[1] == "load_all") {
  suppressMessages(pkgload::load_all(".", quiet = TRUE))
} else {
  suppressMessages(library(rxode2))
}
.rxPruneParse <- utils::getFromNamespace(".rxPruneParse", "rxode2")
.rxPruneWalkR <- utils::getFromNamespace(".rxPruneWalkR", "rxode2")
.rxPruneR <- utils::getFromNamespace(".rxPruneR", "rxode2")
.pruneC <- function(p) {
  .Call(utils::getFromNamespace("_rxode2_rxPruneC", "rxode2"), p$expr, p$strAssign, TRUE)
}
source("tests/testthat/helper-rxPrune-models.R")

.time <- function(f, reps) {
  f()
  .t <- system.time(
    for (i in seq_len(reps)) {
      f()
    }
  )[["elapsed"]]
  .t / reps * 1000
}

.models <- .rxPruneModelSet()
.res <- do.call(
  rbind,
  lapply(names(.models), function(.n) {
    .m <- .models[[.n]]
    .p <- .rxPruneParse(.m)
    stopifnot(identical(.pruneC(.p), .rxPruneWalkR(.p$expr, .p$strAssign)))
    .reps <- max(3L, as.integer(2000 / nchar(.m)))
    .walkR <- .time(function() .rxPruneWalkR(.p$expr, .p$strAssign), .reps)
    .walkC <- .time(function() .pruneC(.p), .reps)
    .totR <- .time(function() .rxPruneR(.m), .reps)
    .totC <- .time(function() rxPrune(.m), .reps)
    data.frame(
      model = .n,
      lines = length(strsplit(.m, "\n")[[1]]),
      walkR_ms = .walkR,
      walkC_ms = .walkC,
      walk_speedup = .walkR / .walkC,
      totalR_ms = .totR,
      totalC_ms = .totC,
      total_speedup = .totR / .totC
    )
  })
)
print(format(.res, digits = 3), row.names = FALSE)
