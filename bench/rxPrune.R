## Benchmark rxPrune(): the C walker against the R walker (.rxPrune()).
##
##   Rscript bench/rxPrune.R           # loads installed rxode2
##   Rscript bench/rxPrune.R load_all  # loads the working tree
##
## Models come from tests/testthat/helper-rxPrune-models.R (the same set the
## equality test uses); needs 'microbenchmark'.  `walk` times only the if/else rewrite of the
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

# median milliseconds, the R and C calls interleaved
.time <- function(fr, fc, times) {
  .mb <- microbenchmark::microbenchmark(r = fr(), c = fc(), times = times)
  .med <- tapply(.mb$time, .mb$expr, stats::median) / 1e6
  c(r = .med[["r"]], c = .med[["c"]])
}

.models <- .rxPruneModelSet()
.res <- do.call(
  rbind,
  lapply(names(.models), function(.n) {
    .m <- .models[[.n]]
    .p <- .rxPruneParse(.m)
    stopifnot(identical(.pruneC(.p), .rxPruneWalkR(.p$expr, .p$strAssign)))
    .times <- max(10L, min(200L, as.integer(20000 / nchar(.m))))
    .walk <- .time(function() .rxPruneWalkR(.p$expr, .p$strAssign), function() .pruneC(.p), .times)
    .tot <- .time(function() .rxPruneR(.m), function() rxPrune(.m), .times)
    data.frame(
      model = .n,
      lines = length(strsplit(.m, "\n")[[1]]),
      walkR_ms = .walk[["r"]],
      walkC_ms = .walk[["c"]],
      walk_speedup = .walk[["r"]] / .walk[["c"]],
      totalR_ms = .tot[["r"]],
      totalC_ms = .tot[["c"]],
      total_speedup = .tot[["r"]] / .tot[["c"]]
    )
  })
)
options(width = 200)
print(format(.res, digits = 3), row.names = FALSE)
