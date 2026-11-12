## Time rxFromSE() on the symengine text nlmixr2est translates while building
## the foceiControl(fast = TRUE) outer model of issue #1682.
##
##   Rscript bench/issue1682-symengine.R <library> [fit]
##
## <library> is a library holding the rxode2 build to time (install origin/main
## into one and the candidate into another; never time a load_all build, which
## compiles at -O0).  The corpora are recorded by bench/issue1682-corpus.R.
##
## For each corpus: rxFromSE() called once per line, as nlmixr2est's
## .foceiAnalyticAugModelDirs() calls it, cold (empty translation cache) and
## then again warm.  With `fit`, also the whole nlmixr2() setup for the
## P = 1..3, K = 3 log cases (needs nlmixr2est; slow on the baseline).

.args <- commandArgs(trailingOnly = TRUE)
.lib <- .args[1]
.fit <- length(.args) >= 2L && .args[2] == "fit"
.libPaths(c(.lib, .libPaths()))
suppressMessages(library(rxode2, lib.loc = .lib))
rxode2::setRxThreads(1L)
.ver <- as.character(utils::packageVersion("rxode2", lib.loc = .lib))
.hasCache <- exists("rxSymEngineCacheReset", envir = asNamespace("rxode2"))
.reset <- function() {
  if (.hasCache) utils::getFromNamespace("rxSymEngineCacheReset", "rxode2")()
}
.stats <- function() {
  if (!.hasCache) {
    return(NULL)
  }
  .Call(utils::getFromNamespace("_rxode2_seStatsR", "rxode2"), TRUE)
}

## nlmixr2est hands rxFromSE() a symbol bound to the text; do the same
.toRx <- function(.l) {
  force(.l)
  rxode2::rxFromSE(.l)
}

.files <- sort(Sys.glob("bench/issue1682-corpus-P*K*-*.rds"))
cat("== rxFromSE() over the #1682 corpora ==\n")
cat("rxode2", .ver, "from", .lib, "\n")
cat(sprintf(
  "%-26s %6s %8s %10s %10s %8s\n",
  "corpus",
  "lines",
  "MB",
  "cold (s)",
  "warm (s)",
  "C share"
))
for (.f in .files) {
  .x <- readRDS(.f)
  .reset()
  .stats()
  gc(FALSE)
  .cold <- system.time(.out <- vapply(.x, .toRx, character(1), USE.NAMES = FALSE))[["elapsed"]]
  .st <- .stats()
  .warm <- system.time(vapply(.x, .toRx, character(1), USE.NAMES = FALSE))[["elapsed"]]
  .share <- if (is.null(.st)) NA_real_ else .st$fromSE[1] / sum(.st$fromSE)
  cat(sprintf(
    "%-26s %6d %8.2f %10.2f %10.2f %8s\n",
    basename(.f),
    length(.x),
    sum(nchar(.x)) / 1e6,
    .cold,
    .warm,
    if (is.na(.share)) "-" else sprintf("%.0f%%", 100 * .share)
  ))
  saveRDS(.out, file.path(tempdir(), paste0("out-", basename(.f))))
}

if (.fit) {
  suppressMessages(library(nlmixr2est))
  source("bench/issue1682-model.R")
  cat("\n== nlmixr2() setup, foceiControl(fast = TRUE, maxOuterIterations = 0) ==\n")
  cat(sprintf("%-10s %10s %14s\n", "case", "setup (s)", "in rxFromSE (s)"))
  for (.P in 1:3) {
    .x <- .fastCase(.P, 3L, "log")
    .ctl <- foceiControl(
      print = 0,
      maxOuterIterations = 0,
      covMethod = "",
      calcTables = FALSE,
      fast = TRUE,
      rxControl = rxode2::rxControl(cores = 1L)
    )
    .reset()
    .prof <- tempfile()
    Rprof(.prof, interval = 0.02, event = "elapsed")
    .t <- system.time(suppressMessages(nlmixr2(.x$model, .x$data, "focei", .ctl)))[["elapsed"]]
    Rprof(NULL)
    .p <- summaryRprof(.prof)$by.total
    .tt <- stats::setNames(.p$total.time, gsub("\"", "", rownames(.p)))
    .fs <- max(c(0, .tt[intersect(c("rxode2::rxFromSE", "rxFromSE"), names(.tt))]))
    cat(sprintf("%-10s %10.1f %14.1f\n", sprintf("P=%d K=3", .P), .t, .fs))
  }
}
