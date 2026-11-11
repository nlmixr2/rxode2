## Record the symengine strings nlmixr2est hands to rxFromSE() while building
## the foceiControl(fast = TRUE) outer model of issue #1682.
##
##   Rscript bench/issue1682-corpus.R P K [log|power]
##
## Writes bench/issue1682-corpus-P<P>K<K>-<form>.rds: a character vector, one
## element per rxFromSE() call, in call order.  bench/issue1682-symengine.R
## times rxFromSE() over it without needing nlmixr2est.
##
## Needs nlmixr2est.  The fit runs to the end of setup (maxOuterIterations = 0),
## so the large cases take as long as the issue reports.

.args <- commandArgs(trailingOnly = TRUE)
.P <- if (length(.args) >= 1L) as.integer(.args[1]) else 1L
.K <- if (length(.args) >= 2L) as.integer(.args[2]) else 1L
.form <- if (length(.args) >= 3L) .args[3] else "log"

suppressMessages(library(nlmixr2est))
rxode2::setRxThreads(1L)

source("bench/issue1682-model.R")

.corpus <- new.env(parent = emptyenv())
.corpus$x <- character(0)
.record <- function(x) {
  .x <- tryCatch(as.character(x), error = function(e) NULL)
  if (is.character(.x) && length(.x) == 1L) {
    .corpus$x <- c(.corpus$x, .x)
  }
}
## rxFromSE() is non-standard-evaluating; the tracer forces `x` the same way
## nlmixr2est's .toRx() already does, so nothing about the call changes
suppressMessages(trace("rxFromSE",
  tracer = bquote(.(.record)(x)),
  where = asNamespace("rxode2"), print = FALSE
))

.x <- .fastCase(.P, .K, .form)
.ctl <- foceiControl(
  print = 0, maxOuterIterations = 0, covMethod = "",
  calcTables = FALSE, fast = TRUE,
  rxControl = rxode2::rxControl(cores = 1L)
)
.t <- system.time(suppressMessages(nlmixr2(.x$model, .x$data, "focei", .ctl)))[["elapsed"]]
untrace("rxFromSE", where = asNamespace("rxode2"))

.file <- sprintf("bench/issue1682-corpus-P%dK%d-%s.rds", .P, .K, .form)
saveRDS(.corpus$x, .file, compress = "xz")
cat(sprintf(
  "P=%d K=%d %s: %d rxFromSE() calls, %.1f MB of input, fit setup %.1f s -> %s\n",
  .P, .K, .form, length(.corpus$x), sum(nchar(.corpus$x)) / 1e6, .t, .file
))
