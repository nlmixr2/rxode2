#!/usr/bin/env Rscript
## Compare two rxode2 benchmark records.
##
## Usage:
##   Rscript bench/compare.R OLD.json NEW.json
##   Rscript bench/compare.R           # newest two records in bench/results

.args <- commandArgs(trailingOnly = TRUE)

.root <- {
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(".")
}

if (length(.args) >= 2) {
  oldF <- .args[1]; newF <- .args[2]
} else {
  fs <- sort(list.files(file.path(.root, "bench", "results"), "\\.json$", full.names = TRUE))
  if (length(fs) < 2) stop("Need at least two records in bench/results/ (or pass two files).")
  oldF <- fs[length(fs) - 1]; newF <- fs[length(fs)]
}

old <- jsonlite::fromJSON(oldF)
new <- jsonlite::fromJSON(newF)

cat(sprintf("OLD %s (%s)\nNEW %s (%s)\n\n",
            basename(oldF), old$git, basename(newF), new$git))
cat(sprintf("  %-22s %10s %10s %9s\n", "case", "old(ms)", "new(ms)", "delta"))
nms <- union(names(old$cases), names(new$cases))
for (nm in nms) {
  o <- old$cases[[nm]]; n <- new$cases[[nm]]
  if (is.null(o) || is.null(n)) {
    cat(sprintf("  %-22s %10s %10s %9s\n", nm,
                if (is.null(o)) "-" else sprintf("%.2f", o * 1000),
                if (is.null(n)) "-" else sprintf("%.2f", n * 1000), "n/a"))
    next
  }
  pct <- (n - o) / o * 100
  flag <- if (pct <= -5) "  <-- faster" else if (pct >= 5) "  <-- SLOWER" else ""
  cat(sprintf("  %-22s %10.2f %10.2f %+8.1f%%%s\n", nm, o * 1000, n * 1000, pct, flag))
}
