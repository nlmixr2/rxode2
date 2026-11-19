# Split rxode2's testthat files into balanced CI shards.
#
# Rscript .github/scripts/ci-shards.R <subset> <index> <n>
#   subset  "core" (the files listed in tests/ci-core.txt) or "all"
#   index   1-based shard number
#   n       number of shards
#
# Prints a testthat `filter` regex for that shard.  Files are assigned
# longest-first to the currently lightest shard using tests/ci-timings.csv
# (file,seconds); a file with no recorded time counts as the median.
.args <- commandArgs(trailingOnly = TRUE)
if (length(.args) != 3L) {
  stop("usage: Rscript .github/scripts/ci-shards.R <core|all> <index> <n>", call. = FALSE)
}
.subset <- .args[1]
.index <- as.integer(.args[2])
.n <- as.integer(.args[3])
if (is.na(.index) || is.na(.n) || .index < 1L || .index > .n) {
  stop("shard index must be in 1..n", call. = FALSE)
}

.files <- sub("^test-(.*)[.][Rr]$", "\\1", list.files("tests/testthat", "^test-.*[.][Rr]$"))

if (.subset == "core") {
  .core <- trimws(readLines("tests/ci-core.txt", warn = FALSE))
  .core <- .core[.core != "" & !startsWith(.core, "#")]
  .missing <- setdiff(.core, .files)
  if (length(.missing) > 0L) {
    stop("tests/ci-core.txt names missing test files: ", paste(.missing, collapse = ", "), call. = FALSE)
  }
  .files <- .core
} else if (.subset != "all") {
  stop("subset must be 'core' or 'all'", call. = FALSE)
}

.time <- rep(NA_real_, length(.files))
if (file.exists("tests/ci-timings.csv")) {
  .tm <- utils::read.csv("tests/ci-timings.csv", stringsAsFactors = FALSE)
  .time <- .tm$seconds[match(.files, .tm$file)]
}
.time[is.na(.time)] <- if (all(is.na(.time))) 1 else stats::median(.time, na.rm = TRUE)

.load <- numeric(.n)
.shard <- integer(length(.files))
for (.i in order(-.time, .files)) {
  .s <- which.min(.load)
  .shard[.i] <- .s
  .load[.s] <- .load[.s] + .time[.i]
}
.mine <- sort(.files[.shard == .index])
message(
  "shard ", .index, "/", .n, " (", .subset, "): ", length(.mine), " files, ~",
  round(.load[.index]), " s weight"
)
if (length(.mine) == 0L) {
  # matches no test file, so the shard runs nothing
  cat("^$\n")
} else {
  cat("^(", paste(gsub("([.+*?^$(){}|\\[\\]\\\\])", "\\\\\\1", .mine), collapse = "|"), ")$\n", sep = "")
}
