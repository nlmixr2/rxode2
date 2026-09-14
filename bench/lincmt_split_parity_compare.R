# Compare two result files written by bench/lincmt_split_parity.R.
#
#   Rscript bench/lincmt_split_parity_compare.R <a.rds> <b.rds>
#
# Exits 0 only when every case is identical(); otherwise lists the cases that
# differ with their largest absolute numeric difference.
.args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(.args) == 2L)

.maxAbsDiff <- function(a, b) {
  .a <- suppressWarnings(as.numeric(unlist(a)))
  .b <- suppressWarnings(as.numeric(unlist(b)))
  if (length(.a) != length(.b)) return(NA_real_)
  max(abs(.a - .b), na.rm = TRUE)
}

.a <- readRDS(.args[1])
.b <- readRDS(.args[2])
.all <- union(names(.a), names(.b))
.bad <- 0L
for (.n in .all) {
  if (identical(.a[[.n]], .b[[.n]])) next
  .bad <- .bad + 1L
  cat(sprintf("DIFF %-50s max|a-b| = %s\n", .n, format(.maxAbsDiff(.a[[.n]], .b[[.n]]))))
}
cat(sprintf("%d of %d cases differ\n", .bad, length(.all)))
quit(status = if (.bad > 0L) 1L else 0L)
