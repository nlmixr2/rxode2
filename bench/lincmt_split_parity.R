# Bitwise parity battery for moving the Stan linCmt code to rxode2lincmt.
#
#   Rscript bench/lincmt_split_parity.R <library> <out.rds>
#   Rscript bench/lincmt_split_parity_compare.R <a.rds> <b.rds>
#
# Solves a fixed set of linCmt() cases with the rxode2 found first in
# <library> and saves every result; the compare script requires identical().
.args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(.args) == 2L)
.here <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))))
.libPaths(c(path.expand(.args[1]), .libPaths()))
suppressMessages(library(rxode2))
cat("rxode2", format(utils::packageVersion("rxode2")), "from", find.package("rxode2"), "\n")
setRxThreads(4L)

.out <- list()
.keep <- function(name, expr) {
  .out[[name]] <<- tryCatch(suppressWarnings(expr), error = function(e) paste("ERROR:", conditionMessage(e)))
  invisible(NULL)
}
.solve <- function(...) rxSolve(..., returnType = "data.frame")

source(file.path(.here, "lincmt_split_parity_solve.R"), local = TRUE)
source(file.path(.here, "lincmt_split_parity_kernel.R"), local = TRUE)

saveRDS(.out, .args[2])
cat("saved", length(.out), "cases to", .args[2], "\n")
