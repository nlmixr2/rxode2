library(rxode2)
library(testthat)
setRxThreads(1L)
library(data.table)
setDTthreads(1L)
if (!identical(Sys.getenv("NOT_CRAN"), "true") &&
      identical(Sys.info()["sysname"], "Darwin")) {
  rxUnloadAll(set=FALSE)
}
## test_check("rxode2", reporter = testthat::LocationReporter)
test_check("rxode2")

## test_check("rxode2",
##   stop_on_failure = FALSE,
##   reporter = testthat::LocationReporter
## )
