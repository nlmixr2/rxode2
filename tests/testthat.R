library(rxode2)
library(testthat)
library(data.table)
# CRAN work-arounds
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  setRxThreads(2L)
  setDTthreads(2L)
  Sys.setenv(OMP_NUM_THREADS = "2")
  Sys.setenv(MKL_NUM_THREADS = "2")
  if (identical(Sys.info()["sysname"], "Darwin")) {
    rxode2::rxUnloadAll(set=FALSE)
  }
}
#test_check("rxode2", reporter = testthat::LocationReporter,
#          stop_on_failure = FALSE)
## if (.Platform$OS.type == "windows") {
  test_check("rxode2", reporter = testthat::LocationReporter,
             stop_on_failure = TRUE)
## } else {
##   test_check("rxode2")
## }
