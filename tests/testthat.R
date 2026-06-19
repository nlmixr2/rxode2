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
## test_check("rxode2", reporter = testthat::LocationReporter,
##            stop_on_failure = TRUE)
withr::with_options(list(rxode2.useLinCmt=FALSE), {
  test_check("rxode2")
})
