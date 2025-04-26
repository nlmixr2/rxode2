library(rxode2)
library(testthat)
setRxThreads(1L)
library(data.table)
setDTthreads(1L)
##test_check("rxode2", reporter = testthat::LocationReporter)
# test_check("rxode2")

test_check("rxode2",
  stop_on_failure = FALSE,
  reporter = testthat::LocationReporter
)
