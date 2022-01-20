Sys.setenv("R_TESTS" = "")
library(rxode2)
library(testthat)
test_check("rxode2")

## test_check("rxode2",
##   stop_on_failure = FALSE,
##   reporter = testthat::LocationReporter
## )
