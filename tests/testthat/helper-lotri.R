## Priors (`prior(x) ~ ...`) and repeated blocks (`same()`) need 'lotri' >= 1.0.5
skipIfOldLotri <- function() {
  testthat::skip_if(utils::packageVersion("lotri") < "1.0.5", "this test needs a newer version of 'lotri' (>= 1.0.5)")
}
