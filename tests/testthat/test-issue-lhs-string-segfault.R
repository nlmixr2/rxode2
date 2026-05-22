test_that("LHS string assignment without explicit levels does not segfault", {
  # This reproduces a condition where an LHS string variable is assigned
  # but no levels are present in the dataset or model factors, which
  # previously caused rxode2_df.cpp to access a nullptr colI during OpenMP output.
  
  mod <- rxode2({
    limit <- "none"
    d/dt(a) <- -a
  })
  
  dat <- data.frame(time=seq(0, 10, by=1), evid=0)
  
  # Before the fix, rxSolve would throw a segmentation fault (core dumped) here.
  # Now it should gracefully complete and limit should be a factor column.
  expect_error(res <- rxSolve(mod, dat), NA)
  
  expect_true(inherits(res, "rxSolve"))
  expect_true("limit" %in% names(res))
})
