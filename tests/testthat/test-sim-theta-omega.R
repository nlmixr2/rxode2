rxTest({
  test_that("a multi-subject draw without omega warns, and repeats the parameters", {
    .w <- testthat::capture_warnings(.x <- rxSimThetaOmega(params = c(k = 1), nSub = 2))
    expect_identical(.w, "multi-subject simulation without 'omega'")
    expect_equal(.x, data.frame(k = c(1, 1)))
  })
})
