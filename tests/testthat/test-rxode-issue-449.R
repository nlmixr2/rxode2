rxode2Test({
  test_that("rxode2 produces an error instead of crashing for bad model types", {
    expect_error(rxSolve(list(a=3), events=et(0)))
  })
})
