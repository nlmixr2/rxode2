test_that("safeZero solving", {


  m <- rxode2({
    nan0 = 0/0
    nanLog = log(-1)
    nanPow = 0^-1
    rPow = 0^-1.5
  })

  et <- et(1)

  unsafe <- rxSolve(m, et, safeZero=FALSE, safePow=FALSE, safeLog=FALSE, useStdPow = TRUE)
  expect_true(is.nan(unsafe$nan0))
  expect_true(is.nan(unsafe$nanLog))
  expect_false(is.finite(unsafe$nanPow))
  expect_false(is.finite(unsafe$rPow))

  safe <- rxSolve(m, et, safeZero=TRUE, safePow=TRUE, safeLog=TRUE, useStdPow=FALSE)

  expect_false(is.nan(safe$nan0))
  expect_false(is.nan(safe$nanLog))
  expect_true(is.finite(safe$nanPow))
  expect_true(is.finite(safe$rPow))

  safe <- rxSolve(m, et, safeZero=TRUE, safePow=FALSE, safeLog=TRUE, useStdPow=FALSE)

  expect_false(is.na(safe$nan0))
  expect_false(is.na(safe$nanLog))
  expect_false(is.finite(safe$nanPow))
  expect_false(is.finite(safe$rPow))

})
