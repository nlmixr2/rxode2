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

test_that("safeLog=2 floors zero but rejects a negative argument", {

  m <- rxode2({
    negLog = log(-1)
    zeroLog = log(0)
  })

  et <- et(1)

  # safeLog=2: zero is a benign numerical touch and keeps the floor; a NEGATIVE
  # argument is a domain error, so it must come back NaN rather than the large finite
  # log(.Machine$double.eps) that safeLog=TRUE gives.  Written for a hand-written
  # likelihood taking log() of a parameter that has to stay positive -- there the
  # floored value reads as a large REWARD for an invalid parameter.
  s2 <- rxSolve(m, et, safeLog=2L)
  expect_true(is.nan(s2$negLog))
  expect_equal(s2$zeroLog, log(.Machine$double.eps))

  # the other two modes are unchanged (guarding the contract the block above pins)
  s1 <- rxSolve(m, et, safeLog=TRUE)
  expect_false(is.nan(s1$negLog))
  expect_equal(s1$negLog, log(.Machine$double.eps))
  expect_equal(s1$zeroLog, log(.Machine$double.eps))

  s0 <- rxSolve(m, et, safeLog=FALSE)
  expect_true(is.nan(s0$negLog))
  expect_true(s0$zeroLog == -Inf)

  # out-of-range values are still rejected
  expect_error(rxSolve(m, et, safeLog=3L))
})
