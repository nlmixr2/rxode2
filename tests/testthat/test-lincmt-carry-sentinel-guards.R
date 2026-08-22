rxTest({
  # The carry sentinels (which1 = -4..-7) index ind->linCmtCarryT, a 4-row
  # buffer, by row = which2 %% (ncmt + oral0).  A hand-written linCmtB() call
  # can pass any ncmt/oral0, so an impossible shape must come back NA instead
  # of reading or writing past the buffer.
  mk <- function(ncmt, oral0, which1, which2) {
    rxode2(sprintf(
      "cp = linCmtB(rx__PTR__, t, 1, %d, %d, %d, %d, 1, 1, 20, 0.5, 30, 0.2, 40, 1.1)",
      ncmt, oral0, which1, which2))
  }
  ev <- et(amt = 100) |> et(c(1, 2))
  for (w1 in c(-4L, -5L, -6L, -7L)) {
    test_that(sprintf("which1=%d with an impossible ncmt returns NA", w1), {
      s <- rxSolve(mk(99L, 0L, w1, 50L), ev, returnType = "data.frame")
      expect_true(all(is.na(s$cp)))
      s <- rxSolve(mk(3L, 2L, w1, 1L), ev, returnType = "data.frame")
      expect_true(all(is.na(s$cp)))
    })
  }
  test_that("a pair index past the cap returns NA", {
    s <- rxSolve(mk(3L, 1L, -7L, 4L * 8L), ev, returnType = "data.frame")
    expect_true(all(is.na(s$cp)))
  })
  test_that("a valid shape still accumulates", {
    # calc_lhs runs on the dose row too, so the observation rows see the
    # third and fourth 0.5 increments
    s <- rxSolve(mk(3L, 1L, -7L, 3L), ev, returnType = "data.frame")
    expect_equal(s$cp, c(1.0, 1.5))
  })
})
