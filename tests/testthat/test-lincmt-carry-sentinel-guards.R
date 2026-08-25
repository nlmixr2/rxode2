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
  test_that("which1=-4 uses the interval since the previous row in the output pass", {
    # 1-cmt IV: d(A_i)/d(A_{i-1}) = exp(-(cl/v) * dt); ind->tprior is stale
    # in the lhs pass, so a per-row dt must not come from it
    a <- "rx__PTR__, t, 1, 1, 0, %d, %d, 1, cl, v, 0, 0, 0, 0, 0"
    m <- suppressWarnings(rxode2(paste0("cp=linCmtB(", sprintf(a, -1L, -1L), ")\n",
                                        "tm=linCmtB(", sprintf(a, -4L, 0L), ")")))
    p <- c(cl = 1, v = 10)
    s <- rxSolve(m, p, et(amt = 100) |> et(c(1, 2, 3.5)), returnType = "data.frame")
    expect_equal(s$tm, exp(-0.1 * c(1, 1, 1.5)), tolerance = 1e-12)
  })
  test_that("which1=-4 as the only linCmtB() call sizes the kernel itself", {
    m <- suppressWarnings(rxode2(
      "tm = linCmtB(rx__PTR__, t, 1, 1, 0, -4, 0, 1, 1, 10, 0, 0, 0, 0, 0)"))
    s <- rxSolve(m, et(amt = 100) |> et(c(1, 2)), returnType = "data.frame")
    expect_equal(s$tm, exp(-0.1 * c(1, 1)), tolerance = 1e-12)
  })
  test_that("a valid shape still accumulates", {
    # calc_lhs runs on the dose row too, so the observation rows see the
    # third and fourth 0.5 increments
    s <- rxSolve(mk(3L, 1L, -7L, 3L), ev, returnType = "data.frame")
    expect_equal(s$cp, c(1.0, 1.5))
  })
  test_that("which1=-8 pins the full -5 advance for the subject's pass", {
    # constant theta: without the pin the 3b.4 fast path skips every -5
    # advance after the first row; with -8 emitted before it, none are skipped
    base <- "rx__PTR__, t, 1, 1, 0, %d, %d, 1, 1, 10, 0, 0, 0, 0, 0"
    mkAdv <- function(pin) {
      suppressWarnings(rxode2(paste0(
        "cp=linCmtB(", sprintf(base, -1L, -1L), ")\n",
        if (pin) paste0("pn=linCmtB(", sprintf(base, -8L, 0L), ")\n") else "",
        "ad=linCmtB(", sprintf(base, -5L, 0L), ")")))
    }
    evA <- et(amt = 100) |> et(c(1, 2, 3))
    prev <- linCmtCarrySetFast(TRUE)
    on.exit(linCmtCarrySetFast(prev), add = TRUE)
    linCmtCarryFastStats(reset = TRUE)
    s0 <- rxSolve(mkAdv(FALSE), evA, returnType = "data.frame")
    skippedNoPin <- linCmtCarryFastStats(reset = TRUE)[["advFast"]]
    s1 <- rxSolve(mkAdv(TRUE), evA, returnType = "data.frame")
    skippedPin <- linCmtCarryFastStats(reset = TRUE)[["advFast"]]
    expect_gt(skippedNoPin, 0)
    expect_equal(skippedPin, 0)
    expect_true(all(s1$pn == 0))
    expect_equal(s0$cp, s1$cp)
  })
})
