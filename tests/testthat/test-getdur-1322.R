rxTest({

  # _getDur()'s `backward == 1` branch has no call site inside rxode2 (the one
  # internal caller always passes 2); it is reachable only through the
  # `t_getDur` slot handed to generated model code and downstream packages.
  # .getDurTest() drives it directly.  Only equality of the evid values matters
  # to the pairing, so plain distinct integers stand in for real event ids.
  .evidInf <- 10101L
  .evidBolus <- 101L

  test_that("backward scan finds the matching infusion start", {
    d <- .getDurTest(time = c(0, 5), dose = c(100, -100),
                     evid = c(.evidInf, .evidInf), idose = c(0L, 1L),
                     l = 1L, backward = 1L)
    expect_equal(d[1], 5)
    expect_equal(d[2], 0)
  })

  test_that("backward scan does not pair an infusion end with a bolus of the same amount (#1322)", {
    # dose 1 is a bolus of +100 that sits between the real infusion start
    # (dose 0) and the infusion end (dose 2).  Pairing on the amount alone
    # matched the bolus and returned 3 instead of 5.
    d <- .getDurTest(time = c(0, 2, 5), dose = c(100, 100, -100),
                     evid = c(.evidInf, .evidBolus, .evidInf),
                     idose = c(0L, 1L, 2L), l = 2L, backward = 1L)
    expect_equal(d[1], 5)
    expect_equal(d[2], 0)
  })

  test_that("backward scan errors when only a different-evid start is available (#1322)", {
    expect_error(.getDurTest(time = c(0, 5), dose = c(100, -100),
                             evid = c(.evidBolus, .evidInf),
                             idose = c(0L, 1L), l = 1L, backward = 1L),
                 "infusion start cannot be found")
  })

  test_that("backward scan errors when the start is missing", {
    expect_error(.getDurTest(time = c(0, 5), dose = c(50, -100),
                             evid = c(.evidInf, .evidInf),
                             idose = c(0L, 1L), l = 1L, backward = 1L),
                 "infusion start cannot be found")
  })

  test_that("an orphaned infusion end at dose 0 does not fall into the forward scan (#1322)", {
    # dose 0 is an infusion end with nothing before it.  The old branch guard
    # (`backward == 1 && l != 0`) sent this to the forward scan, which paired it
    # with the later start and returned a negated duration of 3.
    expect_error(.getDurTest(time = c(5, 8), dose = c(-100, 100),
                             evid = c(.evidInf, .evidInf),
                             idose = c(0L, 1L), l = 0L, backward = 1L),
                 "infusion start cannot be found")
  })

  test_that("forward scan finds the infusion end", {
    d <- .getDurTest(time = c(0, 5), dose = c(100, -100),
                     evid = c(.evidInf, .evidInf), idose = c(0L, 1L),
                     l = 0L, backward = 2L)
    expect_equal(d[1], 5)
    expect_equal(d[2], 1)
  })

  test_that("forward scan returns NA (backward=2) or errors otherwise when the end is missing", {
    expect_true(is.na(.getDurTest(time = c(0, 5), dose = c(100, 50),
                                  evid = c(.evidInf, .evidInf),
                                  idose = c(0L, 1L), l = 0L, backward = 2L)[1]))
    expect_error(.getDurTest(time = c(0, 5), dose = c(100, 50),
                             evid = c(.evidInf, .evidInf),
                             idose = c(0L, 1L), l = 0L, backward = 0L),
                 "infusion end cannot be found")
  })

  test_that("an out of range dose index is caught before idose is read", {
    expect_true(is.na(.getDurTest(time = c(0, 5), dose = c(100, -100),
                                  evid = c(.evidInf, .evidInf),
                                  idose = c(0L, 1L), l = 2L, backward = 2L)[1]))
    expect_error(.getDurTest(time = c(0, 5), dose = c(100, -100),
                             evid = c(.evidInf, .evidInf),
                             idose = c(0L, 1L), l = 2L, backward = 1L),
                 "infusion end cannot be found")
    expect_error(.getDurTest(time = c(0, 5), dose = c(100, -100),
                             evid = c(.evidInf, .evidInf),
                             idose = c(0L, 1L), l = -1L, backward = 1L),
                 "infusion start cannot be found")
  })

  test_that("a modeled rate of zero or less still errors and leaves the solver usable (#1322)", {
    # updateRate() returned from these two paths without restoring ind->idx.
    mod <- rxode2({
      a <- 6
      b <- 0.6
      ri <- 10
      d/dt(intestine) <- -a * intestine
      rate(intestine) <- ri
      d/dt(blood) <- a * intestine - b * blood
    })

    ev <- et() |>
      et(amt = 2 / 24, rate = -1, time = 0, addl = 9, ii = 1) |>
      et(seq(0, 10, by = 1 / 24))

    good <- rxSolve(mod, ev, c(ri = 2))

    expect_error(rxSolve(mod, ev, c(ri = 0)))
    expect_error(rxSolve(mod, ev, c(ri = -1)))

    # the failed solves must not leave anything behind
    expect_equal(as.data.frame(rxSolve(mod, ev, c(ri = 2))),
                 as.data.frame(good))
  })

})
