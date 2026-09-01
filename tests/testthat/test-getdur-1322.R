rxTest({

  # _getDur()'s `backward == 1` branch has no call site inside rxode2 (the one
  # internal caller always passes 2); it is reachable only through the
  # `t_getDur` slot handed to generated model code and downstream packages.
  # .getDurTest() drives it directly.  Only equality of the evid values matters
  # to the pairing, so plain distinct integers stand in for real event ids.
  .evidInf <- 10101L
  .evidInf2 <- 10201L
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

  test_that("forward scan does not pair with another infusion's end (#1322)", {
    # two infusions running at the same rate into different compartments: the
    # first record's end is dose 3, but dose 2 (the other compartment's end)
    # carries the same amount and was matched first.
    d <- .getDurTest(time = c(0, 1, 6, 10), dose = c(10, 10, -10, -10),
                     evid = c(.evidInf, .evidInf2, .evidInf2, .evidInf),
                     idose = c(0L, 1L, 2L, 3L), l = 0L, backward = 2L)
    expect_equal(d[1], 10)
    expect_equal(d[2], 3)
  })

  test_that("dose() reports the whole infusion amount when a same-rate infusion overlaps (#1322)", {
    # the end-to-end shape of the scan above: _getDur() recovers the amount from
    # the rate for tad()/dose(), so the mispairing reported 6 * 10 = 60 here.
    mod <- rxode2({
      d/dt(a) <- -0.1 * a
      d/dt(b) <- -0.1 * b
      dd <- dose()
    })

    ev <- et(amt = 100, rate = 10, cmt = "a") |>
      et(amt = 50, rate = 10, cmt = "b", time = 1) |>
      et(seq(0, 15, by = 1))

    s <- rxSolve(mod, ev)
    expect_equal(s$dd[s$time == 0], 100)
    expect_equal(s$dd[s$time == 1], 50)
  })

  test_that("a steady state infusion with a modeled lag reports the whole dose (#1322)", {
    # the steady state path pushes the lagged infusion's start and stop records
    # into the extra-dose pool, and the amount-only scan paired the start with a
    # stop at its own time, so dose() came out as 0 * rate for every row.
    mod <- rxode2({
      cl <- 1
      v <- 20
      d/dt(central) <- -(cl / v) * central
      alag(central) <- lg
      dd <- dose()
      tl <- tad()
    })

    ev <- et(amt = 100, rate = 100 / 8, ss = 1, ii = 12) |> et(seq(0, 24, by = 2))

    s0 <- rxSolve(mod, ev, c(lg = 0))
    expect_equal(unique(s0$dd), 100)

    # before the lagged dose lands there is no dose yet, which is what a plain
    # (non steady state) lagged infusion has always reported
    s2 <- rxSolve(mod, ev, c(lg = 2))
    expect_true(is.na(s2$dd[s2$time == 0]))
    expect_true(is.na(s2$tl[s2$time == 0]))
    expect_equal(unique(s2$dd[s2$time > 0]), 100)

    ref <- rxSolve(mod, et(amt = 100, rate = 100 / 8) |> et(seq(0, 24, by = 2)),
                   c(lg = 2))
    expect_true(is.na(ref$dd[ref$time == 0]))

    # a steady state infusion with no modeled lag was already right
    modNoLag <- rxode2({
      cl <- 1
      v <- 20
      d/dt(central) <- -(cl / v) * central
      dd <- dose()
    })
    expect_equal(unique(rxSolve(modNoLag, ev)$dd), 100)
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

  test_that("forward scan errors when only a different-evid end is available (#1322)", {
    expect_true(is.na(.getDurTest(time = c(0, 5), dose = c(10, -10),
                                  evid = c(.evidInf, .evidInf2),
                                  idose = c(0L, 1L), l = 0L, backward = 2L)[1]))
    expect_error(.getDurTest(time = c(0, 5), dose = c(10, -10),
                             evid = c(.evidInf, .evidInf2),
                             idose = c(0L, 1L), l = 0L, backward = 0L),
                 "infusion end cannot be found")
  })

  test_that("the test driver rejects arguments it cannot safely read", {
    expect_error(.getDurTest(time = c(0, 5), dose = 100,
                             evid = c(.evidInf, .evidInf),
                             idose = c(0L, 1L), l = 0L, backward = 2L),
                 "lengths are wrong")
    # a negative or past-the-end idose entry would be redirected into the
    # extra-dose pools, which this driver does not have
    expect_error(.getDurTest(time = c(0, 5), dose = c(100, -100),
                             evid = c(.evidInf, .evidInf),
                             idose = c(-1L, 1L), l = 1L, backward = 1L),
                 "out of range")
    expect_error(.getDurTest(time = c(0, 5), dose = c(100, -100),
                             evid = c(.evidInf, .evidInf),
                             idose = c(0L, 2L), l = 0L, backward = 2L),
                 "out of range")
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

    # the other error return: the data asks for a modeled rate but the model
    # only supplies a modeled duration
    modDur <- rxode2({
      a <- 6
      b <- 0.6
      di <- 3
      d/dt(intestine) <- -a * intestine
      dur(intestine) <- di
      d/dt(blood) <- a * intestine - b * blood
    })

    expect_error(rxSolve(modDur, ev, c(di = 3)))
  })

})
