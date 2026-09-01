rxTest({

  # The solver-level consequences of the _getDur() infusion pairing and the
  # updateRate() index restore fixed in nlmixr2/rxode2#1322.  The pairing rules
  # themselves are tested against the C driver in test-getdur-1322.R.

  test_that("dose() reports the whole infusion amount when a same-rate infusion overlaps (#1322)", {
    # _getDur() recovers the amount from the rate for tad()/dose(), so pairing
    # the first infusion's start with the other compartment's end reported
    # 6 * 10 = 60 here.
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
