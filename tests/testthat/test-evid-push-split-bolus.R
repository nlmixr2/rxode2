rxTest({
  # Pushed doses into a splitBolus compartment: room reserved for the split
  # records, and infusions left unsplit.

  test_that("a split bolus pushed as evid=4 reserves enough room (#1322 follow-up)", {
    # splitBolus expands one translated event into splitBolusN-1 records, so an
    # evid=4 push writes 1 + (splitBolusN-1) records where the capacity check
    # only reserved ev.n = 2.  Three targets steps n_all_times by 3 and never
    # lands on the one offset that overruns, so this uses FOUR (nRec = 4 against
    # a reservation of 2), which does reach past the end of the block.  Push
    # repeatedly so the EVID_EXTRA_SIZE slack that hid the overrun is used up.
    # The overrun is a couple of elements that malloc bucketing usually absorbs,
    # so this exercises the boundary rather than failing deterministically
    # without the fix -- it is here to give a heap checker something to catch.
    mSplit <- rxode2({
      splitBolus(depot, depot, central, peri)
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central - q * central + q * peri
      d/dt(peri) <- q * central - q * peri
      cp <- central / v
      if (t < 1) {
        evid_(t + 6, 4, 50, 1, 0, 6, 9, 0)
      }
    })
    mBase <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central - q * central + q * peri
      d/dt(peri) <- q * central - q * peri
      cp <- central / v
    })
    e <- et(amt = 100, time = 0) |> et(seq(0, 72, by = 1))
    # evid=4 resets only on the FIRST addl repetition (t=6, matching NONMEM --
    # see rxode2#1351); write the reference the same way, evid=4 once then
    # plain doses, rather than evid=4 at every repeat.
    eBase <- e |>
      et(amt = 100, time = 0, cmt = 2) |>
      et(amt = 100, time = 0, cmt = 3) |>
      et(amt = 50, time = 6, cmt = 1, evid = 4) |>
      et(amt = 50, time = 6, cmt = 2) |>
      et(amt = 50, time = 6, cmt = 3)
    for (.t in seq(12, 60, by = 6)) {
      eBase <- eBase |>
        et(amt = 50, time = .t, cmt = 1) |>
        et(amt = 50, time = .t, cmt = 2) |>
        et(amt = 50, time = .t, cmt = 3)
    }
    p <- c(ka = 0.5, cl = 1, v = 10, q = 0.3)
    rSplit <- rxSolve(mSplit, p, e)
    rBase <- rxSolve(mBase, p, eBase)
    expect_true(all(is.finite(rSplit$cp)))
    expect_equal(rSplit$depot, rBase$depot, tolerance = 1e-5)
    expect_equal(rSplit$central, rBase$central, tolerance = 1e-5)
    expect_equal(rSplit$peri, rBase$peri, tolerance = 1e-5)
  })

  test_that("a split bolus pushed as evid=1 reserves enough room (#1322 follow-up)", {
    # the same under-reservation as the evid=4 case above, on the commoner
    # spelling: one translated event, splitBolusN-1 records
    mSplit <- rxode2({
      splitBolus(depot, depot, central)
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      cp <- central / v
      if (t < 1) {
        evid_(t + 6, 1, 50, 1, 0, 6, 9, 0)
      }
    })
    mBase <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      cp <- central / v
    })
    e <- et(amt = 100, time = 0) |> et(seq(0, 72, by = 1))
    eBase <- e |> et(amt = 100, time = 0, cmt = 2)
    for (.t in seq(6, 60, by = 6)) {
      eBase <- eBase |>
        et(amt = 50, time = .t, cmt = 1) |>
        et(amt = 50, time = .t, cmt = 2)
    }
    p <- c(ka = 0.5, cl = 1, v = 10)
    rSplit <- rxSolve(mSplit, p, e)
    rBase <- rxSolve(mBase, p, eBase)
    expect_true(all(is.finite(rSplit$cp)))
    expect_equal(rSplit$depot, rBase$depot, tolerance = 1e-5)
    expect_equal(rSplit$central, rBase$central, tolerance = 1e-5)
  })

  test_that("a pushed infusion into a splitBolus compartment is not split", {
    # _rxShouldSplitTranslatedBolus() requires whI == 0, so only a bolus splits;
    # an infusion into the same compartment stays whole
    mSplit <- rxode2({
      splitBolus(depot, depot, central)
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        infuse(100, 10, 1, 0, 0, 0)
      }
    })
    mBase <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      cp <- central / v
    })
    p <- c(ka = 0.5, cl = 1, v = 10)
    obs <- c(0, 1e-8, seq(0.5, 30, by = 0.5))
    rSplit <- rxSolve(mSplit, p, et(obs))
    rBase <- rxSolve(mBase, p, et(amt = 100, time = 0, rate = 10, cmt = 1) |> et(obs))
    expect_equal(rSplit$depot, rBase$depot, tolerance = 1e-5)
    expect_equal(rSplit$central, rBase$central, tolerance = 1e-5)
  })
})
