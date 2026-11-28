rxTest({
  test_that("zero-amount fixed-duration infusion translates without NaN stop time (#1791)", {
    m <- rxode2({
      d/dt(central) <- -0.1 * central
    })

    ev0 <- et(amt = 0, dur = 2) |> et(0:24)
    tr0 <- etTrans(ev0, m)
    expect_false(any(is.nan(tr0$TIME)))
    expect_equal(tr0$TIME[tr0$EVID == 20101], c(0, 2))
    expect_equal(tr0$AMT[tr0$EVID == 20101], c(0, 0))

    s0 <- rxSolve(m, ev0)
    expect_equal(tail(s0$central, 1), 0)

    ev1 <- et(amt = 10) |>
      et(amt = 0, dur = 2, time = 1) |>
      et(0:24)
    s1 <- rxSolve(m, ev1, atol = 1e-10, rtol = 1e-10)
    expect_equal(tail(s1$central, 1), 10 * exp(-0.1 * 24), tolerance = 1e-6)
  })

  test_that("zero-amount fixed-duration infusion handles addl, overlapping infusion, and linCmt (#1791)", {
    m <- rxode2({
      d/dt(central) <- -0.1 * central
    })

    evAddl <- et(amt = 0, dur = 2, ii = 6, addl = 2) |> et(0:24)
    sAddl <- rxSolve(m, evAddl)
    expect_equal(tail(sAddl$central, 1), 0)

    evBase <- et(amt = 10, dur = 5) |> et(0:24)
    evOver <- et(amt = 10, dur = 5) |>
      et(amt = 0, dur = 2, time = 1) |>
      et(0:24)
    expect_equal(
      rxSolve(m, evOver, atol = 1e-10, rtol = 1e-10)$central,
      rxSolve(m, evBase, atol = 1e-10, rtol = 1e-10)$central,
      tolerance = 1e-6
    )

    mLin <- rxode2({
      cl <- 1
      v <- 10
      cp <- linCmt()
    })
    sLin <- rxSolve(mLin, et(amt = 0, dur = 2) |> et(0:24))
    expect_equal(tail(sLin$cp, 1), 0)
  })
})
