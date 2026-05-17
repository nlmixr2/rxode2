rxTest({
  test_that("state-dep alag uses initial state at sort phase (not NA)", {
    # alag(depot) = central * 0.1
    # central=0 → lag=0; central=5 → lag=0.5 hours
    mod <- rxode2({
      alag(depot) <- central * 0.1
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - kel * central
    })
    et <- et(amt = 100) |> et(seq(0, 24, by = 1))
    s0 <- rxSolve(mod, et, c(ka = 0.5, kel = 0.2),
                  inits = c(depot = 0, central = 0))
    s5 <- rxSolve(mod, et, c(ka = 0.5, kel = 0.2),
                  inits = c(depot = 0, central = 5))
    # Previously produced NA due to ypNA placeholder; now should work
    expect_false(any(is.na(s0$central)))
    expect_false(any(is.na(s5$central)))
    # Different initial lag → different trajectories
    expect_false(isTRUE(all.equal(s0$central, s5$central)))
  })

  test_that("dur(cmt) <- state with constant alag: stop event at absolute lagged time", {
    # alag=0.5, dur=state-dep → absolute stop = (t + 0.5) + dur(central)
    mod <- rxode2({
      alag(depot) <- 0.5
      dur(depot) <- central * 0.1 + 1.0
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - kel * central
    })
    et <- et(amt = 100, rate = -2) |> et(seq(0, 24, by = 1))
    s <- rxSolve(mod, et, c(ka = 0.5, kel = 0.2),
                 inits = c(depot = 0, central = 3))
    expect_false(any(is.na(s$central)))
    expect_false(any(is.na(s$depot)))
  })

  test_that("state-dep alag: runtime refresh updates lag for multiple doses", {
    mod <- rxode2({
      alag(depot) <- central * 0.1
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - kel * central
    })
    et <- et() |>
      et(amt = 100, time = 0) |>
      et(amt = 100, time = 6) |>
      et(seq(0, 24, by = 1))
    # State changes between doses → lag differs for second dose; no NA
    s <- rxSolve(mod, et, c(ka = 0.5, kel = 0.2),
                 inits = c(depot = 0, central = 5))
    expect_false(any(is.na(s$central)))
  })

  test_that("state-dep alag: dose fires at dosing_time + state_value (constant lag, numeric)", {
    # Use d/dt(state_val)=0 so lag = state_val stays constant → analytically verifiable.
    # With ka=0.5 (mono-exp depot), depot(t) after a dose at t_fire = 100*exp(-0.5*(t-t_fire)).
    # dose1 raw_t=0, lag=state_val → fires at t=state_val.
    # dose2 raw_t=3, lag=state_val → fires at t=3+state_val.
    # At each obs time, depot = sum of contributions from fired doses.
    mod <- rxode2({
      alag(depot) <- state_val
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - kel * central
      d/dt(state_val) <- 0
    })
    params <- c(ka = 0.5, kel = 0.2)
    et2dose <- et(amt = 100, time = 0, cmt = "depot") |>
      et(amt = 100, time = 3, cmt = "depot") |>
      et(seq(0, 10, by = 1))

    for (sv in c(0, 1, 2)) {
      s <- rxSolve(mod, et2dose, params,
                   inits = c(depot = 0, central = 0, state_val = sv))
      t_fire1 <- sv          # dose1 fires at t=sv
      t_fire2 <- 3 + sv      # dose2 fires at t=3+sv
      for (tobs in seq(0, 10)) {
        expected <- 0
        if (tobs >= t_fire1) expected <- expected + 100 * exp(-0.5 * (tobs - t_fire1))
        if (tobs >= t_fire2) expected <- expected + 100 * exp(-0.5 * (tobs - t_fire2))
        got <- s$depot[s$time == tobs]
        expect_equal(got, expected, tolerance = 1e-3,
                     label = sprintf("state_val=%d, t=%d: depot", sv, tobs))
      }
    }
  })
})
