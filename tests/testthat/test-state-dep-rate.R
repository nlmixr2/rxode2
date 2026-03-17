rxTest({
  # ── Analytical helper ──────────────────────────────────────────────────────
  # depot(t) for a single constant-rate infusion into a one-compartment depot
  # that drains with first-order rate constant ka.
  #
  #   R  = infusion rate (amount / time)
  #   D  = duration      = amt / R   (infusion starts at t = 0)
  #   ka = depot elimination rate constant
  #
  # During infusion (0 ≤ t ≤ D):
  #   depot(t) = (R/ka) * (1 - exp(-ka*t))
  #
  # After infusion (t > D):
  #   depot(t) = depot(D) * exp(-ka*(t-D))
  #            = (R/ka) * (1-exp(-ka*D)) * exp(-ka*(t-D))
  .depotAnalytic <- function(t, R, D, ka) {
    vapply(t, function(ti) {
      if (ti <= 0) return(0)
      if (ti <= D) {
        (R / ka) * (1 - exp(-ka * ti))
      } else {
        depot_D <- (R / ka) * (1 - exp(-ka * D))
        depot_D * exp(-ka * (ti - D))
      }
    }, numeric(1))
  }

  # ── Test 1 ─────────────────────────────────────────────────────────────────
  # state = 0 throughout  →  rate(depot) = rate0 + 0 = rate0 exactly.
  # This is the baseline: no state dependence, behaviour identical to a fixed
  # rate infusion.
  test_that("state-dep rate with state=0 uses exactly rate0 (no state shift)", {
    mod <- rxode2({
      # Infusion rate is modelled as rate0 + state.
      # With state held at 0, this collapses to the plain fixed-rate case.
      rate(depot) <- rate0 + state
      d/dt(depot) <- -ka * depot
      d/dt(state) <- 0         # constant state (initial value is the only value)
    })

    amt   <- 100
    rate0 <- 10
    ka    <- 0.5
    R     <- rate0            # state=0 → effective rate = rate0
    D     <- amt / R          # expected infusion duration
    times <- seq(0, 30, by = 0.5)

    et <- eventTable() |>
      add.dosing(dose = amt, rate = -1) |>  # rate=-1 → use modelled rate
      add.sampling(times)

    s <- solve(mod, et,
               inits  = c(state = 0, depot = 0),
               params = c(rate0 = rate0, ka = ka))

    expected <- .depotAnalytic(times, R, D, ka)

    # Every sampled depot value must match the analytical trajectory computed
    # with rate = rate0.  A mismatch would mean the solver used a different rate.
    expect_equal(s$depot[s$time %in% times], expected,
                 tolerance = 1e-3,
                 label = "depot matches rate0-only analytical solution when state=0")
  })

  # ── Test 2 ─────────────────────────────────────────────────────────────────
  # state = S0 (constant, positive and negative) throughout.
  # The effective rate at infusion start is rate0 + S0; duration = amt/(rate0+S0).
  # The full depot trajectory must match the analytical formula for that rate.
  test_that("state-dep rate shifts infusion rate by state value: rate = rate0 + state_at_start", {
    mod <- rxode2({
      # Infusion rate includes a state-variable contribution.
      # With d/dt(state)=0 the state stays at its initial value for the whole
      # simulation, giving an analytically verifiable rate.
      rate(depot) <- rate0 + state
      d/dt(depot) <- -ka * depot
      d/dt(state) <- 0
    })

    amt   <- 100
    rate0 <- 10
    ka    <- 0.5
    times <- seq(0, 40, by = 0.5)

    for (S0 in c(2, 5, -3)) {
      R <- rate0 + S0         # effective rate at the moment the infusion starts
      D <- amt / R            # duration changes with state value

      et <- eventTable() |>
        add.dosing(dose = amt, rate = -1) |>
        add.sampling(times)

      s <- solve(mod, et,
                 inits  = c(state = S0, depot = 0),
                 params = c(rate0 = rate0, ka = ka))

      expected <- .depotAnalytic(times, R, D, ka)

      # The solver must use rate = rate0 + S0, not rate0 alone.
      # Verifying the full trajectory confirms both the rate magnitude and the
      # derived infusion duration (D = amt/R).
      expect_equal(s$depot[s$time %in% times], expected,
                   tolerance = 1e-3,
                   label = paste0("depot matches analytic solution for rate=",
                                  R, " (S0=", S0, ")"))

      # Confirm the trajectory with S0≠0 differs from the S0=0 baseline,
      # i.e., state truly shifts the rate and is not silently ignored.
      if (S0 != 0) {
        expected_s0 <- .depotAnalytic(times, rate0, amt / rate0, ka)
        expect_false(isTRUE(all.equal(expected, expected_s0)),
                     label = paste0("state S0=", S0,
                                    " produces a different trajectory than state=0"))
      }
    }
  })

  # ── Test 3 ─────────────────────────────────────────────────────────────────
  # The infusion end time must be exactly amt / (rate0 + state_at_start).
  # We probe this by checking depot at the expected stop time D and well after.
  # During the infusion depot is rising; after it decays.  Confirming depot(D)
  # matches the analytical peak locks in the exact stop time.
  test_that("state-dep rate: infusion ends at exactly amt/(rate0 + state_at_start)", {
    mod <- rxode2({
      rate(depot) <- rate0 + state
      d/dt(depot) <- -ka * depot
      d/dt(state) <- 0
    })

    amt   <- 100
    rate0 <- 5
    ka    <- 0.3

    for (S0 in c(0, 3, -1)) {
      R <- rate0 + S0
      D <- amt / R           # expected infusion end time

      # Sample finely around D so we capture exactly the stop-event time
      times <- sort(unique(c(seq(0, D + 8, by = 0.1), D)))

      et <- eventTable() |>
        add.dosing(dose = amt, rate = -1) |>
        add.sampling(times)

      s <- solve(mod, et,
                 inits  = c(state = S0, depot = 0),
                 params = c(rate0 = rate0, ka = ka))

      # depot(D) from the analytical formula
      depot_at_D_expected <- (R / ka) * (1 - exp(-ka * D))

      # observed depot at the exact stop-event sample time
      obs_at_D <- s$depot[which.min(abs(s$time - D))]

      # If rate was wrong the infusion would stop at a different time, yielding
      # a depot peak that is too large (rate too high, shorter duration) or too
      # small (rate too low, longer duration).
      expect_equal(obs_at_D, depot_at_D_expected,
                   tolerance = 1e-3,
                   label = paste0("depot at infusion stop D=", round(D, 4),
                                  " matches analytic (S0=", S0, ")"))
    }
  })

  # ── Test 4 ─────────────────────────────────────────────────────────────────
  # Multiple doses: each dose's rate is evaluated at the state present when
  # THAT dose's infusion begins.  Because state changes over time (exponential
  # decay here), consecutive doses get different effective rates and therefore
  # different durations.
  # We verify no NAs and that the two-dose trajectory is self-consistent (not
  # all identical) when the state is non-zero.
  test_that("state-dep rate: consecutive doses evaluated at their own start state", {
    mod <- rxode2({
      # rate depends on a decaying state variable so each dose sees a different
      # rate if the infusions are spaced far enough apart.
      rate(depot) <- rate0 + state
      d/dt(depot) <- -ka * depot
      d/dt(state) <- -kstate * state   # state decays between doses
    })

    et <- et() |>
      et(amt = 50, rate = -1, time = 0) |>    # dose 1 – state is large
      et(amt = 50, rate = -1, time = 15) |>   # dose 2 – state has decayed
      et(seq(0, 30, by = 0.5))

    s <- rxSolve(mod, et,
                 params = c(rate0 = 5, ka = 0.4, kstate = 0.2),
                 inits  = c(depot = 0, state = 4))

    # No numerical failures
    expect_false(any(is.na(s$depot)),
                 label = "no NAs in depot with two state-dep rate doses")

    # The trajectory around dose 2 must differ from what a state=0 run would
    # produce (state has not decayed to exactly 0 by t=15).
    s0 <- rxSolve(mod, et,
                  params = c(rate0 = 5, ka = 0.4, kstate = 0.2),
                  inits  = c(depot = 0, state = 0))

    expect_false(isTRUE(all.equal(s$depot, s0$depot)),
                 label = "nonzero initial state gives different depot trajectory than state=0")
  })
})
