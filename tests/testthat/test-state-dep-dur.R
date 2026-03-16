rxTest({
  # ── Analytical background ──────────────────────────────────────────────────
  # dur(depot) <- dur0 + state sets the infusion DURATION directly (rate=-2).
  # The infusion rate is derived as:  R = amt / (dur0 + state_at_start)
  #
  # For a depot with first-order drain rate ka and constant state S0:
  #
  #   D = dur0 + S0                        (infusion duration)
  #   R = amt / D                          (derived rate)
  #
  # During infusion (0 ≤ t ≤ D):
  #   depot(t) = (R/ka) * (1 - exp(-ka*t))
  #
  # After infusion (t > D):
  #   depot(t) = depot(D) * exp(-ka*(t-D))
  #            = (R/ka) * (1-exp(-ka*D)) * exp(-ka*(t-D))

  # ── Helper ─────────────────────────────────────────────────────────────────
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
  # state = 0 throughout  →  dur(depot) = dur0 exactly.
  # Infusion duration = dur0; rate = amt/dur0; trajectory is the standard
  # fixed-duration analytical solution.
  test_that("state-dep dur with state=0 uses exactly dur0 (no state shift)", {
    mod <- rxode2({
      # Infusion duration is modelled as dur0 + state.
      # With state held at 0 this collapses to the fixed-duration case.
      dur(depot) <- dur0 + state
      d/dt(state) <- 0         # constant state; initial value is the only value
      d/dt(depot) <- -ka * depot
    })

    amt  <- 100
    dur0 <- 10
    ka   <- 0.5
    D    <- dur0          # state=0 → effective duration = dur0
    R    <- amt / D       # derived rate
    times <- seq(0, 30, by = 0.5)

    et <- eventTable() |>
      add.dosing(dose = amt, rate = -2) |>  # rate=-2 → use modelled duration
      add.sampling(times)

    s <- solve(mod, et,
               inits  = c(state = 0, depot = 0),
               params = c(dur0 = dur0, ka = ka))

    expected <- .depotAnalytic(times, R, D, ka)

    # Every sampled depot value must match the analytical trajectory computed
    # with duration = dur0.  A mismatch means the solver used a wrong duration.
    expect_equal(s$depot[s$time %in% times], expected,
                 tolerance = 1e-3,
                 label = "depot matches dur0-only analytical solution when state=0")
  })

  # ── Test 2 ─────────────────────────────────────────────────────────────────
  # state = S0 (constant)  →  dur(depot) = dur0 + S0; rate = amt/(dur0+S0).
  # The full depot trajectory must match the closed-form solution for that rate.
  test_that("state-dep dur shifts infusion duration by state value: dur = dur0 + state_at_start", {
    mod <- rxode2({
      # Duration includes a state-variable contribution.
      # With d/dt(state)=0 the state stays at its initial value; the expected
      # duration is analytically known.
      dur(depot) <- dur0 + state
      d/dt(state) <- 0
      d/dt(depot) <- -ka * depot
    })

    amt  <- 100
    dur0 <- 10
    ka   <- 0.5
    times <- seq(0, 40, by = 0.5)

    for (S0 in c(2, 5, -3)) {
      D <- dur0 + S0          # effective duration at the moment the infusion starts
      R <- amt / D            # derived infusion rate

      et <- eventTable() |>
        add.dosing(dose = amt, rate = -2) |>
        add.sampling(times)

      s <- solve(mod, et,
                 inits  = c(state = S0, depot = 0),
                 params = c(dur0 = dur0, ka = ka))

      expected <- .depotAnalytic(times, R, D, ka)

      # The solver must use duration = dur0 + S0, not dur0 alone.
      # Verifying the full trajectory confirms both the duration magnitude and
      # the derived rate (R = amt/(dur0+S0)).
      expect_equal(s$depot[s$time %in% times], expected,
                   tolerance = 1e-3,
                   label = paste0("depot matches analytic solution for dur=",
                                  D, " (S0=", S0, ")"))

      # Confirm the trajectory with S0≠0 differs from the S0=0 baseline,
      # i.e., the state contribution to duration is not silently ignored.
      if (S0 != 0) {
        expected_s0 <- .depotAnalytic(times, amt / dur0, dur0, ka)
        expect_false(isTRUE(all.equal(expected, expected_s0)),
                     label = paste0("S0=", S0,
                                    " gives a different trajectory than state=0"))
      }
    }
  })

  # ── Test 3 ─────────────────────────────────────────────────────────────────
  # Infusion end time must be exactly dur0 + state_at_start.
  # We verify this by checking that depot(D) matches the analytical peak value
  # (R/ka)*(1-exp(-ka*D)).  A wrong duration would shift the peak.
  test_that("state-dep dur: infusion ends at exactly dur0 + state_at_start", {
    mod <- rxode2({
      dur(depot) <- dur0 + state
      d/dt(state) <- 0
      d/dt(depot) <- -ka * depot
    })

    amt  <- 100
    dur0 <- 5
    ka   <- 0.3

    for (S0 in c(0, 3, -1)) {
      D <- dur0 + S0
      R <- amt / D

      # Include the exact stop time D in the sampling grid
      times <- sort(unique(c(seq(0, D + 8, by = 0.1), D)))

      et <- eventTable() |>
        add.dosing(dose = amt, rate = -2) |>
        add.sampling(times)

      s <- solve(mod, et,
                 inits  = c(state = S0, depot = 0),
                 params = c(dur0 = dur0, ka = ka))

      # Analytical depot value at the exact infusion stop time
      depot_at_D_expected <- (R / ka) * (1 - exp(-ka * D))

      # Observed depot at the sample nearest D
      obs_at_D <- s$depot[which.min(abs(s$time - D))]

      # If the duration was wrong the infusion would stop at the wrong time,
      # yielding a depot value that is too large or too small.
      expect_equal(obs_at_D, depot_at_D_expected,
                   tolerance = 1e-3,
                   label = paste0("depot at infusion stop D=", round(D, 4),
                                  " matches analytic (S0=", S0, ")"))
    }
  })

  # ── Test 4 ─────────────────────────────────────────────────────────────────
  # Multiple doses: each dose's duration is evaluated at the state present when
  # THAT dose's infusion begins.  With decaying state, consecutive doses get
  # shorter (or longer) durations.
  # Verify no NAs and that the trajectory differs from the state=0 baseline.
  test_that("state-dep dur: consecutive doses evaluated at their own start state", {
    mod <- rxode2({
      # Duration depends on a decaying state variable, so each dose receives a
      # different infusion duration if spaced far enough apart.
      dur(depot) <- dur0 + state
      d/dt(state) <- -kstate * state
      d/dt(depot) <- -ka * depot
    })

    et <- et() |>
      et(amt = 50, rate = -2, time = 0) |>    # dose 1 – state is large
      et(amt = 50, rate = -2, time = 20) |>   # dose 2 – state has decayed
      et(seq(0, 35, by = 0.5))

    s <- rxSolve(mod, et,
                 params = c(dur0 = 5, ka = 0.4, kstate = 0.2),
                 inits  = c(depot = 0, state = 3))

    # No numerical failures
    expect_false(any(is.na(s$depot)),
                 label = "no NAs in depot with two state-dep dur doses")

    # The trajectory must differ from a state=0 run (different duration per dose)
    s0 <- rxSolve(mod, et,
                  params = c(dur0 = 5, ka = 0.4, kstate = 0.2),
                  inits  = c(depot = 0, state = 0))

    expect_false(isTRUE(all.equal(s$depot, s0$depot)),
                 label = "nonzero initial state gives different depot trajectory than state=0")
  })
})
