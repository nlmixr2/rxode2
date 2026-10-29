rxTest({
  # A steady-state dose whose alag() is longer than ii has the doses of the
  # earlier intervals arriving inside the observed one (rxode2#1499)
  .ssLagMod <- rxode2({
    d / dt(depot) <- -ka * depot
    d / dt(c) <- ka * depot - k * c
    alag(depot) <- lag
    f(depot) <- fdepot
  })
  .ii <- 12
  .tobs <- c(0.5, 3, 7, 11, 13, 20, 27)
  .nAddl <- 100
  .ssLagSolve <- function(ev, lag, fdepot = 1, method = "liblsoda") {
    rxSolve(
      .ssLagMod,
      ev,
      params = c(k = 0.05, ka = 0.7, lag = lag, fdepot = fdepot),
      method = method,
      returnType = "data.frame"
    )$c
  }
  .explicit <- function(ev, lag, fdepot = 1) {
    rxSolve(
      .ssLagMod,
      ev,
      params = c(k = 0.05, ka = 0.7, lag = lag, fdepot = fdepot),
      atol = 1e-10,
      rtol = 1e-10,
      returnType = "data.frame"
    )$c
  }

  test_that("a steady-state bolus with alag() > ii matches explicit dosing (#1499)", {
    .explicitEv <- et(amt = 100, ii = .ii, addl = .nAddl) |>
      et(.tobs + .nAddl * .ii)
    for (.lag in c(2, 12, 14, 24, 26, 30)) {
      for (.f in c(1, 0.5)) {
        .ref <- .explicit(.explicitEv, .lag, .f)
        for (.meth in c("liblsoda", "lsoda", "cvode", "rk4")) {
          expect_equal(
            .ssLagSolve(et(amt = 100, ss = 1, ii = .ii) |> et(.tobs), .lag, .f, .meth),
            .ref,
            tolerance = 1e-5
          )
        }
      }
    }
  })

  test_that("an ss=2 bolus with alag() > ii onto an empty system is the ss=1 one (#1499)", {
    for (.lag in c(14, 30)) {
      expect_equal(
        .ssLagSolve(et(amt = 100, ss = 2, ii = .ii) |> et(.tobs), .lag),
        .ssLagSolve(et(amt = 100, ss = 1, ii = .ii) |> et(.tobs), .lag),
        tolerance = 1e-6
      )
      expect_equal(
        .ssLagSolve(et(amt = 100, ss = 2, ii = .ii) |> et(.tobs), .lag),
        .explicit(
          et(amt = 100, ii = .ii, addl = .nAddl) |> et(.tobs + .nAddl * .ii),
          .lag
        ),
        tolerance = 1e-5
      )
    }
  })

  test_that("a steady-state bolus with alag() > ii and addl continues the regimen (#1499)", {
    .t <- c(.tobs, 30, 40, 55)
    .ss <- et(amt = 100, ss = 1, ii = .ii, addl = 3) |> et(.t)
    .ex <- et(amt = 100, ii = .ii, addl = .nAddl + 3) |> et(.t + .nAddl * .ii)
    expect_equal(.ssLagSolve(.ss, 14), .explicit(.ex, 14), tolerance = 1e-5)
  })

  test_that("a later steady-state bolus with alag() > ii resets the earlier one (#1499)", {
    .ss <- et(amt = 100, ss = 1, ii = .ii) |>
      et(amt = 200, ss = 1, ii = .ii, time = 48) |>
      et(c(.tobs, 48 + .tobs))
    .one <- et(amt = 200, ss = 1, ii = .ii) |> et(.tobs)
    expect_equal(
      tail(.ssLagSolve(.ss, 14), length(.tobs)),
      .ssLagSolve(.one, 14),
      tolerance = 1e-6
    )
  })
})
