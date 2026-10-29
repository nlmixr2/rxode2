rxTest({
  # A linCmt() steady-state bolus whose alag() is longer than ii (rxode2#1591)
  .linLagMod <- rxode2({
    cl <- 2
    v <- 20
    ka <- 0.15
    alag(depot) <- lag
    cp <- linCmt()
  })
  .ii <- 12
  .tobs <- c(0.5, 3, 7, 11, 13, 20, 27)
  .nAddl <- 400
  .linLagSolve <- function(ev, lag, ...) {
    rxSolve(.linLagMod, ev, params = c(lag = lag), returnType = "data.frame", ...)$cp
  }

  test_that("a linCmt() steady-state bolus with alag() > ii matches NONMEM ADVAN2 (#1591)", {
    .d <- data.frame(
      ID = 1,
      TIME = c(10, 16, 22, 24, 30),
      EVID = c(1, 0, 0, 0, 0),
      AMT = c(100, 0, 0, 0, 0),
      CMT = c(1, 2, 2, 2, 2),
      II = c(12, 0, 0, 0, 0),
      SS = c(1, 0, 0, 0, 0)
    )
    expect_equal(
      .linLagSolve(.d, 13),
      c(4.5306314936372161, 3.6939077991145908, 3.9551518979304241, 4.3707407381421977),
      tolerance = 1e-7
    )
  })

  test_that("a linCmt() steady-state bolus with alag() > ii matches explicit dosing (#1591)", {
    .explicitEv <- et(amt = 100, ii = .ii, addl = .nAddl) |>
      et(.tobs + .nAddl * .ii)
    for (.lag in c(2, 13, 24, 26, 30)) {
      .ref <- .linLagSolve(.explicitEv, .lag)
      .ss <- et(amt = 100, ss = 1, ii = .ii) |> et(.tobs)
      expect_equal(.linLagSolve(.ss, .lag), .ref, tolerance = 1e-6)
      expect_equal(.linLagSolve(.ss, .lag, ssSolved = FALSE), .ref, tolerance = 1e-6)
      expect_equal(
        .linLagSolve(et(amt = 100, ss = 2, ii = .ii) |> et(.tobs), .lag),
        .ref,
        tolerance = 1e-6
      )
    }
  })
})
