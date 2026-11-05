rxTest({
  # A steady-state record schedules extra doses (infusion starts and ends) that
  # can fall between two records; each one has to be applied at its own time,
  # however many fall between the same two records
  .ssExtraMod <- rxode2({
    d / dt(depot) <- -ka * depot
    d / dt(c) <- ka * depot - k * c
    alag(depot) <- lag
  })
  .ssExtraLinCmt <- function() {
    ini({
      cl <- 0.5
      v <- 10
      ka <- 0.7
      lag <- 14
    })
    model({
      alag(depot) <- lag
      cp <- linCmt(cl, v, ka)
    })
  }
  .ii <- 12
  .tobs <- c(0.5, 1.5, 3, 7, 11, 13, 15, 20, 23.9)
  .nAddl <- 100
  .ssExtraSolve <- function(ev, lag, method = "liblsoda") {
    rxSolve(
      .ssExtraMod,
      ev,
      params = c(k = 0.05, ka = 0.7, lag = lag),
      method = method,
      returnType = "data.frame"
    )$c
  }
  .ssExtraMethods <- c("liblsoda", "lsoda", "lsode", "dop853", "indLin")

  test_that("a steady-state infusion with dur > ii matches explicit dosing", {
    for (.lag in c(0, 2)) {
      .ref <- .ssExtraSolve(
        et(amt = 100, ii = .ii, addl = .nAddl, dur = 20) |>
          et(.tobs + .nAddl * .ii),
        .lag
      )
      for (.meth in .ssExtraMethods) {
        expect_equal(
          .ssExtraSolve(et(amt = 100, ss = 1, ii = .ii, dur = 20) |> et(.tobs), .lag, .meth),
          .ref,
          tolerance = 1e-5
        )
      }
    }
  })

  test_that("a steady-state infusion with alag() > ii matches explicit dosing (#1499)", {
    for (.lag in c(14, 26)) {
      for (.dur in c(0.5, 2, 11.5)) {
        .ref <- .ssExtraSolve(
          et(amt = 100, ii = .ii, addl = .nAddl, dur = .dur) |>
            et(.tobs + .nAddl * .ii),
          .lag
        )
        for (.meth in .ssExtraMethods) {
          expect_equal(
            .ssExtraSolve(et(amt = 100, ss = 1, ii = .ii, dur = .dur) |> et(.tobs), .lag, .meth),
            .ref,
            tolerance = 1e-5
          )
        }
      }
    }
  })

  test_that("a linCmt() steady-state infusion with alag() > ii matches explicit dosing (#1499)", {
    .ss <- rxSolve(
      .ssExtraLinCmt,
      et(amt = 100, ss = 1, ii = .ii, dur = 0.5) |> et(.tobs),
      returnType = "data.frame"
    )$cp
    .ex <- rxSolve(
      .ssExtraLinCmt,
      et(amt = 100, ii = .ii, addl = .nAddl, dur = 0.5) |>
        et(.tobs + .nAddl * .ii),
      returnType = "data.frame"
    )$cp
    expect_equal(.ss, .ex, tolerance = 1e-5)
  })
})
