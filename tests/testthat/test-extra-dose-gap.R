rxTest({
  # A steady-state dose with alag() > ii pushes several doses; every one that
  # falls between two observations is given, not just the first (rxode2#1590)
  .gapMod <- rxode2({
    alag(central) <- lag
    d / dt(central) <- -0.1 * central
    cp <- central / 20
  })
  .gapRef <- rxode2({
    d / dt(central) <- -0.1 * central
    cp <- central / 20
  })
  .ii <- 12
  .t <- c(0.5, 1.5, 6, 11.5, 12.5, 13.5, 16, 24.5, 30, 37, 45, 50, 61, 75)
  # explicit doses arriving at lag - m*ii, no ss and no alag()
  .gapPhys <- function(lag, rate) {
    .st <- lag - .ii * (0:150)
    .ev <- if (rate == 0) {
      et(time = .st, amt = 100, cmt = "central")
    } else {
      et(time = .st, amt = 100, rate = rate, cmt = "central")
    }
    .r <- suppressWarnings(rxSolve(
      .gapRef,
      .ev |> et(.t),
      atol = 1e-10,
      rtol = 1e-10,
      maxsteps = 1e6,
      returnType = "data.frame"
    ))
    .r$cp[match(.t, .r$time)]
  }

  test_that("every extra dose between two observations is given (#1590)", {
    # lag/rate: long infusions (dur 20 and 50 > ii) and a bolus
    for (.case in list(c(36, 5), c(25, 2), c(30, 0))) {
      .ref <- .gapPhys(.case[1], .case[2])
      .ev <- if (.case[2] == 0) {
        et(amt = 100, ss = 1, ii = .ii, cmt = "central")
      } else {
        et(amt = 100, rate = .case[2], ss = 1, ii = .ii, cmt = "central")
      }
      for (.meth in c("liblsoda", "lsoda", "dop853", "rk4", "cvode", "indLin")) {
        .ss <- rxSolve(
          .gapMod,
          .ev |> et(.t),
          params = c(lag = .case[1]),
          method = .meth,
          atol = 1e-10,
          rtol = 1e-10,
          returnType = "data.frame"
        )$cp
        expect_equal(.ss, .ref, tolerance = 1e-4)
      }
    }
  })
})
