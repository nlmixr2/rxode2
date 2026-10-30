rxTest({
  # A steady-state infusion whose alag() is an exact multiple of ii keeps the
  # record's own lagged infusion (rxode2#1590)
  .ssInfLagMod <- rxode2({
    alag(central) <- lag
    rate(central) <- mrate
    dur(central) <- mdur
    d / dt(central) <- -0.1 * central
    cp <- central / 20
  })
  .ssInfLagRef <- rxode2({
    d / dt(central) <- -0.1 * central
    cp <- central / 20
  })
  .ii <- 12
  .t <- c(0.5, 1.5, 6, 11.5, 12.5, 13.5, 16, 24.5, 30, 37, 45)
  # explicit infusions starting at lag - m*ii, no ss and no alag()
  .ssInfLagPhys <- function(lag) {
    .ev <- et(time = lag - .ii * (0:150), amt = 100, rate = 50, cmt = "central") |>
      et(.t)
    .r <- suppressWarnings(rxSolve(
      .ssInfLagRef,
      .ev,
      atol = 1e-10,
      rtol = 1e-10,
      maxsteps = 1e6,
      returnType = "data.frame"
    ))
    .r$cp[match(.t, .r$time)]
  }

  test_that("a steady-state infusion with alag() a multiple of ii keeps its own dose (#1590)", {
    for (.lag in c(12, 24, 36)) {
      .ref <- .ssInfLagPhys(.lag)
      for (.rate in c(50, -1, -2)) {
        for (.meth in c("liblsoda", "lsoda", "rk4")) {
          for (.nonmem in c(TRUE, FALSE)) {
            .ss <- rxSolve(
              .ssInfLagMod,
              et(amt = 100, rate = .rate, ss = 1, ii = .ii, cmt = "central") |>
                et(.t),
              params = c(lag = .lag, mrate = 50, mdur = 2),
              method = .meth,
              nonmem = .nonmem,
              atol = 1e-10,
              rtol = 1e-10,
              ssAtol = 1e-10,
              ssRtol = 1e-10,
              returnType = "data.frame"
            )$cp
            expect_equal(.ss, .ref, tolerance = 1e-6)
          }
        }
      }
    }
  })
})
