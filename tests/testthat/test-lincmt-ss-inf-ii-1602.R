rxTest({
  # The linCmt() steady state of an infusion lasting ii, or of a constant
  # infusion, is solved to a set time rather than an unset one (#1602).
  .o <- c(4, 9, 15, 20)
  .p <- c(ka = 1.2, cl = 0.5, v = 10)
  .lin <- rxode2({
    cp <- linCmt(v, cl, ka)
    f(depot) <- fd
  })
  .ode <- rxode2({
    d / dt(depot) <- -ka * depot
    d / dt(central) <- ka * depot - cl / v * central
    cp <- central / v
    f(depot) <- fd
  })
  test_that("a linCmt() ss infusion lasting ii matches the ODE model", {
    # 200 at rate 20 lasts 10 * f, the interval in each case
    for (.fd in c(1, 1.2)) {
      for (.ss in 1:2) {
        .ev <- et(amt = 100, cmt = "depot") |>
          et(time = 3, amt = 200, rate = 20, ii = 10 * .fd, ss = .ss, cmt = "depot") |>
          et(.o)
        .p2 <- c(.p, fd = .fd)
        expect_equal(rxSolve(.lin, .ev, .p2)$cp, rxSolve(.ode, .ev, .p2)$cp, tolerance = 1e-5)
      }
    }
  })
  test_that("a linCmt() constant ss infusion matches the ODE model", {
    .ev <- et(time = 3, amt = 0, rate = 20, ii = 0, ss = 1, cmt = "depot") |>
      et(.o)
    .p2 <- c(.p, fd = 0.8)
    expect_equal(rxSolve(.lin, .ev, .p2)$cp, rxSolve(.ode, .ev, .p2)$cp, tolerance = 1e-5)
  })
})
