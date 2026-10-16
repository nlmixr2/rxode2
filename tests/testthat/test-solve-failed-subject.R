rxTest({
  test_that("a subject that fails leaves the later subjects solved, whatever the method", {
    # a modeled rate (rate = -1) in a model with no rate() fails subject 1 in every
    # driver; cl = 1, v = 10 and a 100 bolus give cp(1) = 10 exp(-0.1)
    .m <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
    })
    .d <- data.frame(
      ID = rep(1:3, each = 4),
      TIME = rep(c(0, 0.5, 1, 2), 3),
      AMT = rep(c(100, 0, 0, 0), 3),
      EVID = rep(c(1, 0, 0, 0), 3),
      CMT = "central",
      RATE = 0
    )
    .d$RATE[.d$ID == 1 & .d$EVID == 1] <- -1
    .msg <- "some ID(s) could not solve the ODEs correctly; These values are replaced with 'NA'"
    # one or more of each driver family: lsoda, dop853, the boost odeint ports
    # and the fixed-step ones
    for (.meth in c(
      "liblsoda",
      "lsoda",
      "dop853",
      "rk4",
      "ck54",
      "ab",
      "dop5",
      "heun",
      "euler",
      "midpoint",
      "rk3",
      "cvode",
      "trapz",
      "vern65",
      "ros4"
    )) {
      .w <- capture_warnings(
        capture.output(.s <- rxSolve(.m, .d, params = c(cl = 1, v = 10), method = .meth, cores = 1L))
      )
      expect_identical(.w, .msg, label = .meth)
      .s <- as.data.frame(.s)
      expect_identical(as.integer(as.character(unique(.s$id))), 1:3, label = .meth)
      .cp <- .s$cp[.s$time == 1]
      expect_true(is.na(.cp[1]), label = .meth)
      # 1e-4 covers euler at its default step (5e-5 off); the others are within 1e-6
      expect_equal(.cp[2:3], rep(10 * exp(-0.1), 2), tolerance = 1e-4, label = .meth)
    }
  })
})
