rxTest({
  # Steady-state dosing brings delay()-driven states to steady state, with the
  # delay history taken from the converged dosing interval (rxode2#1447).
  .ddeSsMod <- function() {
    model({
      R(0) <- 100
      ka <- 1.2
      V <- 30
      Cl <- 3
      Kin <- 10
      Kout <- 0.1
      Imax <- 0.8
      IC50 <- 1
      d / dt(depot) <- -ka * depot
      alag(depot) <- lagD
      d / dt(central) <- ka * depot - Cl / V * central
      Cd <- delay(central, tau) / V
      d / dt(R) <- Kin * (1 - Imax * Cd / (Cd + IC50)) - Kout * R
    })
  }
  # no x(0): an ss lag with an x(0) statement is a separate issue
  .ddeSsModNoIni <- function() {
    model({
      ka <- 1.2
      V <- 30
      Cl <- 3
      d / dt(depot) <- -ka * depot
      alag(depot) <- lagD
      d / dt(central) <- ka * depot - Cl / V * central
      Cd <- delay(central, tau) / V
      d / dt(R) <- 10 * (1 - 0.8 * Cd / (Cd + 1)) - 0.1 * R
    })
  }
  .obs <- c(0.25, 1, 3, 12, 24)
  # one steady-state dose at `t0` versus the same regimen written as 40
  # explicit doses ending at `t0`
  .ddeSsCompare <- function(method, tau, lagD = 0, rate = 0, t0 = 0,
                            mod = .ddeSsMod) {
    .dose <- data.frame(
      id = 1, time = t0, evid = 1, amt = 100, cmt = "depot",
      rate = rate, ss = 1, ii = 24
    )
    .o <- data.frame(
      id = 1, time = t0 + .obs, evid = 0, amt = NA, cmt = NA,
      rate = 0, ss = 0, ii = 0
    )
    .ss <- rbind(.dose, .o)
    .expl <- rbind(
      data.frame(
        id = 1, time = t0 - 24 * (40:1), evid = 1, amt = 100,
        cmt = "depot", rate = rate, ss = 0, ii = 0
      ),
      transform(.dose, ss = 0, ii = 0),
      .o
    )
    .p <- c(tau = tau, lagD = lagD)
    .s1 <- suppressWarnings(rxSolve(mod(), .ss,
      params = .p, method = method,
      returnType = "data.frame"
    ))
    .s2 <- suppressWarnings(rxSolve(mod(), .expl,
      params = .p, method = method,
      returnType = "data.frame"
    ))
    .s2 <- .s2[.s2$time >= t0 + 0.2, ]
    expect_equal(.s1$time, .s2$time)
    expect_equal(.s1$central, .s2$central, tolerance = 1e-4)
    expect_equal(.s1$R, .s2$R, tolerance = 1e-4)
    # the output pass reads the same shifted history
    expect_equal(.s1$Cd, .s2$Cd, tolerance = 1e-4)
  }

  for (.meth in c("dop853", "dop853+ros4", "ros4", "dop853s")) {
    test_that(paste0("ss=1 bolus brings delay() states to steady state (#1447, ", .meth, ")"), {
      .ddeSsCompare(.meth, tau = 4)
      # delay longer than the dosing interval
      .ddeSsCompare(.meth, tau = 30)
    })
  }

  test_that("ss=1 delay() steady state with an infusion, a lag and a later dose time (#1447)", {
    .ddeSsCompare("dop853", tau = 4, rate = 50)
    .ddeSsCompare("dop853", tau = 4, lagD = 1.5, mod = .ddeSsModNoIni)
    .ddeSsCompare("dop853", tau = 30, t0 = 48)
  })

  test_that("ss=1 after earlier doses replaces the delay() history (#1447)", {
    .ev <- data.frame(
      id = 1, time = c(0, 48, 48 + .obs), evid = c(1, 1, rep(0, 5)),
      amt = c(500, 100, rep(NA, 5)), cmt = c("depot", "depot", rep(NA, 5)),
      ss = c(0, 1, rep(0, 5)), ii = c(0, 24, rep(0, 5))
    )
    .ref <- data.frame(
      id = 1, time = c(48, 48 + .obs), evid = c(1, rep(0, 5)),
      amt = c(100, rep(NA, 5)), cmt = c("depot", rep(NA, 5)),
      ss = c(1, rep(0, 5)), ii = c(24, rep(0, 5))
    )
    .p <- c(tau = 4, lagD = 0)
    .s1 <- rxSolve(.ddeSsMod(), .ev, params = .p, returnType = "data.frame")
    .s2 <- rxSolve(.ddeSsMod(), .ref, params = .p, returnType = "data.frame")
    .s1 <- .s1[.s1$time > 48, ]
    .s2 <- .s2[.s2$time > 48, ]
    expect_equal(.s1$R, .s2$R, tolerance = 1e-6)
  })

  test_that("ss=2 with delay() solves (#1447)", {
    .ev <- data.frame(
      id = 1, time = c(0, 48, 48 + .obs), evid = c(1, 1, rep(0, 5)),
      amt = c(500, 100, rep(NA, 5)), cmt = c("depot", "depot", rep(NA, 5)),
      ss = c(0, 2, rep(0, 5)), ii = c(0, 24, rep(0, 5))
    )
    .s <- rxSolve(.ddeSsMod(),
      .ev,
      params = c(tau = 4, lagD = 0),
      returnType = "data.frame"
    )
    expect_true(all(is.finite(.s$R)))
    expect_true(all(is.finite(.s$Cd)))
  })
})
