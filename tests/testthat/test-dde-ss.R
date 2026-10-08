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
  .ddeSsCompare <- function(method, tau, lagD = 0, rate = 0, t0 = 0, mod = .ddeSsMod) {
    .dose <- data.frame(
      id = 1,
      time = t0,
      evid = 1,
      amt = 100,
      cmt = "depot",
      rate = rate,
      ss = 1,
      ii = 24
    )
    .o <- data.frame(
      id = 1,
      time = t0 + .obs,
      evid = 0,
      amt = NA,
      cmt = NA,
      rate = 0,
      ss = 0,
      ii = 0
    )
    .ss <- rbind(.dose, .o)
    .expl <- rbind(
      data.frame(
        id = 1,
        time = t0 - 24 * (40:1),
        evid = 1,
        amt = 100,
        cmt = "depot",
        rate = rate,
        ss = 0,
        ii = 0
      ),
      transform(.dose, ss = 0, ii = 0),
      .o
    )
    .p <- c(tau = tau, lagD = lagD)
    .s1 <- suppressWarnings(rxSolve(mod(), .ss, params = .p, method = method, returnType = "data.frame"))
    .s2 <- suppressWarnings(rxSolve(mod(), .expl, params = .p, method = method, returnType = "data.frame"))
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
      # delays longer than the dosing interval (and than the steady-state run)
      .ddeSsCompare(.meth, tau = 30)
      .ddeSsCompare(.meth, tau = 300)
    })
  }

  test_that("ss=1 delay() steady state with an infusion, a lag and a later dose time (#1447)", {
    .ddeSsCompare("dop853", tau = 4, rate = 50)
    .ddeSsCompare("dop853", tau = 4, lagD = 1.5, mod = .ddeSsModNoIni)
    .ddeSsCompare("dop853", tau = 30, t0 = 48)
  })

  test_that("a constant steady-state infusion (ii = 0) with delay() (#1447)", {
    .ss <- data.frame(
      id = 1,
      time = c(0, .obs),
      evid = c(1, rep(0, 5)),
      amt = c(0, rep(NA, 5)),
      rate = c(10, rep(0, 5)),
      cmt = c("central", rep(NA, 5)),
      ss = c(1, rep(0, 5)),
      ii = 0
    )
    # the same infusion run long enough to reach steady state, ending at 0
    # (the ss record sets the steady state; the infusion then stops)
    .expl <- data.frame(
      id = 1,
      time = c(-3000, .obs),
      evid = c(1, rep(0, 5)),
      amt = c(30000, rep(NA, 5)),
      rate = c(10, rep(0, 5)),
      cmt = c("central", rep(NA, 5)),
      ss = 0,
      ii = 0
    )
    for (.tau in c(4, 300)) {
      .p <- c(tau = .tau, lagD = 0)
      .s1 <- rxSolve(.ddeSsMod(), .ss, params = .p, returnType = "data.frame")
      .s2 <- suppressWarnings(rxSolve(.ddeSsMod(), .expl, params = .p, returnType = "data.frame"))
      .s2 <- .s2[.s2$time > 0, ]
      expect_equal(.s1$central, .s2$central, tolerance = 1e-4)
      expect_equal(.s1$R, .s2$R, tolerance = 1e-4)
      expect_equal(.s1$Cd, .s2$Cd, tolerance = 1e-4)
    }
  })

  test_that("ss=1 after earlier doses replaces the delay() history (#1447)", {
    .ev <- data.frame(
      id = 1,
      time = c(0, 48, 48 + .obs),
      evid = c(1, 1, rep(0, 5)),
      amt = c(500, 100, rep(NA, 5)),
      cmt = c("depot", "depot", rep(NA, 5)),
      ss = c(0, 1, rep(0, 5)),
      ii = c(0, 24, rep(0, 5))
    )
    .ref <- data.frame(
      id = 1,
      time = c(48, 48 + .obs),
      evid = c(1, rep(0, 5)),
      amt = c(100, rep(NA, 5)),
      cmt = c("depot", rep(NA, 5)),
      ss = c(1, rep(0, 5)),
      ii = c(24, rep(0, 5))
    )
    .p <- c(tau = 4, lagD = 0)
    .s1 <- rxSolve(.ddeSsMod(), .ev, params = .p, returnType = "data.frame")
    .s2 <- rxSolve(.ddeSsMod(), .ref, params = .p, returnType = "data.frame")
    .s1 <- .s1[.s1$time > 48, ]
    .s2 <- .s2[.s2$time > 48, ]
    expect_equal(.s1$R, .s2$R, tolerance = 1e-6)
  })

  test_that("ss=2 adds the steady-state delay() history (#1447)", {
    # linear in the doses, so ss=2 is exact superposition
    .lin <- function() {
      model({
        ka <- 1.2
        V <- 30
        Cl <- 3
        d / dt(depot) <- -ka * depot
        d / dt(central) <- ka * depot - Cl / V * central
        Cd <- delay(central, tau) / V
        d / dt(E) <- 0.5 * Cd - 0.2 * E
      })
    }
    .ev <- data.frame(
      id = 1,
      time = c(0, 48, 48 + .obs),
      evid = c(1, 1, rep(0, 5)),
      amt = c(500, 100, rep(NA, 5)),
      cmt = c("depot", "depot", rep(NA, 5)),
      ss = c(0, 2, rep(0, 5)),
      ii = c(0, 24, rep(0, 5))
    )
    .expl <- data.frame(
      id = 1,
      time = c(48 - 24 * (40:1), 0, 48, 48 + .obs),
      evid = c(rep(1, 42), rep(0, 5)),
      amt = c(rep(100, 40), 500, 100, rep(NA, 5)),
      cmt = c(rep("depot", 42), rep(NA, 5))
    )
    for (.meth in c("dop853", "ros4")) {
      for (.tau in c(4, 30, 300)) {
        .p <- c(tau = .tau)
        .s1 <- rxSolve(.lin(), .ev, params = .p, method = .meth, returnType = "data.frame")
        .s2 <- suppressWarnings(rxSolve(.lin(), .expl, params = .p, method = .meth, returnType = "data.frame"))
        .s1 <- .s1[.s1$time > 48, ]
        .s2 <- .s2[.s2$time > 48, ]
        expect_equal(.s1$central, .s2$central, tolerance = 1e-4)
        expect_equal(.s1$E, .s2$E, tolerance = 1e-4)
        expect_equal(.s1$Cd, .s2$Cd, tolerance = 1e-4)
      }
    }
  })

  test_that("delay() of a dosed state on a fine grid after ss=1 and ss=2 (#1447)", {
    # the dosed state jumps at every dose, so lookups hit history boundaries
    .jump <- function() {
      model({
        d / dt(depot) <- -1.2 * depot
        d / dt(central) <- 1.2 * depot - 0.1 * central
        Dd <- delay(depot, tau)
        d / dt(E) <- 0.3 * Dd - 0.2 * E
      })
    }
    .fine <- seq(48.05, 48 + 60, by = 0.35)
    for (.ss in 1:2) {
      .ev <- data.frame(
        id = 1,
        time = c(0, 48, .fine),
        evid = c(1, 1, rep(0, length(.fine))),
        amt = c(500, 100, rep(NA, length(.fine))),
        cmt = c("depot", "depot", rep(NA, length(.fine))),
        ss = c(0, .ss, rep(0, length(.fine))),
        ii = c(0, 24, rep(0, length(.fine)))
      )
      .pre <- if (.ss == 2) 0 else numeric(0)
      .expl <- data.frame(
        id = 1,
        time = c(48 - 24 * (40:1), .pre, 48, .fine),
        evid = c(rep(1, 41 + length(.pre)), rep(0, length(.fine))),
        amt = c(rep(100, 40), rep(500, length(.pre)), 100, rep(NA, length(.fine))),
        cmt = c(rep("depot", 41 + length(.pre)), rep(NA, length(.fine)))
      )
      for (.tau in c(5, 30)) {
        .p <- c(tau = .tau)
        .s1 <- rxSolve(.jump(), .ev, params = .p, returnType = "data.frame")
        .s2 <- suppressWarnings(rxSolve(.jump(), .expl, params = .p, returnType = "data.frame"))
        .s1 <- .s1[.s1$time > 48, ]
        .s2 <- .s2[.s2$time > 48, ]
        expect_equal(.s1$Dd, .s2$Dd, tolerance = 1e-4)
        expect_equal(.s1$E, .s2$E, tolerance = 1e-4)
      }
    }
  })
})
