rxTest({
  # tieOrder with events that expand to several records at one time: evid=4
  # (reset + dose), infusions (on + off) and doses pushed while solving.  All
  # models have k = cl/v = 0.1 and v = 20, so they share the analytic answers.
  .k <- 0.1
  .v <- 20
  .obs <- c(5, 9.99, 10, 10.01, 12, 15, 20, 25, 30)

  .tieOde <- function() {
    rxode2({
      d / dt(central) <- -0.1 * central
      cp <- central / 20
    })
  }

  .tieLin <- function() {
    rxode2({
      cl <- 2
      v <- 20
      cp <- linCmt()
    })
  }

  # concentration from a bolus `a` at `t0`
  .bol <- function(t, a, t0) {
    ifelse(t >= t0, a * exp(-.k * (t - t0)), 0) / .v
  }
  # concentration from an infusion at `rate` running from `t0` to `t1`
  .inf <- function(t, rate, t0, t1) {
    .in <- function(tt) rate / .k * (1 - exp(-.k * (tt - t0)))
    ifelse(t < t0, 0, ifelse(t <= t1, .in(t), .in(t1) * exp(-.k * (t - t1)))) / .v
  }

  .row <- function(time, evid, amt = 0, rate = 0, ii = 0, addl = 0, ss = 0) {
    data.frame(
      ID = 1,
      TIME = time,
      EVID = evid,
      AMT = amt,
      CMT = 1,
      RATE = rate,
      II = ii,
      ADDL = addl,
      SS = ss
    )
  }

  # the dose rows in the order given, then the observations; `obsFirst` lists
  # each observation before the dose rows at its time
  .tieData <- function(..., obsFirst = FALSE) {
    .doses <- rbind(...)
    .o <- .row(.obs, 0)
    .d <- if (obsFirst) rbind(.o, .doses) else rbind(.doses, .o)
    .d[order(.d$TIME), ]
  }

  .tieSolve <- function(mod, d, tieOrder) {
    rxSolve(mod, d, tieOrder = tieOrder, addDosing = FALSE, returnType = "data.frame")$cp
  }

  # both tie orders give `want`
  .expectBoth <- function(mod, d, want, tol) {
    expect_equal(.tieSolve(mod, d, "evid"), want, tolerance = tol)
    expect_equal(.tieSolve(mod, d, "data"), want, tolerance = tol)
  }

  for (.m in c("ode", "linCmt")) {
    .mod <- if (.m == "ode") .tieOde() else .tieLin()
    .tol <- if (.m == "ode") 1e-5 else 1e-8

    test_that(paste0("an evid=4 bolus or infusion does not depend on tieOrder (", .m, ")"), {
      .expectBoth(
        .mod,
        .tieData(.row(0, 1, 100), .row(10, 4, 100)),
        ifelse(.obs < 10, .bol(.obs, 100, 0), .bol(.obs, 100, 10)),
        .tol
      )
      .expectBoth(
        .mod,
        .tieData(.row(0, 1, 100), .row(10, 4, 100, rate = 10)),
        ifelse(.obs < 10, .bol(.obs, 100, 0), .inf(.obs, 10, 10, 20)),
        .tol
      )
    })

    test_that(paste0("an infusion ending at a co-timed evid=4 infusion does not depend on tieOrder (", .m, ")"), {
      # the first infusion's off record ties with the reset and the new start
      .expectBoth(
        .mod,
        .tieData(.row(0, 1, 100, rate = 10), .row(10, 4, 100, rate = 10)),
        ifelse(.obs < 10, .inf(.obs, 10, 0, 10), .inf(.obs, 10, 10, 20)),
        .tol
      )
    })

    test_that(paste0("evid=4 infusions repeated with addl and dur == ii do not depend on tieOrder (", .m, ")"), {
      # each repeat's start ties with the previous one's off record
      .expectBoth(
        .mod,
        .tieData(.row(0, 4, 100, rate = 10, ii = 10, addl = 2)),
        .inf(.obs, 10, 0, 30),
        .tol
      )
    })

    test_that(paste0("a reset at an infusion's end leaves the system empty in both tie orders (", .m, ")"), {
      .expectBoth(
        .mod,
        .tieData(.row(0, 1, 100, rate = 10), .row(10, 3)),
        ifelse(.obs < 10, .inf(.obs, 10, 0, 10), 0),
        .tol
      )
    })

    test_that(paste0("tieOrder='data' lets an evid=4 infusion wipe an infusion listed before it (", .m, ")"), {
      .d <- .tieData(.row(10, 1, 100, rate = 10), .row(10, 4, 50, rate = 5))
      # data order: the first infusion starts, then the reset wipes it; its
      # off record at t=20 must not leave a negative rate behind
      expect_equal(.tieSolve(.mod, .d, "data"), .inf(.obs, 5, 10, 20), tolerance = .tol)
      # evid order: the reset comes first, so both infusions run
      expect_equal(.tieSolve(.mod, .d, "evid"), .inf(.obs, 15, 10, 20), tolerance = .tol)
    })

    test_that(paste0("an evid=4 infusion listed before a co-timed bolus keeps both in both tie orders (", .m, ")"), {
      .expectBoth(
        .mod,
        .tieData(.row(0, 1, 100), .row(10, 4, 100, rate = 10), .row(10, 1, 100)),
        ifelse(.obs < 10, .bol(.obs, 100, 0), .bol(.obs, 100, 10) + .inf(.obs, 10, 10, 20)),
        .tol
      )
    })

    test_that(
      paste0("tieOrder='data' predicts an observation listed before an evid=4 infusion before the reset (", .m, ")"),
      {
        .d <- .tieData(.row(0, 1, 100), .row(10, 4, 50, rate = 10), obsFirst = TRUE)
        .after <- ifelse(.obs < 10, .bol(.obs, 100, 0), .inf(.obs, 10, 10, 15))
        .before <- .after
        .before[.obs == 10] <- .bol(10, 100, 0)
        expect_equal(.tieSolve(.mod, .d, "data"), .before, tolerance = .tol)
        expect_equal(.tieSolve(.mod, .d, "evid"), .after, tolerance = .tol)
      }
    )
  }

  test_that("a modeled-duration infusion ending at a co-timed evid=4 modeled infusion does not depend on tieOrder", {
    .mod <- rxode2({
      d / dt(central) <- -0.1 * central
      dur(central) <- 10
      cp <- central / 20
    })
    .expectBoth(
      .mod,
      .tieData(.row(0, 1, 100, rate = -2), .row(10, 4, 100, rate = -2)),
      ifelse(.obs < 10, .inf(.obs, 10, 0, 10), .inf(.obs, 10, 10, 20)),
      1e-5
    )
    .expectBoth(
      .mod,
      .tieData(.row(0, 4, 100, rate = -2, ii = 10, addl = 2)),
      .inf(.obs, 10, 0, 30),
      1e-5
    )
  })

  test_that("a modeled-rate infusion ending at a co-timed evid=4 modeled infusion does not depend on tieOrder", {
    .mod <- rxode2({
      d / dt(central) <- -0.1 * central
      rate(central) <- 10
      cp <- central / 20
    })
    .expectBoth(
      .mod,
      .tieData(.row(0, 1, 100, rate = -1), .row(10, 4, 100, rate = -1)),
      ifelse(.obs < 10, .inf(.obs, 10, 0, 10), .inf(.obs, 10, 10, 20)),
      1e-5
    )
  })

  test_that("a pushed reset() during an infusion leaves the system empty in both tie orders", {
    .mod <- rxode2({
      d / dt(central) <- -0.1 * central
      cp <- central / 20
      if (t == 10) {
        reset()
      }
    })
    .want <- ifelse(.obs <= 10, .inf(.obs, 10, 5, 15), 0)
    .expectBoth(.mod, .tieData(.row(5, 1, 100, rate = 10)), .want, 1e-5)
  })

  test_that("a pushed evid=4 dose follows the co-timed data records with tieOrder='data'", {
    .mod <- rxode2({
      d / dt(central) <- -0.1 * central
      cp <- central / 20
      if (t == 10) {
        evid_(t, 4, 50, 1, 0, 0, 0, 0)
      }
    })
    .after <- ifelse(.obs < 10, .bol(.obs, 100, 0), .bol(.obs, 50, 10))
    # fired while evaluating the observation at t=10: that observation is
    # computed before the pushed reset + dose in both tie orders
    .before <- .after
    .before[.obs == 10] <- .bol(10, 100, 0)
    .expectBoth(.mod, .tieData(.row(0, 1, 100)), .before, 1e-5)
    # fired at the co-timed data dose: evid order applies the pushed reset +
    # dose before the observation, data order after it; the reset wipes the
    # data dose either way
    .d <- .tieData(.row(0, 1, 100), .row(10, 1, 100))
    expect_equal(.tieSolve(.mod, .d, "evid"), .after, tolerance = 1e-5)
    .before[.obs == 10] <- .bol(10, 100, 0) + .bol(10, 100, 10)
    expect_equal(.tieSolve(.mod, .d, "data"), .before, tolerance = 1e-5)
  })
})
