rxTest({
  # A modeled rate (rate(cmt), RATE = -1) is re-read at each data record until
  # the dose is delivered: by default the rate read at a record holds until the
  # next one; with nonmem = TRUE it runs on (t[j-1], t[j]] at the rate computed
  # with data record j, like NONMEM's R1 (rxode2#1586).
  .k <- 0.05
  .step <- function(a, h, r) a * exp(-.k * h) + r / .k * (1 - exp(-.k * h))

  # one-compartment amount at the observations; `rateFn(j)` is the rate read
  # from data record j, `doses` the (time, amt) of every dose, addl included
  .exact <- function(d, doses, rateFn) {
    .tRec <- d$TIME
    .rem <- numeric(0)
    .a <- 0
    .t <- 0
    .out <- numeric(0)
    .advance <- function(tEnd, r) {
      while (.t < tEnd) {
        .on <- which(.rem > 0)
        .h <- tEnd - .t
        if (length(.on) > 0) {
          .h <- min(.h, .rem[.on] / r)
        }
        .a <<- .step(.a, .h, r * length(.on))
        .rem[.on] <<- pmax(.rem[.on] - r * .h, 0)
        .rem[.rem < 1e-9] <<- 0
        .t <<- .t + .h
      }
    }
    for (.j in seq_along(.tRec)) {
      .r <- rateFn(.j)
      # doses inside (t, t[j]] start where they fall
      for (.dt in doses$time[doses$time > .t & doses$time < .tRec[.j]]) {
        .advance(.dt, .r)
        .rem <- c(.rem, doses$amt[doses$time == .dt][1])
      }
      .advance(.tRec[.j], .r)
      if (d$EVID[.j] == 0) {
        .out <- c(.out, .a)
      }
      if (d$EVID[.j] == 1) .rem <- c(.rem, d$AMT[.j])
    }
    .out
  }

  .m <- rxode2({
    cl <- 0.5
    v <- 10
    rate(central) <- 50 / X
    d/dt(central) <- -cl / v * central
    cp <- central / v
  })

  .d <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2, 6),
    EVID = c(1, 0, 0, 0),
    AMT = c(100, 0, 0, 0),
    RATE = c(-1, 0, 0, 0),
    X = c(4, 2, 1, 1)
  )

  test_that("a modeled rate is re-read from the record ending each interval", {
    .a1 <- .step(0, 1, 25)
    .a2 <- .step(.a1, 1, 50)
    .a6 <- .step(.step(.a2, 0.5, 50), 3.5, 0)
    .nm <- c(.a1, .a2, .a6) / 10
    expect_equal(
      .exact(.d, data.frame(time = numeric(0), amt = numeric(0)), function(j) 50 / .d$X[j]) / 10,
      .nm,
      tolerance = 1e-12
    )
    for (.meth in c("liblsoda", "lsoda", "dop853")) {
      for (.cov in c("nocb", "locf")) {
        .s <- rxSolve(
          .m,
          .d,
          nonmem = TRUE,
          method = .meth,
          covsInterpolation = .cov,
          addDosing = FALSE,
          atol = 1e-12,
          rtol = 1e-12,
          returnType = "data.frame"
        )
        expect_equal(.s$cp, .nm, tolerance = 1e-8, info = paste(.meth, .cov))
      }
    }
  })

  test_that("by default the rate read at a record holds until the next one", {
    .a1 <- .step(0, 1, 12.5)
    .a2 <- .step(.a1, 1, 25)
    .a6 <- .step(.step(.a2, 1.25, 50), 2.75, 0)
    .locf <- c(.a1, .a2, .a6) / 10
    expect_equal(
      .exact(.d, data.frame(time = numeric(0), amt = numeric(0)), function(j) 50 / .d$X[max(j - 1, 1)]) / 10,
      .locf,
      tolerance = 1e-12
    )
    for (.meth in c("liblsoda", "lsoda", "dop853")) {
      for (.cov in c("nocb", "locf")) {
        .s <- rxSolve(
          .m,
          .d,
          method = .meth,
          covsInterpolation = .cov,
          addDosing = FALSE,
          atol = 1e-12,
          rtol = 1e-12,
          returnType = "data.frame"
        )
        expect_equal(.s$cp, .locf, tolerance = 1e-8, info = paste(.meth, .cov))
      }
    }
  })

  test_that("rateAtDoseTime = TRUE keeps the rate read at the dose", {
    .b <- c(.step(0, 1, 12.5), .step(0, 2, 12.5), .step(0, 6, 12.5)) / 10
    for (.nm in c(TRUE, FALSE)) {
      .s <- rxSolve(
        .m,
        .d,
        nonmem = .nm,
        rateAtDoseTime = TRUE,
        addDosing = FALSE,
        atol = 1e-12,
        rtol = 1e-12,
        returnType = "data.frame"
      )
      expect_equal(.s$cp, .b, tolerance = 1e-8, info = paste("nonmem", .nm))
    }
    expect_false(rxControl()$rateAtDoseTime)
    expect_error(rxSolve(.m, .d, rateAtDoseTime = NA))
  })

  test_that("a time-dependent modeled rate reads the record time across addl doses", {
    .mt <- rxode2({
      cl <- 0.5
      v <- 10
      rate(central) <- 100 / (1 + 0.1 * time)
      d/dt(central) <- -cl / v * central
      cp <- central / v
    })
    .dd <- data.frame(
      ID = 1,
      TIME = c(0, 0.5, 1.5, 6, 12.7, 13.4, 18, 24.2, 30),
      EVID = c(1, 0, 0, 0, 0, 0, 0, 0, 0),
      AMT = c(100, 0, 0, 0, 0, 0, 0, 0, 0),
      RATE = c(-1, 0, 0, 0, 0, 0, 0, 0, 0),
      II = c(12, 0, 0, 0, 0, 0, 0, 0, 0),
      ADDL = c(2, 0, 0, 0, 0, 0, 0, 0, 0)
    )
    .ref <- .exact(.dd, data.frame(time = c(12, 24), amt = 100), function(j) 100 / (1 + 0.1 * .dd$TIME[j])) / 10
    for (.meth in c("liblsoda", "dop853")) {
      .s <- rxSolve(
        .mt,
        .dd,
        nonmem = TRUE,
        method = .meth,
        addDosing = FALSE,
        atol = 1e-12,
        rtol = 1e-12,
        returnType = "data.frame"
      )
      expect_equal(.s$cp, .ref, tolerance = 1e-8, info = .meth)
    }
  })

  test_that("an infusion ended by a turn-off or reset is not re-read", {
    .base <- data.frame(
      ID = 1,
      TIME = c(2, 3, 4, 8),
      EVID = c(1, 0, 0, 0),
      AMT = c(100, 0, 0, 0),
      RATE = c(-1, 0, 0, 0),
      CMT = 1,
      X = c(4, 2, 1, 1)
    )
    .ref <- rxSolve(
      .m,
      .base,
      nonmem = TRUE,
      addDosing = FALSE,
      atol = 1e-12,
      rtol = 1e-12,
      returnType = "data.frame"
    )$cp
    for (.ev in c(2, 3)) {
      .dd <- rbind(
        data.frame(
          ID = 1,
          TIME = c(0, 1),
          EVID = c(1, .ev),
          AMT = c(100, 0),
          RATE = c(-1, 0),
          CMT = c(1, ifelse(.ev == 2, -1, 1)),
          X = 4
        ),
        .base
      )
      .s <- rxSolve(.m, .dd, nonmem = TRUE, addDosing = FALSE, atol = 1e-12, rtol = 1e-12, returnType = "data.frame")
      expect_equal(tail(.s$cp, 3), .ref, tolerance = 1e-8, info = paste("evid", .ev))
    }
  })

  test_that("an infusion end does not re-read a state-dependent rate", {
    .ms <- rxode2({
      rate(a) <- 100 + a
      dur(b) <- 1
      d/dt(a) <- 0
      d/dt(b) <- 0
    })
    .dd <- data.frame(
      ID = 1,
      TIME = c(0, 0, 10),
      EVID = c(1, 1, 0),
      AMT = c(10000, 10, 0),
      RATE = c(-1, -2, 0),
      CMT = c(1, 2, 1)
    )
    .s <- rxSolve(.ms, .dd, nonmem = TRUE, addDosing = FALSE, returnType = "data.frame")
    expect_equal(.s$a, 1000, tolerance = 1e-6)
  })

  test_that("nonmem = TRUE does not read an addl repeat's covariates", {
    .da <- .d
    .da$II <- c(1.5, 0, 0, 0)
    .da$ADDL <- c(1, 0, 0, 0)
    .ref <- .exact(.da, data.frame(time = 1.5, amt = 100), function(j) 50 / .da$X[j]) / 10
    .s <- rxSolve(.m, .da, nonmem = TRUE, addDosing = FALSE, atol = 1e-12, rtol = 1e-12, returnType = "data.frame")
    expect_equal(.s$cp, .ref, tolerance = 1e-8)
  })

  test_that("an infusion a steady-state reset cancelled is not re-read", {
    .dr <- data.frame(
      ID = 1,
      TIME = c(0, 1, 2, 6),
      EVID = c(1, 1, 0, 0),
      AMT = c(100, 10, 0, 0),
      RATE = c(-1, 0, 0, 0),
      SS = c(0, 1, 0, 0),
      II = c(0, 24, 0, 0),
      X = c(4, 2, 1, 1)
    )
    .solve <- function(...) {
      rxSolve(.m, .dr, addDosing = FALSE, atol = 1e-12, rtol = 1e-12, returnType = "data.frame", ...)$cp
    }
    for (.nm in c(TRUE, FALSE)) {
      expect_equal(
        .solve(nonmem = .nm),
        .solve(nonmem = .nm, rateAtDoseTime = TRUE),
        tolerance = 1e-6,
        info = paste("nonmem", .nm)
      )
    }
  })

  .md <- rxode2({
    cl <- 0.5
    v <- 10
    dur(central) <- X
    d/dt(central) <- -cl / v * central
    cp <- central / v
  })
  .dd <- .d
  .dd$RATE[1] <- -2

  .durSolve <- function(d, ...) {
    rxSolve(.md, d, addDosing = FALSE, atol = 1e-12, rtol = 1e-12, returnType = "data.frame", ...)$cp
  }

  test_that("a re-read modeled duration moves the end and spreads what is left", {
    # t = 0: dur 4 (25/h); t = 1: dur 2, 75 left over (1, 2] (75/h)
    .a1 <- .step(0, 1, 25)
    .a2 <- .step(.a1, 1, 75)
    .ref <- c(.a1, .a2, .step(.a2, 4, 0)) / 10
    for (.meth in c("liblsoda", "lsoda", "dop853")) {
      expect_equal(.durSolve(.dd, method = .meth), .ref, tolerance = 1e-8, info = .meth)
    }
  })

  test_that("a modeled duration re-read into the past delivers no more", {
    .dp <- .dd
    .dp$X <- c(4, 0.5, 0.5, 0.5)
    .a1 <- .step(0, 1, 25)
    .ref <- c(.a1, .step(.a1, 1, 0), .step(.a1, 5, 0)) / 10
    expect_equal(.durSolve(.dp), .ref, tolerance = 1e-8)
    # the dose is already delivered when the duration grows again
    .dg <- .dd
    .dg$TIME <- c(0, 1, 5, 6)
    .dg$X <- c(4, 2, 10, 10)
    .a1 <- .step(0, 1, 25)
    .a2 <- .step(.a1, 1, 75)
    .ref <- c(.a1, .step(.a2, 3, 0), .step(.a2, 4, 0)) / 10
    expect_equal(.durSolve(.dg), .ref, tolerance = 1e-8)
  })

  test_that("nonmem = TRUE fixes a modeled duration unless durAtDoseTime = FALSE", {
    .fixed <- c(.step(0, 1, 25), .step(0, 2, 25), .step(.step(0, 4, 25), 2, 0)) / 10
    expect_equal(.durSolve(.dd, nonmem = TRUE), .fixed, tolerance = 1e-8)
    expect_equal(.durSolve(.dd, durAtDoseTime = TRUE), .fixed, tolerance = 1e-8)
    # NOCB: (0, 1] reads dur 2 at t = 1 (100 over 2 h); (1, 2] reads dur 1, an
    # end at 1 that has passed
    .a1 <- .step(0, 1, 50)
    .ref <- c(.a1, .step(.a1, 1, 0), .step(.a1, 5, 0)) / 10
    expect_equal(.durSolve(.dd, nonmem = TRUE, durAtDoseTime = FALSE), .ref, tolerance = 1e-8)
    expect_false(rxControl()$durAtDoseTime)
    expect_true(rxControl(nonmem = TRUE)$durAtDoseTime)
  })
})
