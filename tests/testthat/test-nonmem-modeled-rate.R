rxTest({
  # With rxSolve(..., nonmem = TRUE), a modeled rate (rate(cmt), RATE = -1)
  # runs on (t[j-1], t[j]] at the rate computed with data record j until the
  # dose is delivered, like NONMEM's R1 (rxode2#1586).
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

  test_that("without nonmem = TRUE the rate is fixed at the dose", {
    .b <- c(.step(0, 1, 12.5), .step(0, 2, 12.5), .step(0, 6, 12.5)) / 10
    .s <- rxSolve(.m, .d, addDosing = FALSE, atol = 1e-12, rtol = 1e-12, returnType = "data.frame")
    expect_equal(.s$cp, .b, tolerance = 1e-8)
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

  test_that("a modeled duration is still fixed at the dose", {
    .md <- rxode2({
      cl <- 0.5
      v <- 10
      dur(central) <- X
      d/dt(central) <- -cl / v * central
      cp <- central / v
    })
    .dd <- .d
    .dd$RATE[1] <- -2
    .s1 <- rxSolve(.md, .dd, nonmem = TRUE, addDosing = FALSE, returnType = "data.frame")
    .s2 <- rxSolve(.md, .dd, nonmem = FALSE, addDosing = FALSE, returnType = "data.frame")
    expect_equal(.s1$cp, .s2$cp)
  })
})
