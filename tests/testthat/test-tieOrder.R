rxTest({
  # Records tied at a time, as in rxode2#1593: NONMEM processes them in file
  # order, so a dose listed before a reset is wiped by it
  .tieOde <- function() {
    rxode2({
      cl <- 2
      v <- 20
      ka <- 0.15
      d / dt(depot) <- -ka * depot
      d / dt(central) <- ka * depot - cl / v * central
      cp <- central / v
    })
  }

  .tieLin <- function() {
    rxode2({
      cl <- 2
      v <- 20
      ka <- 0.15
      cp <- linCmt()
    })
  }

  .tieObs <- c(10.5, 12.5, 20)

  .tieData <- function(id, first, second) {
    .ev <- function(e) {
      data.frame(
        TIME = 10,
        EVID = e,
        AMT = ifelse(e %in% c(1, 4), 100, 0),
        CMT = 1
      )
    }
    cbind(
      ID = id,
      rbind(
        .ev(first),
        .ev(second),
        data.frame(TIME = .tieObs, EVID = 0, AMT = 0, CMT = 2)
      )
    )
  }

  .tieAll <- function() {
    rbind(
      .tieData(1, 1, 3), # dose, then reset
      .tieData(2, 3, 1), # reset, then dose
      .tieData(3, 1, 4) # dose, then reset + dose
    )
  }

  # one dose of 100 at time 10
  .tieOneDose <- function(t) {
    .k <- 0.1
    .ka <- 0.15
    100 * .ka / (20 * (.ka - .k)) * (exp(-.k * (t - 10)) - exp(-.ka * (t - 10)))
  }

  for (.m in c("ode", "linCmt")) {
    .mod <- if (.m == "ode") .tieOde() else .tieLin()
    .tol <- if (.m == "ode") 1e-5 else 1e-10

    test_that(paste0("tieOrder='data' processes tied records in data order (", .m, ")"), {
      .r <- rxSolve(
        .mod,
        .tieAll(),
        tieOrder = "data",
        addDosing = FALSE,
        returnType = "data.frame"
      )
      .one <- .tieOneDose(.tieObs)
      expect_equal(.r$cp, c(0 * .tieObs, .one, .one), tolerance = .tol)
    })

    test_that(paste0("nonmem=TRUE defaults to tieOrder='data' (", .m, ")"), {
      .r <- rxSolve(
        .mod,
        .tieAll(),
        nonmem = TRUE,
        addDosing = FALSE,
        returnType = "data.frame"
      )
      .one <- .tieOneDose(.tieObs)
      expect_equal(.r$cp, c(0 * .tieObs, .one, .one), tolerance = .tol)
      # nonmem=TRUE can still ask for the evid order
      .r <- rxSolve(
        .mod,
        .tieAll(),
        nonmem = TRUE,
        tieOrder = "evid",
        addDosing = FALSE,
        returnType = "data.frame"
      )
      expect_equal(.r$cp, c(.one, .one, 2 * .one), tolerance = .tol)
    })

    test_that(paste0("tieOrder='evid' (the default) does not depend on row order (", .m, ")"), {
      .r <- rxSolve(.mod, .tieAll(), addDosing = FALSE, returnType = "data.frame")
      .one <- .tieOneDose(.tieObs)
      expect_equal(.r$cp, c(.one, .one, 2 * .one), tolerance = .tol)
    })
  }

  test_that("tieOrder='data' predicts an observation listed before a co-timed dose before the dose", {
    .mod <- rxode2({
      d / dt(central) <- -0.1 * central
      cp <- central / 20
    })
    .d <- data.frame(
      ID = 1,
      TIME = c(0, 5, 5, 6),
      EVID = c(1, 0, 1, 0),
      AMT = c(100, 0, 100, 0),
      CMT = 1
    )
    .pre <- 100 * exp(-0.5) / 20
    .post <- (100 * exp(-0.5) + 100) / 20
    .r <- rxSolve(.mod, .d, tieOrder = "data", addDosing = FALSE, returnType = "data.frame")
    expect_equal(.r$cp[.r$time == 5], .pre, tolerance = 1e-5)
    expect_equal(.r$cp[.r$time == 6], .post * exp(-0.1), tolerance = 1e-5)
    .r <- rxSolve(.mod, .d, addDosing = FALSE, returnType = "data.frame")
    expect_equal(.r$cp[.r$time == 5], .post, tolerance = 1e-5)
  })

  test_that("tieOrder is checked and kept in rxControl()", {
    expect_error(rxControl(tieOrder = "file"))
    expect_equal(rxControl()$tieOrder, "evid")
    expect_equal(rxControl(nonmem = TRUE)$tieOrder, "data")
    expect_equal(rxControl(nonmem = TRUE, tieOrder = "evid")$tieOrder, "evid")
  })
})
