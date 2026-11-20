rxTest({
  # With rxSolve(..., nonmem = TRUE), f(), rate() and dur() of an addl repeat
  # read `time` as the first data record at or after the repeat, like
  # NONMEM's F1/R1/D1 (rxode2#1583).
  .pk <- function(fexpr) {
    eval(bquote(rxode2({
      cl <- 0.5
      v <- 10
      ka <- 1
      f(depot) <- .(fexpr)
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      pred <- central / v
    })))
  }
  .fTime <- .pk(quote(ifelse(t >= 13, 0.5, 1)))

  # 100 q12h (ADDL = 3) into depot; no record at the repeat at 12
  .d <- data.frame(
    ID = 1,
    TIME = c(0, 8, 14, 20, 40),
    DV = 0,
    MDV = c(1, 0, 0, 0, 0),
    EVID = c(1, 0, 0, 0, 0),
    AMT = c(100, NA, NA, NA, NA),
    CMT = c(1, 2, 2, 2, 2),
    ADDL = c(3, 0, 0, 0, 0),
    II = c(12, 0, 0, 0, 0)
  )

  .solve <- function(m, d, nonmem = TRUE, col = "pred") {
    rxSolve(
      m,
      d,
      nonmem = nonmem,
      addlKeepsCov = FALSE,
      covsInterpolation = "nocb",
      addDosing = FALSE,
      atol = 1e-12,
      rtol = 1e-12,
      returnType = "data.frame"
    )[[col]]
  }

  test_that("the model flags dose properties that may read time", {
    expect_equal(rxModelVars(.fTime)$flags[["doseTime"]], 1L)
    expect_equal(rxModelVars(.pk(0.5))$flags[["doseTime"]], 0L)
    .noDose <- rxode2({
      d/dt(central) <- -0.1 * central
      k <- t
    })
    expect_equal(rxModelVars(.noDose)$flags[["doseTime"]], 0L)
    # linCmt() reads `t` itself; that is not a dose property reading it
    .lin <- rxode2({
      cl <- 1.1
      v <- 20
      f(central) <- 0.5
      cp <- linCmt()
    })
    expect_equal(rxModelVars(.lin)$flags[["doseTime"]], 0L)
  })

  test_that("f() of an addl repeat reads the next record's time (NONMEM F1)", {
    # NONMEM 7.5.1, ferx-core anchor addl_ftime
    expect_equal(.solve(.fTime, .d), c(7.052469, 9.277216, 7.398650, 9.300061), tolerance = 1e-6)
  })

  test_that("without nonmem = TRUE f() reads the repeat's own time", {
    expect_equal(.solve(.fTime, .d, nonmem = FALSE), c(7.052469, 13.327230, 10.924880, 10.597940), tolerance = 1e-6)
  })

  test_that("a record at the repeat's time is the one f() reads", {
    .d12 <- rbind(
      .d,
      data.frame(
        ID = 1,
        TIME = 12,
        DV = 0,
        MDV = 0,
        EVID = 0,
        AMT = NA,
        CMT = 2,
        ADDL = 0,
        II = 0
      )
    )
    .d12 <- .d12[order(.d12$TIME), ]
    expect_equal(.solve(.fTime, .d12), .solve(.fTime, .d12, nonmem = FALSE))
  })

  test_that("a time-independent f() is unchanged", {
    .m <- .pk(0.5)
    expect_equal(.solve(.m, .d), .solve(.m, .d, nonmem = FALSE))
  })

  # 100 q12h, one repeat at 12, modeled duration; no record at 12
  .d2 <- data.frame(
    ID = 1,
    TIME = c(0, 6, 14, 15),
    EVID = c(1, 0, 0, 0),
    AMT = c(100, 0, 0, 0),
    RATE = c(-2, 0, 0, 0),
    ADDL = c(1, 0, 0, 0),
    II = c(12, 0, 0, 0)
  )
  # NONMEM 7.5.1, ferx-core anchor addl_dt
  .nmDur <- c(7.501561, 14.520558, 13.812382)

  test_that("dur() of an addl repeat reads the next record's time (NONMEM D1)", {
    .m <- rxode2({
      cl <- 0.5
      v <- 10
      dur(central) <- 0.5 + 0.1 * t
      d/dt(central) <- -cl / v * central
      cp <- central / v
    })
    expect_equal(.solve(.m, .d2, col = "cp"), .nmDur, tolerance = 1e-6)
  })

  test_that("rate() of an addl repeat reads the next record's time (NONMEM R1)", {
    .m <- rxode2({
      cl <- 0.5
      v <- 10
      rate(central) <- 100 / (0.5 + 0.1 * t)
      d/dt(central) <- -cl / v * central
      cp <- central / v
    })
    .d3 <- .d2
    .d3$RATE[1] <- -1
    expect_equal(.solve(.m, .d3, col = "cp"), .nmDur, tolerance = 1e-6)
  })

  test_that("linCmt() models read the same time", {
    .m <- rxode2({
      cl <- 0.5
      v <- 10
      dur(central) <- 0.5 + 0.1 * t
      cp <- linCmt()
    })
    expect_equal(.solve(.m, .d2, col = "cp"), .nmDur, tolerance = 1e-6)
  })
})
