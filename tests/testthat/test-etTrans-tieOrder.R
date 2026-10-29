rxTest({
  .tieMod <- function() {
    rxode2({
      cl <- 2
      v <- 20
      ka <- 0.15
      d / dt(depot) <- -ka * depot
      d / dt(central) <- ka * depot - cl / v * central
      cp <- central / v
    })
  }

  # one subject: an observation at time 0, two records tied at time 10, then
  # observations (a reset can not be a subject's first record)
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
        data.frame(TIME = 0, EVID = 0, AMT = 0, CMT = 2),
        .ev(first),
        .ev(second),
        data.frame(TIME = c(10.5, 12.5, 20), EVID = 0, AMT = 0, CMT = 2)
      )
    )
  }

  .tieEvid <- function(d, tieOrder) {
    .t <- as.data.frame(etTrans(d, .tieMod(), tieOrder = tieOrder))
    .t$EVID[.t$TIME == 10]
  }

  test_that("tieOrder='evid' (the default) puts a reset before a co-timed dose", {
    .d1 <- .tieData(1, 1, 3)
    .d2 <- .tieData(1, 3, 1)
    expect_equal(.tieEvid(.d1, "evid"), c(3L, 101L))
    expect_equal(.tieEvid(.d2, "evid"), c(3L, 101L))
    expect_identical(
      as.data.frame(etTrans(.d1, .tieMod())),
      as.data.frame(etTrans(.d1, .tieMod(), tieOrder = "evid"))
    )
  })

  test_that("tieOrder='data' keeps the row order of records at the same time", {
    expect_equal(.tieEvid(.tieData(1, 1, 3), "data"), c(101L, 3L))
    expect_equal(.tieEvid(.tieData(1, 3, 1), "data"), c(3L, 101L))
    # evid=4 is a reset followed by its dose, after the earlier dose
    expect_equal(.tieEvid(.tieData(1, 1, 4), "data"), c(101L, 3L, 101L))
  })

  test_that("tieOrder='data' keeps an observation listed before a co-timed dose", {
    .d <- data.frame(
      ID = 1,
      TIME = c(0, 5, 5, 6),
      EVID = c(1, 0, 1, 0),
      AMT = c(100, 0, 100, 0),
      CMT = 1
    )
    .data <- as.data.frame(etTrans(.d, .tieMod(), tieOrder = "data"))
    expect_equal(.data$EVID, c(101L, 0L, 101L, 0L))
    .evid <- as.data.frame(etTrans(.d, .tieMod()))
    expect_equal(.evid$EVID, c(101L, 101L, 0L, 0L))
  })

  test_that("tieOrder='data' still sorts by subject and time", {
    .d <- rbind(.tieData(2, 3, 1), .tieData(1, 1, 3))
    .d <- .d[c(10:12, 7:9, 4:6, 1:3), ]
    .t <- as.data.frame(etTrans(.d, .tieMod(), tieOrder = "data"))
    expect_equal(as.integer(.t$ID), rep(1:2, each = 6))
    expect_false(is.unsorted(.t$TIME[.t$ID == 1]))
    expect_equal(.t$EVID[.t$TIME == 10 & .t$ID == 1], c(101L, 3L))
    expect_equal(.t$EVID[.t$TIME == 10 & .t$ID == 2], c(3L, 101L))
  })

  test_that("tieOrder must be 'evid' or 'data'", {
    expect_error(etTrans(.tieData(1, 1, 3), .tieMod(), tieOrder = "file"))
  })
})
