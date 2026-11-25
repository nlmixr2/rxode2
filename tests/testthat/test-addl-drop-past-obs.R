rxTest({
  .pk <- function() {
    ini({
      ka <- 1
      cl <- 1
      v <- 10
    })
    model({
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      cp <- central / v
    })
  }
  .m <- rxode2(.pk)

  # translate with and without truncation, then require the same solve
  .same <- function(m, ev, ...) {
    .a <- etTrans(ev, m, keepDosingOnly = TRUE, ...)
    .b <- etTrans(ev, m, keepDosingOnly = TRUE, addlDropPastObs = TRUE, ...)
    .sa <- as.data.frame(rxSolve(m, .a))
    .sb <- as.data.frame(rxSolve(m, .b))
    expect_equal(.sb, .sa)
    c(nrow(.a), nrow(.b))
  }

  test_that("addl repeats past the last observation are not expanded", {
    .ev <- et(amt = 100, ii = 24, addl = 10000) |> et(0:48)
    .a <- etTrans(.ev, .m, keepDosingOnly = TRUE)
    .b <- etTrans(.ev, .m, keepDosingOnly = TRUE, addlDropPastObs = TRUE)
    expect_equal(nrow(.a), 10050L)
    expect_equal(nrow(.b), 53L)
    # the first repeat past the last record stays
    expect_equal(max(.b$TIME), 72)
    # the default does not truncate
    expect_equal(nrow(etTrans(.ev, .m, keepDosingOnly = TRUE)), 10050L)
  })

  test_that("bolus addl solves the same with truncation", {
    .ev <- et(amt = 100, ii = 24, addl = 20) |> et(0:48)
    .n <- .same(.m, .ev)
    expect_lt(.n[2], .n[1])
  })

  test_that("an infusion straddling the last observation solves the same", {
    .ev <- et(amt = 100, ii = 24, addl = 20, dur = 30, cmt = "central") |>
      et(seq(0, 50, by = 2.5))
    .n <- .same(.m, .ev)
    expect_lt(.n[2], .n[1])
    .ev <- et(amt = 100, ii = 24, addl = 20, rate = 5, cmt = "central") |>
      et(seq(0, 50, by = 2.5))
    .n <- .same(.m, .ev)
    expect_lt(.n[2], .n[1])
  })

  test_that("steady-state doses with addl solve the same", {
    .ev <- et(amt = 100, ii = 24, addl = 20, ss = 1) |> et(0:48)
    .n <- .same(.m, .ev)
    expect_lt(.n[2], .n[1])
    .n <- .same(.m, .ev, addlDropSs = FALSE)
    expect_lt(.n[2], .n[1])
  })

  test_that("reset events solve the same", {
    .d <- data.frame(
      id = 1L,
      time = c(0, 10, 30, 30, 40, 50),
      evid = c(1L, 0L, 3L, 0L, 0L, 0L),
      amt = c(100, NA, NA, NA, NA, NA),
      ii = c(12, 0, 0, 0, 0, 0),
      addl = c(20L, 0L, 0L, 0L, 0L, 0L),
      cmt = 1L,
      dv = c(NA, 1, NA, 1, 1, 1)
    )
    .n <- .same(.m, .d)
    .d$evid[3] <- 4L
    .d$amt[3] <- 50
    .d$ii[3] <- 12
    .d$addl[3] <- 20L
    .n <- .same(.m, .d)
    expect_lt(.n[2], .n[1])
  })

  test_that("lagged doses keep every repeat", {
    .lag <- function() {
      ini({
        ka <- 1
        cl <- 1
        v <- 10
      })
      model({
        d/dt(depot) <- -ka * depot
        alag(depot) <- 2
        d/dt(central) <- ka * depot - cl / v * central
        cp <- central / v
      })
    }
    .ml <- rxode2(.lag)
    .ev <- et(amt = 100, ii = 24, addl = 20) |> et(0:48)
    .n <- .same(.ml, .ev)
    expect_equal(.n[2], .n[1])
    # an unlagged compartment of the same model still truncates
    .ev <- et(amt = 100, ii = 24, addl = 20, cmt = "central") |> et(0:48)
    .n <- .same(.ml, .ev)
    expect_lt(.n[2], .n[1])
  })

  test_that("dose-only subjects and subjects whose time moves back are unchanged", {
    .d <- data.frame(
      id = c(1L, 1L, 1L, 2L, 3L, 3L, 3L, 3L),
      time = c(0, 10, 20, 0, 0, 10, 0, 10),
      evid = c(1L, 0L, 0L, 1L, 1L, 0L, 3L, 0L),
      amt = c(100, NA, NA, 100, 100, NA, NA, NA),
      ii = c(12, 0, 0, 12, 12, 0, 0, 0),
      addl = c(20L, 0L, 0L, 20L, 20L, 0L, 0L, 0L),
      cmt = 1L,
      dv = c(NA, 1, 1, NA, NA, 1, NA, 1)
    )
    .a <- etTrans(.d, .m, keepDosingOnly = TRUE)
    .b <- etTrans(.d, .m, keepDosingOnly = TRUE, addlDropPastObs = TRUE)
    .cnt <- function(x, id) sum(as.integer(x$ID) == id)
    expect_lt(.cnt(.b, 1L), .cnt(.a, 1L))
    expect_equal(.cnt(.b, 2L), .cnt(.a, 2L))
    expect_equal(.cnt(.b, 3L), .cnt(.a, 3L))
  })

  test_that("a trailing dose row keeps the repeats before it", {
    .d <- data.frame(
      id = 1L,
      time = c(0, 1, 40),
      evid = c(1L, 0L, 1L),
      amt = c(100, NA, 100),
      ii = c(10, 0, 0),
      addl = c(10L, 0L, 0L),
      cmt = 1L,
      dv = c(NA, 1, NA)
    )
    .b <- etTrans(.d, .m, keepDosingOnly = TRUE, addlDropPastObs = TRUE)
    expect_equal(sort(.b$TIME[.b$EVID == 101]), c(0, 10, 20, 30, 40, 40, 50))
    .n <- .same(.m, .d)
    expect_lt(.n[2], .n[1])
  })

  test_that("classic infusion records and reset-only subjects keep every repeat", {
    .d <- data.frame(
      id = 1L,
      time = c(0, 30, 35),
      evid = c(10201L, 10201L, 0L),
      amt = c(10, -10, NA),
      ii = c(10, 10, 0),
      addl = c(2L, 2L, 0L),
      dv = c(NA, NA, 1)
    )
    .a <- etTrans(.d, .m, keepDosingOnly = TRUE)
    .b <- etTrans(.d, .m, keepDosingOnly = TRUE, addlDropPastObs = TRUE)
    expect_equal(nrow(.b), nrow(.a))
    .d <- data.frame(
      id = 1L,
      time = c(0, 1),
      evid = c(3L, 1L),
      amt = c(NA, 100),
      ii = c(0, 24),
      addl = c(0L, 4L),
      cmt = 1L,
      dv = NA_real_
    )
    .a <- etTrans(.d, .m, keepDosingOnly = TRUE)
    .b <- etTrans(.d, .m, keepDosingOnly = TRUE, addlDropPastObs = TRUE)
    expect_equal(nrow(.b), nrow(.a))
    expect_error(etTrans(.d, .m, addlDropPastObs = NA))
  })

  test_that("an mdv data set truncates and solves the same", {
    .d <- data.frame(
      id = 1L,
      time = c(0, 0:48),
      mdv = c(1L, rep(0L, 49)),
      amt = c(100, rep(0, 49)),
      ii = c(24, rep(0, 49)),
      addl = c(20L, rep(0L, 49)),
      cmt = 1L,
      dv = c(NA, rep(1, 49))
    )
    .n <- .same(.m, .d)
    expect_lt(.n[2], .n[1])
  })

  test_that("an mdv 0 observation with an amount counts as an observation", {
    .d <- data.frame(
      id = 1L,
      time = c(0, 1, 25),
      mdv = c(1L, 0L, 0L),
      amt = c(100, 0, 5),
      ii = c(10, 0, 0),
      addl = c(2L, 0L, 0L),
      cmt = 1L,
      dv = c(NA, 1, 1)
    )
    .a <- etTrans(.d, .m, keepDosingOnly = TRUE)
    .b <- etTrans(.d, .m, keepDosingOnly = TRUE, addlDropPastObs = TRUE)
    expect_equal(nrow(.b), nrow(.a))
  })

  test_that("models reading later records or pushing events keep every repeat", {
    .ev <- et(amt = 100, ii = 24, addl = 20) |> et(0:48)
    .keepsAll <- function(m) {
      .a <- etTrans(.ev, m, keepDosingOnly = TRUE)
      .b <- etTrans(.ev, m, keepDosingOnly = TRUE, addlDropPastObs = TRUE)
      expect_equal(nrow(.b), nrow(.a))
    }
    .keepsAll(rxode2({
      d/dt(depot) <- -depot
      x <- lead(time, 2)
    }))
    .keepsAll(rxode2({
      d/dt(depot) <- -depot
      x <- last(time)
    }))
    .keepsAll(rxode2({
      d/dt(depot) <- -depot
      x <- lead0(time)
    }))
    .keepsAll(rxode2({
      d/dt(depot) <- -depot
      if (t < 1) evid_(t + 0.1, 1, 10, 1, 0, 0, 0, 0)
    }))
    # lag() and a function whose name ends in "last" still truncate
    .m2 <- rxode2({
      d/dt(depot) <- -depot
      x <- lag(time)
      notlast <- 1
    })
    .a <- etTrans(.ev, .m2, keepDosingOnly = TRUE)
    .b <- etTrans(.ev, .m2, keepDosingOnly = TRUE, addlDropPastObs = TRUE)
    expect_lt(nrow(.b), nrow(.a))
  })

  test_that("models with model times keep every repeat", {
    .mt <- function() {
      ini({
        ka <- 1
        cl <- 1
        v <- 10
      })
      model({
        mtime(t1) <- 100
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - cl / v * central
        cp <- central / v
      })
    }
    .mm <- rxode2(.mt)
    .ev <- et(amt = 100, ii = 24, addl = 20) |> et(0:48)
    .a <- etTrans(.ev, .mm, keepDosingOnly = TRUE)
    .b <- etTrans(.ev, .mm, keepDosingOnly = TRUE, addlDropPastObs = TRUE)
    expect_equal(nrow(.b), nrow(.a))
  })
})

rxTest({
  .pk <- function() {
    ini({
      ka <- 1
      cl <- 1
      v <- 10
    })
    model({
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      cp <- central / v
    })
  }
  .m <- rxode2(.pk)

  test_that("rxSolve() drops addl repeats past the last observation", {
    expect_true(rxControl()$addlDropPastObs)
    expect_error(rxControl(addlDropPastObs = NA))
    .ev <- et(amt = 100, ii = 24, addl = 20) |> et(0:48)
    .s1 <- as.data.frame(rxSolve(.m, .ev))
    .s2 <- as.data.frame(rxSolve(.m, .ev, addlDropPastObs = FALSE))
    expect_equal(.s1, .s2)
    .d <- as.data.frame(.ev)
    expect_equal(
      as.data.frame(rxSolve(.m, .d)),
      as.data.frame(rxSolve(.m, .d, addlDropPastObs = FALSE))
    )
    # a long maintenance regimen with a short observation window solves
    .ev <- et(amt = 100, ii = 24, addl = 10000) |> et(0:48)
    .s <- rxSolve(.m, .ev)
    expect_equal(nrow(.s), 49L)
    expect_equal(as.data.frame(.s), .s1)
  })

  test_that("rxSolve() keeps every repeat when the output shows doses", {
    .ev <- et(amt = 100, ii = 24, addl = 20) |> et(0:48)
    .s1 <- as.data.frame(rxSolve(.m, .ev, addDosing = TRUE))
    .s2 <- as.data.frame(rxSolve(.m, .ev, addDosing = TRUE, addlDropPastObs = FALSE))
    expect_equal(.s1, .s2)
    expect_equal(sum(.s1$evid == 1), 21L)
    .s1 <- as.data.frame(rxSolve(.m, .ev, addDosing = NA))
    expect_equal(nrow(.s1), nrow(as.data.frame(rxSolve(.m, .ev, addDosing = NA, addlDropPastObs = FALSE))))
    .s1 <- as.data.frame(rxSolve(.m, .ev, addDosing = NULL))
    expect_equal(.s1, as.data.frame(rxSolve(.m, .ev, addDosing = NULL, addlDropPastObs = FALSE)))
  })

  test_that("rxSolve() without observations samples across every repeat", {
    .ev <- et(amt = 100, ii = 24, addl = 4)
    .s1 <- as.data.frame(rxSolve(.m, .ev))
    .s2 <- as.data.frame(rxSolve(.m, .ev, addlDropPastObs = FALSE))
    expect_equal(.s1, .s2)
    .d <- data.frame(
      id = 1L,
      time = c(0, 1, 2),
      evid = c(1L, 2L, 2L),
      amt = c(100, NA, NA),
      ii = c(24, 0, 0),
      addl = c(4L, 0L, 0L),
      cmt = 1L
    )
    .s1 <- as.data.frame(rxSolve(.m, .d))
    .s2 <- as.data.frame(rxSolve(.m, .d, addlDropPastObs = FALSE))
    expect_equal(.s1, .s2)
  })
})
