rxTest({
  .tteConst <- function() {
    ini({
      lambda <- 0.1
    })
    model({
      haz <- lambda
      ev ~ tte(haz, maxEventNumber = 1)
    })
  }

  test_that("tte() single events: layout, distribution and root accuracy", {
    .s <- withr::with_seed(1, as.data.frame(rxSolve(.tteConst(), et(c(0, 30)) |> et(id = 1:1000))))
    expect_true(all(c("sim", "rx.tteH.ev") %in% names(.s)))
    # start row (0), then either the event (1) or the censoring row (0)
    expect_true(all(table(.s$id) == 2L))
    expect_true(all(.s$sim[.s$time == 0] == 0))
    .last <- .s[!duplicated(.s$id, fromLast = TRUE), ]
    expect_true(all(.last$sim[.last$time < 30] == 1))
    expect_true(all(.last$sim[.last$time == 30] == 0))
    .ev <- .s$time[.s$sim == 1]
    expect_gt(stats::ks.test(.ev, function(x) pexp(x, 0.1) / pexp(30, 0.1))$p.value, 0.01)
    expect_equal(length(.ev) / 1000, 1 - exp(-3), tolerance = 0.05)
  })

  test_that("tte() event times invert the cumulative hazard", {
    f <- function() {
      ini({
        te <- 10
        sh <- 1.5
      })
      model({
        haz <- sh / te * (time / te)^(sh - 1)
        ev ~ tte(haz, maxEventNumber = 1)
      })
    }
    .s <- withr::with_seed(2, as.data.frame(rxSolve(f(), et(c(0, 100)) |> et(id = 1:300))))
    .e <- .s[.s$sim == 1, ]
    # the solved cumulative hazard at the event is the closed form
    expect_equal(.e$rx.tteH.ev, (.e$time / 10)^1.5, tolerance = 1e-5)
    expect_gt(stats::ks.test(.e$time, function(x) pweibull(x, 1.5, 10) / pweibull(100, 1.5, 10))$p.value, 0.01)
  })

  test_that("tte() repeated events and maxEventNumber", {
    f <- function() {
      ini({
        lambda <- 0.5
      })
      model({
        haz <- lambda
        ev ~ tte(haz)
      })
    }
    .s <- withr::with_seed(3, as.data.frame(rxSolve(f(), et(c(0, 20)) |> et(id = 1:300))))
    .n <- tapply(.s$sim, .s$id, sum)
    expect_equal(mean(.n), 10, tolerance = 0.05)
    # censoring row at the end of the window
    .last <- .s[!duplicated(.s$id, fromLast = TRUE), ]
    expect_true(all(.last$time == 20 & .last$sim == 0))
    .s3 <- withr::with_seed(3, as.data.frame(rxSolve(
      f() |> model(ev ~ tte(haz, maxEventNumber = 3)),
      et(c(0, 20)) |> et(id = 1:50)
    )))
    expect_true(all(tapply(.s3$sim, .s3$id, sum) <= 3))
  })

  test_that("tte() rightCensoringTime past the last record", {
    f <- function() {
      ini({
        lambda <- 0.001
      })
      model({
        haz <- lambda
        ev ~ tte(haz, maxEventNumber = 1, rightCensoringTime = 50)
      })
    }
    .s <- withr::with_seed(4, as.data.frame(rxSolve(f(), et(0) |> et(id = 1:5))))
    .last <- .s[!duplicated(.s$id, fromLast = TRUE), ]
    expect_true(all(.last$time == 50 | .last$sim == 1))
  })

  test_that("tte() interval censoring", {
    f <- function() {
      ini({
        lambda <- 0.05
      })
      model({
        haz <- lambda
        ev ~ tte(haz, eventType = "intervalCensored", intervalLength = 10, rightCensoringTime = 100)
      })
    }
    .s <- withr::with_seed(5, as.data.frame(rxSolve(f(), et(0) |> et(id = 1:20))))
    expect_true(all(.s$time %in% seq(0, 100, by = 10)))
    expect_true(all(table(.s$id) == 11L))
    expect_true(all(.s$sim %in% c(0, 1)))
    expect_true(any(.s$sim == 1))
    .s1 <- withr::with_seed(5, as.data.frame(rxSolve(
      f() |> model(ev ~ tte(haz, eventType = "intervalCensored", maxEventNumber = 1, intervalLength = 10, rightCensoringTime = 100)),
      et(0) |> et(id = 1:20)
    )))
    # rows stop at the interval holding the first event
    .by <- split(.s1, .s1$id)
    for (.d in .by) {
      .w <- which(.d$sim == 1)
      expect_true(length(.w) <= 1L)
      if (length(.w) == 1L) expect_equal(.w, nrow(.d))
    }
  })

  test_that("tte() simulation is reproducible and thread-count independent", {
    .e <- et(c(0, 30)) |> et(id = 1:200)
    .a <- withr::with_seed(9, as.data.frame(rxSolve(.tteConst(), .e, cores = 1)))
    .b <- withr::with_seed(9, as.data.frame(rxSolve(.tteConst(), .e, cores = 2)))
    .c <- withr::with_seed(10, as.data.frame(rxSolve(.tteConst(), .e, cores = 1)))
    expect_identical(.a$time, .b$time)
    expect_false(identical(.a$time, .c$time))
  })

  test_that("tte() simulation requires liblsoda", {
    expect_error(rxSolve(.tteConst(), et(c(0, 30)), method = "dop853"), "liblsoda")
  })
})
