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

  test_that("tte() hazards fed back by tteTime() and tteCount()", {
    f <- function() {
      ini({
        te <- 5
        sh <- 2
      })
      model({
        haz <- sh / te * ((time - tteTime(ev)) / te)^(sh - 1)
        ev ~ tte(haz, maxEventNumber = 2)
      })
    }
    .s <- withr::with_seed(6, as.data.frame(rxSolve(f(), et(c(0, 500)) |> et(id = 1:500))))
    .s <- .s[.s$sim == 1, ]
    .g <- unlist(lapply(split(.s$time, .s$id), diff))
    expect_gt(stats::ks.test(.g, function(x) pweibull(x, 2, 5))$p.value, 0.01)
    g <- function() {
      ini({
        lambda <- 0.1
      })
      model({
        haz <- lambda * (1 + tteCount(ev))
        ev ~ tte(haz, maxEventNumber = 3)
      })
    }
    .s <- withr::with_seed(7, as.data.frame(rxSolve(g(), et(c(0, 1000)) |> et(id = 1:500))))
    .s <- .s[.s$sim == 1, ]
    .s$n <- ave(.s$time, .s$id, FUN = seq_along)
    .s$gap <- ave(.s$time, .s$id, FUN = function(t) diff(c(0, t)))
    expect_equal(as.numeric(tapply(.s$gap, .s$n, mean)), 1 / (0.1 * 1:3), tolerance = 0.15)
    expect_error(
      rxode2(function() {
        ini({
          lambda <- 0.1
        })
        model({
          haz <- lambda * tteTime(x)
          ev ~ tte(haz)
        })
      }),
      "does not name a tte"
    )
  })

  .ttePk <- function() {
    ini({
      tcl <- log(1)
      tv <- log(10)
      add.sd <- 0.1
      lambda <- 0.02
      eta.cl ~ 0.1
    })
    model({
      cl <- exp(tcl + eta.cl)
      v <- exp(tv)
      d / dt(central) <- -cl / v * central
      cp <- central / v
      haz <- lambda * (1 + cp)
      cp ~ add(add.sd)
      drop ~ tte(haz, maxEventNumber = 1, dropout = TRUE)
    })
  }

  test_that("tte() next to a PK endpoint, with dropout", {
    .e <- et(amt = 100, cmt = "central") |>
      et(seq(0, 48, by = 4), cmt = "cp") |>
      et(c(0, 48), cmt = "drop") |>
      et(id = 1:20)
    .s <- withr::with_seed(8, as.data.frame(rxSolve(.ttePk(), .e)))
    for (.d in split(.s, .s$id)) {
      .ev <- .d$time[.d$CMT == 4 & .d$sim == 1]
      if (length(.ev) == 1L) {
        # no observation of any endpoint after the dropout
        expect_true(all(.d$time <= .ev))
      } else {
        expect_true(any(.d$CMT == 3 & .d$time == 48))
      }
    }
    .noDrop <- .ttePk() |> model(drop ~ tte(haz, maxEventNumber = 1))
    .s2 <- withr::with_seed(8, as.data.frame(rxSolve(.noDrop, .e)))
    expect_true(all(tapply(.s2$time[.s2$CMT == 3], .s2$id[.s2$CMT == 3], max) == 48))
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
