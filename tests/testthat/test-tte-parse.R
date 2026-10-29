rxTest({
  test_that("tte() endpoints parse into $predDf without new columns", {
    f <- function() {
      ini({
        lambda <- 0.1
        beta <- 0.01
      })
      model({
        haz <- lambda * exp(beta * time)
        seizure ~ tte(
          haz,
          eventType = "intervalCensored",
          maxEventNumber = 1,
          rightCensoringTime = 120,
          intervalLength = 10
        )
      })
    }
    ui <- f()
    expect_equal(as.character(ui$predDf$distribution), "tte")
    expect_equal(levels(ui$predDf$distribution)[20], "tte")
    expect_equal(ui$predDf$cond, "seizure")
    # options are re-read from the model, not stored as FIX parameters
    expect_equal(ui$iniDf$name, c("lambda", "beta"))
    expect_true(all(is.na(ui$iniDf$err)))
    .o <- .rxTteOptions(ui, ui$predDf[1, ])
    expect_identical(.o$hazard, quote(haz))
    expect_equal(.o$eventType, "intervalCensored")
    expect_equal(.o$maxEventNumber, 1)
    expect_equal(.o$rightCensoringTime, 120)
    expect_equal(.o$intervalLength, 10)
    expect_false(.o$dropout)
    expect_length(.o$err, 0)
  })

  test_that("tte() defaults and an expression hazard", {
    f <- function() {
      ini({
        lambda <- 0.1
      })
      model({
        k <- lambda
        ev ~ tte(lambda * 2)
      })
    }
    ui <- f()
    .o <- .rxTteOptions(ui, ui$predDf[1, ])
    expect_identical(.o$hazard, quote(lambda * 2))
    expect_equal(.o$eventType, "exact")
    expect_equal(.o$maxEventNumber, Inf)
    expect_true(is.na(.o$rightCensoringTime))
    expect_true(is.na(.o$intervalLength))
    expect_false(.o$dropout)
  })

  test_that("tte() works next to other endpoints", {
    f <- function() {
      ini({
        tcl <- log(2)
        tv <- log(10)
        add.sd <- 0.1
        lambda <- 0.1
      })
      model({
        cl <- exp(tcl)
        v <- exp(tv)
        d/dt(central) <- -cl / v * central
        cp <- central / v
        haz <- lambda * cp
        cp ~ add(add.sd)
        drop ~ tte(haz, maxEventNumber = 1, dropout = TRUE)
      })
    }
    ui <- f()
    expect_equal(as.character(ui$predDf$distribution), c("norm", "tte"))
    .o <- .rxTteOptions(ui, ui$predDf[2, ])
    expect_true(.o$dropout)
  })

  test_that("tte() option validation", {
    .bad <- function(rhs) {
      .f <- function() {
        ini({
          lambda <- 0.1
        })
        model({
          haz <- lambda
          ev ~ RHS
        })
      }
      body(.f) <- do.call(substitute, list(body(.f), list(RHS = rhs)))
      .f
    }
    .expectBad <- function(rhs, pattern) {
      expect_error(expect_message(rxode2(.bad(rhs)), pattern), "syntax/parsing")
    }
    .expectBad(quote(tte()), "hazard")
    .expectBad(quote(tte(haz, foo = 1)), "unknown 'tte\\(\\)' option")
    .expectBad(quote(tte(haz, eventType = "left")), "eventType")
    .expectBad(quote(tte(haz, maxEventNumber = 1.5)), "whole number")
    .expectBad(quote(tte(haz, maxEventNumber = -1)), "positive")
    .expectBad(quote(tte(haz, rightCensoringTime = 0)), "positive")
    .expectBad(quote(tte(haz, intervalLength = "a")), "positive")
    .expectBad(quote(tte(haz, dropout = TRUE)), "maxEventNumber=1")
    .expectBad(quote(tte(haz, haz)), "only unnamed")
    .expectBad(quote(tte(haz) + add(lambda)), "incorrectly added")
  })

  test_that("tte() options survive piping", {
    f <- function() {
      ini({
        lambda <- 0.1
      })
      model({
        haz <- lambda
        ev ~ tte(haz, maxEventNumber = 3, rightCensoringTime = 50)
      })
    }
    ui <- f() |> ini(lambda = 0.2)
    .o <- .rxTteOptions(ui, ui$predDf[1, ])
    expect_equal(.o$maxEventNumber, 3)
    expect_equal(.o$rightCensoringTime, 50)
    expect_equal(as.character(ui$predDf$distribution), "tte")
  })
})
