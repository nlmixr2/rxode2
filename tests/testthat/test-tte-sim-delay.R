rxTest({
  .tight <- function(model, events) {
    as.data.frame(rxSolve(model, events, atol = 1e-12, rtol = 1e-12))
  }
  # H of a reference that integrates the hazard as an ordinary state, at the
  # simulated event times of each subject; cnt is the event count (locf)
  .tteRefH <- function(s, ref, dose, cnt = FALSE) {
    .ev <- s[s$sim == 1 & s$time > 0, ]
    unlist(lapply(split(.ev, .ev$id), function(.d) {
      .e <- as.data.frame(dose |> et(sort(unique(c(0, .d$time)))))
      if (cnt) .e$cnt <- findInterval(.e$time, .d$time)
      .g <- .tight(ref(), .e)
      .g <- .g[!duplicated(.g$time, fromLast = TRUE), ]
      .g$H[match(.d$time, .g$time)]
    }))
  }
  .dose <- et(amt = 100, cmt = "central")
  .win <- function(dose, n = 10) dose |> et(c(0, 48), cmt = "ev") |> et(id = 1:n)

  test_that("tte() with delay() written inside the hazard", {
    f <- function() {
      ini({
        k <- 0.3
        lambda <- 0.005
        beta <- 0.02
      })
      model({
        d/dt(central) <- -k * central
        ev ~ tte(lambda * exp(beta * delay(central, 2)))
      })
    }
    ref <- function() {
      ini({
        k <- 0.3
        lambda <- 0.005
        beta <- 0.02
      })
      model({
        d/dt(central) <- -k * central
        d/dt(H) <- lambda * exp(beta * delay(central, 2))
      })
    }
    .s <- withr::with_seed(3, as.data.frame(rxSolve(f(), .win(.dose))))
    .ev <- .s[.s$sim == 1 & .s$time > 0, ]
    expect_true(nrow(.ev) > 0)
    expect_equal(.ev$rx.tteH.ev, unname(.tteRefH(.s, ref, .dose)), tolerance = 1e-5)
  })

  test_that("tte() hazard of the delayed cumulative hazard", {
    f <- function() {
      ini({
        k <- 0.3
        lambda <- 0.01
      })
      model({
        d/dt(central) <- -k * central
        haz <- lambda * (1 + delay(rx.tteH.ev, 2))
        ev ~ tte(haz)
      })
    }
    ref <- function() {
      ini({
        k <- 0.3
        lambda <- 0.01
      })
      model({
        d/dt(central) <- -k * central
        d/dt(H) <- lambda * (1 + delay(H, 2))
      })
    }
    .s <- withr::with_seed(3, as.data.frame(rxSolve(f(), .win(.dose))))
    expect_equal(.s$rx.tteH.ev[.s$sim == 1 & .s$time > 0], unname(.tteRefH(.s, ref, .dose)), tolerance = 1e-5)
    # the hazard has no steady state to delay
    expect_error(
      rxSolve(f(), .win(et(amt = 100, cmt = "central", ii = 24, ss = 1))),
      "no steady state"
    )
    # ss without ii is an ordinary dose
    expect_no_error(rxSolve(f(), .win(et(amt = 100, cmt = "central", ss = 1), 2)))
    # a steady-state dose pushed by the model fails the subject in the solver
    .p <- f() |> model(if (t < 1e-8) evid_(0, 1, 100, 1, 0, 24, 0, 1), append = 2)
    expect_warning(.r <- as.data.frame(rxSolve(.p, .win(et(0, cmt = "central"), 2))), "could not solve")
    expect_true(all(is.na(.r$central)))
  })

  test_that("tte() events feeding a delayed state", {
    f <- function() {
      ini({
        k <- 0.3
        lambda <- 0.02
      })
      model({
        d/dt(central) <- -k * central
        d/dt(x) <- tteCount(ev) - x
        haz <- lambda * (1 + delay(x, 2))
        ev ~ tte(haz)
      })
    }
    ref <- function() {
      ini({
        k <- 0.3
        lambda <- 0.02
      })
      model({
        d/dt(central) <- -k * central
        d/dt(x) <- cnt - x
        d/dt(H) <- lambda * (1 + delay(x, 2))
      })
    }
    .s <- withr::with_seed(3, as.data.frame(rxSolve(f(), .win(.dose))))
    .ev <- .s[.s$sim == 1 & .s$time > 0, ]
    expect_true(any(table(.ev$id) > 1))
    expect_equal(.ev$rx.tteH.ev, unname(.tteRefH(.s, ref, .dose, cnt = TRUE)), tolerance = 1e-4)
  })

  test_that("tte() with delay() and steady-state doses", {
    f <- function() {
      ini({
        k <- 0.3
        lambda <- 0.005
        beta <- 0.02
      })
      model({
        d/dt(central) <- -k * delay(central, 2)
        haz <- lambda * exp(beta * central)
        ev ~ tte(haz)
      })
    }
    ref <- function() {
      ini({
        k <- 0.3
        lambda <- 0.005
        beta <- 0.02
      })
      model({
        d/dt(central) <- -k * delay(central, 2)
        d/dt(H) <- lambda * exp(beta * central)
      })
    }
    .e <- et(amt = 100, cmt = "central", ii = 24, ss = 1) |> et(c(0, 24), cmt = "ev") |> et(id = 1:30)
    .s <- withr::with_seed(3, as.data.frame(rxSolve(f(), .e)))
    .ev <- .s[.s$sim == 1, ]
    expect_true(nrow(.ev) > 0)
    # steady state reached with explicit doses, the window 960 h later
    .g <- .tight(ref(), et(amt = 100, cmt = "central", ii = 24, addl = 40) |> et(960 + sort(unique(c(0, .ev$time)))))
    .h <- .g$H[match(960 + .ev$time, .g$time)] - .g$H[.g$time == 960]
    expect_equal(.ev$rx.tteH.ev, .h, tolerance = 1e-4)
  })

  test_that("tte() interval censoring with delay() matches the exact events", {
    f <- function(type) {
      eval(bquote(function() {
        ini({
          k <- 0.3
          lambda <- 0.005
          beta <- 0.02
          eta.k ~ 0.01
        })
        model({
          d/dt(central) <- -k * exp(eta.k) * delay(central, 2)
          haz <- lambda * exp(beta * central)
          ev ~ tte(haz, maxEventNumber = 1, eventType = .(type), intervalLength = 6)
        })
      }))
    }
    .e <- .win(.dose, 50)
    .ex <- withr::with_seed(5, as.data.frame(rxSolve(f("exact")(), .e)))
    .ic <- withr::with_seed(5, as.data.frame(rxSolve(f("intervalCensored")(), .e)))
    .tEx <- .ex[.ex$sim == 1, ]
    .tIc <- .ic[.ic$sim == 1, ]
    expect_true(nrow(.tEx) > 0)
    expect_equal(.tIc$id, .tEx$id)
    expect_equal(.tIc$time, ceiling(.tEx$time / 6) * 6)
  })
})
