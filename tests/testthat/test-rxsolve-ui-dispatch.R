## `rxSolve.rxUi()` is not registered as an S3 method, so `rxSolve(ui, ...)`
## only reaches it when the call is made from somewhere that can see the
## rxode2 namespace.  Inside the package -- and inside `test_check()`, whose
## test environment is parented on the namespace -- it is found directly.
## From user code it is not: dispatch lands on `rxSolve.default()`, which hands
## the model back to `rxSolve()` further down.  These tests make that user path
## explicit by calling from an environment parented on the global environment,
## so the re-dispatch is exercised however the suite is run.
rxTest({

  .rxSolveAsUser <- function(...) {
    .env <- new.env(parent = globalenv())
    .env$.args <- list(...)
    eval(quote(do.call(rxode2::rxSolve, .args)), .env)
  }

  test_that("re-dispatching a rxUi keeps the meta block's sigma", {
    # the whole control is expanded into named arguments on the way back to
    # rxSolve(), and `meta` is only read for options the caller did not name --
    # so naming all of them used to hide `sigma` and the solve was rejected for
    # the residual parameters it draws from it
    .f <- function() {
      sigma <- lotri({
        err1 ~ 0.05
        err2 ~ 0.05
      })
      ini({
        ka <- 0.294
        cl <- 18.6
        v <- 40.2
      })
      model({
        d/dt(depot) <- -ka * depot
        d/dt(center) <- ka * depot - cl / v * center
        cp <- center / v + err1
        ef <- cp * exp(err2)
      })
    }
    .u <- suppressMessages(rxode2(.f))
    .e <- et(amt = 10) |> et(seq(0, 24, length.out = 13))
    .s <- .rxSolveAsUser(.u, events = .e)
    expect_true(inherits(.s, "rxSolve"))
    # the residual variables were simulated, not left at zero
    expect_false(isTRUE(all.equal(.s$cp, .s$center / 40.2)))
  })

  test_that("re-dispatching a rxUi with method='indLin' keeps its parameters", {
    # the matExp() conversion used to run on the ui itself, replacing it with a
    # plain model built from the ui's equations -- so the ini() values were
    # never supplied and the solve asked for them
    .f <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka)
        cl <- exp(tcl)
        v <- exp(tv)
        d/dt(depot) <- -ka * depot
        d/dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        cp ~ add(add.sd)
      })
    }
    .u <- suppressMessages(rxode2(.f))
    .e <- et(amt = 10) |> et(seq(0, 24, length.out = 13))
    .s <- .rxSolveAsUser(.u, events = .e, method = "indLin")
    expect_true(inherits(.s, "rxSolve"))
    expect_equal(.s$cp,
                 .rxSolveAsUser(.u, events = .e, method = "liblsoda",
                                useLinCmt = FALSE, atol = 1e-12, rtol = 1e-12)$cp,
                 tolerance = 1e-8)
  })

})
