rxTest({

  test_that("odeToLin renames non-standard compartment names to linCmt standard", {
    # ODE model uses 'center' (not 'central'); odeToLin must rename f/rate
    # references so the converted model parses correctly.
    m <- suppressMessages(rxode2(function() {
      ini({
        tka <- 0.45; tcl <- 1; tv <- 3.45
        eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1; add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v <- exp(tv + eta.v)
        d/dt(depot)  <- -ka * depot
        d/dt(center) <- ka * depot - cl/v * center
        cp <- center / v
        cp ~ add(add.sd)
      })
    }))
    .conv <- suppressMessages(odeToLin(m))
    expect_equal(rxModelVars(.conv)$state, c("depot", "central"))

    # Results should match the ODE model (tight tolerances)
    ev <- et(amt = 100, ii = 24, addl = 2) |> et(seq(0, 72, by = 1))
    set.seed(1)
    r_ode <- suppressMessages(rxSolve(m,   ev, useLinCmt = FALSE))
    set.seed(1)
    r_lin <- suppressMessages(rxSolve(.conv, ev))
    expect_lt(max(abs(r_ode$cp - r_lin$cp), na.rm = TRUE), 1e-5)
  })

  test_that("odeToLin preserves f/rate modifiers with non-standard compartment names", {
    # Model uses 'center' with f() and rate() — conversion must rename to 'central'
    m <- suppressMessages(rxode2(function() {
      ini({
        tka <- 0.45; trate <- 0.4; tcl <- 1; tv <- 3.45
        fDepot <- logit(0.5)
        eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1; add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v <- exp(tv + eta.v)
        d/dt(depot)  <- -ka * depot
        d/dt(center) <- ka * depot - cl/v * center
        f(depot)  <- expit(fDepot)
        f(center) <- 1 - expit(fDepot)
        rate(center) <- exp(trate)
        cp <- center / v
        cp ~ add(add.sd)
      })
    }))

    # Conversion must succeed (no parse error)
    .conv <- tryCatch(suppressMessages(odeToLin(m)), error = function(e) e)
    expect_false(inherits(.conv, "error"),
                 label = "odeToLin with f/rate on non-standard cmt name")
    expect_equal(rxModelVars(.conv)$state, c("depot", "central"))
  })

  test_that("useLinCmt=TRUE solve works for model with non-standard cmt names + f/rate", {
    # Regression test for test-et.R Fix for #732: model uses 'center' not 'central'
    m <- suppressMessages(rxode2(function() {
      ini({
        tka <- 0.45; trate <- 0.4; tcl <- 1; tv <- 3.45
        fDepot <- logit(0.5)
        eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1; add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v <- exp(tv + eta.v)
        d/dt(depot)  <- -ka * depot
        d/dt(center) <- ka * depot - cl/v * center
        f(depot)  <- expit(fDepot)
        f(center) <- 1 - expit(fDepot)
        rate(center) <- exp(trate)
        cp <- center / v
        cp ~ add(add.sd)
      })
    }))
    ev <- et(amt = 320, evid = 1, cmt = 1, time = 0) |>
      et(amt = 320, evid = 1, cmt = 2, time = 0) |>
      et(amt = 320, evid = 4, time = 72, cmt = 1) |>
      et(amt = 320, evid = 1, time = 72, cmt = 2) |>
      et(seq(0, 100, by = 1)) |>
      et(id = 1:4)
    d <- suppressMessages(rxSolve(m, ev, addDosing = TRUE, useLinCmt = TRUE,
                                  seed = 123))
    times <- sort(unique(d[d$evid == 1 & d$cmt == 2, "time"]))
    expect_equal(times, c(0, 72))
  })

})
