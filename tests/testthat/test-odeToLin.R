rxTest({

  ## Helper: tight-tolerance ODE vs linCmt comparison
  .chkConv <- function(mOde, ev, tol = 1e-5, seed = 1L) {
    .conv <- suppressMessages(odeToLin(mOde))
    set.seed(seed); .rOde <- suppressMessages(rxSolve(mOde,  ev, useLinCmt = FALSE))
    set.seed(seed); .rLin <- suppressMessages(rxSolve(.conv, ev))
    list(conv = .conv, diff = max(abs(.rOde$cp - .rLin$cp), na.rm = TRUE), tol = tol)
  }

  ## -----------------------------------------------------------------------
  ## Detection: models that SHOULD convert
  ## -----------------------------------------------------------------------

  test_that("odeToLin detects 1-cmt IV (no depot)", {
    .m <- suppressMessages(rxode2(function() {
      ini({ tcl <- 1; tv <- 3.45; add.sd <- 0.7 })
      model({
        cl <- exp(tcl); v <- exp(tv)
        d/dt(central) <- -cl/v * central
        cp <- central / v
        cp ~ add(add.sd)
      })
    }))
    .info <- rxode2:::.odeToLinDetect(.m)
    expect_equal(.info$ncmt,  1L)
    expect_equal(.info$oral0, 0L)
  })

  test_that("odeToLin detects 1-cmt oral", {
    .m <- suppressMessages(rxode2(function() {
      ini({ tka <- 0.45; tcl <- 1; tv <- 3.45; add.sd <- 0.7 })
      model({
        ka <- exp(tka); cl <- exp(tcl); v <- exp(tv)
        d/dt(depot)   <- -ka * depot
        d/dt(central) <- ka * depot - cl/v * central
        cp <- central / v
        cp ~ add(add.sd)
      })
    }))
    .info <- rxode2:::.odeToLinDetect(.m)
    expect_equal(.info$ncmt,  1L)
    expect_equal(.info$oral0, 1L)
  })

  test_that("odeToLin detects 2-cmt IV", {
    .m <- suppressMessages(rxode2(function() {
      ini({ tcl <- 1; tv <- 3.45; tq <- 0.5; tvp <- 6; add.sd <- 0.7 })
      model({
        cl <- exp(tcl); v <- exp(tv); q <- exp(tq); vp <- exp(tvp)
        d/dt(central) <- -(cl+q)/v * central + q/vp * periph
        d/dt(periph)  <- q/v * central - q/vp * periph
        cp <- central / v
        cp ~ add(add.sd)
      })
    }))
    .info <- rxode2:::.odeToLinDetect(.m)
    expect_equal(.info$ncmt,  2L)
    expect_equal(.info$oral0, 0L)
  })

  test_that("odeToLin detects 2-cmt oral", {
    .m <- suppressMessages(rxode2(function() {
      ini({ tka <- 0.45; tcl <- 1; tv <- 3.45; tq <- 0.5; tvp <- 6; add.sd <- 0.7 })
      model({
        ka <- exp(tka); cl <- exp(tcl); v <- exp(tv); q <- exp(tq); vp <- exp(tvp)
        d/dt(depot)   <- -ka * depot
        d/dt(central) <- ka * depot - (cl+q)/v * central + q/vp * periph
        d/dt(periph)  <- q/v * central - q/vp * periph
        cp <- central / v
        cp ~ add(add.sd)
      })
    }))
    .info <- rxode2:::.odeToLinDetect(.m)
    expect_equal(.info$ncmt,  2L)
    expect_equal(.info$oral0, 1L)
  })

  test_that("odeToLin detects 3-cmt IV", {
    .m <- suppressMessages(rxode2(function() {
      ini({ tcl <- 1; tv <- 3; tq <- 0.5; tvp <- 6; tq2 <- 0.3; tvp2 <- 10; add.sd <- 0.7 })
      model({
        cl <- exp(tcl); v <- exp(tv); q <- exp(tq); vp <- exp(tvp)
        q2 <- exp(tq2); vp2 <- exp(tvp2)
        d/dt(central) <- -(cl+q+q2)/v * central + q/vp * periph1 + q2/vp2 * periph2
        d/dt(periph1) <- q/v * central - q/vp * periph1
        d/dt(periph2) <- q2/v * central - q2/vp2 * periph2
        cp <- central / v
        cp ~ add(add.sd)
      })
    }))
    .info <- rxode2:::.odeToLinDetect(.m)
    expect_equal(.info$ncmt,  3L)
    expect_equal(.info$oral0, 0L)
  })

  test_that("odeToLin detects 3-cmt oral", {
    .m <- suppressMessages(rxode2(function() {
      ini({ tka <- 0.45; tcl <- 1; tv <- 3; tq <- 0.5; tvp <- 6;
            tq2 <- 0.3; tvp2 <- 10; add.sd <- 0.7 })
      model({
        ka <- exp(tka); cl <- exp(tcl); v <- exp(tv)
        q <- exp(tq); vp <- exp(tvp); q2 <- exp(tq2); vp2 <- exp(tvp2)
        d/dt(depot)   <- -ka * depot
        d/dt(central) <- ka * depot - (cl+q+q2)/v * central + q/vp * periph1 + q2/vp2 * periph2
        d/dt(periph1) <- q/v * central - q/vp * periph1
        d/dt(periph2) <- q2/v * central - q2/vp2 * periph2
        cp <- central / v
        cp ~ add(add.sd)
      })
    }))
    .info <- rxode2:::.odeToLinDetect(.m)
    expect_equal(.info$ncmt,  3L)
    expect_equal(.info$oral0, 1L)
  })

  ## -----------------------------------------------------------------------
  ## Detection: models that should NOT convert
  ## -----------------------------------------------------------------------

  test_that("odeToLin returns NULL for nonlinear ODE", {
    .m <- suppressMessages(rxode2(function() {
      ini({ tcl <- 1; tv <- 3.45; km <- 10; add.sd <- 0.7 })
      model({
        cl <- exp(tcl); v <- exp(tv); km <- exp(km)
        d/dt(central) <- -cl * central / (km + central)
        cp <- central / v
        cp ~ add(add.sd)
      })
    }))
    expect_null(rxode2:::.odeToLinDetect(.m))
  })

  test_that("odeToLin returns NULL for wrong topology (two depots)", {
    .m <- suppressMessages(rxode2(function() {
      ini({ tka1 <- 0.4; tka2 <- 0.2; tcl <- 1; tv <- 3.45; add.sd <- 0.7 })
      model({
        ka1 <- exp(tka1); ka2 <- exp(tka2); cl <- exp(tcl); v <- exp(tv)
        d/dt(depot1)  <- -ka1 * depot1
        d/dt(depot2)  <- -ka2 * depot2
        d/dt(central) <- ka1 * depot1 + ka2 * depot2 - cl/v * central
        cp <- central / v
        cp ~ add(add.sd)
      })
    }))
    expect_null(rxode2:::.odeToLinDetect(.m))
  })

  test_that("odeToLin returns NULL when output line is nonlinear in central", {
    .m <- suppressMessages(rxode2(function() {
      ini({ tcl <- 1; tv <- 3.45; add.sd <- 0.7 })
      model({
        cl <- exp(tcl); v <- exp(tv)
        d/dt(central) <- -cl/v * central
        cp <- central^2 / v
        cp ~ add(add.sd)
      })
    }))
    expect_null(rxode2:::.odeToLinDetect(.m))
  })

  ## -----------------------------------------------------------------------
  ## Correctness: converted model matches ODE (tight tolerances)
  ## -----------------------------------------------------------------------

  test_that("odeToLin 1-cmt oral conversion is numerically correct", {
    .m <- suppressMessages(rxode2(function() {
      ini({ tka <- 0.45; tcl <- 1; tv <- 3.45;
            eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1; add.sd <- 0.7 })
      model({
        ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v <- exp(tv + eta.v)
        d/dt(depot)   <- -ka * depot
        d/dt(central) <- ka * depot - cl/v * central
        cp <- central / v
        cp ~ add(add.sd)
      })
    }))
    .ev <- et(amt = 100, ii = 24, addl = 3) |> et(seq(0, 96, by = 1))
    .res <- .chkConv(.m, .ev)
    expect_equal(rxModelVars(.res$conv)$state, c("depot", "central"))
    expect_lt(.res$diff, .res$tol)
  })

  test_that("odeToLin 2-cmt oral conversion is numerically correct", {
    .m <- suppressMessages(rxode2(function() {
      ini({ tka <- 0.45; tcl <- 1; tv <- 3; tq <- 0.5; tvp <- 6;
            eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1; add.sd <- 0.7 })
      model({
        ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v <- exp(tv + eta.v)
        q <- exp(tq); vp <- exp(tvp)
        d/dt(depot)   <- -ka * depot
        d/dt(central) <- ka * depot - (cl+q)/v * central + q/vp * periph
        d/dt(periph)  <- q/v * central - q/vp * periph
        cp <- central / v
        cp ~ add(add.sd)
      })
    }))
    .ev <- et(amt = 100, ii = 24, addl = 3) |> et(seq(0, 96, by = 1))
    .res <- .chkConv(.m, .ev)
    expect_equal(rxModelVars(.res$conv)$state, c("depot", "central"))
    expect_lt(.res$diff, .res$tol)
  })

  test_that("odeToLin 3-cmt oral conversion is numerically correct", {
    .m <- suppressMessages(rxode2(function() {
      ini({ tka <- 0.45; tcl <- 1; tv <- 3; tq <- 0.5; tvp <- 6;
            tq2 <- 0.3; tvp2 <- 10;
            eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1; add.sd <- 0.7 })
      model({
        ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v <- exp(tv + eta.v)
        q <- exp(tq); vp <- exp(tvp); q2 <- exp(tq2); vp2 <- exp(tvp2)
        d/dt(depot)   <- -ka * depot
        d/dt(central) <- ka * depot - (cl+q+q2)/v * central + q/vp * periph1 + q2/vp2 * periph2
        d/dt(periph1) <- q/v * central - q/vp * periph1
        d/dt(periph2) <- q2/v * central - q2/vp2 * periph2
        cp <- central / v
        cp ~ add(add.sd)
      })
    }))
    .ev <- et(amt = 100, ii = 24, addl = 3) |> et(seq(0, 96, by = 1))
    .res <- .chkConv(.m, .ev)
    expect_equal(rxModelVars(.res$conv)$state, c("depot", "central"))
    expect_lt(.res$diff, .res$tol)
  })

  ## -----------------------------------------------------------------------
  ## Non-standard compartment names (renamed to depot/central)
  ## -----------------------------------------------------------------------

  test_that("odeToLin renames non-standard compartment names to linCmt standard", {
    .m <- suppressMessages(rxode2(function() {
      ini({
        tka <- 0.45; tcl <- 1; tv <- 3.45;
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
    .conv <- suppressMessages(odeToLin(.m))
    expect_equal(rxModelVars(.conv)$state, c("depot", "central"))

    .ev <- et(amt = 100, ii = 24, addl = 2) |> et(seq(0, 72, by = 1))
    set.seed(1); .rOde <- suppressMessages(rxSolve(.m,    .ev, useLinCmt = FALSE))
    set.seed(1); .rLin <- suppressMessages(rxSolve(.conv, .ev))
    expect_lt(max(abs(.rOde$cp - .rLin$cp), na.rm = TRUE), 1e-5)
  })

  test_that("odeToLin renames f/rate modifiers for non-standard compartment names", {
    .m <- suppressMessages(rxode2(function() {
      ini({
        tka <- 0.45; trate <- 0.4; tcl <- 1; tv <- 3.45
        fDepot <- logit(0.5)
        eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1; add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v <- exp(tv + eta.v)
        d/dt(depot)  <- -ka * depot
        d/dt(center) <- ka * depot - cl/v * center
        f(depot)     <- expit(fDepot)
        f(center)    <- 1 - expit(fDepot)
        rate(center) <- exp(trate)
        cp <- center / v
        cp ~ add(add.sd)
      })
    }))
    .conv <- tryCatch(suppressMessages(odeToLin(.m)), error = function(e) e)
    expect_false(inherits(.conv, "error"),
                 label = "odeToLin with f/rate on non-standard cmt name must not error")
    expect_equal(rxModelVars(.conv)$state, c("depot", "central"))
  })

  test_that("useLinCmt=TRUE solve works for model with non-standard cmt names + f/rate", {
    # Regression: test-et.R 'Fix for #732' -- model uses 'center' not 'central'
    .m <- suppressMessages(rxode2(function() {
      ini({
        tka <- 0.45; trate <- 0.4; tcl <- 1; tv <- 3.45
        fDepot <- logit(0.5)
        eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1; add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v <- exp(tv + eta.v)
        d/dt(depot)  <- -ka * depot
        d/dt(center) <- ka * depot - cl/v * center
        f(depot)     <- expit(fDepot)
        f(center)    <- 1 - expit(fDepot)
        rate(center) <- exp(trate)
        cp <- center / v
        cp ~ add(add.sd)
      })
    }))
    .ev <- et(amt = 320, evid = 1, cmt = 1, time = 0) |>
      et(amt = 320, evid = 1, cmt = 2, time = 0) |>
      et(amt = 320, evid = 4, time = 72, cmt = 1) |>
      et(amt = 320, evid = 1, time = 72, cmt = 2) |>
      et(seq(0, 100, by = 1)) |>
      et(id = 1:4)
    .d <- suppressMessages(rxSolve(.m, .ev, addDosing = TRUE, useLinCmt = TRUE,
                                   seed = 123))
    .times <- sort(unique(.d[.d$evid == 1L & .d$cmt == 2L, "time"]))
    expect_equal(.times, c(0, 72))
  })

  ## -----------------------------------------------------------------------
  ## useLinCmt flag controls ODE vs linCmt path
  ## -----------------------------------------------------------------------

  test_that("useLinCmt flag controls whether ODE or linCmt solver is used", {
    .m <- suppressMessages(rxode2(function() {
      ini({ tka <- 0.45; tcl <- 1; tv <- 3.45; add.sd <- 0.7 })
      model({
        ka <- exp(tka); cl <- exp(tcl); v <- exp(tv)
        d/dt(depot)   <- -ka * depot
        d/dt(central) <- ka * depot - cl/v * central
        cp <- central / v
        cp ~ add(add.sd)
      })
    }))
    .ev <- et(amt = 100, ii = 24, addl = 1) |> et(seq(0, 48, by = 1))
    .mLin <- suppressMessages(odeToLin(.m))

    .getDll <- function(r) {
      get("dll", envir = attr(attr(r, "class"), ".rxode2.env"), inherits = FALSE)
    }

    # useLinCmt=TRUE must use the same DLL as explicit odeToLin conversion
    .rLinDirect <- suppressMessages(rxSolve(.mLin, .ev))
    .rLinAuto   <- suppressMessages(rxSolve(.m,    .ev, useLinCmt = TRUE))
    expect_equal(.getDll(.rLinAuto), .getDll(.rLinDirect),
                 label = "useLinCmt=TRUE must use same DLL as odeToLin conversion")

    # useLinCmt=FALSE must use a different DLL (the original ODE model)
    .rOde <- suppressMessages(rxSolve(.m, .ev, useLinCmt = FALSE))
    expect_false(identical(.getDll(.rOde), .getDll(.rLinAuto)),
                 label = "useLinCmt=FALSE must use original ODE DLL, not linCmt DLL")
  })

})
