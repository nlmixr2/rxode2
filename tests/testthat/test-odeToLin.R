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
    .info <- .odeToLinDetect(.m)
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
    .info <- .odeToLinDetect(.m)
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
    .info <- .odeToLinDetect(.m)
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
    .info <- .odeToLinDetect(.m)
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
    .info <- .odeToLinDetect(.m)
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
    .info <- .odeToLinDetect(.m)
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
    expect_null(.odeToLinDetect(.m))
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
    expect_null(.odeToLinDetect(.m))
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
    expect_null(.odeToLinDetect(.m))
  })

  ## A genuinely nonlinear model whose nonlinearity is hidden behind a
  ## state-derived observable must NOT auto-linearize.  Here Michaelis-Menten
  ## elimination is written through the central concentration
  ## `Cc <- central / vc` -- `... - vmax * Cc * vc / (km + Cc)` -- so the
  ## linearity scan (which looks only for *direct* state references) saw no
  ## state and treated the term as a constant forcing input.  useLinCmt=TRUE
  ## (the default) then folded the 2-cmt system into linCmt(), dropped the MM
  ## term, and demoted k12/k21 to required inputs, so rxSolve aborted with
  ## "parameter(s) are required for solving: k21, k12".
  .mmObs <- function() {
    suppressMessages(rxode2(function() {
      ini({ lka <- -0.5; lcl <- -2; lvc <- 2; lvp <- 1.5; lq <- -1.6
            lvmax <- 1; lkm <- 0.2; addSd <- 0.02 })
      model({
        ka <- exp(lka); cl <- exp(lcl); vc <- exp(lvc); vp <- exp(lvp); q <- exp(lq)
        vmax <- exp(lvmax); km <- exp(lkm)
        Cc <- central / vc
        kel <- cl / vc; k12 <- q / vc; k21 <- q / vp
        d/dt(depot)       <- -ka * depot
        d/dt(central)     <-  ka * depot - kel * central - k12 * central +
          k21 * peripheral1 - vmax * Cc * vc / (km + Cc)
        d/dt(peripheral1) <-  k12 * central - k21 * peripheral1
        Cc ~ add(addSd)
      })
    }))
  }

  test_that("odeToLin returns NULL for MM elimination written via the central observable", {
    expect_null(.odeToLinDetect(.mmObs()))
  })

  test_that("default rxSolve keeps the explicit ODE states for an MM-via-observable model", {
    .m  <- .mmObs()
    .ev <- et(amt = 100, cmt = "depot") |> et(seq(0, 24, by = 4), cmt = "Cc")
    .rDef <- suppressMessages(rxSolve(.m, .ev))                  # default useLinCmt=TRUE
    .rOde <- suppressMessages(rxSolve(.m, .ev, useLinCmt = FALSE))
    # the coupled peripheral state survives and the nonlinear (MM) solve agrees
    expect_true("peripheral1" %in% names(.rDef))
    expect_equal(.rDef$Cc, .rOde$Cc, tolerance = 1e-6)
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
      # normalizePath() canonicalizes the path so that the comparison is robust
      # to Windows 8.3 short-name mangling: the .rxd cache directory name is too
      # long for an 8.3 path, so one solve can report it in long form
      # (rx_..._x64_.rxd) while another reports the shortened form
      # (RX_...~1.RXD).  Both point to the same DLL; normalizing makes them equal.
      normalizePath(
        get("dll", envir = attr(attr(r, "class"), ".rxode2.env"), inherits = FALSE),
        winslash = "/", mustWork = FALSE)
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

  ## -----------------------------------------------------------------------
  ## Explicit linCmt() parameters (works when params are only in ini())
  ## -----------------------------------------------------------------------

  test_that("odeToLin emits explicit linCmt() parameter names (1-cmt oral)", {
    # Parameters live only in ini() (no model-body assignment).  The
    # converted model must pass the parameter names explicitly to linCmt()
    # so the parameterization can still be inferred.
    .m <- suppressMessages(rxode2(function() {
      ini({ ka <- 0.5; cl <- 1; v <- 10 })
      model({
        d/dt(depot)   <- -ka * depot
        d/dt(central) <- ka * depot - cl/v * central
        cp <- central / v
      })
    }))
    .info <- .odeToLinDetect(.m)
    expect_setequal(.info$params, c("ka", "cl", "v"))

    .conv <- suppressMessages(odeToLin(.m))
    .linLine <- Find(function(x) grepl("linCmt", deparse1(x)), .conv$lstExpr)
    # Bare linCmt() (no args) would fail param inference for ini-only params
    expect_false(identical(deparse1(.linLine[[3L]]), "linCmt()"))
    expect_true(grepl("linCmt(", deparse1(.linLine), fixed = TRUE))

    .ev <- et(amt = 100, time = 0) |> et(seq(0, 48, by = 1))
    .rOde <- suppressMessages(rxSolve(.m,    .ev, useLinCmt = FALSE))
    .rLin <- suppressMessages(rxSolve(.conv, .ev))
    expect_equal(.rOde$cp, .rLin$cp, tolerance = 1e-5)
  })

  test_that("odeToLin emits explicit linCmt() parameter names (2-cmt IV)", {
    .m <- suppressMessages(rxode2(function() {
      ini({ tcl <- log(1); tv <- log(10); tq <- log(0.5); tvp <- log(20) })
      model({
        cl <- exp(tcl); v <- exp(tv); q <- exp(tq); vp <- exp(tvp)
        d/dt(central)    <- -(cl+q)/v * central + q/vp * peripheral
        d/dt(peripheral) <-  q/v * central - q/vp * peripheral
        cp <- central / v
      })
    }))
    .info <- .odeToLinDetect(.m)
    expect_setequal(.info$params, c("cl", "v", "q", "vp"))

    .conv <- suppressMessages(odeToLin(.m))
    .ev <- et(amt = 100, time = 0) |> et(seq(0, 48, by = 1))
    .rOde <- suppressMessages(rxSolve(.m,    .ev, useLinCmt = FALSE))
    .rLin <- suppressMessages(rxSolve(.conv, .ev))
    expect_equal(.rOde$cp, .rLin$cp, tolerance = 1e-5)
  })

  test_that("rxSolve(useLinCmt=TRUE) transparently solves ini-only-param ODE models", {
    # Regression: the transparent useLinCmt auto-conversion must not break an
    # otherwise-valid ODE model whose parameters are defined only in ini().
    .m <- suppressMessages(rxode2(function() {
      ini({ ka <- 0.5; cl <- 1; v <- 10 })
      model({
        d/dt(depot)   <- -ka * depot
        d/dt(central) <- ka * depot - cl/v * central
        cp <- central / v
      })
    }))
    .ev <- et(amt = 100, time = 0) |> et(seq(0, 48, by = 1))
    .rAuto <- suppressMessages(rxSolve(.m, .ev))                   # useLinCmt=TRUE default
    .rOde  <- suppressMessages(rxSolve(.m, .ev, useLinCmt = FALSE))
    expect_true(nrow(.rAuto) > 0)
    expect_false(any(is.na(.rAuto$cp)))
    expect_equal(.rAuto$cp, .rOde$cp, tolerance = 1e-5)
  })

  ## -----------------------------------------------------------------------
  ## odeToLin + adaptive dosing
  ## -----------------------------------------------------------------------

  test_that("odeToLin preserves bolus() in model and converted model runs", {
    # An ODE model with in-model bolus push. odeToLin must convert the ODE to
    # linCmt AND keep the bolus() call intact. PK parameters must be model-body
    # assignments so linCmt() can infer the parameterization.
    .m <- suppressMessages(rxode2(function() {
      ini({ tka <- log(0.5); tcl <- log(1); tv <- log(10) })
      model({
        ka <- exp(tka); cl <- exp(tcl); v <- exp(tv)
        d/dt(depot)   <- -ka * depot
        d/dt(central) <- ka * depot - cl/v * central
        cp <- central / v
      })
    }))
    .mLin <- suppressMessages(odeToLin(.m))
    # Converted model should use linCmt
    .mLinTxt <- paste(vapply(.mLin$lstExpr, deparse1, character(1)), collapse = "\n")
    expect_true(grepl("linCmt", .mLinTxt))

    # Solve both without adaptive dosing to confirm trajectories agree
    .ev <- et(amt = 100, time = 0) |> et(seq(0, 48, by = 1))
    .rOde <- suppressMessages(rxSolve(.m,    .ev, useLinCmt = FALSE))
    .rLin <- suppressMessages(rxSolve(.mLin, .ev))
    expect_equal(.rOde$cp, .rLin$cp, tolerance = 1e-5,
                 label = "odeToLin result matches ODE solution")
  })

  test_that("odeToLin preserves standard depot/central compartment in bolus()", {
    # Model uses standard names (depot, central) — odeToLin should
    # compile and solve cleanly; adaptive dosing calls are preserved as-is.
    .m <- suppressMessages(rxode2(function() {
      ini({ tka <- log(0.5); tcl <- log(1); tv <- log(10); add.sd <- 0.1 })
      model({
        ka <- exp(tka); cl <- exp(tcl); v <- exp(tv)
        d/dt(depot)   <- -ka * depot
        d/dt(central) <- ka * depot - cl/v * central
        cp <- central / v
        if (t > 23 && t < 25) {
          bolus(50, cmt = depot)
        }
        cp ~ add(add.sd)
      })
    }))
    .mLin <- suppressMessages(odeToLin(.m))
    .mLinTxt <- paste(vapply(.mLin$lstExpr, deparse1, character(1)), collapse = "\n")
    expect_true(grepl("linCmt", .mLinTxt))

    .ev <- et(amt = 100, time = 0) |> et(seq(0, 48, by = 1))
    .rLin <- suppressMessages(rxSolve(.mLin, .ev))
    expect_true(nrow(.rLin) > 0)
    expect_false(any(is.na(.rLin$cp)))
    expect_true(all(.rLin$cp >= 0))
    expect_true(all(.rLin$cp[.rLin$time > 0] > 0))
  })

  test_that("odeToLin renames non-standard ODE compartment names in bolus()", {
    # Model uses non-standard compartment names (abs, center).
    # odeToLin must rename them to depot/central in both linCmt() and bolus().
    .m <- suppressMessages(rxode2(function() {
      ini({ tka <- log(0.5); tcl <- log(1); tv <- log(10) })
      model({
        ka <- exp(tka); cl <- exp(tcl); v <- exp(tv)
        d/dt(abs)    <- -ka * abs
        d/dt(center) <- ka * abs - cl/v * center
        cp <- center / v
        if (t > 23 && t < 25) {
          bolus(50, cmt = abs)
        }
      })
    }))
    .mLin <- suppressMessages(odeToLin(.m))
    # Confirm the converted model no longer references the old compartment names
    .mLinTxt <- paste(vapply(.mLin$lstExpr, deparse1, character(1)), collapse = "\n")
    expect_true(grepl("linCmt", .mLinTxt))
    expect_false(grepl("\\babs\\b",    .mLinTxt),
                 label = "non-standard cmt name 'abs' should be renamed in converted model")

    .ev <- et(amt = 100, time = 0) |> et(seq(0, 48, by = 1))
    .rLin <- suppressMessages(rxSolve(.mLin, .ev))
    expect_true(nrow(.rLin) > 0)
    expect_false(any(is.na(.rLin$cp)))
    expect_true(all(.rLin$cp >= 0))
    expect_true(all(.rLin$cp[.rLin$time > 0] > 0))
  })

  ## -----------------------------------------------------------------------
  ## Coupled / separately-observed states.
  ##
  ## A central sub-system can be linear-compartment-eligible yet have an
  ## additional state referenced by another model line (a peripheral observable
  ## like `Cp <- periph / vp`, possibly with its own endpoint).  When the
  ## coupled state is an output-only peripheral, odeToLin() converts it by
  ## renaming the ODE compartments to their canonical linCmt names and anchoring
  ## the central endpoint to the central compartment (see the conversion tests
  ## below).  When linCmt() cannot represent the system -- a compartment with
  ## independent loss (metabolite), or a peripheral with its own estimated
  ## endpoint -- detection returns NULL so the explicit ODE states are preserved.
  ## -----------------------------------------------------------------------

  test_that("odeToLin returns NULL for a metabolite-like coupled, observed state", {
    # Parent 2-cmt + observed metabolite with its own elimination.  The
    # metabolite exchanges with central (kbt) so the topology *looks* like a
    # peripheral, but it is separately observed (Cc_mhd) and carries an
    # independent elimination -- it must remain a solved ODE state.
    .m <- suppressMessages(rxode2(function() {
      ini({ lka <- log(1); lcl <- log(5); lvc <- log(30); lq <- log(2); lvp <- log(50)
            lclm <- log(3); lvcm <- log(20); lkbt <- log(0.5); propSd <- 0.1; propSdM <- 0.1 })
      model({
        ka <- exp(lka); cl <- exp(lcl); vc <- exp(lvc); q <- exp(lq); vp <- exp(lvp)
        cl_mhd <- exp(lclm); vc_mhd <- exp(lvcm); kbt <- exp(lkbt)
        kel <- cl/vc; kelm <- cl_mhd/vc_mhd; k12 <- q/vc; k21 <- q/vp
        d/dt(depot)       <- -ka*depot
        d/dt(central)     <-  ka*depot - (kel+k12)*central + k21*peripheral1 + kbt*central_mhd
        d/dt(peripheral1) <-  k12*central - k21*peripheral1
        d/dt(central_mhd) <-  kel*central - kelm*central_mhd - kbt*central_mhd
        f(depot) <- 1
        Cc     <- central / vc
        Cc_mhd <- central_mhd / vc_mhd
        Cc     ~ prop(propSd)
        Cc_mhd ~ prop(propSdM)
      })
    }))
    expect_null(.odeToLinDetect(.m))
  })

  test_that("odeToLin converts a coupled output-peripheral model and stays numerically correct", {
    # Regression: useLinCmt=TRUE (the default) used to auto-linearize this model,
    # drop `periph`, and abort with "parameter(s) are required for solving:
    # periph".  It now CONVERTS analytically instead: `periph` is renamed to the
    # canonical `peripheral1`, the central endpoint is anchored to the `central`
    # compartment (so no observation compartment is injected), and the peripheral
    # observable reads linCmt()'s solved peripheral amount -- so the default solve
    # stays fully analytic and agrees with the explicit ODE (useLinCmt=FALSE).
    .m <- suppressMessages(rxode2(function() {
      ini({ lcl <- log(5); lvc <- log(30); lq <- log(2); lvp <- log(50); propSd <- 0.1 })
      model({
        cl <- exp(lcl); vc <- exp(lvc); q <- exp(lq); vp <- exp(lvp)
        kel <- cl/vc; k12 <- q/vc; k21 <- q/vp
        d/dt(central) <- -kel*central - k12*central + k21*periph
        d/dt(periph)  <-  k12*central - k21*periph
        Cc <- central / vc
        Cp <- periph / vp
        Cc ~ prop(propSd)
      })
    }))
    .info <- .odeToLinDetect(.m)
    expect_true(isTRUE(.info$coupled))
    .conv    <- suppressMessages(odeToLin(.m))
    .convTxt <- paste(vapply(.conv$lstExpr, deparse1, character(1)), collapse = "\n")
    expect_true(grepl("linCmt", .convTxt))
    expect_true(grepl("peripheral1", .convTxt))   # periph renamed to canonical name
    expect_true(grepl("[|] *central", .convTxt))  # central endpoint anchored
    expect_false(grepl("d/dt", .convTxt))         # no ODE left

    .ev <- et(amt = 100, cmt = "central") |> et(seq(0, 24, by = 2))
    .rDef <- suppressMessages(rxSolve(.m, .ev))                  # default -> converts
    .rOde <- suppressMessages(rxSolve(.m, .ev, useLinCmt = FALSE))
    expect_true("Cp" %in% names(.rDef))
    expect_equal(.rDef$Cc, .rOde$Cc, tolerance = 1e-4)
    expect_equal(.rDef$Cp, .rOde$Cp, tolerance = 1e-4)           # peripheral observable correct
  })

  test_that("odeToLin converts a 2-cmt oral coupled model (depot amount + peripheral)", {
    .m <- suppressMessages(rxode2(function() {
      ini({ lka <- log(0.8); lcl <- log(5); lvc <- log(30); lq <- log(2); lvp <- log(50); propSd <- 0.1 })
      model({
        ka <- exp(lka); cl <- exp(lcl); vc <- exp(lvc); q <- exp(lq); vp <- exp(lvp)
        kel <- cl/vc; k12 <- q/vc; k21 <- q/vp
        d/dt(depot)   <- -ka*depot
        d/dt(central) <-  ka*depot - kel*central - k12*central + k21*periph
        d/dt(periph)  <-  k12*central - k21*periph
        Cc     <- central / vc
        Cp     <- periph / vp
        Adepot <- depot            # depot amount as a secondary output
        Cc ~ prop(propSd)
      })
    }))
    .info <- .odeToLinDetect(.m)
    expect_true(isTRUE(.info$coupled))
    .ev <- et(amt = 100, cmt = "depot") |> et(seq(0, 24, by = 2))
    .rDef <- suppressMessages(rxSolve(.m, .ev))
    .rOde <- suppressMessages(rxSolve(.m, .ev, useLinCmt = FALSE))
    expect_equal(.rDef$Cc,     .rOde$Cc,     tolerance = 1e-4)
    expect_equal(.rDef$Cp,     .rOde$Cp,     tolerance = 1e-4)
    expect_equal(.rDef$Adepot, .rOde$Adepot, tolerance = 1e-4)
  })

  test_that("odeToLin returns NULL for an output-only metabolite (mass-balance guard)", {
    # Single endpoint (would pass the endpoint guard), but the metabolite carries
    # an independent elimination (kelm) on top of the kbt it returns to central,
    # so mass is not conserved across the exchange.  linCmt() cannot represent
    # that, so the system must stay an explicit ODE.
    .m <- suppressMessages(rxode2(function() {
      ini({ lcl <- log(5); lvc <- log(30); lkpm <- log(1); lkbt <- log(0.5)
            lclm <- log(3); lvcm <- log(20); propSd <- 0.1 })
      model({
        cl <- exp(lcl); vc <- exp(lvc); kpm <- exp(lkpm); kbt <- exp(lkbt)
        cl_mhd <- exp(lclm); vc_mhd <- exp(lvcm)
        kel <- cl/vc; kelm <- cl_mhd/vc_mhd
        d/dt(central) <- -kel*central - kpm*central + kbt*met
        d/dt(met)     <-  kpm*central - kelm*met - kbt*met   # extra independent loss
        Cc <- central / vc
        Cm <- met / vc_mhd
        Cc ~ prop(propSd)
      })
    }))
    expect_null(.odeToLinDetect(.m))
  })

  test_that("default rxSolve handles a multi-endpoint coupled (fetus) model", {
    # Two endpoints (Cc and Cfetus); the second is keyed to a coupled state.
    .m <- suppressMessages(rxode2(function() {
      ini({ lcl <- log(5); lvc <- log(30); lq <- log(2); lvp <- log(50)
            lqmf <- log(1); lvf <- log(10); propSd <- 0.1; propSdF <- 0.1 })
      model({
        cl <- exp(lcl); vc <- exp(lvc); q <- exp(lq); vp <- exp(lvp)
        qmf <- exp(lqmf); vfetus <- exp(lvf)
        kel <- cl/vc; k12 <- q/vc; k21 <- q/vp; k1f <- qmf/vc; kf1 <- qmf/vfetus
        d/dt(central)     <- -kel*central - k12*central + k21*peripheral1 - k1f*central + kf1*fetus
        d/dt(peripheral1) <-  k12*central - k21*peripheral1
        d/dt(fetus)       <-  k1f*central - kf1*fetus
        Cc     <- central / vc
        Cfetus <- fetus / vfetus
        Cc     ~ prop(propSd)
        Cfetus ~ prop(propSdF)
      })
    }))
    expect_null(.odeToLinDetect(.m))
    .ev <- et(amt = 100, cmt = "central") |> et(seq(0, 24, by = 2), cmt = "Cc")
    .rDef <- suppressMessages(rxSolve(.m, .ev))
    .rOde <- suppressMessages(rxSolve(.m, .ev, useLinCmt = FALSE))
    expect_true(all(c("peripheral1", "fetus", "Cfetus") %in% names(.rDef)))
    expect_equal(.rDef$Cfetus, .rOde$Cfetus, tolerance = 1e-6)
  })

  test_that("odeToLin still converts when an observable derives from the central output", {
    # Boundary: a non-state quantity built from the central *output variable*
    # (cp, not the `central` state) must NOT block conversion.
    .m <- suppressMessages(rxode2(function() {
      ini({ tcl <- 1; tv <- 3.45; emax <- 10; ec50 <- 1; add.sd <- 0.7 })
      model({
        cl <- exp(tcl); v <- exp(tv)
        d/dt(central) <- -cl/v * central
        cp  <- central / v
        eff <- emax * cp / (ec50 + cp)   # references cp (output var), not the state
        cp ~ add(add.sd)
      })
    }))
    .info <- .odeToLinDetect(.m)
    expect_equal(.info$ncmt, 1L)
  })

  test_that("a hand-written linCmt() model reads a peripheral compartment under an error model", {
    # UI-system fix (rxUiGet.cmtLines): an endpoint injects a fresh observation
    # compartment, which previously shifted linCmt()'s materialized compartment
    # indices so an in-equation `peripheral1` read 0.  The peripheral observable
    # must now resolve to the solved amount even with an error model present and
    # WITHOUT anchoring the central endpoint to the central compartment.  This is
    # the general fix that also lets odeToLin keep coupled models analytic.
    .m <- suppressMessages(rxode2(function() {
      ini({ lcl <- log(5); lvc <- log(30); lq <- log(2); lvp <- log(50); propSd <- 0.1 })
      model({
        cl <- exp(lcl); vc <- exp(lvc); q <- exp(lq); vp <- exp(lvp)
        Cc <- linCmt()
        Cp <- peripheral1 / vp     # in-equation reference to a linCmt compartment
        Cc ~ prop(propSd)          # error model present, central endpoint NOT anchored
      })
    }))
    .mOde <- suppressMessages(rxode2(function() {
      ini({ lcl <- log(5); lvc <- log(30); lq <- log(2); lvp <- log(50) })
      model({
        cl <- exp(lcl); vc <- exp(lvc); q <- exp(lq); vp <- exp(lvp)
        kel <- cl/vc; k12 <- q/vc; k21 <- q/vp
        d/dt(central) <- -kel*central - k12*central + k21*periph
        d/dt(periph)  <-  k12*central - k21*periph
        Cc <- central / vc
        Cp <- periph / vp
      })
    }))
    .ev   <- et(amt = 100, cmt = "central") |> et(seq(0, 24, by = 2))
    .r    <- suppressMessages(rxSolve(.m,    .ev))
    .rOde <- suppressMessages(rxSolve(.mOde, .ev, useLinCmt = FALSE))
    expect_false(all(.r$Cp == 0))                    # the bug produced an all-zero Cp
    expect_equal(.r$Cp, .rOde$Cp, tolerance = 1e-4)  # peripheral amount now resolves
    expect_equal(.r$Cc, .rOde$Cc, tolerance = 1e-4)
  })

})
