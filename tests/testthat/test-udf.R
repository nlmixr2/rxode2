rxTest({
  udf <- function(x, y, ...) {
    x + y
  }

  expect_error(rxode2parse("b <- udf(x, y)"))

  udf <- function(x, y) {
    x + y
  }

  expect_error(rxode2parse("b <- udf(x, y)"), NA)

  expect_error(rxode2parse("b <- udf(x, y, z)"))

  rxode2parse("b <- udf(x, y)", code = "udf.c")

  expect_true(file.exists("udf.c"))

  if (file.exists("udf.c")) {
    lines <- readLines("udf.c")
    unlink("udf.c")
    expect_false(file.exists("udf.c"))
  }

  .w <- which(grepl("b =_udf(\"udf\",", lines, fixed = TRUE))
  expect_true(length(.w) > 0)

  .w <- which(grepl("double __udf[2]", lines, fixed = TRUE))
  expect_true(length(.w) > 0)

  e <- et(1:10) |> as.data.frame()

  e$x <- 1:10
  e$y <- 21:30

  gg <- function(x, y) {
    x + y
  }

  f <- rxode2({
    z = gg(x, y)
  })

  test_that("udf1 works well", {
    expect_warning(rxSolve(f, e))

    d <- suppressWarnings(rxSolve(f, e))

    expect_true(all(d$z == d$x + d$y))
  })

  # parse a model whose user function is in its own frame, so the next
  # model's user function is in a different environment
  .udfOtherEnv <- function() {
    udfOther <- function(x, y) x - y
    invisible(rxode2({
      z <- udfOther(x, y)
    }))
  }

  test_that("user functions from different environments resolve in consecutive models", {
    .plainA <- function(data) {
      udfConsA <- function(x, y) x + 2 * y
      .m <- rxode2({
        z <- udfConsA(x, y)
      })
      suppressWarnings(rxSolve(.m, data))
    }
    .plainA2 <- function(data) {
      udfConsA2 <- function(x, y) x + 3 * y
      .m <- rxode2({
        z <- udfConsA2(x, y)
      })
      suppressWarnings(rxSolve(.m, data))
    }
    .uiFunB <- function() {
      ini({
        t1 <- 1
      })
      model({
        z <- udfConsB(x, y) * t1
      })
    }
    .uiB <- function(data) {
      udfConsB <- function(x, y) x + 4 * y
      suppressWarnings(rxSolve(rxode2(.uiFunB), data))
    }
    .uiD <- function(data) {
      udfConsD <- function(x, y) x + 5 * y
      .ui <- rxode2(function() {
        ini({
          t1 <- 1
        })
        model({
          z <- udfConsD(x, y) * t1
        })
      })
      suppressWarnings(rxSolve(.ui, data))
    }
    # each model's user function is in a different environment from the last
    .steps <- list(
      list(.plainA, 2),
      list(.plainA2, 3),
      list(.uiB, 4),
      list(.plainA, 2),
      list(.uiD, 5),
      list(.uiB, 4),
      list(.uiD, 5)
    )
    for (.s in .steps) {
      .d <- .s[[1]](e)
      expect_equal(.d$z, .d$x + .s[[2]] * .d$y)
    }
  })

  test_that("a model built before another one still solves", {
    .buildM1 <- function() {
      udfNestM1 <- function(x, y) x + 6 * y
      rxode2({
        z <- udfNestM1(x, y)
      })
    }
    .buildM2 <- function() {
      udfNestM2 <- function(x, y) x + 7 * y
      rxode2({
        z <- udfNestM2(x, y)
      })
    }
    .m1 <- .buildM1()
    .m2 <- .buildM2()
    .d <- suppressWarnings(rxSolve(.m1, e))
    expect_equal(.d$z, .d$x + 6 * .d$y)
    .d <- suppressWarnings(rxSolve(.m2, e))
    expect_equal(.d$z, .d$x + 7 * .d$y)
  })

  test_that("a model can be built inside a user function during a solve", {
    udfBuilds <- function(x, y) {
      udfBuildsInner <- function(a, b) a + b
      invisible(rxode2({
        w <- udfBuildsInner(p, q)
      }))
      x + 8 * y
    }
    .m <- rxode2({
      z <- udfBuilds(x, y)
    })
    .d <- suppressWarnings(rxSolve(.m, e))
    expect_equal(.d$z, .d$x + 8 * .d$y)
  })

  test_that("a ui model finds a user function in an enclosing scope", {
    udfScopeFun <- function(x, y) x + 9 * y
    .uiFun <- function() {
      ini({
        t1 <- 1
      })
      model({
        z <- udfScopeFun(x, y) * t1
      })
    }
    .solveUi <- function(data) suppressWarnings(rxSolve(rxode2(.uiFun), data))
    .udfOtherEnv()
    .d <- .solveUi(e)
    expect_equal(.d$z, .d$x + 9 * .d$y)
  })

  # now modify gg
  gg <- function(x, y, z) {
    x + y + z
  }

  test_that("udf with 3 arguments works", {
    expect_error(rxSolve(f, e))
  })

  # now modify gg back to 2 arguments
  gg <- function(x, y) {
    x * y
  }

  test_that("when changing gg the results will be different", {
    # different solve results but still runs

    d <- suppressWarnings(rxSolve(f, e))

    expect_true(all(d$z == d$x * d$y))
  })

  rm(gg)

  test_that("Without a udf, the solve errors", {
    expect_error(rxSolve(f, e))
  })

  gg <- function(x, ...) {
    x
  }

  test_that("cannot solve with udf functions that have ...", {
    expect_error(rxSolve(f, e))
  })

  gg <- function(x, y) {
    stop("running me")
  }

  test_that("functions that error will error the solve", {
    expect_error(rxSolve(f, e))
  })

  gg <- function(x, y) {
    "running "
  }

  test_that("runs with improper output will error", {
    expect_error(rxSolve(f, e))
  })

  gg <- function(x, y) {
    "3"
  }

  test_that("error for invalid input", {
    expect_error(rxSolve(f, e))
  })

  gg <- function(x, y) {
    3L
  }

  test_that("test symengine functions work with udf funs", {
    expect_equal(rxToSE("gg(x,y)"), "gg(x, y)")

    expect_error(rxToSE("gg()"), "user function")

    expect_error(rxFromSE("Derivative(gg(a,b),a)"), NA)

    expect_error(rxFromSE("Derivative(gg(a),a)"))

    expect_error(rxFromSE("Derivative(gg(),a)"))
  })

  gg <- function(x, ...) {
    x
  }

  test_that("test that functions with ... will error symengine translation", {
    expect_error(rxToSE("gg(x,y)"))

    expect_error(rxFromSE("Derivative(gg(a,b),a)"))
  })

  ## manual functions in C vs R functions

  gg <- function(x, y) {
    x + y
  }

  test_that("R vs C functions", {
    d <- suppressWarnings(rxSolve(f, e))
    expect_true(all(d$z == d$x + d$y))
  })

  # now add a C function with different values
  rxFun("gg", c("x", "y"), "double gg(double x, double y) { return x*y;}")

  test_that("C functions rule", {
    d <- suppressWarnings(rxSolve(f, e))

    expect_true(all(d$z == d$x * d$y))
  })

  rxRmFun("gg")

  test_that("c conversion", {
    udf <- function(x, y) {
      a <- x + y
      b <- a^2
      a + b
    }

    expect_true(grepl("R_pow_di[(]", rxode2:::rxFun2c(udf)[[1]]$cCode))

    udf <- function(x, y) {
      a <- x + y
      b <- a^x
      a + b
    }

    expect_true(grepl("R_pow[(]", rxode2:::rxFun2c(udf)[[1]]$cCode))

    udf <- function(x, y) {
      a <- x + y
      b <- cos(a) + x
      a + b
    }

    expect_true(grepl("cos[(]", rxode2:::rxFun2c(udf)[[1]]$cCode))

    udf <- function(x, y) {
      if (a < b) {
        return(b^2)
      }
      a + b
    }

    expect_true(grepl("if [(]", rxode2:::rxFun2c(udf)[[1]]$cCode))

    udf <- function(x, y) {
      a <- x
      b <- x^2 + a
      if (a < b) {
        return(b^2)
      } else {
        a + b
      }
    }

    expect_true(grepl("else [{]", rxode2:::rxFun2c(udf)[[1]]$cCode))

    udf <- function(x, y) {
      a <- x
      b <- x^2 + a
      if (a < b) {
        return(b^2)
      } else if (a > b + 3) {
        return(a + b)
      }
      a^2 + b^2
    }

    expect_true(grepl("else if [(]", rxode2:::rxFun2c(udf)[[1]]$cCode))

    udf <- function(x, y) {
      a <- x
      b <- x^2 + a
      if (a < b) {
        return(b^2)
      } else if (a > b + 3) {
        b <- 3
        if (a > 2) {
          a <- 2
        }
        return(a + b)
      }
      a^2 + b^2
    }

    expect_true(grepl("else if [(]", rxode2:::rxFun2c(udf)[[1]]$cCode))

    udf <- function(x, y) {
      a <- x + y
      x <- a^2
      x
    }

    expect_error(rxode2:::rxFun2c(udf)[[1]]$cCode)

    udf <- function(x, y) {
      a <- x
      b <- x^2 + a
      if (a < b) {
        b^2
      } else {
        a + b
      }
    }

    rxFun(udf)
    rxRmFun("udf")
  })

  test_that("udf with model functions", {
    gg <- function(x, y) {
      x / y
    }

    # Step 1 - Create a model specification
    f <- function() {
      ini({
        KA <- .291
        CL <- 18.6
        V2 <- 40.2
        Q <- 10.5
        V3 <- 297.0
        Kin <- 1.0
        Kout <- 1.0
        EC50 <- 200.0
      })
      model({
        # A 4-compartment model, 3 PK and a PD (effect) compartment
        # (notice state variable names 'depot', 'centr', 'peri', 'eff')
        C2 <- gg(centr, V2)
        C3 <- peri/V3
        d/dt(depot) <- -KA*depot
        d/dt(centr) <- KA*depot - CL*C2 - Q*C2 + Q*C3
        d/dt(peri)  <-                    Q*C2 - Q*C3
        d/dt(eff)   <- Kin - Kout*(1-C2/(EC50+C2))*eff
        eff(0) <- 1
      })
    }

    u <- f()

    # this pre-compiles and displays the simulation model
    u$simulationModel

    # Step 2 - Create the model input as an EventTable,
    # including dosing and observation (sampling) events

    # QD (once daily) dosing for 5 days.

    qd <- eventTable(amount.units = "ug", time.units = "hours")
    qd$add.dosing(dose = 10000, nbr.doses = 5, dosing.interval = 24)

    # Sample the system hourly during the first day, every 8 hours
    # then after

    qd$add.sampling(0:24)
    qd$add.sampling(seq(from = 24 + 8, to = 5 * 24, by = 8))

    # Step 3 - set starting parameter estimates and initial
    # values of the state

    # Step 4 - Fit the model to the data
    expect_error(suppressWarnings(solve(u, qd)), NA)

    u1 <- u$simulationModel

    expect_error(suppressWarnings(solve(u1, qd)), NA)

    u2 <- u$simulationIniModel
    expect_error(suppressWarnings(solve(u2, qd)), NA)

    expect_error(suppressWarnings(rxSolve(f, qd)), NA)
  })

  test_that("symengine load", {
    mod <- "tke=THETA[1];\nprop.sd=THETA[2];\neta.ke=ETA[1];\nke=gg(tke,exp(eta.ke));\nipre=gg(10,exp(-ke*t));\nlipre=log(ipre);\nrx_yj_~2;\nrx_lambda_~1;\nrx_low_~0;\nrx_hi_~1;\nrx_pred_f_~ipre;\nrx_pred_~rx_pred_f_;\nrx_r_~(rx_pred_f_*prop.sd)^2;\n"

    gg <- function(x, y) {
      x * y
    }

    expect_error(rxS(mod, TRUE, TRUE), NA)

    rxFun(gg)

    rm(gg)

    expect_error(rxS(mod, TRUE, TRUE), NA)

    rxRmFun("gg")
  })
})


rxTest({
  test_that("udf type 2 (that changes ui models upon parsing)", {
    expect_error(rxModelVars("a <- linMod(x, 3)"), NA)
    expect_error(rxModelVars("a <- linMod(x, 3, b)"))
    expect_error(rxModelVars("a <- linMod(x)"))
    expect_error(rxModelVars("a <- linMod()"))

    f <- rxode2({
      a <- linMod(x, 3)
    })

    e <- et(1:10)

    expect_error(rxSolve(f, e, c(x = 2)), "ui user function")

    # Test a linear model construction

    f <- function() {
      ini({
        d <- 4
      })
      model({
        a <- linMod(time, 3)
        b <-  d
      })
    }

    tmp <- f()

    expect_equal(tmp$iniDf$name, c("d", "rx.linMod.time1a", "rx.linMod.time1b", "rx.linMod.time1c", "rx.linMod.time1d"))

    expect_equal(
      modelExtract(tmp, a),
      "a <- (rx.linMod.time1a + rx.linMod.time1b * time + rx.linMod.time1c * time^2 + rx.linMod.time1d * time^3)"
    )

    # Test a linear model construction without an intercept
    f <- function() {
      ini({
        d <- 4
      })
      model({
        a <- linMod0(time, 3) + d
      })
    }

    tmp <- f()

    expect_equal(tmp$iniDf$name, c("d", "rx.linMod.time1a", "rx.linMod.time1b", "rx.linMod.time1c"))

    expect_equal(
      modelExtract(tmp, a),
      "a <- (rx.linMod.time1a * time + rx.linMod.time1b * time^2 + rx.linMod.time1c * time^3) + d"
    )

    # Now test the use of 2 linear models in the UI
    f <- function() {
      ini({
        d <- 4
      })
      model({
        a <- linMod(time, 3)
        b <- linMod(time, 3)
        c <- d
      })
    }

    tmp <- f()

    expect_equal(
      tmp$iniDf$name,
      c(
        "d",
        "rx.linMod.time1a",
        "rx.linMod.time1b",
        "rx.linMod.time1c",
        "rx.linMod.time1d",
        "rx.linMod.time2a",
        "rx.linMod.time2b",
        "rx.linMod.time2c",
        "rx.linMod.time2d"
      )
    )

    expect_equal(
      modelExtract(tmp, a),
      "a <- (rx.linMod.time1a + rx.linMod.time1b * time + rx.linMod.time1c * time^2 + rx.linMod.time1d * time^3)"
    )

    expect_equal(
      modelExtract(tmp, b),
      "b <- (rx.linMod.time2a + rx.linMod.time2b * time + rx.linMod.time2c * time^2 + rx.linMod.time2d * time^3)"
    )

    f <- function() {
      ini({
        d <- 4
      })
      model({
        a <- linModB(time, 3)
        b <-  d
      })
    }

    tmp <- f()

    expect_equal(
      modelExtract(tmp, rx.linMod.time.f1),
      "rx.linMod.time.f1 <- rx.linMod.time1a + rx.linMod.time1b * time + rx.linMod.time1c * time^2 + rx.linMod.time1d * time^3"
    )

    expect_equal(modelExtract(tmp, a), "a <- rx.linMod.time.f1")

    f <- function() {
      ini({
        d <- 4
      })
      model({
        a <- linModB0(time, 3) + d
      })
    }

    tmp <- f()

    expect_equal(
      modelExtract(tmp, rx.linMod.time.f1),
      "rx.linMod.time.f1 <- rx.linMod.time1a * time + rx.linMod.time1b * time^2 + rx.linMod.time1c * time^3"
    )

    expect_equal(modelExtract(tmp, a), "a <- rx.linMod.time.f1 + d")

    f <- function() {
      ini({
        d <- 4
      })
      model({
        a <- linModA(time, 1) + d
      })
    }

    tmp <- f()

    expect_equal(
      modelExtract(tmp, rx.linMod.time.f1),
      "rx.linMod.time.f1 <- rx.linMod.time1a + rx.linMod.time1b * time"
    )

    expect_equal(modelExtract(tmp, a), "a <- 0 + d")

    f <- function() {
      ini({
        d <- 4
      })
      model({
        a <- linModA0(time, 1) + d
      })
    }

    tmp <- f()

    expect_equal(modelExtract(tmp, rx.linMod.time.f1), "rx.linMod.time.f1 <- rx.linMod.time1a * time")

    expect_equal(modelExtract(tmp, a), "a <- 0 + d")

    f <- function() {
      ini({
        d <- 4
      })
      model({
        a <- linMod(power=3, variable="x") + d
      })
    }

    tmp <- f()

    expect_equal(
      modelExtract(tmp, a),
      "a <- (rx.linMod.x1a + rx.linMod.x1b * x + rx.linMod.x1c * x^2 + rx.linMod.x1d * x^3) + d"
    )

    expect_false(tmp$uiUseData)

    ## Formula interface
    f <- function() {
      ini({
        d <- 4
      })
      model({
        a <- linMod0(dv~x^3) + d
      })
    }

    tmp <- f()

    expect_equal(modelExtract(tmp, a), "a <- linModD0(x, 3, dv) + d")
    expect_true(tmp$uiUseData)

    ## Formula interface
    f <- function() {
      ini({
        d <- 4
      })
      model({
        a <- linMod0(~x^3) + d
      })
    }

    tmp <- f()

    expect_equal(modelExtract(tmp, a), "a <- (rx.linMod.x1a * x + rx.linMod.x1b * x^2 + rx.linMod.x1c * x^3) + d")

    ## Formula interface
    f <- function() {
      ini({
        d <- 4
      })
      model({
        a <- linMod0(~x^6) + d
      })
    }

    tmp <- f()

    expect_equal(
      modelExtract(tmp, a),
      "a <- (rx.linMod.x1a * x + rx.linMod.x1b * x^2 + rx.linMod.x1c * x^3 + rx.linMod.x1d * x^4 + rx.linMod.x1e * x^5 + rx.linMod.x1f * x^6) + d"
    )

    # This checks to make sure that the variables are not in the model
    # before adding them
    f <- function() {
      ini({
        d <- 4
      })
      model({
        a <- linModM0(~x^6) + d
      })
    }

    tmp <- f()

    expect_equal(modelExtract(tmp, a), "a <- (x1a * x + x1b * x^2 + x1c * x^3 + x1d * x^4 + x1e * x^5 + x1f * x^6) + d")

    f <- function() {
      ini({
        d <- 4
      })
      model({
        a <- linModM(~x^6) + d
      })
    }

    tmp <- f()

    expect_equal(
      modelExtract(tmp, a),
      "a <- (x1a + x1b * x + x1c * x^2 + x1d * x^3 + x1e * x^4 + x1f * x^5 + x1g * x^6) + d"
    )

    rxWithSeed(42, {
      q <- seq(from = 0, to = 20, by = 0.1)

      y <- 500 + 42 * q^2 + 0.4 * (q - 10)^3

      df <- data.frame(q = q, y = y)

      f <- function() {
        model({
          a <- linMod(y~q^3)
        })
      }

      f <- f()

      expect_equal(modelExtract(f, a), "a <- linModD(q, 3, y)")

      rxUdfUiData(df)

      try({
        if (f$uiUseData) {
          f <- rxode2(as.function(f))
          expect_false(any(f$theta == 0))
        }
      })
      rxUdfUiData(NULL)
    })
  })

  test_that("linMod() keeps the between subject variability in iniDf", {
    f <- function() {
      ini({
        tcl <- log(2.7)
        tv <- 3.45
        tp <- 0.1
        eta.cl ~ 0.3
        add.sd <- 0.7
      })
      model({
        cl <- exp(tcl + eta.cl)
        v <- exp(tv)
        p <- tp * linMod(time, 1)
        d / dt(center) <- -cl / v * center
        cp <- center / v + p
        cp ~ add(add.sd)
      })
    }
    .u <- f()
    expect_equal(
      .u$iniDf$name,
      c("tcl", "tv", "tp", "add.sd", "rx.linMod.time1a", "rx.linMod.time1b", "eta.cl")
    )
    expect_equal(.u$iniDf$neta1[.u$iniDf$name == "eta.cl"], 1)
    expect_false("eta.cl" %in% .u$covariates)
    expect_equal(.u$muRefDataFrame$eta, "eta.cl")
    expect_identical(rownames(.u$iniDf), as.character(seq_len(nrow(.u$iniDf))))
  })
})

rxTest({
  # issue #1409; each test starts as a fresh session does, with no udf
  # environment set
  .freshUdfEnv <- function() {
    .old <- .udfEnv$envir
    .udfEnv$envir <- NULL
    withr::defer(.udfEnv$envir <- .old, envir = parent.frame())
  }

  test_that("a later model uses its own user function, not an earlier caller's", {
    .freshUdfEnv()
    .f1 <- function() {
      myfun1409 <- function(x) x + 1
      rxToSE("a + b")
      NULL
    }
    invisible(.f1())
    .f2 <- function() {
      myfun1409 <- function(x) x + 2
      .m <- rxode2({
        y <- myfun1409(t)
      })
      suppressWarnings(rxSolve(.m, et(0:1)))$y
    }
    expect_equal(.f2(), c(2, 3))
  })

  test_that("the caller's frame is not kept as the primary udf environment", {
    .freshUdfEnv()
    .frame <- NULL
    .f <- function() {
      .frame <<- environment()
      rxToSE("a + b")
      NULL
    }
    invisible(.f())
    expect_false(identical(.udfEnv$envir, .frame))
    expect_equal(.udfEnv$depth, 0L)
  })

  test_that("a model built inside a function solves after the function returns", {
    .g <- function() {
      h1409 <- function(x) x * 10
      rxode2({
        y <- h1409(t)
      })
    }
    .m <- .g()
    expect_equal(suppressWarnings(rxSolve(.m, et(0:2)))$y, c(0, 10, 20))
    expect_equal(.udfEnv$depth, 0L)
  })

  test_that("parsing another model does not change the function a solve calls", {
    .a <- function() {
      same1409 <- function(x) x + 1
      .m <- rxode2({
        y <- same1409(t)
      })
      suppressWarnings(rxSolve(.m, et(0:1)))
      NULL
    }
    invisible(.a())
    .b <- function() {
      same1409 <- function(x) x + 100
      rxode2({
        y <- same1409(t)
      })
    }
    invisible(.b())
    # what a compiled solve calls back into
    expect_equal(.udfCall("same1409", list(1)), 2)
  })

  test_that("rxSolve() keeps an environment set with .udfEnvSet() outside of it", {
    .freshUdfEnv()
    .m <- rxode2({
      y <- t
    })
    .e <- new.env()
    .udfEnvSet(.e)
    suppressWarnings(rxSolve(.m, et(0:1)))
    expect_identical(.udfEnv$envir, .e)
    expect_equal(.udfEnv$depth, 0L)
  })

  test_that("the udf scope ends on error and when its restore is dropped", {
    .freshUdfEnv()
    expect_error(rxode2({
      y <- notAFunction1409(t)
    }))
    expect_equal(.udfEnv$depth, 0L)
    .dropped <- function() {
      .udfEnvLocal(environment())
      on.exit(NULL)
      NULL
    }
    .dropped()
    expect_equal(.udfEnv$depth, 1L)
    invisible(rxToSE("a + b"))
    expect_equal(.udfEnv$depth, 0L)
    # an unscoped .udfEnvSet() (as nlmixr2est calls it) is not ignored either
    .dropped()
    .e <- new.env()
    .udfEnvSet(.e)
    expect_equal(.udfEnv$depth, 0L)
    expect_identical(.udfEnv$envir, .e)
  })
})
