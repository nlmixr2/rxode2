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

  rxode2parse("b <- udf(x, y)", code="udf.c")

  expect_true(file.exists("udf.c"))

  if (file.exists("udf.c")) {
    lines <- readLines("udf.c")
    unlink("udf.c")
    expect_false(file.exists("udf.c"))
  }

  .w <- which(grepl("b =_udf(\"udf\",", lines, fixed=TRUE))
  expect_true(length(.w) > 0)

  .w <- which(grepl("double __udf[2]", lines, fixed=TRUE))
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

  test_that("functions that error will error the solve",{
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
  rxFun("gg", c("x", "y"),
        "double gg(double x, double y) { return x*y;}")


  test_that("C functions rule", {


    d <- suppressWarnings(rxSolve(f, e))

    expect_true(all(d$z == d$x * d$y))

  })

  rxRmFun("gg")

  test_that("c conversion", {

    udf <- function(x, y) {
      a <- x + y
      b <- a ^ 2
      a + b
    }

    expect_true(grepl("R_pow_di[(]", rxFun2c(udf)[[1]]$cCode))

    udf <- function(x, y) {
      a <- x + y
      b <- a ^ x
      a + b
    }

    expect_true(grepl("R_pow[(]", rxFun2c(udf)[[1]]$cCode))

    udf <- function(x, y) {
      a <- x + y
      b <- cos(a) + x
      a + b
    }

    expect_true(grepl("cos[(]", rxFun2c(udf)[[1]]$cCode))

    udf <- function(x, y) {
      if (a < b) {
        return(b ^ 2)
      }
      a + b
    }

    expect_true(grepl("if [(]", rxFun2c(udf)[[1]]$cCode))


    udf <- function(x, y) {
      a <- x
      b <- x ^ 2 + a
      if (a < b) {
        return(b ^ 2)
      } else {
        a + b
      }
    }

    expect_true(grepl("else [{]", rxFun2c(udf)[[1]]$cCode))

    udf <- function(x, y) {
      a <- x
      b <- x ^ 2 + a
      if (a < b) {
        return(b ^ 2)
      } else if (a > b + 3) {
        return(a + b)
      }
      a ^ 2 + b ^ 2
    }

    expect_true(grepl("else if [(]", rxFun2c(udf)[[1]]$cCode))

    udf <- function(x, y) {
      a <- x
      b <- x ^ 2 + a
      if (a < b) {
        return(b ^ 2)
      } else if (a > b + 3) {
        b <- 3
        if (a > 2) {
          a <- 2
        }
        return(a + b)
      }
      a ^ 2 + b ^ 2
    }

    expect_true(grepl("else if [(]", rxFun2c(udf)[[1]]$cCode))

    udf <- function(x, y) {
      a <- x + y
      x <- a ^ 2
      x
    }

    expect_error(rxFun2c(udf)[[1]]$cCode)


    udf <- function(x, y) {
      a <- x
      b <- x ^ 2 + a
      if (a < b) {
        b ^ 2
      } else {
        a + b
      }
    }

    rxFun(udf)
    rxRmFun("udf")

  })

  test_that("udf with model functions", {

    gg <- function(x, y) {
      x/y
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


test_that("udf type 2 (that changes ui models upon parsing)", {

  expect_error(rxModelVars("a <- linMod(x, 3)"), NA)
  expect_error(rxModelVars("a <- linMod(x, 3, b)"))
  expect_error(rxModelVars("a <- linMod(x)"))
  expect_error(rxModelVars("a <- linMod()"))

  f <- rxode2({
    a <- linMod(x, 3)
  })

  e <- et(1:10)

  expect_error(rxSolve(f, e, c(x=2)), "ui user function")

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

  expect_equal(tmp$iniDf$name,
              c("d", "rx.linMod.time1a", "rx.linMod.time1b", "rx.linMod.time1c",
                "rx.linMod.time1d"))

  expect_equal(modelExtract(tmp, a),
               "a <- (rx.linMod.time1a + rx.linMod.time1b * time + rx.linMod.time1c * time^2 + rx.linMod.time1d * time^3)")

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

  expect_equal(tmp$iniDf$name,
              c("d", "rx.linMod.time1a", "rx.linMod.time1b", "rx.linMod.time1c"))


  expect_equal(modelExtract(tmp, a),
               "a <- (rx.linMod.time1a * time + rx.linMod.time1b * time^2 + rx.linMod.time1c * time^3) + d")

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

  expect_equal(tmp$iniDf$name,
               c("d", "rx.linMod.time1a", "rx.linMod.time1b", "rx.linMod.time1c", "rx.linMod.time1d",
                 "rx.linMod.time2a", "rx.linMod.time2b", "rx.linMod.time2c", "rx.linMod.time2d"))

    expect_equal(modelExtract(tmp, a),
                 "a <- (rx.linMod.time1a + rx.linMod.time1b * time + rx.linMod.time1c * time^2 + rx.linMod.time1d * time^3)")

    expect_equal(modelExtract(tmp, b),
                 "b <- (rx.linMod.time2a + rx.linMod.time2b * time + rx.linMod.time2c * time^2 + rx.linMod.time2d * time^3)")

})
