rxTest({

  test_that("sync covariates", {

    dat <- structure(list(ID = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
                                 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L,
                                 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                 3L, 3L, 3L, 3L, 3L, 3L, 3L),
                          TIME = c(0, 0, 0, 0.5, 0.5, 1, 1,
                                   1.5, 1.5, 2, 2, 2.5, 2.5, 3, 3, 3.5, 3.5, 4, 4, 4.5, 4.5, 5,
                                   5, 5.5, 5.5, 6, 6, 0, 0, 0, 0.5, 0.5, 1, 1, 1.5, 1.5, 2, 2, 2.5,
                                   2.5, 3, 3, 3.5, 3.5, 4, 4, 4.5, 4.5, 5, 5, 5.5, 5.5, 6, 6, 0,
                                   0, 0, 0.5, 0.5, 1, 1, 1.5, 1.5, 2, 2, 2.5, 2.5, 3, 3, 3.5, 3.5,
                                   4, 4, 4.5, 4.5, 5, 5, 5.5, 5.5, 6, 6),
                          EVID = c(80101L, 60101L,
                                   0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
                                   0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 80101L, 60101L, 0L, 0L, 0L,
                                   0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
                                   0L, 0L, 0L, 0L, 0L, 0L, 80101L, 60101L, 0L, 0L, 0L, 0L, 0L, 0L,
                                   0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
                                   0L, 0L, 0L),
                          AMT = c(10, 10, NA, NA, NA, NA, NA, NA, NA, NA,
                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                  NA, 10, 10, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 10, 10, NA, NA,
                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                  NA, NA, NA, NA, NA, NA, NA),
                          II = c(0, 0, 0, 0, 0, 0, 0, 0, 0,
                                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                 0, 0, 0, 0, 0, 0, 0, 0, 0),
                          DV = c(NA, NA, 0, 3.8, 0.9, 5.8,
                                 2.5, 3.2, 3.2, 1.8, 2.9, 1, 2.2, 0.5, 1.6, 0.3, 1.1, 0.2, 0.8,
                                 0.1, 0.5, 0, 0.3, 0, 0.2, 0, 0.1, NA, NA, 0, 3.4, 0.7, 7.3, 2.4,
                                 2.8, 2.5, 1.3, 2.7, 1, 1.6, 0.6, 1.1, 0.3, 0.8, 0.2, 0.8, 0.1,
                                 0.5, 0, 0.3, 0, 0.2, 0, 0.2, NA, NA, 0, 2.8, 1.1, 6.7, 2.7, 3.9,
                                 3.6, 1.5, 3.1, 0.7, 2.4, 0.5, 1.5, 0.3, 1.2, 0.1, 0.5, 0.1, 0.6,
                                 0.1, 0.4, 0, 0.2, 0, 0.2),
                          CMT = c(1L, 1L, 4L, 3L, 4L, 3L, 4L,
                                  3L, 4L, 3L, 4L, 3L, 4L, 3L, 4L, 3L, 4L, 3L, 4L, 3L, 4L, 3L, 4L,
                                  3L, 4L, 3L, 4L, 1L, 1L, 4L, 3L, 4L, 3L, 4L, 3L, 4L, 3L, 4L, 3L,
                                  4L, 3L, 4L, 3L, 4L, 3L, 4L, 3L, 4L, 3L, 4L, 3L, 4L, 3L, 4L, 1L,
                                  1L, 4L, 3L, 4L, 3L, 4L, 3L, 4L, 3L, 4L, 3L, 4L, 3L, 4L, 3L, 4L,
                                  3L, 4L, 3L, 4L, 3L, 4L, 3L, 4L, 3L, 4L),
                          nlmixrRowNums =
                            c(1,
                              NA, 14, 2, 15, 3, 16, 4, 17, 5, 18, 6, 19, 7, 20, 8, 21, 9, 22,
                              10, 23, 11, 24, 12, 25, 13, 26, 27, NA, 40, 28, 41, 29, 42, 30,
                              43, 31, 44, 32, 45, 33, 46, 34, 47, 35, 48, 36, 49, 37, 50, 38,
                              51, 39, 52, 53, NA, 66, 54, 67, 55, 68, 56, 69, 57, 70, 58, 71,
                              59, 72, 60, 73, 61, 74, 62, 75, 63, 76, 64, 77, 65, 78)),
                     class = "data.frame", row.names = c(NA, -81L))

    par <- structure(c(0, 0, 0, 0.693147180559945, 0.693147180559945, 0.693147180559945,
                       2.30258509299405, 2.30258509299405, 2.30258509299405, 2.30258509299405,
                       2.30258509299405, 2.30258509299405, 0, 0, 0, 1, 1, 1, 1, 1, 1,
                       -1.86782087447842, 0, 0, -2.44813462994314, 0, 0, 0.0831313226164289,
                       0, 0, -2.28843604434224, 0, 0, 1.97816823869187e-05, 0, 0),
                     dim = c(3L, 12L),
                     dimnames = list(NULL,
                                     c("THETA[1]", "THETA[2]", "THETA[3]",
                                       "THETA[4]", "THETA[5]", "THETA[6]", "THETA[7]", "ETA[1]", "ETA[2]",
                                       "ETA[3]", "ETA[4]", "ETA[5]")))


    rx <- rxode2({
      param(THETA[1], THETA[2], THETA[3], THETA[4], THETA[5], THETA[6],
            THETA[7], ETA[1], ETA[2], ETA[3], ETA[4], ETA[5])
      cmt(center)
      cmt(meta)
      rx_expr_5 ~ ETA[3] + THETA[1]
      rx_expr_6 ~ ETA[1] + THETA[2]
      rx_expr_7 ~ ETA[2] + THETA[4]
      rx_expr_10 ~ exp(rx_expr_5)
      d/dt(center) = -rx_expr_10 * center - exp(rx_expr_6 - (rx_expr_7)) *
        center
      rx_expr_8 ~ ETA[5] + THETA[5]
      rx_expr_11 ~ exp(rx_expr_8)
      dur(center) = rx_expr_11
      rx_expr_9 ~ ETA[4] + THETA[3]
      d/dt(meta) = rx_expr_10 * center - exp(rx_expr_9 - (rx_expr_7)) *
        meta
      center(0) = 0
      meta(0) = 0
      rx_expr_0 ~ CMT == 4
      rx_expr_1 ~ CMT == 3
      rx_expr_2 ~ 1 - (rx_expr_0)
      rx_yj_ ~ 2 * (rx_expr_2) * (rx_expr_1) + 2 * (rx_expr_0)
      rx_expr_3 ~ (rx_expr_0)
      rx_expr_4 ~ (rx_expr_2)
      rx_expr_12 ~ rx_expr_4 * (rx_expr_1)
      rx_lambda_ ~ rx_expr_12 + rx_expr_3
      rx_hi_ ~ rx_expr_12 + rx_expr_3
      rx_low_ ~ 0
      rx_expr_13 ~ exp(-(rx_expr_7))
      rx_expr_14 ~ rx_expr_13 * meta
      rx_expr_15 ~ rx_expr_13 * center
      rx_expr_17 ~ rx_expr_14 * (rx_expr_0)
      rx_expr_18 ~ rx_expr_15 * (rx_expr_2)
      rx_expr_19 ~ rx_expr_18 * (rx_expr_1)
      rx_pred_ = (rx_expr_0) * (rx_expr_17 + rx_expr_19) + rx_expr_18 *
        Rx_pow_di((rx_expr_1), 2)
      rx_r_ = (rx_expr_0) * Rx_pow_di(((rx_expr_17 + rx_expr_19) *
                                         THETA[7]), 2) + (rx_expr_2) * Rx_pow_di((rx_expr_15 *
                                                                                    THETA[6] * (rx_expr_1)), 2) * (rx_expr_1)
      tkm = THETA[1]
      tcl = THETA[2]
      tclm = THETA[3]
      tv = THETA[4]
      tdur0 = THETA[5]
      prop.err = THETA[6]
      prop.err2 = THETA[7]
      eta.cl = ETA[1]
      eta.v = ETA[2]
      eta.km = ETA[3]
      eta.clm = ETA[4]
      eta.dur0 = ETA[5]
      km = rx_expr_10
      cl = exp(rx_expr_6)
      clm = exp(rx_expr_9)
      v = exp(rx_expr_7)
      dur0 = rx_expr_11
      cp = rx_expr_15
      cm = rx_expr_14
      tad = tad()
      dosenum = dosenum()
      cmt(cp)
      cmt(cm)
      dvid(3, 4)
    })

    s <- rxSolve(rx, dat, par, returnType="data.frame",
                 keep=c("nlmixrRowNums", "DV"), subsetNonmem=TRUE, addCov=TRUE)

    expect_equal(sort(unique(s[s$time==0.5,"CMT"])), 3:4)

  })

  skip_if_not_installed("units")

  for (meth in c("liblsoda", "lsoda")) { ## Dop is very close but doesn't match precisely.

    # context(sprintf("Simple test for time-varying covariates (%s)", meth))

    ode <- rxode2({
      b <- -1
      d/dt(X) <- a * X + Y * Z
      d/dt(Y) <- b * (Y - Z)
      d/dt(Z) <- -X * Y + c * Y - Z
      printf("%.10f,%.10f\n", t, c)
    })

    odeLin <- rxode2({
      linear(c)
      b <- -1
      d/dt(X) <- a * X + Y * Z
      d/dt(Y) <- b * (Y - Z)
      d/dt(Z) <- -X * Y + c * Y - Z
      printf("%.10f,%.10f\n", t, c)
    })

    odeLin <- rxode2({
      linear(c)
      b <- -1
      d/dt(X) <- a * X + Y * Z
      d/dt(Y) <- b * (Y - Z)
      d/dt(Z) <- -X * Y + c * Y - Z
      printf("%.10f,%.10f\n", t, c)
    })

    odeLocf <- rxode2({
      locf(c)
      b <- -1
      d/dt(X) <- a * X + Y * Z
      d/dt(Y) <- b * (Y - Z)
      d/dt(Z) <- -X * Y + c * Y - Z
      printf("%.10f,%.10f\n", t, c)
    })

    odeNocb <- rxode2({
      nocb(c)
      b <- -1
      d/dt(X) <- a * X + Y * Z
      d/dt(Y) <- b * (Y - Z)
      d/dt(Z) <- -X * Y + c * Y - Z
      printf("%.10f,%.10f\n", t, c)
    })

    odeMidpoint <- rxode2({
      midpoint(c)
      b <- -1
      d/dt(X) <- a * X + Y * Z
      d/dt(Y) <- b * (Y - Z)
      d/dt(Z) <- -X * Y + c * Y - Z
      printf("%.10f,%.10f\n", t, c)
    })

    et <- eventTable(time.units = "hr") # default time units
    et$add.sampling(seq(from = 0, to = 10, by = 0.5))

    cov <- data.frame(c = et$get.EventTable()$time + units::set_units(1, h))

    et0 <- et

    et <- cbind(et, cov)

    cov.lin <- approxfun(et$time, et$c, yleft = et$c[1], yright = et$c[length(cov$c)])

    t <- tempfile("temp", fileext = ".csv")
    suppressWarnings(.rxWithSink(t, {
      cat("t,c\n")
      out <- rxSolve(ode,
                     params = c(a = -8 / 3, b = -10),
                     events = et,
                     inits = c(X = 1, Y = 1, Z = 1),
                     addCov = TRUE,
                     covsInterpolation = "linear",
                     method = meth
                     )
    }))

    lin.interp <- read.csv(t)
    unlink(t)

    lin.interp$c2 <- cov.lin(lin.interp$t)

    test_that("time varying covariates output covariate in data frame", {
      expect_equal(cov$c, out$c)
    })

    test_that("Linear Approximation matches approxfun.", {
      expect_equal(lin.interp$c, lin.interp$c2)
    })

    t <- tempfile("temp", fileext = ".csv")
    suppressWarnings(.rxWithSink(t, {
      cat("t,c\n")
      out <- rxSolve(odeLin,
                     params = c(a = -8 / 3, b = -10),
                     events = et,
                     inits = c(X = 1, Y = 1, Z = 1),
                     addCov = TRUE,
                     covsInterpolation = "nocb",
                     method = meth
                     )
    }))

    lin.interp <- read.csv(t)
    unlink(t)

    lin.interp$c2 <- cov.lin(lin.interp$t)

    test_that("Linear Approximation matches approxfun and overrides covsInterpolation", {
      expect_equal(lin.interp$c, lin.interp$c2)
    })

    ## NONMEM interpolation
    suppressWarnings(.rxWithSink(t, {
      cat("t,c\n")
      out <- rxSolve(ode,
                     params = c(a = -8 / 3, b = -10),
                     events = et,
                     inits = c(X = 1, Y = 1, Z = 1),
                     covsInterpolation = "nocb", addCov = TRUE,
                     method = meth
                     )
    }))

    lin.interp <- read.csv(t)
    unlink(t)

    cov.lin <- approxfun(out$time, out$c,
                         yleft = cov$c[1], yright = cov$c[length(cov$c)],
                         method = "constant", f = 1
                         )
    lin.interp$c2 <- cov.lin(lin.interp$t)

    test_that("NOCB Approximation similar to approxfun.", {
      expect_equal(lin.interp$c, lin.interp$c2)
    })

    suppressWarnings(.rxWithSink(t, {
      cat("t,c\n")
      out <- rxSolve(odeNocb,
                     params = c(a = -8 / 3, b = -10),
                     events = et,
                     inits = c(X = 1, Y = 1, Z = 1),
                     covsInterpolation = "locf", addCov = TRUE,
                     method = meth
                     )
    }))

    lin.interp <- read.csv(t)
    unlink(t)

    cov.lin <- approxfun(out$time, out$c,
                         yleft = cov$c[1], yright = cov$c[length(cov$c)],
                         method = "constant", f = 1
                         )
    lin.interp$c2 <- cov.lin(lin.interp$t)

    test_that("NOCB Approximation similar to approxfun and overrides locf when in ode", {
      expect_equal(lin.interp$c, lin.interp$c2)
    })

    ## midpoint interpolation
    suppressWarnings(.rxWithSink(t, {
      cat("t,c\n")
      out <- rxSolve(ode,
                     params = c(a = -8 / 3, b = -10),
                     events = et,
                     inits = c(X = 1, Y = 1, Z = 1),
                     covsInterpolation = "midpoint", addCov = TRUE,
                     method = meth
                     )
    }))
    lin.interp <- read.csv(t)
    unlink(t)

    cov.lin <- approxfun(out$time, out$c,
                         yleft = cov$c[1], yright = cov$c[length(cov$c)],
                         method = "constant", f = 0.5
                         )

    lin.interp$c2 <- cov.lin(lin.interp$t)

    test_that("midpoint Approximation similar to approxfun.", {
      expect_equal(lin.interp$c, lin.interp$c2)
    })


    ## midpoint interpolation
    suppressWarnings(.rxWithSink(t, {
      cat("t,c\n")
      out <- rxSolve(odeMidpoint,
                     params = c(a = -8 / 3, b = -10),
                     events = et,
                     inits = c(X = 1, Y = 1, Z = 1),
                     covsInterpolation = "locf", addCov = TRUE,
                     method = meth
                     )
    }))
    lin.interp <- read.csv(t)
    unlink(t)

    cov.lin <- approxfun(out$time, out$c,
                         yleft = cov$c[1], yright = cov$c[length(cov$c)],
                         method = "constant", f = 0.5
                         )

    lin.interp$c2 <- cov.lin(lin.interp$t)

    test_that("midpoint Approximation similar to approxfun and overrides covsInterpolation method", {
      expect_equal(lin.interp$c, lin.interp$c2)
    })

    ## covs_interpolation
    suppressWarnings(.rxWithSink(t, {
      cat("t,c\n")
      out <- rxSolve(ode,
                     params = c(a = -8 / 3, b = -10),
                     events = et,
                     inits = c(X = 1, Y = 1, Z = 1),
                     covsInterpolation = "locf", addCov = TRUE,
                     method = meth
                     )
    }))

    lin.interp <- read.csv(t)
    unlink(t)

    cov.lin <- approxfun(out$time, out$c,
                         yleft = cov$c[1], yright = cov$c[length(cov$c)],
                         method = "constant")

    lin.interp$c2 <- cov.lin(lin.interp$t)

    test_that("Constant Approximation similar to approxfun.", {
      expect_equal(lin.interp$c, lin.interp$c2)
    })

    ## covs_interpolation
    suppressWarnings(.rxWithSink(t, {
      cat("t,c\n")
      out <- rxSolve(odeLocf,
                     params = c(a = -8 / 3, b = -10),
                     events = et,
                     inits = c(X = 1, Y = 1, Z = 1),
                     covsInterpolation = "nocb", addCov = TRUE,
                     method = meth
                     )
    }))

    lin.interp <- read.csv(t)
    unlink(t)

    cov.lin <- approxfun(out$time, out$c,
                         yleft = cov$c[1], yright = cov$c[length(cov$c)],
                         method = "constant")

    lin.interp$c2 <- cov.lin(lin.interp$t)

    test_that("Constant Approximation similar to approxfun and overrides covsInterpolation", {
      expect_equal(lin.interp$c, lin.interp$c2)
    })

    out <- as.data.frame(out)
    out <- out[, names(out) != "c"]

    suppressWarnings(.rxWithSink(t, {
      out1 <-
        rxSolve(ode,
                params = c(a = -8 / 3, b = -10, c = 0),
                events = et,
                inits = c(X = 1, Y = 1, Z = 1), addCov = TRUE,
                method = meth
                )
    }))
    unlink(t)

    out1 <- as.data.frame(out1)

    test_that("time varying covariates produce different outputs", {
      expect_false(isTRUE(all.equal(out, out1)))
    })

    cov <- data.frame(
      c = et0$get.EventTable()$time + units::set_units(1, hr),
      a = -et0$get.EventTable()$time / units::set_units(100, hr)
    )

    et <- cbind(et0, cov)

    suppressWarnings(.rxWithSink(t, {
      out <- rxSolve(ode,
                     params = c(a = -8 / 3, b = -10),
                     events = et,
                     inits = c(X = 1, Y = 1, Z = 1),
                     addCov = TRUE,
                     method = meth
                     )

      out3 <- rxSolve(ode,
                      params = c(a = -8 / 3, b = -10),
                      events = et,
                      inits = c(X = 1, Y = 1, Z = 1),
                      addCov = TRUE,
                      method = meth
                      )
    }))
    unlink(t)

    test_that("time varying covariates output covariate in data frame", {
      expect_equal(cov$c[et0$get.obs.rec()], out$c)
      expect_equal(cov$a[et0$get.obs.rec()], out$a)
    })

    cov <- data.frame(c = et0$get.EventTable()$time + units::set_units(1, hr))
    et <- cbind(et0, cov)

    suppressWarnings(.rxWithSink(t, {
      out2 <- rxSolve(ode,
                      params = c(a = -8 / 3, b = -10),
                      events = et,
                      inits = c(X = 1, Y = 1, Z = 1),
                      addCov = TRUE,
                      method = meth
                      )
    }))
    unlink(t)

    test_that("Before assinging the time varying to -8/3, out and out2 should be different", {
      expect_false(isTRUE(all.equal(out, out2)))
    })

    # context(sprintf("Test First Assignment (%s)", meth))

    ## Assign a time-varying to a simple parameter
    suppressWarnings(.rxWithSink(t, {
      out$a <- -8 / 3
    }))
    unlink(t)

    test_that("The out$a=-8/3 works.", {
      expect_equal(as.data.frame(out), as.data.frame(out2))
    })

    # context(sprintf("Test Second Assignment (%s)", meth))

    suppressWarnings(.rxWithSink(t, {
      out$a <- out3$a
    }))
    unlink(t)

    test_that("the out$a = time varying covariate works.", {
      expect_equal(as.data.frame(out), as.data.frame(out3))
    })

    # context(sprintf("Covariate solve with data frame event table (%s)", meth))

    ## Covariate solve for data frame
    d3 <- data.frame(
      TIME = c(0, 0, 2.99270072992701, 192, 336, 456),
      AMT = c(137L, 0L, -137L, 0L, 0L, 0L),
      V2I = c(909L, 909L, 909L, 909L, 909L, 909L),
      V1I = c(545L, 545L, 545L, 545L, 545L, 545L),
      CLI = c(471L, 471L, 471L, 471L, 471L, 471L),
      EVID = c(10101L, 0L, 10101L, 0L, 0L, 0L)
    )

    mod1 <- rxode2({
      d/dt(A_centr) <- -A_centr * (CLI / V1I + 204 / V1I) + 204 * A_periph / V2I
      d/dt(A_periph) <- 204 * A_centr / V1I - 204 * A_periph / V2I
      d/dt(A_circ) <- -4 * A_circ * exp(-ETA[2] - THETA[2]) + 4 * A_tr3 * exp(-ETA[2] - THETA[2])
      A_circ(0) <- exp(ETA[1] + THETA[1])
      d/dt(A_prol) <- 4 * A_prol * Rx_pow(exp(ETA[1] + THETA[1]) / A_circ, exp(THETA[4])) * (-A_centr * exp(ETA[3] + THETA[3]) / V1I + 1) * exp(-ETA[2] - THETA[2]) - 4 * A_prol * exp(-ETA[2] - THETA[2])
      A_prol(0) <- exp(ETA[1] + THETA[1])
      d/dt(A_tr1) <- 4 * A_prol * exp(-ETA[2] - THETA[2]) - 4 * A_tr1 * exp(-ETA[2] - THETA[2])
      A_tr1(0) <- exp(ETA[1] + THETA[1])
      d/dt(A_tr2) <- 4 * A_tr1 * exp(-ETA[2] - THETA[2]) - 4 * A_tr2 * exp(-ETA[2] - THETA[2])
      A_tr2(0) <- exp(ETA[1] + THETA[1])
      d/dt(A_tr3) <- 4 * A_tr2 * exp(-ETA[2] - THETA[2]) - 4 * A_tr3 * exp(-ETA[2] - THETA[2])
      A_tr3(0) <- exp(ETA[1] + THETA[1])
    })

    tmp <-
      rxSolve(
        mod1, d3,
        setNames(
          c(2.02103, 4.839305, 3.518676, -1.391113, 0.108127023, -0.064170725, 0.087765769),
          c(sprintf("THETA[%d]", 1:4), sprintf("ETA[%d]", 1:3))
        ),
        addCov = TRUE,
        method = meth
      )

    test_that("Data Frame single subject solve", {
      expect_equal(
        tmp %>%
          dplyr::select(CLI, V1I, V2I) %>% as.data.frame(),
        d3 %>% dplyr::filter(EVID == 0) %>%
          dplyr::select(CLI, V1I, V2I) %>% as.data.frame()
      )
      expect_equal(names(tmp$params), mod1$params[-(1:3)])
    })

    d3 <- data.frame(
      ID = c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
      TIME = c(0, 0, 2.99270072992701, 192, 336, 456, 0, 0, 3.07272727272727, 432),
      AMT = c(137L, 0L, -137L, 0L, 0L, 0L, 110L, 0L, -110L, 0L),
      V2I = c(909L, 909L, 909L, 909L, 909L, 909L, 942L, 942L, 942L, 942L),
      V1I = c(545L, 545L, 545L, 545L, 545L, 545L, 306L, 306L, 306L, 306L),
      CLI = c(471L, 471L, 471L, 471L, 471L, 471L, 405L, 405L, 405L, 405L),
      EVID = c(10101L, 0L, 10101L, 0L, 0L, 0L, 10101L, 0L, 10101L, 0L)
    )

    par2 <-
      matrix(
        c(
          2.02103, 4.839305, 3.518676, -1.391113, 0.108127023, -0.064170725, 0.087765769,
          2.02103, 4.839305, 3.518676, -1.391113, -0.064170725, 0.087765769, 0.108127023
        ),
        nrow = 2, byrow = T,
        dimnames = list(NULL, c(sprintf("THETA[%d]", 1:4), sprintf("ETA[%d]", 1:3)))
      )

    tmp <- rxSolve(mod1, d3, par2, addCov = TRUE, cores = 2, method = meth)

    test_that("Data Frame multi subject solve", {
      expect_equal(
        tmp %>%
          dplyr::select(CLI, V1I, V2I) %>% as.data.frame(),
        d3 %>%
          dplyr::filter(EVID == 0) %>%
          dplyr::select(CLI, V1I, V2I) %>% as.data.frame()
      )
      expect_equal(names(tmp$params)[-1], mod1$params[-(1:3)])
    })

    ## Now check missing covariate values.

    d3na <-
      data.frame(
        ID = c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
        TIME = c(0, 0, 2.99270072992701, 192, 336, 456, 0, 0, 3.07272727272727, 432),
        AMT = c(137L, 0L, -137L, 0L, 0L, 0L, 110L, 0L, -110L, 0L),
        V2I = c(909L, NA_integer_, 909L, 909L, 909L, 909L, 942L, 942L, 942L, 942L),
        V1I = c(545L, 545L, 545L, 545L, 545L, 545L, 306L, 306L, 306L, NA_integer_),
        CLI = c(471L, 471L, 471L, 471L, NA_integer_, 471L, 405L, 405L, 405L, 405L),
        EVID = c(10101L, 0L, 10101L, 0L, 0L, 0L, 10101L, 0L, 10101L, 0L)
      )

    expect_warning(
      tmp <- rxSolve(mod1, d3na, par2, addCov = TRUE, cores = 2, method = meth),
    NA)

    tmp2 <- rxSolve(mod1, d3, par2, addCov = TRUE, cores = 2, method = meth)

    # context(sprintf("Test NA extrapolation for %s solving", meth))
    test_that("NA solve is the same", {
      for (i in c("id", "time", "A_centr", "A_periph", "A_circ", "A_prol", "A_tr1", "A_tr2", "A_tr3")) {
        expect_equal(tmp[[i]], tmp2[[i]])
      }
    })

    d3na <- data.frame(
      ID = c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
      TIME = c(0, 0, 2.99270072992701, 192, 336, 456, 0, 0, 3.07272727272727, 432),
      AMT = c(137L, 0L, -137L, 0L, 0L, 0L, 110L, 0L, -110L, 0L),
      V2I = c(909L, NA_integer_, 909L, 909L, 909L, 909L, 942L, 942L, 942L, 942L),
      V1I = c(545L, 545L, 545L, 545L, 545L, 545L, NA_integer_, NA_integer_, NA_integer_, NA_integer_),
      CLI = c(471L, 471L, 471L, 471L, NA_integer_, 471L, 405L, 405L, 405L, 405L),
      EVID = c(10101L, 0L, 10101L, 0L, 0L, 0L, 10101L, 0L, 10101L, 0L)
    )

    test_that("All covariates are NA give a warning", {
      expect_warning(expect_warning(
        rxSolve(mod1, d3na, par2, addCov = TRUE, cores = 2, method = meth),
        "column 'V1I' has only 'NA' values for id '2'"))
    })
  }

  # time-varying covariates work with ODEs

  test_that("time varying covariates lhs", {

    dfadvan <- data.frame(
      ID = c(
        1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
        1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
        2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
        2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L
      ),
      TIME = c(
        0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 12L, 13L, 14L, 15L,
        16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 0L, 1L, 2L, 3L,
        4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 12L, 13L, 14L, 15L, 16L,
        17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L
      ), AMT = c(
        100L, 0L, 0L,
        0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 100L, 0L, 0L, 0L, 0L,
        0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 100L, 0L, 0L, 0L, 0L, 0L, 0L,
        0L, 0L, 0L, 0L, 0L, 0L, 100L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
        0L, 0L, 0L, 0L
      ), MDV = c(
        1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
        0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
        0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L,
        0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L
      ), CLCR = c(
        120L, 120L,
        120L, 120L, 120L, 120L, 120L, 120L, 120L, 120L, 120L, 120L, 120L,
        120L, 120L, 120L, 30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L,
        30L, 30L, 30L, 30L, 30L, 30L, 120L, 120L, 120L, 120L, 120L, 120L,
        120L, 120L, 120L, 120L, 120L, 120L, 120L, 120L, 120L, 120L, 120L,
        120L, 120L, 120L, 120L
      )
    )

    mod <- rxode2({
      CLpop <- 2 # clearance
      Vpop <- 10 # central volume of distribution
      CL <- CLpop * (CLCR / 100)
      V <- Vpop
    })

    mod2 <- rxode2({
      CLpop <- 2 # clearance
      Vpop <- 10 # central volume of distribution
      CL <- CLpop * (CLCR / 100)
      V <- Vpop
      d / dt(matt) <- 0
    })

    x1 <- rxSolve(mod, dfadvan, keep = "CL")
    x2 <- rxSolve(mod2, dfadvan, keep = "CL")
    expect_equal(x1$CL, x2$CL)
  })
})
