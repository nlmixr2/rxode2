rxTest({
  test_that("evid=3 reset time", {

    mod1 <- rxode2({
      KA <- 2.94E-01
      CL <- 1.86E+01
      V2 <- 4.02E+01
      Q <- 1.05E+01
      V3 <- 2.97E+02
      Kin <- 1
      Kout <- 1
      EC50 <- 200
      C2 <- centr / V2
      C3 <- peri / V3
      d / dt(depot) <- -KA * depot
      d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
      d / dt(peri) <- Q * C2 - Q * C3
      d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
      eff(0) <- 1
      printf("%f\n", time)
    })

    et_1 <-
      et(dose = 10000, addl = 0, ii = 0) |>
      et(0:8)

    et_reset <- et(evid = 3)

    et_2 <-
      et(dose = 20000, addl = 0, ii = 0) |>
      et(0:8)

    et <- dplyr::bind_rows(et_1, et_reset, et_2)

    tmp <- etTrans(et, mod1)

    expect_true(!identical(tmp$TIME, et$time))

    tmp <- as.data.frame(tmp)

    expect_equal(tmp$TIME, et$time)

    et2 <- rbind(data.frame(id=1, et[, names(et) != "id"]),
                 data.frame(id=2, et[, names(et) != "id"]))

    tmp <- etTrans(et2, mod1)

    expect_true(!identical(tmp$TIME, et2$time))

    tmp <- as.data.frame(tmp)

    expect_equal(tmp$TIME, et2$time)

    t <- tempfile("test-evid3", fileext = ".csv")

    suppressWarnings(.rxWithSink(t, {
      cat("t\n")
      x <- rxSolve(mod1, et)
    }))

    d <- read.csv(t)
    unlink(t)

    expect_true(all(d$t < 9))
    expect_true(all(x$time < 9))
    expect_true(!all(x$C2[x$resetno == 1] == x$C2[x$resetno == 2]))
    expect_true(x$eff[1] == 1)
  })


  test_that("evid=3 reset time mixed", {
    mod1 <- rxode2({
      KA <- 2.94E-01
      CL <- 1.86E+01
      V2 <- 4.02E+01
      Q <- 1.05E+01
      V3 <- 2.97E+02
      Kin <- 1
      Kout <- 1
      EC50 <- 200
      C2 <- linCmt()
      d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
      eff(0) <- 1
      printf("%f\n", time)
    })

    et_1 <-
      et(dose = 10000, addl = 0, ii = 0) |>
      et(0:8)

    et_reset <- et(evid = 3)

    et_2 <-
      et(dose = 20000, addl = 0, ii = 0) |>
      et(0:8)

    et <- dplyr::bind_rows(et_1, et_reset, et_2)

    t <- tempfile("test-evid3", fileext = ".csv")

    suppressWarnings(.rxWithSink(t, {
      cat("t\n")
      x <- rxSolve(mod1, et)
    }))

    d <- read.csv(t)
    unlink(t)

    expect_true(all(d$t < 9))
    expect_true(all(x$time < 9))
    expect_true(!all(x$C2[x$resetno == 1] == x$C2[x$resetno == 2]))
    expect_true(x$eff[1] == 1)
  })



  test_that("evid=3 reset time linCmt", {

    mod1 <- rxode2({
      KA <- 2.94E-01
      CL <- 1.86E+01
      V2 <- 4.02E+01
      Q <- 1.05E+01
      V3 <- 2.97E+02
      Kin <- 1
      Kout <- 1
      EC50 <- 200
      C2 <- linCmt()
      printf("%f\n", time)
    })

    et1 <-
      # et(amount.units='mg', time.units='hours') |>
      et(dose = 10000, addl = 0, ii = 0) |>
      # et(amt=20000, nbr.doses=5, start.time=120, dosing.interval=24) |>
      et(0:8)

    et_reset <- et(evid = 3)

    et_2 <-
      # et(amount.units='mg', time.units='hours') |>
      et(dose = 20000, addl = 0, ii = 0) |>
      # et(amt=20000, nbr.doses=5, start.time=120, dosing.interval=24) |>
      et(0:8)

    et <- dplyr::bind_rows(et1, et_reset, et_2)

    t <- tempfile("test-evid3", fileext = ".csv")

    suppressWarnings(.rxWithSink(t, {
      cat("t\n")
      x <- rxSolve(mod1, et)
    }))

    d <- read.csv(t)
    unlink(t)

    expect_true(all(d$t < 9))
    expect_true(all(x$time < 9))
    expect_true(!all(x$C2[x$resetno == 1] == x$C2[x$resetno == 2]))
  })

  test_that("warning for unsorted data with evid=3", {
    mod1 <- rxode2({
      KA <- 2.94E-01
      CL <- 1.86E+01
      V2 <- 4.02E+01
      Q <- 1.05E+01
      V3 <- 2.97E+02
      Kin <- 1
      Kout <- 1
      EC50 <- 200
      C2 <- linCmt()
    })

    et1 <-
      et(dose = 10000, addl = 0, ii = 0) |>
      et(0:8)

    etReset <- et(evid = 3)

    et2 <-
      et(dose = 20000, addl = 0, ii = 0) |>
      et(0:8)

    et <- dplyr::bind_rows(et1, etReset, et2)

    et$time[5] <- 9

    expect_warning(x <- rxSolve(mod1, et))
    expect_false(any(names(x) == "resetno"))
  })

  test_that("tad is non-negative after SS dosing (EVID=3+110 reset)", {
    # Regression test: SS dosing inserts EVID=3 (time reset) + EVID=110 (SS bolus).
    # handleTlastInline was called for EVID=3 which has cmt=-1, corrupting tlast
    # and producing negative tad values for observations after the SS reset.

    # ODE model
    mod_ode <- rxode2({
      KA <- 2.94E-01
      CL <- 1.86E+01
      V2 <- 4.02E+01
      d/dt(depot) <- -KA * depot
      d/dt(centr) <- KA * depot - CL / V2 * centr
      cp <- centr / V2
      t_ad <- tad()
    })

    # linCmt model
    mod_lin <- rxode2({
      KA <- 2.94E-01
      CL <- 1.86E+01
      V2 <- 4.02E+01
      cp <- linCmt()
      t_ad <- tad()
    })

    # Multi-subject event table: SS bolus (ss=1) then repeated doses.
    # Using 3 subjects with slightly different observation schedules so
    # maxShift (computed globally) is > 0, which was the trigger for the bug.
    ev <- dplyr::bind_rows(
      data.frame(id = 1, time = 0,  evid = 4, amt = 320, ii = 12, ss = 1, cmt = 1),
      data.frame(id = 1, time = 0,  evid = 0, amt = 0,   ii = 0,  ss = 0, cmt = 1, dv = NA),
      data.frame(id = 1, time = 0.5, evid = 0, amt = 0,  ii = 0,  ss = 0, cmt = 1, dv = NA),
      data.frame(id = 1, time = 1,  evid = 0, amt = 0,   ii = 0,  ss = 0, cmt = 1, dv = NA),
      data.frame(id = 1, time = 12, evid = 101, amt = 320, ii = 0, ss = 0, cmt = 1),
      data.frame(id = 1, time = 12.5, evid = 0, amt = 0, ii = 0,  ss = 0, cmt = 1, dv = NA),
      data.frame(id = 2, time = 0,  evid = 4, amt = 320, ii = 12, ss = 1, cmt = 1),
      data.frame(id = 2, time = 0.3, evid = 0, amt = 0,  ii = 0,  ss = 0, cmt = 1, dv = NA),
      data.frame(id = 2, time = 1,  evid = 0, amt = 0,   ii = 0,  ss = 0, cmt = 1, dv = NA),
      data.frame(id = 2, time = 12, evid = 101, amt = 320, ii = 0, ss = 0, cmt = 1),
      data.frame(id = 2, time = 12.8, evid = 0, amt = 0, ii = 0,  ss = 0, cmt = 1, dv = NA),
      data.frame(id = 3, time = 0,  evid = 4, amt = 320, ii = 12, ss = 1, cmt = 1),
      data.frame(id = 3, time = 0.2, evid = 0, amt = 0,  ii = 0,  ss = 0, cmt = 1, dv = NA),
      data.frame(id = 3, time = 12, evid = 101, amt = 320, ii = 0, ss = 0, cmt = 1),
      data.frame(id = 3, time = 12.4, evid = 0, amt = 0, ii = 0,  ss = 0, cmt = 1, dv = NA)
    )

    suppressWarnings({
      x_ode <- rxSolve(mod_ode, ev)
      x_lin <- rxSolve(mod_lin, ev)
    })

    obs_ode <- x_ode[!is.na(x_ode$cp), ]
    obs_lin <- x_lin[!is.na(x_lin$cp), ]

    expect_true(all(obs_ode$t_ad >= 0),
                info = paste("ODE model: negative tad values found:",
                             paste(obs_ode$t_ad[obs_ode$t_ad < 0], collapse = ", ")))
    expect_true(all(obs_lin$t_ad >= 0),
                info = paste("linCmt model: negative tad values found:",
                             paste(obs_lin$t_ad[obs_lin$t_ad < 0], collapse = ", ")))
  })
})
