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
})
