rxTest({

  expectSimWithout <- function(x) {
    expect_null(x$thetaMat)
    expect_null(x$omegaList)
    expect_null(x$sigmaList)
  }

  expectSimTheta <- function(x) {
    expect_type(x$thetaMat,"double")
    expect_true(inherits(x$thetaMat, "matrix"))
    expect_null(x$omegaList)
    expect_null(x$sigmaList)
  }

  expectSimAll <- function(x) {
    expect_type(x$thetaMat,"double")
    expect_true(inherits(x$thetaMat, "matrix"))
    expect_true(inherits(x$omegaList, "list"))
    expect_type(x$omegaList[[1]], "double")
    expect_true(inherits(x$omegaList[[1]], "matrix"))
    expect_true(inherits(x$sigmaList, "list"))
    expect_type(x$sigmaList[[1]], "double")
    expect_true(inherits(x$sigmaList[[1]], "matrix"))
  }

  for (meth in c("dop853", "liblsoda", "lsoda")) {
    # context(sprintf("Test Parallel/Multi-subject Solve (%s)", meth))

    mod <- rxode2({
      d/dt(intestine) <- -a * intestine
      d/dt(blood) <- a * intestine - b * blood
    })

    et <- eventTable(time.units = "days")
    et$add.sampling(seq(0, 10, length.out = 50))
    et$add.dosing(
      dose = 2 / 24, rate = 2, strt.time = 0,
      nbr.doses = 10, dosing.interval = 1
    )

    p <- data.frame(a = 6, b = seq(0.4, 0.9, length.out = 4))

    pk1 <- suppressWarnings(rxSolve(mod, p, et, cores = 1, method = meth))

    expectSimWithout(pk1)

    pk2 <- suppressWarnings(rxSolve(mod, p, et, cores = 2, method = meth)) # CRAN requirement of at most 2 cores.

    expectSimWithout(pk2)

    test_that("Parallel Solve gives same results a single threaded solve", {
      expect_equal(as.data.frame(pk1), as.data.frame(pk2))
    })

    ## Test mixed solved and ODEs
    mod2 <- rxode2({
      ## the order of variables do not matter, the type of compartmental
      ## model is determined by the parameters specified.
      CL ~ TCL * exp(eta.Cl)
      C2 ~ linCmt(KA, CL, V2, Q, V3)
      eff(0) <- 1 ## This specifies that the effect compartment starts at 1.
      d / dt(eff) ~ Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
      ##
      resp <- eff + err1
      pk <- C2 * exp(err2)
    })


    ev <- eventTable(amount.units = "mg", time.units = "hours") %>%
      add.dosing(dose = 10000, nbr.doses = 10, dosing.interval = 12, dosing.to = 2) %>%
      add.dosing(dose = 20000, nbr.doses = 5, start.time = 120, dosing.interval = 24, dosing.to = 2) %>%
      add.sampling(0:240)

    ## Add Residual differences
    sigma <- diag(2) * 0.05
    dimnames(sigma) <- list(c("err1", "err2"), c("err1", "err2"))

    pk3 <-
      suppressWarnings(rxSolve(
        mod2,
        c(KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02, Kin = 1, Kout = 1, EC50 = 200),
        omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
        nSub = 4, ev, sigma = sigma, cores = 2, method = meth,
        ))

    expectSimWithout(pk3)

    pk3a <-
      suppressWarnings(rxSolve(
        mod2,
        c(KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02, Kin = 1, Kout = 1, EC50 = 200),
        omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
        nSub = 4, ev, sigma = sigma, cores = 2, method = meth, addDosing = TRUE
      ))

    expectSimWithout(pk3a)

    pk4 <-
      suppressWarnings(rxSolve(
        mod2,
        c(KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02, Kin = 1, Kout = 1, EC50 = 200),
        omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
        nSub = 4, ev, sigma = sigma, cores = 1, method = meth
      ))

    expectSimWithout(pk4)

    test_that("Can solve the system.", {
      expect_true(rxIs(pk3, "data.frame"))
      expect_false(all(pk3$pk == 0))
      expect_true(rxIs(pk3a, "data.frame"))
      expect_false(all(pk3a$pk == 0))
      expect_true(rxIs(pk4, "data.frame"))
      expect_false(all(pk4$pk == 0))
    })

    mod3 <- rxode2({
      C2 <- prod(centr, 1 / V2)
      C3 ~ prod(peri, 1 / V3)
      CL ~ prod(TCL, exp(eta.Cl))
      d/dt(depot) ~ prod(-KA, depot)
      d/dt(centr) ~ sum(prod(KA, depot), -prod(CL, C2), -prod(Q, C2), prod(Q, C3))
      d/dt(peri) ~ sum(prod(Q, C2), -prod(Q, C3))
      d/dt(eff) <- sum(Kin, -prod(Kout, sum(1, -prod(C2, 1 / sum(EC50, C2))), eff))
      e1 <- err1
      e2 <- err2
      resp <- sum(eff, e1)
      pk <- prod(C2, exp(e2))
    })

    pk3 <-
      suppressWarnings(rxSolve(
        mod2,
        c(KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02, Kin = 1, Kout = 1, EC50 = 200),
        omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
        nSub = 4, ev, sigma = sigma, cores = 2, method = meth
      ))

    expectSimWithout(pk3)

    pk4 <-
      suppressWarnings(rxSolve(
        mod2,
        c(KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02, Kin = 1, Kout = 1, EC50 = 200),
        omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
        nSub = 4, ev, sigma = sigma, cores = 1, method = meth
      ))

    expectSimWithout(pk4)

    test_that("Can solve the system.", {
      expect_false(all(pk3$pk == 0))
      expect_true(rxIs(pk3, "data.frame"))
      expect_false(all(pk3$pk == 0))
      expect_true(rxIs(pk4, "data.frame"))
    })

    mod2 <- rxode2({
      C2 <- centr / V2
      C3 ~ peri / V3
      CL ~ TCL * exp(eta.Cl)
      d/dt(depot) ~ -KA * depot
      d/dt(centr) ~ KA * depot - CL * C2 - Q * C2 + Q * C3
      d/dt(peri) ~ Q * C2 - Q * C3
      d/dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
      eff(0) <- 1000
      e1 <- err1
      e2 <- err2
      resp <- eff + e1
      pk <- C2 * exp(e2)
    })

    pk3 <-
      suppressWarnings(rxSolve(
        mod2,
        c(KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02, Kin = 1, Kout = 1, EC50 = 200),
        omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
        nSub = 4, ev, sigma = sigma, cores = 2, method = meth
      ))

    expectSimWithout(pk3)

    pk4 <-
      suppressWarnings(rxSolve(
        mod2,
        c(KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02, Kin = 1, Kout = 1, EC50 = 200),
        omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
        nSub = 4, ev, sigma = sigma, cores = 1, method = meth
      ))

    expectSimWithout(pk4)

    test_that("Can solve the system.", {
      expect_false(all(pk3$pk == 0))
      expect_true(rxIs(pk3, "data.frame"))
      expect_false(all(pk4$pk == 0))
      expect_true(rxIs(pk4, "data.frame"))
    })

    pk2 <-
      suppressWarnings(rxSolve(
        mod2,
        c(KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02, Kin = 1, Kout = 1, EC50 = 200, err1 = 0, err2 = 0),
        omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
        nSub = 4, ev, cores = 1, method = meth
      ))

    expectSimWithout(pk2)

    pk3 <-
      suppressWarnings(rxSolve(
        mod2,
        c(KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02, Kin = 1, Kout = 1, EC50 = 200, err1 = 0, err2 = 0),
        omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
        nSub = 4, ev, cores = 2, method = meth
      ))

    expectSimWithout(pk3)


    ## "Study" Differences
    thetaMat <- diag(3) * 0.01
    dimnames(thetaMat) <- list(NULL, c("KA", "TCL", "V2"))

    pk4 <-
      suppressWarnings(rxSolve(
        mod2,
        c(KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02, Kin = 1, Kout = 1, EC50 = 200),
        omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
        nSub = 4, nStud = 4, thetaMat = thetaMat, sigma = sigma, ev, cores = 1, method = meth
      ))

    expectSimTheta(pk4)

    pk5 <-
      suppressWarnings(rxSolve(
        mod2,
        c(KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02, Kin = 1, Kout = 1, EC50 = 200),
        omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
        nSub = 4, nStud = 4, thetaMat = thetaMat, sigma = sigma, ev, cores = 2, method = meth
      ))

    expectSimTheta(pk5)

    pk6 <-
      suppressWarnings(rxSolve(
        mod2,
        c(KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02, Kin = 1, Kout = 1, EC50 = 200),
        omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
        nSub = 4, nStud = 4, thetaMat = thetaMat, sigma = sigma, ev, cores = 1, dfSub = 4, dfObs = 4,
        method = meth
      ))

    expectSimAll(pk6)

    pk7 <-
      suppressWarnings(rxSolve(
        mod2,
        c(KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02, Kin = 1, Kout = 1, EC50 = 200),
        omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
        nSub = 4, nStud = 4, thetaMat = thetaMat, sigma = sigma, ev, cores = 2, dfSub = 4, dfObs = 4,
        method = meth
      ))

    expectSimAll(pk7)

    test_that("Can solve the system.", {
      expect_true(rxIs(pk2, "data.frame"))
      expect_null(pk2$thetaMat)
      expect_null(pk2$omegaList)
      expect_null(pk2$sigmaList)
      expect_true(rxIs(pk3, "data.frame"))
      expect_null(pk3$thetaMat)
      expect_null(pk3$omegaList)
      expect_null(pk3$sigmaList)
      expect_true(rxIs(pk4, "data.frame"))
      expect_true(is(pk4$thetaMat, "matrix"))
      expect_null(pk4$omegaList)
      expect_null(pk4$sigmaList)
      expect_true(rxIs(pk5, "data.frame"))
      expect_true(is(pk5$thetaMat, "matrix"))
      expect_null(pk5$omegaList)
      expect_null(pk5$sigmaList)
      expect_true(rxIs(pk6, "data.frame"))
      expect_true(is(pk6$thetaMat, "matrix"))
      expect_type(pk6$omegaList, "list")
      expect_type(pk6$sigmaList, "list")
      expect_true(rxIs(pk7, "data.frame"))
      expect_true(is.matrix(pk7$thetaMat))
      expect_type(pk7$omegaList, "list")
      expect_type(pk7$sigmaList, "list")
    })
  }

  test_that("Can solve the system.", {

    ## Now Try multi-subject data.
    skip_if_not(file.exists(test_path("test-data-setup.qs")))
    dat <- qs::qread(test_path("test-data-setup.qs"))

    pk7a <-
      suppressWarnings(rxSolve(
        mod2,
        c(KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02, Kin = 1, Kout = 1, EC50 = 200),
        omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
        sigma = sigma, dat, cores = 1, method = meth
      ))

    expectSimWithout(pk7a)

    pk8 <-
      suppressWarnings(rxSolve(
        mod2,
        c(KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02, Kin = 1, Kout = 1, EC50 = 200),
        omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
        thetaMat = thetaMat, sigma = sigma, dat, nStud = 4, cores = 1, method = meth
      ))

    expectSimTheta(pk8)

    pk9 <-
      suppressWarnings(rxSolve(
        mod2,
        c(KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02, Kin = 1, Kout = 1, EC50 = 200),
        omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
        thetaMat = thetaMat, sigma = sigma, dat, nStud = 4, cores = 2, method = meth
      ))

    expectSimTheta(pk9)

    pk10 <-
      suppressWarnings(rxSolve(
        mod2,
        c(KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02, Kin = 1, Kout = 1, EC50 = 200),
        omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
        thetaMat = thetaMat, sigma = sigma, dat, nStud = 4, cores = 2, method = meth, simVariability = FALSE
      ))

    expectSimWithout(pk10)

    pk11 <-
      suppressWarnings(rxSolve(
        mod2,
        c(KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02, Kin = 1, Kout = 1, EC50 = 200),
        omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
        thetaMat = thetaMat, sigma = sigma, dat, nStud = 4, cores = 1, method = meth, simVariability = FALSE
      ))

    expectSimWithout(pk11)

    pk12 <-
      suppressWarnings(rxSolve(
        mod2,
        c(KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02, Kin = 1, Kout = 1, EC50 = 200),
        omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
        thetaMat = thetaMat, sigma = sigma, dat, nStud = 1, cores = 1, method = meth, dfSub = 4, dfObs = 4
      ))

    expectSimWithout(pk12)

    pk13 <-
      suppressWarnings(rxSolve(
        mod2,
        c(KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02, Kin = 1, Kout = 1, EC50 = 200),
        omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
        thetaMat = thetaMat, sigma = sigma, dat, nStud = 1, cores = 1, method = meth, dfSub = 4, dfObs = 4, simVariability=TRUE
      ))

    expectSimAll(pk13)

    expect_true(rxIs(pk8, "data.frame"))
    expect_true(rxIs(pk9, "data.frame"))
    expect_true(rxIs(pk10, "data.frame"))
    expect_true(rxIs(pk11, "data.frame"))

  })
})
