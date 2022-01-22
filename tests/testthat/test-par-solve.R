for (meth in c("dop853", "liblsoda", "lsoda")) {
  # context(sprintf("Test Parallel/Multi-subject Solve (%s)", meth))
  
  .rxSolve <- function(...) {
    suppressWarnings({
      rxSolve(...)
    })
  }
  
  mod <- rxode2({
    d / dt(intestine) <- -a * intestine
    d / dt(blood) <- a * intestine - b * blood
  })
  
  et <- eventTable(time.units = "days")
  et$add.sampling(seq(0, 10, length.out = 50))
  et$add.dosing(
    dose = 2 / 24, rate = 2, strt.time = 0,
    nbr.doses = 10, dosing.interval = 1
  )
  
  p <- data.frame(a = 6, b = seq(0.4, 0.9, length.out = 4))
  
  pk1 <- .rxSolve(mod, p, et, cores = 1, method = meth)
  
  pk2 <- .rxSolve(mod, p, et, cores = 2, method = meth) # CRAN requirement of at most 2 cores.
  
  
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
  
  pk3 <- .rxSolve(mod2, c(
    KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
    Kin = 1, Kout = 1, EC50 = 200
  ),
  omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
  nSub = 4, ev, sigma = sigma, cores = 2, method = meth,
  )
  
  
  pk3a <- .rxSolve(mod2, c(
    KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
    Kin = 1, Kout = 1, EC50 = 200
  ),
  omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
  nSub = 4, ev, sigma = sigma, cores = 2, method = meth, addDosing = TRUE
  )
  
  pk4 <- .rxSolve(mod2, c(
    KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
    Kin = 1, Kout = 1, EC50 = 200
  ),
  omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
  nSub = 4, ev, sigma = sigma, cores = 1, method = meth
  )
  
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
    d / dt(depot) ~ prod(-KA, depot)
    d / dt(centr) ~ sum(prod(KA, depot), -prod(CL, C2), -prod(Q, C2), prod(Q, C3))
    d / dt(peri) ~ sum(prod(Q, C2), -prod(Q, C3))
    d / dt(eff) <- sum(Kin, -prod(Kout, sum(1, -prod(C2, 1 / sum(EC50, C2))), eff))
    e1 <- err1
    e2 <- err2
    resp <- sum(eff, e1)
    pk <- prod(C2, exp(e2))
  })
  
  pk3 <- .rxSolve(mod2, c(
    KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
    Kin = 1, Kout = 1, EC50 = 200
  ),
  omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
  nSub = 4, ev, sigma = sigma, cores = 2, method = meth
  )
  
  pk4 <- .rxSolve(mod2, c(
    KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
    Kin = 1, Kout = 1, EC50 = 200
  ),
  omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
  nSub = 4, ev, sigma = sigma, cores = 1, method = meth
  )
  
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
    d / dt(depot) ~ -KA * depot
    d / dt(centr) ~ KA * depot - CL * C2 - Q * C2 + Q * C3
    d / dt(peri) ~ Q * C2 - Q * C3
    d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
    eff(0) <- 1000
    e1 <- err1
    e2 <- err2
    resp <- eff + e1
    pk <- C2 * exp(e2)
  })
  
  pk3 <- .rxSolve(mod2, c(
    KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
    Kin = 1, Kout = 1, EC50 = 200
  ),
  omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
  nSub = 4, ev, sigma = sigma, cores = 2, method = meth
  )
  
  pk4 <- .rxSolve(mod2, c(
    KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
    Kin = 1, Kout = 1, EC50 = 200
  ),
  omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
  nSub = 4, ev, sigma = sigma, cores = 1, method = meth
  )
  
  test_that("Can solve the system.", {
    expect_false(all(pk3$pk == 0))
    expect_true(rxIs(pk3, "data.frame"))
    expect_false(all(pk4$pk == 0))
    expect_true(rxIs(pk4, "data.frame"))
  })
  
  pk2 <- .rxSolve(mod2, c(
    KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
    Kin = 1, Kout = 1, EC50 = 200, err1 = 0, err2 = 0
  ),
  omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
  nSub = 4, ev, cores = 1, method = meth
  )
  
  pk3 <- .rxSolve(mod2, c(
    KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
    Kin = 1, Kout = 1, EC50 = 200, err1 = 0, err2 = 0
  ),
  omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
  nSub = 4, ev, cores = 2, method = meth
  )
  
  ## "Study" Differences
  thetaMat <- diag(3) * 0.01
  dimnames(thetaMat) <- list(NULL, c("KA", "TCL", "V2"))
  
  pk4 <- .rxSolve(mod2, c(
    KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
    Kin = 1, Kout = 1, EC50 = 200
  ),
  omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
  nSub = 4, nStud = 4, thetaMat = thetaMat, sigma = sigma, ev, cores = 1, method = meth
  )
  
  pk5 <- .rxSolve(mod2, c(
    KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
    Kin = 1, Kout = 1, EC50 = 200
  ),
  omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
  nSub = 4, nStud = 4, thetaMat = thetaMat, sigma = sigma, ev, cores = 2, method = meth
  )
  
  pk6 <- .rxSolve(mod2, c(
    KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
    Kin = 1, Kout = 1, EC50 = 200
  ),
  omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
  nSub = 4, nStud = 4, thetaMat = thetaMat, sigma = sigma, ev, cores = 1, dfSub = 4, dfObs = 4,
  method = meth
  )
  
  pk7 <- .rxSolve(mod2, c(
    KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
    Kin = 1, Kout = 1, EC50 = 200
  ),
  omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
  nSub = 4, nStud = 4, thetaMat = thetaMat, sigma = sigma, ev, cores = 2, dfSub = 4, dfObs = 4,
  method = meth
  )
  
  test_that("Can solve the system.", {
    expect_true(rxIs(pk2, "data.frame"))
    expect_equal(class(pk2$thetaMat), "NULL")
    expect_equal(class(pk2$omegaList), "NULL")
    expect_equal(class(pk2$sigmaList), "NULL")
    expect_true(rxIs(pk3, "data.frame"))
    expect_equal(class(pk3$thetaMat), "NULL")
    expect_equal(class(pk3$omegaList), "NULL")
    expect_equal(class(pk3$sigmaList), "NULL")
    expect_true(rxIs(pk4, "data.frame"))
    expect_true(is(pk4$thetaMat, "matrix"))
    expect_equal(class(pk4$omegaList), "NULL")
    expect_equal(class(pk4$sigmaList), "NULL")
    expect_true(rxIs(pk5, "data.frame"))
    expect_true(is(pk5$thetaMat, "matrix"))
    expect_equal(class(pk5$omegaList), "NULL")
    expect_equal(class(pk5$sigmaList), "NULL")
    expect_true(rxIs(pk6, "data.frame"))
    expect_true(is(pk6$thetaMat, "matrix"))
    expect_equal(class(pk6$omegaList), "list")
    expect_equal(class(pk6$sigmaList), "list")
    expect_true(rxIs(pk7, "data.frame"))
    expect_true(is(pk7$thetaMat, "matrix"))
    expect_equal(class(pk7$omegaList), "list")
    expect_equal(class(pk7$sigmaList), "list")
  })
}

test_that("Can solve the system.", {
  ## Now Try multi-subject data.
  skip_if(!file.exists(test_path("test-data-setup.qs")))
  dat <- qs::qread(test_path("test-data-setup.qs"))
  
  pk7a <- .rxSolve(mod2, c(
    KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
    Kin = 1, Kout = 1, EC50 = 200
  ),
  omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
  sigma = sigma, dat, cores = 1, method = meth
  )
  
  pk8 <- .rxSolve(mod2, c(
    KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
    Kin = 1, Kout = 1, EC50 = 200
  ),
  omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
  thetaMat = thetaMat, sigma = sigma, dat, nStud = 4, cores = 1, method = meth
  )
  
  pk9 <- .rxSolve(mod2, c(
    KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
    Kin = 1, Kout = 1, EC50 = 200
  ),
  omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
  thetaMat = thetaMat, sigma = sigma, dat, nStud = 4, cores = 2, method = meth
  )
  
  pk10 <- .rxSolve(mod2, c(
    KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
    Kin = 1, Kout = 1, EC50 = 200
  ),
  omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
  thetaMat = thetaMat, sigma = sigma, dat, nStud = 4, cores = 2, method = meth, simVariability = FALSE
  )
  
  pk11 <- .rxSolve(mod2, c(
    KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
    Kin = 1, Kout = 1, EC50 = 200
  ),
  omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
  thetaMat = thetaMat, sigma = sigma, dat, nStud = 4, cores = 1, method = meth, simVariability = FALSE
  )
  
  expect_true(rxIs(pk8, "data.frame"))
  expect_true(rxIs(pk9, "data.frame"))
  expect_true(rxIs(pk10, "data.frame"))
  expect_true(rxIs(pk11, "data.frame"))
})
